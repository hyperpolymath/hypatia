# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.VCL.RemoteCache do
  @moduledoc """
  VCL Remote Cache -- git-clone-based cache for remote verisim-data stores.

  Enables VCL multi-store federation by cloning or pulling remote verisim-data
  repositories into a local cache directory. Each remote URL maps to a unique
  subdirectory under the cache root. Clones are reused within their TTL window;
  after expiry the next access triggers a `git pull` to refresh.

  ## Cache Layout

      /tmp/hypatia-vcl-cache/
      ├── <sha256-of-url-1>/    # git clone of remote store 1
      ├── <sha256-of-url-2>/    # git clone of remote store 2
      └── ...

  ## Thread Safety

  An ETS table (`hypatia_remote_cache`) tracks per-URL metadata (clone path,
  last-fetched timestamp). A per-URL lock prevents concurrent clones of the
  same repository -- the first caller clones while others wait.

  ## Supported URL Schemes

  - `https://` git URLs (GitHub, GitLab, Bitbucket, self-hosted)
  - `git@` SSH URLs are NOT supported (requires SSH key management)
  - GitHub raw content URLs are normalised to their repository clone URL
  """

  require Logger

  @default_cache_dir "/tmp/hypatia-vcl-cache"
  @default_ttl_seconds 300
  @ets_table :hypatia_remote_cache
  # Note: System.cmd/3 does not support :timeout in Elixir 1.19+.
  # Git operations are bounded by the remote server's TCP timeout.

  # ---------------------------------------------------------------------------
  # Public API
  # ---------------------------------------------------------------------------

  @doc """
  Ensure a remote verisim-data repository is cached locally.

  Clones the repository on first access, or runs `git pull` if the existing
  clone has exceeded its TTL. Returns the absolute path to the local clone
  directory on success.

  ## Options

    * `:ttl` -- time-to-live in seconds before a pull is triggered (default: 300)
    * `:cache_dir` -- override the cache root directory (default: `/tmp/hypatia-vcl-cache`)

  ## Examples

      {:ok, "/tmp/hypatia-vcl-cache/a1b2c3..."} =
        RemoteCache.cache_remote_store("https://github.com/org/verisim-data")

  ## Returns

    * `{:ok, local_path}` -- the local clone is ready for querying
    * `{:error, reason}` -- clone or pull failed
  """
  @spec cache_remote_store(String.t(), keyword()) :: {:ok, String.t()} | {:error, String.t()}
  def cache_remote_store(url, opts \\ []) do
    ttl = Keyword.get(opts, :ttl, @default_ttl_seconds)
    cache_dir = Keyword.get(opts, :cache_dir, @default_cache_dir)

    # Normalise GitHub raw content URLs to clone-able repository URLs.
    clone_url = normalise_url(url)

    # Derive a stable, filesystem-safe directory name from the URL.
    url_hash = :crypto.hash(:sha256, clone_url) |> Base.encode16(case: :lower) |> String.slice(0, 16)
    local_path = Path.join(cache_dir, url_hash)

    ensure_ets()

    # Per-URL lock: only one process clones/pulls a given URL at a time.
    # Others wait by polling every 250ms until the lock is released.
    lock_key = {:lock, url_hash}

    case acquire_lock(lock_key) do
      :acquired ->
        try do
          result = do_cache(clone_url, local_path, ttl, cache_dir)
          result
        after
          release_lock(lock_key)
        end

      :wait ->
        wait_for_lock(lock_key, local_path, _attempts = 0)
    end
  end

  @doc """
  Evict a specific URL from the cache, removing both the ETS entry and
  the cloned directory on disk.

  ## Returns

    * `:ok` -- entry removed (or was not present)
    * `{:error, reason}` -- filesystem removal failed
  """
  @spec evict(String.t(), keyword()) :: :ok | {:error, String.t()}
  def evict(url, opts \\ []) do
    cache_dir = Keyword.get(opts, :cache_dir, @default_cache_dir)
    clone_url = normalise_url(url)
    url_hash = :crypto.hash(:sha256, clone_url) |> Base.encode16(case: :lower) |> String.slice(0, 16)
    local_path = Path.join(cache_dir, url_hash)

    ensure_ets()
    :ets.delete(@ets_table, url_hash)

    if File.exists?(local_path) do
      case File.rm_rf(local_path) do
        {:ok, _} -> :ok
        {:error, reason, _} -> {:error, "Failed to remove #{local_path}: #{inspect(reason)}"}
      end
    else
      :ok
    end
  end

  @doc """
  Evict all cache entries whose TTL has expired. Intended for periodic
  cleanup (e.g. from a GenServer timer or scheduled task).

  ## Returns

  The number of entries evicted.
  """
  @spec evict_expired(keyword()) :: non_neg_integer()
  def evict_expired(opts \\ []) do
    ttl = Keyword.get(opts, :ttl, @default_ttl_seconds)
    cache_dir = Keyword.get(opts, :cache_dir, @default_cache_dir)
    now = System.monotonic_time(:second)

    ensure_ets()

    expired =
      :ets.tab2list(@ets_table)
      |> Enum.filter(fn
        {_hash, %{fetched_at: fetched_at}} -> now - fetched_at > ttl
        _ -> false
      end)

    Enum.each(expired, fn {hash, _meta} ->
      local_path = Path.join(cache_dir, hash)
      :ets.delete(@ets_table, hash)

      if File.exists?(local_path) do
        File.rm_rf(local_path)
      end
    end)

    length(expired)
  end

  @doc """
  List all currently cached entries with their metadata.

  ## Returns

  A list of maps: `[%{url_hash: String.t(), path: String.t(), fetched_at: integer(), url: String.t()}]`
  """
  @spec list_cached() :: [map()]
  def list_cached do
    ensure_ets()

    :ets.tab2list(@ets_table)
    |> Enum.filter(fn
      {{:lock, _}, _} -> false
      _ -> true
    end)
    |> Enum.map(fn {hash, meta} ->
      %{
        url_hash: hash,
        path: Map.get(meta, :path, ""),
        fetched_at: Map.get(meta, :fetched_at, 0),
        url: Map.get(meta, :url, "")
      }
    end)
  end

  # ---------------------------------------------------------------------------
  # Internal: Clone / Pull Logic
  # ---------------------------------------------------------------------------

  # Perform the actual clone or pull, updating ETS metadata on success.
  @spec do_cache(String.t(), String.t(), non_neg_integer(), String.t()) ::
          {:ok, String.t()} | {:error, String.t()}
  defp do_cache(clone_url, local_path, ttl, cache_dir) do
    now = System.monotonic_time(:second)
    url_hash = Path.basename(local_path)

    case :ets.lookup(@ets_table, url_hash) do
      [{^url_hash, %{fetched_at: fetched_at}}] when now - fetched_at < ttl ->
        # Cache hit -- still within TTL.
        Logger.debug("VCL RemoteCache: cache hit for #{url_hash} (age: #{now - fetched_at}s)")
        {:ok, local_path}

      _ ->
        # Cache miss or expired -- clone or pull.
        if File.dir?(Path.join(local_path, ".git")) do
          pull_existing(clone_url, local_path, url_hash, now)
        else
          clone_fresh(clone_url, local_path, url_hash, now, cache_dir)
        end
    end
  end

  # Clone a repository for the first time.
  defp clone_fresh(clone_url, local_path, url_hash, now, cache_dir) do
    File.mkdir_p!(cache_dir)

    # Use --depth 1 to minimise bandwidth and disk usage. The federation
    # layer only reads HEAD files, not history.
    Logger.info("VCL RemoteCache: cloning #{clone_url} -> #{local_path}")

    case System.cmd("git", ["clone", "--depth", "1", "--single-branch", clone_url, local_path],
           stderr_to_stdout: true
         ) do
      {_output, 0} ->
        :ets.insert(@ets_table, {url_hash, %{fetched_at: now, path: local_path, url: clone_url}})
        Logger.info("VCL RemoteCache: clone complete for #{url_hash}")
        {:ok, local_path}

      {output, code} ->
        Logger.error("VCL RemoteCache: git clone failed (exit #{code}): #{String.trim(output)}")
        # Clean up partial clone on failure.
        File.rm_rf(local_path)
        {:error, "git clone failed (exit #{code}): #{String.trim(output)}"}
    end
  end

  # Pull latest changes into an existing clone.
  defp pull_existing(clone_url, local_path, url_hash, now) do
    Logger.info("VCL RemoteCache: pulling #{clone_url} in #{local_path}")

    case System.cmd("git", ["pull", "--ff-only"],
           cd: local_path,
           stderr_to_stdout: true
         ) do
      {_output, 0} ->
        :ets.insert(@ets_table, {url_hash, %{fetched_at: now, path: local_path, url: clone_url}})
        Logger.info("VCL RemoteCache: pull complete for #{url_hash}")
        {:ok, local_path}

      {output, code} ->
        # Pull failed -- the clone may be in a bad state. Log but still return
        # the path so that stale data can be queried (better than nothing).
        Logger.warning(
          "VCL RemoteCache: git pull failed (exit #{code}): #{String.trim(output)}; using stale clone"
        )

        :ets.insert(@ets_table, {url_hash, %{fetched_at: now, path: local_path, url: clone_url}})
        {:ok, local_path}
    end
  end

  # ---------------------------------------------------------------------------
  # Internal: URL Normalisation
  # ---------------------------------------------------------------------------

  # Convert GitHub raw content URLs to clone-able repository URLs.
  # e.g. "https://raw.githubusercontent.com/org/repo/main/file.json"
  #   -> "https://github.com/org/repo.git"
  @spec normalise_url(String.t()) :: String.t()
  defp normalise_url(url) do
    cond do
      String.contains?(url, "raw.githubusercontent.com") ->
        # Pattern: https://raw.githubusercontent.com/<owner>/<repo>/<ref>/...
        uri = URI.parse(url)
        parts = String.split(uri.path || "", "/", trim: true)

        case parts do
          [owner, repo | _rest] ->
            "https://github.com/#{owner}/#{repo}.git"

          _ ->
            url
        end

      true ->
        url
    end
  end

  # ---------------------------------------------------------------------------
  # Internal: ETS Table Management
  # ---------------------------------------------------------------------------

  # Lazily create the ETS table if it does not exist. Uses `:public` access
  # so that concurrent processes (including test processes) can read/write.
  defp ensure_ets do
    case :ets.info(@ets_table) do
      :undefined ->
        :ets.new(@ets_table, [:named_table, :set, :public, read_concurrency: true])

      _ ->
        :ok
    end
  rescue
    # Another process may have created the table between the info check and
    # the new call -- that is fine.
    ArgumentError -> :ok
  end

  # ---------------------------------------------------------------------------
  # Internal: Per-URL Locking
  # ---------------------------------------------------------------------------

  # Attempt to acquire an advisory lock for a URL hash. Returns :acquired if
  # this process won, or :wait if another process holds the lock.
  defp acquire_lock(lock_key) do
    case :ets.insert_new(@ets_table, {lock_key, self()}) do
      true -> :acquired
      false -> :wait
    end
  end

  defp release_lock(lock_key) do
    :ets.delete(@ets_table, lock_key)
  end

  # Wait for another process to finish cloning/pulling. Polls every 250ms
  # for up to 120 attempts (30 seconds). If the lock is still held after
  # that, returns the path anyway (the clone may be usable from a prior run).
  @max_lock_wait_attempts 120

  defp wait_for_lock(lock_key, local_path, attempts) when attempts >= @max_lock_wait_attempts do
    Logger.warning("VCL RemoteCache: lock wait timeout for #{inspect(lock_key)}, proceeding with existing data")

    if File.dir?(Path.join(local_path, ".git")) do
      {:ok, local_path}
    else
      {:error, "Lock wait timeout and no cached clone exists"}
    end
  end

  defp wait_for_lock(lock_key, local_path, attempts) do
    Process.sleep(250)
    ensure_ets()

    case :ets.lookup(@ets_table, lock_key) do
      [] ->
        # Lock released -- the clone/pull is done.
        if File.dir?(Path.join(local_path, ".git")) do
          {:ok, local_path}
        else
          {:error, "Clone was not created by the locking process"}
        end

      _ ->
        wait_for_lock(lock_key, local_path, attempts + 1)
    end
  rescue
    # ETS table may have been deleted by another process (e.g. test cleanup).
    # Treat as "lock released" and check the filesystem directly.
    ArgumentError ->
      if File.dir?(Path.join(local_path, ".git")) do
        {:ok, local_path}
      else
        {:error, "ETS table unavailable and no cached clone exists"}
      end
  end
end
