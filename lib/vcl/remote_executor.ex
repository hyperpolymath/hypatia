# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.VCL.RemoteExecutor do
  @moduledoc """
  Executes a VCL query across an explicit list of remote verisim-data
  repositories.

  Complements `Hypatia.VCL.FileExecutor`, which handles at most one remote
  URL at a time via `{:remote, url}` / `{:remote, url, sub_source}`.
  `RemoteExecutor` handles the multi-remote case:

      FROM FEDERATION REMOTE IN ["https://…/a", "https://…/b"]
      WITH DRIFT MODERATE

  The AST source for this query is `{:federation_remote, urls,
  drift_policy}`. `Hypatia.VCL.Client` inspects the source and routes
  here; every other source type still flows through `FileExecutor`.

  ## Flow

    1. Fetch each URL in parallel via
       `Hypatia.VCL.RemoteCache.cache_remote_store/2` (same cache the
       single-remote path uses; concurrent calls are safe).
    2. For each cached clone, build a per-store `{:remote, url, sub_source}`
       AST and execute it through `FileExecutor`. This reuses the existing
       drift-policy and WHERE-clause logic without duplication.
    3. Tag each returned item with `"_source_url" => url` so downstream
       consumers can see which remote a row came from.
    4. Merge all items, then apply the query's `LIMIT` / `OFFSET` across
       the union.

  ## Design notes

    * **Sub-source.** Unless the query explicitly says otherwise, each
      remote is scanned as a full federation (`{:federation, "*",
      drift_policy}`) — matching what a bare `FROM FEDERATION REMOTE url`
      does today.
    * **Per-remote pagination is disabled.** The individual remote
      queries run with `limit: nil, offset: 0`; pagination is applied
      once at the end across the merged set. Without this, `LIMIT 50`
      would cap each remote at 50 and the merged result could reach
      `50 × N` rows.
    * **Failure isolation.** A failure fetching one remote (timeout,
      clone error) is logged as a warning and treated as an empty
      result for that remote. The overall query still returns whatever
      the healthy remotes produced.
  """

  require Logger
  alias Hypatia.VCL.FileExecutor

  @fetch_timeout 60_000

  @doc """
  Execute `ast` across `urls` and return `{:ok, merged_items}`.

  `opts` is passed through to `RemoteCache` via `FileExecutor`'s
  existing `cache_opts` mechanism.
  """
  def execute(urls, ast, opts \\ []) when is_list(urls) do
    sub_source = default_sub_source(ast)

    merged =
      urls
      |> Task.async_stream(
        fn url -> run_one(url, sub_source, ast, opts) end,
        max_concurrency: max(length(urls), 1),
        timeout: @fetch_timeout,
        on_timeout: :kill_task
      )
      |> Enum.flat_map(fn
        {:ok, {url, items}} -> Enum.map(items, &tag_source(&1, url))
        {:exit, reason} ->
          Logger.warning("RemoteExecutor task exited: #{inspect(reason)}")
          []
      end)

    paginated = paginate(merged, ast.limit, ast.offset)
    {:ok, paginated}
  end

  # ── helpers ──────────────────────────────────────────────────────────────

  defp run_one(url, sub_source, ast, opts) do
    # Don't let the per-remote executor apply LIMIT/OFFSET; we need the
    # full set so the final merge can paginate consistently.
    remote_ast = %{ast | source: {:remote, url, sub_source}, limit: nil, offset: 0}

    case FileExecutor.execute(remote_ast, opts) do
      {:ok, items} when is_list(items) ->
        {url, items}

      {:error, reason} ->
        Logger.warning("RemoteExecutor skipped #{url}: #{inspect(reason)}")
        {url, []}

      other ->
        Logger.warning("RemoteExecutor got unexpected reply from #{url}: #{inspect(other)}")
        {url, []}
    end
  end

  defp default_sub_source(%{source: {:federation_remote, _urls, drift_policy}}) do
    {:federation, "*", drift_policy}
  end

  defp default_sub_source(_ast), do: {:federation, "*", nil}

  defp tag_source(item, url) when is_map(item), do: Map.put(item, "_source_url", url)
  defp tag_source(item, _url), do: item

  defp paginate(results, nil, _offset), do: results
  defp paginate(results, limit, nil), do: Enum.take(results, limit)
  defp paginate(results, limit, 0), do: Enum.take(results, limit)

  defp paginate(results, limit, offset) when is_integer(offset) do
    results |> Enum.drop(offset) |> Enum.take(limit)
  end
end
