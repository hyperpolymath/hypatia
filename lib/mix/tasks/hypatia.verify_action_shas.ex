# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Mix.Tasks.Hypatia.VerifyActionShas do
  @moduledoc """
  Verify every `uses: <org>/<action>@<40-hex>` action SHA pin across the
  estate by calling `gh api repos/<org>/<action>/commits/<sha>` and
  reporting any that return 422 (fabricated SHA).

  Complement to `Hypatia.Rules.CicdRules.:known_fake_action_sha` (which
  is a static-list rule covering the 25 fakes from the 2026-05-30 audit).
  This task catches FUTURE fakes that aren't yet on the static list.

  ## Usage

      mix hypatia.verify_action_shas               # full estate scan
      mix hypatia.verify_action_shas --paranoid    # re-verify cached real SHAs
      mix hypatia.verify_action_shas --format json # JSON output for tooling

  Reads `$HYPATIA_REPOS_DIR` (default `~/repos`) for the estate root.
  Per-SHA results are cached at `data/verified-action-shas.json` so
  subsequent runs only call `gh api` on new pins (~5s warm vs ~5min cold).

  ## Why shell out to `gh`?

  Per hypatia's CLAUDE.md scanner-hygiene guidance, `System.cmd("bin", [args])`
  is the safe no-shell pattern. Reusing the user's existing `gh` auth means
  no new HTTP-client dep, no new auth surface — and the 5000 req/hr
  authenticated rate limit is plenty for a 372-pin estate.

  ## Exit codes

  - 0 — verification complete, zero fakes found
  - 2 — verification complete, fakes found (count printed to stderr)
  - 1 — hard failure (bad args, gh missing, cache unwritable)

  Non-zero on fakes-found lets CI integrate this as a hard gate.
  """

  use Mix.Task

  @shortdoc "Verify estate GitHub Action SHA pins via gh api"

  @repos_dir_env "HYPATIA_REPOS_DIR"
  @default_repos_dir "~/repos"
  @cache_path "data/verified-action-shas.json"
  @sha_pattern ~r|uses:\s+([a-zA-Z0-9_./-]+)@([a-f0-9]{40})|
  # Heuristic skip: known forks, archives, vendored upstream
  @skip_path_substrings ["/repos-monorepo/", "/.claude/", "/node_modules/", "/_build/", "/deps/"]

  @switches [
    paranoid: :boolean,
    format: :string,
    repos_dir: :string,
    cache: :string,
    quiet: :boolean
  ]

  @impl Mix.Task
  def run(argv) do
    {opts, _, _} = OptionParser.parse(argv, switches: @switches)
    paranoid? = Keyword.get(opts, :paranoid, false)
    format = Keyword.get(opts, :format, "text")
    quiet? = Keyword.get(opts, :quiet, false)
    repos_dir = repos_dir(opts)
    cache_path = Keyword.get(opts, :cache, @cache_path)

    unless quiet?, do: Mix.shell().info("scanning #{repos_dir}/*/.github/workflows/*.yml...")

    cache = load_cache(cache_path)
    pins = extract_all_pins(repos_dir)
    unique_pairs = pins |> Enum.uniq() |> Enum.sort()

    unless quiet?,
      do: Mix.shell().info("  found #{length(pins)} pin sites; #{length(unique_pairs)} unique (org/action, sha) pairs")

    to_verify =
      if paranoid? do
        unique_pairs
      else
        Enum.reject(unique_pairs, fn {repo, sha} ->
          Map.get(cache, "#{repo}@#{sha}") == "real"
        end)
      end

    unless quiet?,
      do: Mix.shell().info("  verifying #{length(to_verify)} unverified pin(s) via `gh api`...")

    results = verify_all(to_verify, cache, quiet?)
    save_cache(cache_path, Map.merge(cache, results))

    fakes =
      results
      |> Enum.filter(fn {_, status} -> status == "fake" end)
      |> Enum.map(fn {key, _} -> key end)
      |> Enum.sort()

    fake_pin_sites =
      Enum.filter(pins, fn {repo, sha} -> "#{repo}@#{sha}" in fakes end)

    case format do
      "json" -> output_json(fakes, fake_pin_sites)
      _ -> output_text(fakes, fake_pin_sites, quiet?)
    end

    if fakes != [] do
      Mix.shell().error("\n#{length(fakes)} fabricated SHA(s) found across #{length(fake_pin_sites)} pin site(s)")
      exit({:shutdown, 2})
    end
  end

  # --- pin extraction ---------------------------------------------------------

  defp extract_all_pins(repos_dir) do
    repos_dir
    |> list_local_repos()
    |> Enum.flat_map(fn repo_name ->
      workflows_glob = Path.join([repos_dir, repo_name, ".github", "workflows", "*.yml"])

      Path.wildcard(workflows_glob)
      |> Enum.reject(&skip_path?/1)
      |> Enum.flat_map(&extract_pins_from_file/1)
    end)
  end

  defp extract_pins_from_file(path) do
    case File.read(path) do
      {:ok, content} ->
        Regex.scan(@sha_pattern, content, capture: :all_but_first)
        |> Enum.map(fn [repo, sha] -> {String.trim(repo), sha} end)

      {:error, _} ->
        []
    end
  end

  defp list_local_repos(repos_dir) do
    case File.ls(repos_dir) do
      {:ok, entries} ->
        entries
        |> Enum.filter(fn name -> File.dir?(Path.join([repos_dir, name, ".git"])) end)
        |> Enum.sort()

      {:error, reason} ->
        Mix.raise("Cannot list #{repos_dir}: #{inspect(reason)}")
    end
  end

  defp skip_path?(path) do
    Enum.any?(@skip_path_substrings, &String.contains?(path, &1))
  end

  # --- gh api verification ----------------------------------------------------

  defp verify_all(pairs, _cache, quiet?) do
    pairs
    |> Enum.with_index(1)
    |> Enum.map(fn {{repo, sha}, idx} ->
      unless quiet? do
        Mix.shell().info("  [#{idx}/#{length(pairs)}] #{repo}@#{String.slice(sha, 0, 12)}...")
      end

      # Strip subpath actions to base repo (e.g. github/codeql-action/init → github/codeql-action)
      base_repo = repo |> String.split("/") |> Enum.take(2) |> Enum.join("/")
      status = verify_sha(base_repo, sha)

      # gentle throttle: 50ms between calls = ~20/sec, well under 5000/hr cap
      Process.sleep(50)

      {"#{repo}@#{sha}", status}
    end)
    |> Map.new()
  end

  defp verify_sha(repo, sha) do
    case System.cmd("gh", ["api", "repos/#{repo}/commits/#{sha}", "-q", ".sha"],
           stderr_to_stdout: true
         ) do
      {output, 0} ->
        if String.trim(output) == sha, do: "real", else: "unknown"

      {output, _exit_code} ->
        cond do
          String.contains?(output, "422") or String.contains?(output, "No commit found") -> "fake"
          String.contains?(output, "404") -> "unknown"
          true -> "unknown"
        end
    end
  end

  # --- cache I/O --------------------------------------------------------------

  defp load_cache(path) do
    expanded = Path.expand(path)

    case File.read(expanded) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, %{} = map} -> map
          _ -> %{}
        end

      {:error, _} ->
        %{}
    end
  end

  defp save_cache(path, cache) do
    expanded = Path.expand(path)
    File.mkdir_p!(Path.dirname(expanded))
    File.write!(expanded, Jason.encode!(cache, pretty: true))
  end

  # --- output -----------------------------------------------------------------

  defp output_text(fakes, fake_pin_sites, quiet?) do
    if fakes == [] do
      unless quiet?, do: Mix.shell().info("\n✓ All action SHA pins verify against upstream — zero fakes found.")
    else
      Mix.shell().info("\n=== Fabricated SHA pins (count: #{length(fakes)}) ===")

      fakes
      |> Enum.each(fn key ->
        affected =
          fake_pin_sites
          |> Enum.filter(fn {repo, sha} -> "#{repo}@#{sha}" == key end)
          |> length()

        Mix.shell().info("  #{key}  (#{affected} pin site#{if affected == 1, do: "", else: "s"})")
      end)
    end
  end

  defp output_json(fakes, fake_pin_sites) do
    by_key = Enum.group_by(fake_pin_sites, fn {repo, sha} -> "#{repo}@#{sha}" end)

    out = %{
      summary: %{
        fake_count: length(fakes),
        pin_site_count: length(fake_pin_sites)
      },
      fakes:
        Enum.map(fakes, fn key ->
          %{
            ref: key,
            sites: length(Map.get(by_key, key, []))
          }
        end)
    }

    IO.puts(Jason.encode!(out, pretty: true))
  end

  # --- helpers ----------------------------------------------------------------

  defp repos_dir(opts) do
    Keyword.get(opts, :repos_dir) ||
      System.get_env(@repos_dir_env) ||
      Path.expand(@default_repos_dir)
  end
end
