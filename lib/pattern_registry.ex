# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.PatternRegistry do
  @moduledoc """
  Deduplicates findings into canonical patterns and tracks them across repos.

  Reads and writes to verisim-data/patterns/registry.json.
  Groups scan weak_points by (category, description_fingerprint),
  assigns pattern IDs, tracks occurrence counts, and detects trends.

  ## Sync semantics (rebuild, not append)

  `sync_from_scans/1` REBUILDS the per-repo state for every repo present
  in the scan batch, instead of appending to it. The previous append-only
  behaviour had two systemic defects (observed live on the 2026-06
  estate data):

    1. **Counts inflated multiplicatively** — every sync re-added the
       same weak_points, so `occurrences` grew with each pipeline run
       rather than tracking reality.
    2. **Nothing ever resolved** — repos stayed in `repos_affected_list`
       after going clean, so the fleet kept dispatching fixes for
       findings (and files) that no longer existed.

  Now each pattern stores `repo_occurrences` (a `repo => count` map) as
  the source of truth. A sync replaces the entries for scanned repos and
  leaves unscanned repos' entries untouched, so partial syncs stay
  correct. A pattern whose `repo_occurrences` empties out is kept for
  history with `occurrences: 0` and `trend: "resolved"` — downstream
  routing iterates `repos_affected_list`, so resolved patterns dispatch
  nothing.

  ## Input hygiene

  Suppressed weak_points (`"suppressed": true` — panic-attack's
  kanren/context/classification suppression verdict) are rejected at
  intake. panic-attack's contract is explicit: consumers must filter on
  `suppressed: false`; the suppressed entries remain in the scan JSON
  only for auditability.

  Heuristic categories are severity-capped at intake (see
  `@heuristic_severity_caps`): a "potential pattern detected" guess must
  not enter the registry as Critical. Mirrors the precedent of
  `license_finding_severity_cap/1` in `lib/rules/cicd_rules.ex`.
  """

  require Logger

  @verisimdb_data_path Application.compile_env(:hypatia, :verisimdb_data_path, "data/verisim")

  @pa_rule_map %{
    "UncheckedAllocation" => "PA001",
    "UnboundedLoop" => "PA002",
    "BlockingIO" => "PA003",
    "UnsafeCode" => "PA004",
    "PanicPath" => "PA005",
    "RaceCondition" => "PA006",
    "DeadlockPotential" => "PA007",
    "ResourceLeak" => "PA008",
    "CommandInjection" => "PA009",
    "UnsafeDeserialization" => "PA010",
    "DynamicCodeExecution" => "PA011",
    "UnsafeFFI" => "PA012",
    "AtomExhaustion" => "PA013",
    "InsecureProtocol" => "PA014",
    "ExcessivePermissions" => "PA015",
    "PathTraversal" => "PA016",
    "HardcodedSecret" => "PA017",
    "UncheckedError" => "PA018",
    "InfiniteRecursion" => "PA019",
    "UnsafeTypeCoercion" => "PA020",
    "ProofDrift" => "PA021",
    "CryptoMisuse" => "PA022",
    "SupplyChain" => "PA023",
    "InputBoundary" => "PA024",
    "MutationGap" => "PA025",
    # UX patterns -- from cross-platform container test harness
    "HardcodedAbsolutePath" => "UX001",
    "MissingQuickstart" => "UX002",
    "MissingDoctorRecipe" => "UX003",
    "MissingHealRecipe" => "UX004",
    "MissingContractiles" => "UX005",
    "MissingGuixNix" => "UX006",
    "MissingLlmWarmup" => "UX007",
    "MissingExplainme" => "UX008",
    "OversizedManifest" => "UX009",
    "MissingAdjustContractile" => "UX010"
  }

  # Heuristic detectors whose findings are pattern guesses, not confirmed
  # vulnerabilities. panic-attack 2.5.5 emits UnboundedAllocation
  # ("Potential unbounded allocation pattern detected") at Critical, which
  # made one keyword matcher ~70% of all estate Criticals. Cap such
  # categories so prioritisation reflects confirmation strength. The cap
  # only ever lowers severity, never raises it.
  @heuristic_severity_caps %{"UnboundedAllocation" => "Medium"}

  @severity_rank %{"Critical" => 4, "High" => 3, "Medium" => 2, "Low" => 1, "Info" => 0}

  @doc """
  Build/update the pattern registry from scan data.

  Takes a list of scan results (from VerisimConnector.fetch_all_scans/0)
  and rebuilds the per-repo pattern state for every repo in the batch.
  See the moduledoc for the full sync semantics.
  """
  def sync_from_scans(scans) do
    registry = load_registry()
    substitutions = load_substitutions()
    now = DateTime.utc_now() |> DateTime.to_iso8601()

    scanned_repos = MapSet.new(scans, & &1.repo)
    fresh = aggregate_fresh(scans, substitutions)
    previous = Map.get(registry, "patterns", %{})

    all_ids = MapSet.union(MapSet.new(Map.keys(previous)), MapSet.new(Map.keys(fresh)))

    patterns =
      for id <- all_ids, into: %{} do
        {id, merge_pattern(id, Map.get(previous, id), Map.get(fresh, id), scanned_repos, now)}
      end

    updated_registry =
      registry
      |> Map.put("patterns", patterns)
      |> Map.put("last_updated", now)

    save_registry(updated_registry)

    active = Enum.count(Map.values(patterns), &(Map.get(&1, "occurrences", 0) > 0))
    resolved = map_size(patterns) - active
    Logger.info("Pattern registry synced: #{active} active, #{resolved} resolved patterns")

    {:ok, updated_registry}
  end

  @doc "Get a single pattern by ID."
  def get_pattern(pattern_id) do
    registry = load_registry()
    Map.get(Map.get(registry, "patterns", %{}), pattern_id)
  end

  @doc "Get all patterns."
  def all_patterns do
    registry = load_registry()

    Map.get(registry, "patterns", %{})
    |> Map.values()
  end

  @doc "Get only patterns with at least one live occurrence."
  def active_patterns do
    all_patterns()
    |> Enum.filter(fn p -> Map.get(p, "occurrences", 0) > 0 end)
  end

  @doc "Filter patterns by category (PA rule number)."
  def patterns_by_category(category) do
    all_patterns()
    |> Enum.filter(fn p -> Map.get(p, "category") == category end)
  end

  @doc "Get patterns by trend direction: :increasing, :decreasing, :stable, :new, :resolved"
  def trending_patterns(direction) do
    direction_str = Atom.to_string(direction)

    all_patterns()
    |> Enum.filter(fn p -> Map.get(p, "trend") == direction_str end)
  end

  @doc "Count total occurrences of a pattern across all repos."
  def pattern_occurrences(pattern_id) do
    case get_pattern(pattern_id) do
      nil -> 0
      pattern -> Map.get(pattern, "occurrences", 0)
    end
  end

  # --- Private ---

  # Aggregate the current scan batch into
  # pattern_id => %{meta, repo_occurrences, repo_paths}.
  defp aggregate_fresh(scans, substitutions) do
    Enum.reduce(scans, %{}, fn scan, acc ->
      weak_points =
        scan.scan
        |> Map.get("weak_points", [])
        |> Enum.reject(&(Map.get(&1, "suppressed", false) == true))

      program_path = Map.get(scan.scan, "program_path", "")
      repo = scan.repo

      Enum.reduce(weak_points, acc, fn wp, inner ->
        category = Map.get(wp, "category", "unknown")
        description = Map.get(wp, "description", category)
        severity = cap_severity(category, Map.get(wp, "severity", "Medium"))

        pa_rule = Map.get(@pa_rule_map, category, "PA000")
        pattern_id = "#{pa_rule}-#{fingerprint(description)}"

        entry =
          Map.get(inner, pattern_id, %{
            "category" => category,
            "severity" => severity,
            "description" => description,
            "pa_rule" => pa_rule,
            "triangle_tier" => get_triangle_tier(category, substitutions),
            "repo_occurrences" => %{},
            "repo_paths" => %{}
          })

        entry =
          entry
          |> update_in(["repo_occurrences", repo], &((&1 || 0) + 1))
          |> put_in(["repo_paths", repo], program_path)

        Map.put(inner, pattern_id, entry)
      end)
    end)
  end

  # Merge one pattern's previous state with its fresh aggregation.
  # Repos in `scanned_repos` are authoritative from `fresh`; repos not
  # scanned this batch carry over from `previous`.
  defp merge_pattern(id, previous, fresh, scanned_repos, now) do
    prev_ro = previous_repo_occurrences(previous)
    carried_ro = Map.reject(prev_ro, fn {repo, _} -> MapSet.member?(scanned_repos, repo) end)

    # Nothing about this pattern was rescanned -> leave it untouched.
    if fresh == nil and carried_ro == prev_ro do
      previous
    else
      fresh_ro = if fresh, do: Map.get(fresh, "repo_occurrences", %{}), else: %{}
      merged_ro = Map.merge(carried_ro, fresh_ro)
      total = merged_ro |> Map.values() |> Enum.sum()
      prev_total = if previous, do: Map.get(previous, "occurrences", 0), else: 0

      prev_paths = if previous, do: Map.get(previous, "repo_paths", %{}), else: %{}
      fresh_paths = if fresh, do: Map.get(fresh, "repo_paths", %{}), else: %{}

      base = previous || %{}
      meta = fresh || %{}

      base
      |> Map.merge(
        Map.take(meta, ["category", "severity", "description", "pa_rule", "triangle_tier"])
      )
      |> Map.put("id", id)
      |> Map.put_new("recipe_id", nil)
      |> Map.put_new("first_seen", now)
      |> Map.put("repo_occurrences", merged_ro)
      |> Map.put(
        "repo_paths",
        Map.merge(prev_paths, fresh_paths) |> Map.take(Map.keys(merged_ro))
      )
      |> Map.put("occurrences", total)
      |> Map.put("repos_affected", map_size(merged_ro))
      |> Map.put("repos_affected_list", merged_ro |> Map.keys() |> Enum.sort())
      |> Map.put("trend", trend(previous, prev_total, total))
      |> then(fn p ->
        if total > 0 do
          p |> Map.put("last_seen", now) |> Map.delete("resolved_at")
        else
          Map.put_new(p, "resolved_at", now)
        end
      end)
    end
  end

  # The previous schema had no `repo_occurrences`; synthesize one
  # occurrence per affected repo. This deliberately resets the inflated
  # legacy totals on first rebuild.
  defp previous_repo_occurrences(nil), do: %{}

  defp previous_repo_occurrences(previous) do
    case Map.get(previous, "repo_occurrences") do
      ro when is_map(ro) and map_size(ro) > 0 ->
        ro

      _ ->
        previous
        |> Map.get("repos_affected_list", [])
        |> Map.new(&{&1, 1})
    end
  end

  defp trend(nil, _prev_total, _total), do: "new"
  defp trend(_previous, _prev_total, 0), do: "resolved"

  defp trend(_previous, prev_total, total) do
    cond do
      prev_total == 0 -> "increasing"
      total > prev_total -> "increasing"
      total < prev_total -> "decreasing"
      true -> "stable"
    end
  end

  defp cap_severity(category, severity) do
    case Map.get(@heuristic_severity_caps, category) do
      nil ->
        severity

      cap ->
        if Map.get(@severity_rank, severity, 0) > Map.get(@severity_rank, cap, 0) do
          cap
        else
          severity
        end
    end
  end

  defp registry_path do
    Path.join(Path.expand(@verisimdb_data_path), "patterns/registry.json")
  end

  defp substitutions_path do
    Path.join(Path.expand(@verisimdb_data_path), "recipes/proven-substitutions.json")
  end

  defp load_registry do
    case File.read(registry_path()) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, data} -> data
          {:error, _} -> %{"patterns" => %{}, "last_updated" => nil}
        end

      {:error, _} ->
        %{"patterns" => %{}, "last_updated" => nil}
    end
  end

  defp save_registry(registry) do
    case Jason.encode(registry, pretty: true) do
      {:ok, json} -> File.write!(registry_path(), json <> "\n")
      {:error, reason} -> Logger.error("Failed to save registry: #{inspect(reason)}")
    end
  end

  defp load_substitutions do
    case File.read(substitutions_path()) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, data} -> Map.get(data, "substitutions", [])
          {:error, _} -> []
        end

      {:error, _} ->
        []
    end
  end

  defp get_triangle_tier(category, substitutions) do
    case Enum.find(substitutions, fn s -> Map.get(s, "category") == category end) do
      nil -> "control"
      sub -> Map.get(sub, "triangle_tier", "control")
    end
  end

  defp fingerprint(description) when is_binary(description) do
    description
    |> String.downcase()
    |> String.replace(~r/[^a-z0-9]+/, "-")
    |> String.replace(~r/-+/, "-")
    |> String.trim("-")
    |> String.slice(0, 30)
  end

  defp fingerprint(_), do: "unknown"
end
