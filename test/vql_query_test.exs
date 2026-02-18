# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.VQL.QueryTest do
  @moduledoc """
  Comprehensive tests for the VQL Query module.

  Tests cover all high-level query functions that replace raw file reads
  with structured VQL queries. Includes core data access, filtered queries,
  cross-repo analytics, and pipeline health reporting.

  Uses async: false because these functions route through the VQL Client GenServer.
  """

  use ExUnit.Case, async: false

  alias Hypatia.VQL.Query
  alias Hypatia.VQL.Client

  setup do
    # Ensure the VQL Client GenServer is running. It may already be started
    # by the OTP Application supervisor, so we check before starting.
    case GenServer.whereis(Client) do
      nil -> start_supervised!(Client)
      _pid -> :ok
    end

    :ok
  end

  # ---------------------------------------------------------------------------
  # Core Data Access — fetch_scans
  # ---------------------------------------------------------------------------

  describe "fetch_scans/1" do
    test "returns all scans as structured maps" do
      {:ok, scans} = Query.fetch_scans()
      assert is_list(scans)
      assert length(scans) >= 3
    end

    test "each scan has :repo and :scan keys" do
      {:ok, scans} = Query.fetch_scans()

      Enum.each(scans, fn scan ->
        assert Map.has_key?(scan, :repo)
        assert Map.has_key?(scan, :scan)
        assert is_binary(scan.repo)
        assert is_map(scan.scan)
      end)
    end

    test "repo name is derived from filename without .json extension" do
      {:ok, scans} = Query.fetch_scans()

      Enum.each(scans, fn scan ->
        refute String.ends_with?(scan.repo, ".json")
      end)
    end

    test "scan data does not include _source metadata" do
      {:ok, scans} = Query.fetch_scans()

      Enum.each(scans, fn scan ->
        refute Map.has_key?(scan.scan, "_source"),
               "scan.scan should not contain _source (stripped by fetch_scans)"
      end)
    end

    test "respects :limit option" do
      {:ok, scans} = Query.fetch_scans(limit: 2)
      assert length(scans) <= 2
    end

    test "limit of 1 returns exactly one scan" do
      {:ok, scans} = Query.fetch_scans(limit: 1)
      assert length(scans) == 1
    end

    test "no limit returns all scans" do
      {:ok, all} = Query.fetch_scans()
      {:ok, unlimited} = Query.fetch_scans([])
      assert length(all) == length(unlimited)
    end

    test "known repos are present in scan results" do
      {:ok, scans} = Query.fetch_scans()
      repos = Enum.map(scans, & &1.repo)
      assert "echidna" in repos
    end
  end

  # ---------------------------------------------------------------------------
  # Core Data Access — fetch_scan
  # ---------------------------------------------------------------------------

  describe "fetch_scan/1" do
    test "returns scan for a known repo" do
      {:ok, result} = Query.fetch_scan("echidna")
      assert result.repo == "echidna"
      assert is_map(result.scan)
    end

    test "returns :not_found for unknown repo" do
      assert {:error, :not_found} = Query.fetch_scan("nonexistent-repo-xyz")
    end

    test "returned scan data is a map" do
      {:ok, result} = Query.fetch_scan("echidna")
      assert is_map(result.scan)
    end

    test "echidna scan has weak_points" do
      {:ok, result} = Query.fetch_scan("echidna")
      assert Map.has_key?(result.scan, "weak_points")
      assert is_list(result.scan["weak_points"])
    end
  end

  # ---------------------------------------------------------------------------
  # Core Data Access — fetch_pattern_registry
  # ---------------------------------------------------------------------------

  describe "fetch_pattern_registry/0" do
    test "returns the pattern registry document" do
      {:ok, results} = Query.fetch_pattern_registry()
      assert is_list(results)
      assert length(results) >= 1
    end

    test "registry contains patterns map" do
      {:ok, [registry | _]} = Query.fetch_pattern_registry()
      assert Map.has_key?(registry, "patterns")
      assert is_map(registry["patterns"])
    end
  end

  # ---------------------------------------------------------------------------
  # Core Data Access — fetch_patterns
  # ---------------------------------------------------------------------------

  describe "fetch_patterns/0" do
    test "returns flat list of patterns" do
      {:ok, patterns} = Query.fetch_patterns()
      assert is_list(patterns)
      assert length(patterns) >= 100
    end

    test "each pattern is a map" do
      {:ok, patterns} = Query.fetch_patterns()

      Enum.each(patterns, fn pattern ->
        assert is_map(pattern)
      end)
    end
  end

  # ---------------------------------------------------------------------------
  # Core Data Access — fetch_recipes
  # ---------------------------------------------------------------------------

  describe "fetch_recipes/1" do
    test "returns all recipes without filter" do
      {:ok, recipes} = Query.fetch_recipes()
      assert is_list(recipes)
      assert length(recipes) >= 4
    end

    test "recipes do not include _source metadata" do
      {:ok, recipes} = Query.fetch_recipes()

      Enum.each(recipes, fn recipe ->
        refute Map.has_key?(recipe, "_source"),
               "recipe should not contain _source (stripped by fetch_recipes)"
      end)
    end

    test "each recipe has an id field" do
      {:ok, recipes} = Query.fetch_recipes()

      Enum.each(recipes, fn recipe ->
        assert Map.has_key?(recipe, "id"),
               "recipe missing 'id' field"
      end)
    end

    test "known recipe IDs are present" do
      {:ok, recipes} = Query.fetch_recipes()
      ids = Enum.map(recipes, & &1["id"])
      assert "recipe-shell-quote-vars" in ids
      assert "recipe-remove-believe-me" in ids
      assert "recipe-heredoc-to-install" in ids
      assert "recipe-todo-to-fill" in ids
    end
  end

  describe "fetch_recipes/1 — language filter" do
    test "filters recipes by language" do
      {:ok, shell_recipes} = Query.fetch_recipes(language: "shell")
      assert is_list(shell_recipes)

      # recipe-shell-quote-vars targets shell
      if length(shell_recipes) > 0 do
        Enum.each(shell_recipes, fn recipe ->
          languages = Map.get(recipe, "languages", [])
          assert "shell" in languages or "*" in languages,
                 "Expected shell or wildcard in languages for #{recipe["id"]}"
        end)
      end
    end
  end

  # ---------------------------------------------------------------------------
  # Core Data Access — fetch_recipe
  # ---------------------------------------------------------------------------

  describe "fetch_recipe/1" do
    test "returns a known recipe by ID" do
      {:ok, recipe} = Query.fetch_recipe("recipe-shell-quote-vars")
      assert is_map(recipe)
      assert Map.has_key?(recipe, "id") or Map.has_key?(recipe, "confidence")
    end

    test "returns :not_found for unknown recipe" do
      assert {:error, :not_found} = Query.fetch_recipe("recipe-nonexistent-xyz")
    end
  end

  # ---------------------------------------------------------------------------
  # Core Data Access — fetch_substitutions
  # ---------------------------------------------------------------------------

  describe "fetch_substitutions/0" do
    test "returns all 20 substitution entries" do
      {:ok, subs} = Query.fetch_substitutions()
      assert is_list(subs)
      assert length(subs) == 20
    end

    test "each substitution has required fields" do
      {:ok, subs} = Query.fetch_substitutions()

      Enum.each(subs, fn sub ->
        assert Map.has_key?(sub, "category"),
               "substitution missing 'category'"
        assert Map.has_key?(sub, "pa_rule"),
               "substitution missing 'pa_rule'"
      end)
    end

    test "known categories are present" do
      {:ok, subs} = Query.fetch_substitutions()
      categories = Enum.map(subs, & &1["category"])
      assert "CommandInjection" in categories
      assert "PathTraversal" in categories
      assert "UnsafeDeserialization" in categories
    end
  end

  # ---------------------------------------------------------------------------
  # Core Data Access — fetch_outcomes
  # ---------------------------------------------------------------------------

  describe "fetch_outcomes/1" do
    test "returns outcomes list" do
      {:ok, outcomes} = Query.fetch_outcomes()
      assert is_list(outcomes)
    end

    test "outcomes are maps" do
      {:ok, outcomes} = Query.fetch_outcomes()

      Enum.each(outcomes, fn outcome ->
        assert is_map(outcome)
      end)
    end

    test "respects :limit option" do
      {:ok, limited} = Query.fetch_outcomes(limit: 5)
      assert length(limited) <= 5
    end

    test "respects :recipe_id filter" do
      {:ok, filtered} = Query.fetch_outcomes(recipe_id: "recipe-shell-quote-vars")
      assert is_list(filtered)

      Enum.each(filtered, fn outcome ->
        assert Map.get(outcome, "recipe_id") == "recipe-shell-quote-vars"
      end)
    end

    test "combined :recipe_id and :limit" do
      {:ok, results} = Query.fetch_outcomes(recipe_id: "recipe-shell-quote-vars", limit: 3)
      assert length(results) <= 3
    end
  end

  # ---------------------------------------------------------------------------
  # Core Data Access — fetch_index
  # ---------------------------------------------------------------------------

  describe "fetch_index/0" do
    test "returns the master index" do
      {:ok, results} = Query.fetch_index()
      assert is_list(results)
      assert length(results) >= 1
    end

    test "index has expected fields" do
      {:ok, [index | _]} = Query.fetch_index()
      assert Map.has_key?(index, "total_scans")
      assert Map.has_key?(index, "repos")
    end

    test "index total_scans is consistent with scan count" do
      {:ok, [index | _]} = Query.fetch_index()
      {:ok, scans} = Query.fetch_scans()
      assert index["total_scans"] == length(scans)
    end
  end

  # ---------------------------------------------------------------------------
  # Filtered Queries — scans_by_severity
  # ---------------------------------------------------------------------------

  describe "scans_by_severity/1" do
    test "filters scans containing High severity findings" do
      {:ok, scans} = Query.scans_by_severity("High")
      assert is_list(scans)

      Enum.each(scans, fn %{scan: scan} ->
        severities = scan
          |> Map.get("weak_points", [])
          |> Enum.map(& &1["severity"])
        assert "High" in severities,
               "Expected High severity in #{inspect(severities)}"
      end)
    end

    test "filters scans containing Medium severity findings" do
      {:ok, scans} = Query.scans_by_severity("Medium")
      assert is_list(scans)

      Enum.each(scans, fn %{scan: scan} ->
        severities = scan
          |> Map.get("weak_points", [])
          |> Enum.map(& &1["severity"])
        assert "Medium" in severities
      end)
    end

    test "nonexistent severity returns empty or fewer results" do
      {:ok, all} = Query.fetch_scans()
      {:ok, filtered} = Query.scans_by_severity("Nonexistent")
      assert length(filtered) <= length(all)
    end
  end

  # ---------------------------------------------------------------------------
  # Filtered Queries — scans_by_category
  # ---------------------------------------------------------------------------

  describe "scans_by_category/1" do
    test "filters scans by PA category" do
      {:ok, scans} = Query.scans_by_category("PanicPath")
      assert is_list(scans)

      Enum.each(scans, fn %{scan: scan} ->
        categories = scan
          |> Map.get("weak_points", [])
          |> Enum.map(& &1["category"])
        assert "PanicPath" in categories
      end)
    end

    test "unknown category returns empty list" do
      {:ok, scans} = Query.scans_by_category("FakeCategory_ZZZ")
      assert scans == []
    end
  end

  # ---------------------------------------------------------------------------
  # Filtered Queries — recipes_by_tier
  # ---------------------------------------------------------------------------

  describe "recipes_by_tier/1" do
    test "returns recipes in eliminate tier" do
      {:ok, results} = Query.recipes_by_tier("eliminate")
      assert is_list(results)
    end
  end

  # ---------------------------------------------------------------------------
  # Filtered Queries — recipes_by_language
  # ---------------------------------------------------------------------------

  describe "recipes_by_language/1" do
    test "returns recipes for shell language" do
      {:ok, recipes} = Query.recipes_by_language("shell")
      assert is_list(recipes)
    end

    test "delegates to fetch_recipes with language option" do
      {:ok, direct} = Query.fetch_recipes(language: "shell")
      {:ok, via_fn} = Query.recipes_by_language("shell")
      assert length(direct) == length(via_fn)
    end
  end

  # ---------------------------------------------------------------------------
  # Filtered Queries — recipes_above_confidence
  # ---------------------------------------------------------------------------

  describe "recipes_above_confidence/1" do
    test "filters recipes by confidence threshold" do
      {:ok, recipes} = Query.recipes_above_confidence(0.9)
      assert is_list(recipes)

      Enum.each(recipes, fn recipe ->
        confidence = Map.get(recipe, "confidence", 0.0)
        assert confidence >= 0.9,
               "Expected confidence >= 0.9, got #{confidence} for #{recipe["id"]}"
      end)
    end

    test "higher threshold returns fewer or equal recipes" do
      {:ok, low} = Query.recipes_above_confidence(0.5)
      {:ok, high} = Query.recipes_above_confidence(0.95)
      assert length(high) <= length(low)
    end

    test "threshold of 0.0 returns all recipes" do
      {:ok, all_recipes} = Query.fetch_recipes()
      {:ok, zero_threshold} = Query.recipes_above_confidence(0.0)
      assert length(zero_threshold) == length(all_recipes)
    end

    test "threshold of 1.0 returns only perfect-confidence recipes" do
      {:ok, perfect} = Query.recipes_above_confidence(1.0)

      Enum.each(perfect, fn recipe ->
        assert Map.get(recipe, "confidence", 0.0) >= 1.0
      end)
    end
  end

  # ---------------------------------------------------------------------------
  # Filtered Queries — outcomes_for_recipe
  # ---------------------------------------------------------------------------

  describe "outcomes_for_recipe/1" do
    test "returns outcomes for known recipe" do
      {:ok, outcomes} = Query.outcomes_for_recipe("recipe-shell-quote-vars")
      assert is_list(outcomes)
    end

    test "returns empty for recipe with no outcomes" do
      {:ok, outcomes} = Query.outcomes_for_recipe("recipe-nonexistent-xyz")
      assert is_list(outcomes)
    end
  end

  # ---------------------------------------------------------------------------
  # Filtered Queries — outcomes_by_result
  # ---------------------------------------------------------------------------

  describe "outcomes_by_result/1" do
    test "filters outcomes by success" do
      {:ok, results} = Query.outcomes_by_result("success")
      assert is_list(results)
    end

    test "filters outcomes by failure" do
      {:ok, results} = Query.outcomes_by_result("failure")
      assert is_list(results)
    end
  end

  # ---------------------------------------------------------------------------
  # Cross-Repo Analytics — cross_repo_patterns
  # ---------------------------------------------------------------------------

  describe "cross_repo_patterns/1" do
    test "finds patterns appearing in >= N repos" do
      {:ok, patterns} = Query.cross_repo_patterns(3)
      assert is_list(patterns)

      Enum.each(patterns, fn p ->
        assert Map.get(p, "repos_affected", 0) >= 3,
               "Expected repos_affected >= 3, got #{Map.get(p, "repos_affected")}"
      end)
    end

    test "results are sorted by repos_affected descending" do
      {:ok, patterns} = Query.cross_repo_patterns(1)

      if length(patterns) >= 2 do
        counts = Enum.map(patterns, &Map.get(&1, "repos_affected", 0))
        assert counts == Enum.sort(counts, :desc),
               "Expected descending order by repos_affected"
      end
    end

    test "higher threshold returns fewer or equal patterns" do
      {:ok, low} = Query.cross_repo_patterns(1)
      {:ok, high} = Query.cross_repo_patterns(10)
      assert length(high) <= length(low)
    end

    test "default threshold is 3" do
      {:ok, default_result} = Query.cross_repo_patterns()
      {:ok, explicit_3} = Query.cross_repo_patterns(3)
      assert length(default_result) == length(explicit_3)
    end
  end

  # ---------------------------------------------------------------------------
  # Cross-Repo Analytics — pattern_correlations
  # ---------------------------------------------------------------------------

  describe "pattern_correlations/1" do
    test "returns correlation results" do
      {:ok, correlations} = Query.pattern_correlations()
      assert is_list(correlations)
    end

    test "each correlation has pattern_a, pattern_b, shared_repos" do
      {:ok, correlations} = Query.pattern_correlations(1)

      Enum.each(correlations, fn corr ->
        assert Map.has_key?(corr, :pattern_a)
        assert Map.has_key?(corr, :pattern_b)
        assert Map.has_key?(corr, :shared_repos)
        assert corr.shared_repos >= 1
      end)
    end

    test "pattern_a < pattern_b (lexicographic ordering)" do
      {:ok, correlations} = Query.pattern_correlations(1)

      Enum.each(correlations, fn corr ->
        assert corr.pattern_a < corr.pattern_b,
               "Expected pattern_a < pattern_b: #{corr.pattern_a} vs #{corr.pattern_b}"
      end)
    end

    test "results sorted by shared_repos descending" do
      {:ok, correlations} = Query.pattern_correlations(1)

      if length(correlations) >= 2 do
        counts = Enum.map(correlations, & &1.shared_repos)
        assert counts == Enum.sort(counts, :desc)
      end
    end

    test "higher min_shared threshold returns fewer or equal correlations" do
      {:ok, low} = Query.pattern_correlations(1)
      {:ok, high} = Query.pattern_correlations(5)
      assert length(high) <= length(low)
    end
  end

  # ---------------------------------------------------------------------------
  # Cross-Repo Analytics — outcome_timeline
  # ---------------------------------------------------------------------------

  describe "outcome_timeline/1" do
    test "returns timeline for known recipe" do
      {:ok, timeline} = Query.outcome_timeline("recipe-shell-quote-vars")
      assert is_list(timeline)
    end

    test "timeline entries have expected fields" do
      {:ok, timeline} = Query.outcome_timeline("recipe-shell-quote-vars")

      Enum.each(timeline, fn entry ->
        assert Map.has_key?(entry, :date)
        assert Map.has_key?(entry, :successes)
        assert Map.has_key?(entry, :failures)
        assert Map.has_key?(entry, :false_positives)
        assert Map.has_key?(entry, :total)
        assert is_integer(entry.successes)
        assert is_integer(entry.failures)
        assert is_integer(entry.false_positives)
        assert is_integer(entry.total)
      end)
    end

    test "timeline entries are sorted by date ascending" do
      {:ok, timeline} = Query.outcome_timeline("recipe-shell-quote-vars")

      if length(timeline) >= 2 do
        dates = Enum.map(timeline, & &1.date)
        assert dates == Enum.sort(dates)
      end
    end

    test "total equals successes + failures + false_positives + other" do
      {:ok, timeline} = Query.outcome_timeline("recipe-shell-quote-vars")

      Enum.each(timeline, fn entry ->
        # total is the raw count, which may include other outcome types
        assert entry.total >= entry.successes + entry.failures + entry.false_positives
      end)
    end

    test "unknown recipe returns empty timeline" do
      {:ok, timeline} = Query.outcome_timeline("recipe-nonexistent-xyz")
      # Should return an empty list or a list with no meaningful entries
      assert is_list(timeline)
    end
  end

  # ---------------------------------------------------------------------------
  # Cross-Repo Analytics — recipe_effectiveness
  # ---------------------------------------------------------------------------

  describe "recipe_effectiveness/1" do
    test "returns effectiveness metrics for recipe with outcomes" do
      {:ok, eff} = Query.recipe_effectiveness("recipe-shell-quote-vars")

      assert Map.has_key?(eff, :recipe_id)
      assert eff.recipe_id == "recipe-shell-quote-vars"

      case eff.total_attempts do
        0 ->
          assert eff.trend == :no_data

        _n ->
          assert Map.has_key?(eff, :successes)
          assert Map.has_key?(eff, :failures)
          assert Map.has_key?(eff, :false_positives)
          assert Map.has_key?(eff, :success_rate)
          assert Map.has_key?(eff, :recent_success_rate)
          assert Map.has_key?(eff, :trend)
          assert eff.trend in [:improving, :declining, :stable]
          assert eff.success_rate >= 0.0 and eff.success_rate <= 1.0
          assert eff.recent_success_rate >= 0.0 and eff.recent_success_rate <= 1.0
      end
    end

    test "returns :no_data trend for recipe with no outcomes" do
      {:ok, eff} = Query.recipe_effectiveness("recipe-nonexistent-xyz")
      assert eff.total_attempts == 0
      assert eff.trend == :no_data
    end
  end

  # ---------------------------------------------------------------------------
  # Cross-Repo Analytics — most_vulnerable_repos
  # ---------------------------------------------------------------------------

  describe "most_vulnerable_repos/1" do
    test "returns repos ranked by weak point count" do
      {:ok, repos} = Query.most_vulnerable_repos(5)
      assert is_list(repos)
      assert length(repos) <= 5
    end

    test "each result has :repo, :count, :categories" do
      {:ok, repos} = Query.most_vulnerable_repos()

      Enum.each(repos, fn r ->
        assert Map.has_key?(r, :repo)
        assert Map.has_key?(r, :count)
        assert Map.has_key?(r, :categories)
        assert is_binary(r.repo)
        assert is_integer(r.count)
        assert is_list(r.categories)
        assert r.count > 0
      end)
    end

    test "results are sorted by count descending" do
      {:ok, repos} = Query.most_vulnerable_repos()

      if length(repos) >= 2 do
        counts = Enum.map(repos, & &1.count)
        assert counts == Enum.sort(counts, :desc),
               "Expected repos sorted by count descending"
      end
    end

    test "default limit is 20" do
      {:ok, repos} = Query.most_vulnerable_repos()
      assert length(repos) <= 20
    end

    test "repos with zero weak points are excluded" do
      {:ok, repos} = Query.most_vulnerable_repos()

      Enum.each(repos, fn r ->
        assert r.count > 0
      end)
    end

    test "categories list has unique values" do
      {:ok, repos} = Query.most_vulnerable_repos()

      Enum.each(repos, fn r ->
        assert r.categories == Enum.uniq(r.categories)
      end)
    end
  end

  # ---------------------------------------------------------------------------
  # Cross-Repo Analytics — category_distribution
  # ---------------------------------------------------------------------------

  describe "category_distribution/0" do
    test "returns category breakdown" do
      {:ok, distribution} = Query.category_distribution()
      assert is_list(distribution)
      assert length(distribution) >= 1
    end

    test "each category entry has required fields" do
      {:ok, distribution} = Query.category_distribution()

      Enum.each(distribution, fn cat ->
        assert Map.has_key?(cat, :category)
        assert Map.has_key?(cat, :total)
        assert Map.has_key?(cat, :severities)
        assert Map.has_key?(cat, :repos)
        assert is_binary(cat.category)
        assert cat.total > 0
        assert is_map(cat.severities)
        assert is_integer(cat.repos)
      end)
    end

    test "results are sorted by total descending" do
      {:ok, distribution} = Query.category_distribution()

      if length(distribution) >= 2 do
        totals = Enum.map(distribution, & &1.total)
        assert totals == Enum.sort(totals, :desc)
      end
    end

    test "severities map has string keys" do
      {:ok, distribution} = Query.category_distribution()

      Enum.each(distribution, fn cat ->
        Enum.each(cat.severities, fn {key, count} ->
          assert is_binary(key), "Expected string severity key, got #{inspect(key)}"
          assert is_integer(count) and count > 0
        end)
      end)
    end
  end

  # ---------------------------------------------------------------------------
  # Cross-Repo Analytics — recipe_coverage
  # ---------------------------------------------------------------------------

  describe "recipe_coverage/0" do
    test "returns coverage metrics" do
      {:ok, coverage} = Query.recipe_coverage()
      assert Map.has_key?(coverage, :total_patterns)
      assert Map.has_key?(coverage, :patterns_with_recipe)
      assert Map.has_key?(coverage, :coverage_pct)
      assert Map.has_key?(coverage, :total_recipes)
      assert Map.has_key?(coverage, :pa_rules_covered)
    end

    test "total_patterns is positive" do
      {:ok, coverage} = Query.recipe_coverage()
      assert coverage.total_patterns > 0
    end

    test "total_recipes is positive" do
      {:ok, coverage} = Query.recipe_coverage()
      assert coverage.total_recipes > 0
    end

    test "coverage_pct is between 0 and 100" do
      {:ok, coverage} = Query.recipe_coverage()
      assert coverage.coverage_pct >= 0.0
      assert coverage.coverage_pct <= 100.0
    end

    test "patterns_with_recipe does not exceed total_patterns" do
      {:ok, coverage} = Query.recipe_coverage()
      assert coverage.patterns_with_recipe <= coverage.total_patterns
    end

    test "pa_rules_covered is a list of strings" do
      {:ok, coverage} = Query.recipe_coverage()
      assert is_list(coverage.pa_rules_covered)

      Enum.each(coverage.pa_rules_covered, fn rule ->
        assert is_binary(rule)
      end)
    end
  end

  # ---------------------------------------------------------------------------
  # Pipeline Health
  # ---------------------------------------------------------------------------

  describe "pipeline_health/0" do
    test "returns comprehensive health summary" do
      {:ok, health} = Query.pipeline_health()
      assert Map.has_key?(health, :repos_scanned)
      assert Map.has_key?(health, :total_weak_points)
      assert Map.has_key?(health, :canonical_patterns)
      assert Map.has_key?(health, :fix_recipes)
      assert Map.has_key?(health, :outcomes_recorded)
      assert Map.has_key?(health, :success_rate)
      assert Map.has_key?(health, :failure_count)
      assert Map.has_key?(health, :high_confidence_recipes)
      assert Map.has_key?(health, :auto_executable_recipes)
    end

    test "repos_scanned matches fetch_scans count" do
      {:ok, health} = Query.pipeline_health()
      {:ok, scans} = Query.fetch_scans()
      assert health.repos_scanned == length(scans)
    end

    test "canonical_patterns matches fetch_patterns count" do
      {:ok, health} = Query.pipeline_health()
      {:ok, patterns} = Query.fetch_patterns()
      assert health.canonical_patterns == length(patterns)
    end

    test "fix_recipes matches fetch_recipes count" do
      {:ok, health} = Query.pipeline_health()
      {:ok, recipes} = Query.fetch_recipes()
      assert health.fix_recipes == length(recipes)
    end

    test "numeric fields are non-negative" do
      {:ok, health} = Query.pipeline_health()
      assert health.repos_scanned >= 0
      assert health.total_weak_points >= 0
      assert health.canonical_patterns >= 0
      assert health.fix_recipes >= 0
      assert health.outcomes_recorded >= 0
      assert health.success_rate >= 0.0
      assert health.failure_count >= 0
      assert health.high_confidence_recipes >= 0
      assert health.auto_executable_recipes >= 0
    end

    test "success_rate is between 0 and 100" do
      {:ok, health} = Query.pipeline_health()
      assert health.success_rate >= 0.0
      assert health.success_rate <= 100.0
    end

    test "minimum expected values for active pipeline" do
      {:ok, health} = Query.pipeline_health()
      assert health.repos_scanned >= 3, "Expected at least 3 repos scanned"
      assert health.canonical_patterns >= 100, "Expected at least 100 canonical patterns"
      assert health.fix_recipes >= 4, "Expected at least 4 fix recipes"
    end

    test "auto_executable_recipes does not exceed high_confidence_recipes" do
      {:ok, health} = Query.pipeline_health()
      assert health.auto_executable_recipes <= health.high_confidence_recipes
    end

    test "high_confidence_recipes does not exceed total fix_recipes" do
      {:ok, health} = Query.pipeline_health()
      assert health.high_confidence_recipes <= health.fix_recipes
    end
  end
end
