# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.VQL.QueryTest do
  use ExUnit.Case, async: false

  alias Hypatia.VQL.Query
  alias Hypatia.VQL.Client

  setup do
    # Client may already be started by Application supervisor
    case GenServer.whereis(Client) do
      nil -> start_supervised!(Client)
      _pid -> :ok
    end

    :ok
  end

  # ---------------------------------------------------------------------------
  # Core Data Access
  # ---------------------------------------------------------------------------

  describe "fetch_scans/1" do
    test "returns all scans" do
      {:ok, scans} = Query.fetch_scans()
      assert is_list(scans)
      assert length(scans) >= 3

      Enum.each(scans, fn scan ->
        assert Map.has_key?(scan, :repo)
        assert Map.has_key?(scan, :scan)
        assert is_binary(scan.repo)
        assert is_map(scan.scan)
      end)
    end

    test "respects limit option" do
      {:ok, scans} = Query.fetch_scans(limit: 2)
      assert length(scans) <= 2
    end
  end

  describe "fetch_scan/1" do
    test "returns scan for a known repo" do
      {:ok, scan} = Query.fetch_scan("echidna")
      assert scan.repo == "echidna"
      assert is_map(scan.scan)
    end

    test "returns error for unknown repo" do
      {:error, :not_found} = Query.fetch_scan("nonexistent-repo-xyz")
    end
  end

  describe "fetch_pattern_registry/0" do
    test "returns the pattern registry" do
      {:ok, results} = Query.fetch_pattern_registry()
      assert is_list(results)
      assert length(results) >= 1

      [registry | _] = results
      assert Map.has_key?(registry, "patterns")
    end
  end

  describe "fetch_patterns/0" do
    test "returns flat list of patterns" do
      {:ok, patterns} = Query.fetch_patterns()
      assert is_list(patterns)
      assert length(patterns) >= 100
    end
  end

  describe "fetch_recipes/1" do
    test "returns all recipes" do
      {:ok, recipes} = Query.fetch_recipes()
      assert is_list(recipes)
      assert length(recipes) >= 4
    end
  end

  describe "fetch_index/0" do
    test "returns the master index" do
      {:ok, results} = Query.fetch_index()
      assert is_list(results)
      [index | _] = results
      assert Map.has_key?(index, "total_scans")
    end
  end

  describe "fetch_substitutions/0" do
    test "returns substitutions list" do
      {:ok, subs} = Query.fetch_substitutions()
      assert is_list(subs)
      assert length(subs) == 20
    end
  end

  # ---------------------------------------------------------------------------
  # Filtered Queries
  # ---------------------------------------------------------------------------

  describe "scans_by_severity/1" do
    test "filters by severity" do
      {:ok, scans} = Query.scans_by_severity("High")
      assert is_list(scans)

      Enum.each(scans, fn %{scan: scan} ->
        high = scan
          |> Map.get("weak_points", [])
          |> Enum.any?(&(Map.get(&1, "severity") == "High"))
        assert high
      end)
    end
  end

  describe "scans_by_category/1" do
    test "filters by category" do
      {:ok, scans} = Query.scans_by_category("PanicPath")
      assert is_list(scans)

      Enum.each(scans, fn %{scan: scan} ->
        has_category = scan
          |> Map.get("weak_points", [])
          |> Enum.any?(&(Map.get(&1, "category") == "PanicPath"))
        assert has_category
      end)
    end
  end

  describe "recipes_above_confidence/1" do
    test "filters recipes by confidence threshold" do
      {:ok, recipes} = Query.recipes_above_confidence(0.9)
      assert is_list(recipes)

      Enum.each(recipes, fn recipe ->
        assert Map.get(recipe, "confidence", 0.0) >= 0.9
      end)
    end
  end

  # ---------------------------------------------------------------------------
  # Cross-Repo Analytics
  # ---------------------------------------------------------------------------

  describe "cross_repo_patterns/1" do
    test "finds patterns in multiple repos" do
      {:ok, patterns} = Query.cross_repo_patterns(3)
      assert is_list(patterns)

      Enum.each(patterns, fn p ->
        assert Map.get(p, "repos_affected", 0) >= 3
      end)
    end

    test "higher threshold returns fewer patterns" do
      {:ok, low} = Query.cross_repo_patterns(1)
      {:ok, high} = Query.cross_repo_patterns(10)
      assert length(low) >= length(high)
    end
  end

  describe "most_vulnerable_repos/1" do
    test "returns repos ranked by weak point count" do
      {:ok, repos} = Query.most_vulnerable_repos(5)
      assert is_list(repos)
      assert length(repos) <= 5

      if length(repos) >= 2 do
        [first, second | _] = repos
        assert first.count >= second.count
      end

      Enum.each(repos, fn r ->
        assert Map.has_key?(r, :repo)
        assert Map.has_key?(r, :count)
        assert Map.has_key?(r, :categories)
        assert r.count > 0
      end)
    end
  end

  describe "category_distribution/0" do
    test "returns category breakdown" do
      {:ok, distribution} = Query.category_distribution()
      assert is_list(distribution)
      assert length(distribution) >= 1

      Enum.each(distribution, fn cat ->
        assert Map.has_key?(cat, :category)
        assert Map.has_key?(cat, :total)
        assert cat.total > 0
      end)
    end
  end

  describe "recipe_coverage/0" do
    test "returns coverage metrics" do
      {:ok, coverage} = Query.recipe_coverage()
      assert Map.has_key?(coverage, :total_patterns)
      assert Map.has_key?(coverage, :patterns_with_recipe)
      assert Map.has_key?(coverage, :coverage_pct)
      assert Map.has_key?(coverage, :total_recipes)
      assert coverage.total_patterns > 0
      assert coverage.total_recipes > 0
    end
  end

  describe "pipeline_health/0" do
    test "returns comprehensive health summary" do
      {:ok, health} = Query.pipeline_health()
      assert Map.has_key?(health, :repos_scanned)
      assert Map.has_key?(health, :total_weak_points)
      assert Map.has_key?(health, :canonical_patterns)
      assert Map.has_key?(health, :fix_recipes)
      assert Map.has_key?(health, :outcomes_recorded)
      assert Map.has_key?(health, :success_rate)

      assert health.repos_scanned >= 3
      assert health.canonical_patterns >= 100
      assert health.fix_recipes >= 4
    end
  end
end
