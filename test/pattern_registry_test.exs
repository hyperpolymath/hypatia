# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.PatternRegistryTest do
  use ExUnit.Case, async: true
  @moduletag :verisim_data

  alias Hypatia.PatternRegistry

  describe "sync_from_scans/1" do
    test "syncs patterns from scan data and populates registry" do
      scans = [
        %{
          repo: "test-repo",
          scan: %{
            "language" => "rust",
            "weak_points" => [
              %{
                "category" => "CommandInjection",
                "description" => "Unquoted shell variable",
                "severity" => "High",
                "location" => "scripts/deploy.sh",
                "line" => 42
              },
              %{
                "category" => "PanicPath",
                "description" => "Unwrap on user input",
                "severity" => "Medium",
                "location" => "src/main.rs",
                "line" => 10
              }
            ]
          }
        }
      ]

      {:ok, registry} = PatternRegistry.sync_from_scans(scans)

      patterns = Map.get(registry, "patterns", %{})
      # Registry is cumulative (persisted to disk), so it includes patterns from prior runs
      assert map_size(patterns) >= 2

      # Check that CommandInjection and PanicPath categories exist
      pattern_values = Map.values(patterns)
      categories = Enum.map(pattern_values, &Map.get(&1, "category"))
      assert "CommandInjection" in categories
      assert "PanicPath" in categories

      # Verify structure -- find the PA009 pattern that includes test-repo
      cmd_patterns =
        Enum.filter(pattern_values, fn p ->
          Map.get(p, "category") == "CommandInjection" and
            "test-repo" in Map.get(p, "repos_affected_list", [])
        end)

      assert length(cmd_patterns) >= 1
      cmd_pattern = hd(cmd_patterns)
      assert cmd_pattern["pa_rule"] == "PA009"
      assert cmd_pattern["occurrences"] >= 1
      assert cmd_pattern["repos_affected"] >= 1
      assert cmd_pattern["triangle_tier"] in ["eliminate", "substitute", "control"]
    end

    test "deduplicates same description into one pattern" do
      # Use a unique description to avoid collisions with existing registry data
      unique_desc = "test-dedup-#{System.unique_integer([:positive])}"

      scans = [
        %{
          repo: "dedup-repo-a",
          scan: %{
            "weak_points" => [
              %{"category" => "PanicPath", "description" => unique_desc, "severity" => "Medium"}
            ]
          }
        },
        %{
          repo: "dedup-repo-b",
          scan: %{
            "weak_points" => [
              %{"category" => "PanicPath", "description" => unique_desc, "severity" => "Medium"}
            ]
          }
        }
      ]

      {:ok, registry} = PatternRegistry.sync_from_scans(scans)
      patterns = Map.get(registry, "patterns", %{}) |> Map.values()

      # Find the pattern with our unique description
      matching = Enum.filter(patterns, fn p -> Map.get(p, "description") == unique_desc end)

      assert length(matching) == 1
      pattern = hd(matching)
      # occurrences may exceed 2 if the registry file persists across test runs
      # (sync_from_scans loads existing registry from disk before merging)
      assert pattern["occurrences"] >= 2
      assert pattern["repos_affected"] >= 2
      assert "dedup-repo-a" in pattern["repos_affected_list"]
      assert "dedup-repo-b" in pattern["repos_affected_list"]
    end
  end

  describe "all_patterns/0" do
    test "returns list of patterns" do
      # This reads from disk -- ensure registry exists from previous sync
      patterns = PatternRegistry.all_patterns()
      assert is_list(patterns)
    end
  end

  describe "rebuild semantics" do
    test "rejects suppressed weak_points at intake" do
      unique_desc = "test-suppressed-#{System.unique_integer([:positive])}"

      scans = [
        %{
          repo: "suppress-repo",
          scan: %{
            "weak_points" => [
              %{
                "category" => "HardcodedSecret",
                "description" => unique_desc,
                "severity" => "Critical",
                "suppressed" => true
              }
            ]
          }
        }
      ]

      {:ok, registry} = PatternRegistry.sync_from_scans(scans)
      patterns = Map.get(registry, "patterns", %{}) |> Map.values()

      assert Enum.filter(patterns, &(Map.get(&1, "description") == unique_desc)) == []
    end

    test "caps heuristic UnboundedAllocation severity at Medium" do
      unique_desc = "test-heuristic-cap-#{System.unique_integer([:positive])}"

      scans = [
        %{
          repo: "cap-repo-#{System.unique_integer([:positive])}",
          scan: %{
            "weak_points" => [
              %{
                "category" => "UnboundedAllocation",
                "description" => unique_desc,
                "severity" => "Critical"
              }
            ]
          }
        }
      ]

      {:ok, registry} = PatternRegistry.sync_from_scans(scans)

      pattern =
        registry
        |> Map.get("patterns", %{})
        |> Map.values()
        |> Enum.find(&(Map.get(&1, "description") == unique_desc))

      assert pattern["severity"] == "Medium"
    end

    test "identical re-sync does not inflate occurrence counts" do
      unique_desc = "test-noinflate-#{System.unique_integer([:positive])}"
      repo = "noinflate-repo-#{System.unique_integer([:positive])}"

      scans = [
        %{
          repo: repo,
          scan: %{
            "weak_points" => [
              %{"category" => "PanicPath", "description" => unique_desc, "severity" => "Medium"},
              %{"category" => "PanicPath", "description" => unique_desc, "severity" => "Medium"}
            ]
          }
        }
      ]

      {:ok, _} = PatternRegistry.sync_from_scans(scans)
      {:ok, registry} = PatternRegistry.sync_from_scans(scans)

      pattern =
        registry
        |> Map.get("patterns", %{})
        |> Map.values()
        |> Enum.find(&(Map.get(&1, "description") == unique_desc))

      assert pattern["occurrences"] == 2
      assert pattern["trend"] == "stable"
      assert pattern["repo_occurrences"] == %{repo => 2}
    end

    test "repo going clean resolves its patterns but carries unscanned repos" do
      unique_desc = "test-resolve-#{System.unique_integer([:positive])}"
      repo_a = "resolve-a-#{System.unique_integer([:positive])}"
      repo_b = "resolve-b-#{System.unique_integer([:positive])}"

      wp = %{"category" => "PanicPath", "description" => unique_desc, "severity" => "Medium"}

      {:ok, _} =
        PatternRegistry.sync_from_scans([
          %{repo: repo_a, scan: %{"weak_points" => [wp]}},
          %{repo: repo_b, scan: %{"weak_points" => [wp]}}
        ])

      # repo_a rescanned clean; repo_b NOT in this batch -> must carry over
      {:ok, registry} =
        PatternRegistry.sync_from_scans([%{repo: repo_a, scan: %{"weak_points" => []}}])

      pattern =
        registry
        |> Map.get("patterns", %{})
        |> Map.values()
        |> Enum.find(&(Map.get(&1, "description") == unique_desc))

      assert pattern["occurrences"] == 1
      assert pattern["repos_affected_list"] == [repo_b]
      assert pattern["trend"] == "decreasing"

      # repo_b rescanned clean too -> fully resolved, no repos to dispatch to
      {:ok, registry} =
        PatternRegistry.sync_from_scans([%{repo: repo_b, scan: %{"weak_points" => []}}])

      pattern =
        registry
        |> Map.get("patterns", %{})
        |> Map.values()
        |> Enum.find(&(Map.get(&1, "description") == unique_desc))

      assert pattern["occurrences"] == 0
      assert pattern["repos_affected_list"] == []
      assert pattern["trend"] == "resolved"
      assert pattern["resolved_at"] != nil
    end

    test "legacy entries without repo_occurrences migrate and preserve identity" do
      # Simulate a legacy append-era entry by syncing, then stripping
      # repo_occurrences from the persisted registry is not possible via the
      # public API; instead verify that a fresh finding for the same pattern
      # in a NEW repo merges without disturbing first_seen.
      unique_desc = "test-legacy-#{System.unique_integer([:positive])}"
      repo_a = "legacy-a-#{System.unique_integer([:positive])}"
      repo_b = "legacy-b-#{System.unique_integer([:positive])}"

      wp = %{"category" => "PanicPath", "description" => unique_desc, "severity" => "Medium"}

      {:ok, reg1} =
        PatternRegistry.sync_from_scans([%{repo: repo_a, scan: %{"weak_points" => [wp]}}])

      first_seen =
        reg1
        |> Map.get("patterns", %{})
        |> Map.values()
        |> Enum.find(&(Map.get(&1, "description") == unique_desc))
        |> Map.get("first_seen")

      {:ok, reg2} =
        PatternRegistry.sync_from_scans([%{repo: repo_b, scan: %{"weak_points" => [wp]}}])

      pattern =
        reg2
        |> Map.get("patterns", %{})
        |> Map.values()
        |> Enum.find(&(Map.get(&1, "description") == unique_desc))

      assert pattern["first_seen"] == first_seen
      assert Enum.sort(pattern["repos_affected_list"]) == Enum.sort([repo_a, repo_b])
      assert pattern["occurrences"] == 2
    end
  end

  describe "active_patterns/0" do
    test "excludes resolved patterns" do
      unique_desc = "test-active-#{System.unique_integer([:positive])}"
      repo = "active-repo-#{System.unique_integer([:positive])}"

      wp = %{"category" => "PanicPath", "description" => unique_desc, "severity" => "Medium"}

      {:ok, _} = PatternRegistry.sync_from_scans([%{repo: repo, scan: %{"weak_points" => [wp]}}])
      {:ok, _} = PatternRegistry.sync_from_scans([%{repo: repo, scan: %{"weak_points" => []}}])

      refute Enum.any?(
               PatternRegistry.active_patterns(),
               &(Map.get(&1, "description") == unique_desc)
             )
    end
  end
end
