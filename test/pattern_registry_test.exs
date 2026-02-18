# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.PatternRegistryTest do
  use ExUnit.Case, async: true

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

      # Verify structure — find the PA009 pattern that includes test-repo
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
      matching =
        Enum.filter(patterns, fn p -> Map.get(p, "description") == unique_desc end)

      assert length(matching) == 1
      pattern = hd(matching)
      assert pattern["occurrences"] == 2
      assert pattern["repos_affected"] == 2
      assert "dedup-repo-a" in pattern["repos_affected_list"]
      assert "dedup-repo-b" in pattern["repos_affected_list"]
    end
  end

  describe "all_patterns/0" do
    test "returns list of patterns" do
      # This reads from disk — ensure registry exists from previous sync
      patterns = PatternRegistry.all_patterns()
      assert is_list(patterns)
    end
  end
end
