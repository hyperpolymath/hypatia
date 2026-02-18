# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.PatternAnalyzerTest do
  use ExUnit.Case, async: false

  alias Hypatia.PatternAnalyzer

  describe "analyze_all_scans/0" do
    test "runs full pipeline successfully" do
      {:ok, summary} = PatternAnalyzer.analyze_all_scans()

      assert is_map(summary)
      assert summary.total_repos >= 3
      assert summary.total_actions >= 1
      assert Map.has_key?(summary, :triangle_breakdown)
      assert Map.has_key?(summary, :manifest_path)

      # Triangle breakdown should have all three tiers
      tb = summary.triangle_breakdown
      assert Map.has_key?(tb, :eliminate)
      assert Map.has_key?(tb, :substitute)
      assert Map.has_key?(tb, :control)
    end
  end

  describe "generate_summary/2" do
    test "generates summary from routed actions and scans" do
      recipe = %{"confidence" => 0.99}
      pattern = %{"id" => "test", "description" => "test"}

      actions = [
        {:eliminate, recipe, pattern},
        {:substitute, recipe, pattern},
        {:control, pattern}
      ]

      scans = [
        %{repo: "test", scan: %{"weak_points" => [%{}, %{}]}}
      ]

      summary = PatternAnalyzer.generate_summary(actions, scans)
      assert summary.total_repos == 1
      assert summary.total_weak_points == 2
      assert summary.total_actions == 3
      assert summary.triangle_breakdown.eliminate == 1
      assert summary.triangle_breakdown.substitute == 1
      assert summary.triangle_breakdown.control == 1
    end
  end
end
