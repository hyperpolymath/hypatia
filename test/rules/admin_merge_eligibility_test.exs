# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.AdminMergeEligibilityTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.AdminMergeEligibility, as: AM

  describe "am010_phantom_only_blocker?/1 — phantom-context-only blocker" do
    test "eligible when the only required context not passing is in phantoms" do
      # affinescript 2026-05-28: every required context green except
      # `spark-theatre-gate / SPARK Theatre Gate`, which is phantom.
      pr_state = %{
        required_contexts: ["lint", "semgrep", "spark-theatre-gate / SPARK Theatre Gate"],
        phantom_contexts: ["spark-theatre-gate / SPARK Theatre Gate"],
        rollup: [
          %{"name" => "lint", "conclusion" => "SUCCESS"},
          %{"name" => "semgrep", "conclusion" => "SUCCESS"}
        ]
      }

      assert AM.am010_phantom_only_blocker?(pr_state) == {:eligible, "AM010"}
    end

    test "eligible when all required pass and a phantom is also present in rollup but unconcluded" do
      pr_state = %{
        required_contexts: ["lint", "phantom-thing"],
        phantom_contexts: ["phantom-thing"],
        rollup: [
          %{"name" => "lint", "conclusion" => "SUCCESS"}
        ]
      }

      assert AM.am010_phantom_only_blocker?(pr_state) == {:eligible, "AM010"}
    end

    test "not eligible when a non-phantom required context is failing" do
      pr_state = %{
        required_contexts: ["lint", "semgrep", "phantom-thing"],
        phantom_contexts: ["phantom-thing"],
        rollup: [
          %{"name" => "lint", "conclusion" => "SUCCESS"},
          %{"name" => "semgrep", "conclusion" => "FAILURE"}
        ]
      }

      assert AM.am010_phantom_only_blocker?(pr_state) == :not_phantom_only
    end

    test "not eligible when phantom_contexts is empty" do
      pr_state = %{
        required_contexts: ["lint"],
        phantom_contexts: [],
        rollup: [%{"name" => "lint", "conclusion" => "SUCCESS"}]
      }

      assert AM.am010_phantom_only_blocker?(pr_state) == :not_phantom_only
    end

    test "not eligible when required_contexts is empty" do
      pr_state = %{
        required_contexts: [],
        phantom_contexts: ["spark-theatre-gate / SPARK Theatre Gate"],
        rollup: []
      }

      assert AM.am010_phantom_only_blocker?(pr_state) == :not_phantom_only
    end

    test "NEUTRAL and SKIPPED count as passing" do
      pr_state = %{
        required_contexts: ["lint", "optional-skipped", "phantom-thing"],
        phantom_contexts: ["phantom-thing"],
        rollup: [
          %{"name" => "lint", "conclusion" => "NEUTRAL"},
          %{"name" => "optional-skipped", "conclusion" => "SKIPPED"}
        ]
      }

      assert AM.am010_phantom_only_blocker?(pr_state) == {:eligible, "AM010"}
    end

    test "non-map input falls through to :not_phantom_only" do
      assert AM.am010_phantom_only_blocker?(nil) == :not_phantom_only
      assert AM.am010_phantom_only_blocker?("garbage") == :not_phantom_only
    end
  end

  describe "rule_catalog/0 includes AM010" do
    test "AM010 entry is present" do
      catalog = AM.rule_catalog()
      assert Map.has_key?(catalog, "AM010")
      assert catalog["AM010"] =~ "phantom"
    end
  end
end
