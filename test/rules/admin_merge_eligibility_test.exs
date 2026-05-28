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

    # ALARP type-1 safety: a context flagged phantom by BP008 (sample-based,
    # main-branch) may still be in-progress on THIS PR if it's a path-
    # filtered workflow whose path was changed here but not on recent main
    # commits. Admin-merging would bypass an actively-running check.
    test "in-progress phantom on this PR is NOT treated as phantom (waits for it)" do
      pr_state = %{
        required_contexts: ["lint", "path-filtered-thing"],
        phantom_contexts: ["path-filtered-thing"],
        rollup: [
          %{"name" => "lint", "conclusion" => "SUCCESS"},
          %{"name" => "path-filtered-thing", "conclusion" => nil, "status" => "in_progress"}
        ]
      }

      assert AM.am010_phantom_only_blocker?(pr_state) == :not_phantom_only
    end

    test "queued phantom on this PR is NOT treated as phantom" do
      pr_state = %{
        required_contexts: ["lint", "phantom-thing"],
        phantom_contexts: ["phantom-thing"],
        rollup: [
          %{"name" => "lint", "conclusion" => "SUCCESS"},
          %{"name" => "phantom-thing", "conclusion" => "QUEUED"}
        ]
      }

      assert AM.am010_phantom_only_blocker?(pr_state) == :not_phantom_only
    end

    test "pending status (lowercase) on phantom is NOT treated as phantom" do
      pr_state = %{
        required_contexts: ["lint", "phantom-thing"],
        phantom_contexts: ["phantom-thing"],
        rollup: [
          %{"name" => "lint", "conclusion" => "SUCCESS"},
          %{"name" => "phantom-thing", "conclusion" => nil, "status" => "pending"}
        ]
      }

      assert AM.am010_phantom_only_blocker?(pr_state) == :not_phantom_only
    end

    test "all phantoms in-progress collapses set to empty → not eligible" do
      pr_state = %{
        required_contexts: ["lint", "phantom-a", "phantom-b"],
        phantom_contexts: ["phantom-a", "phantom-b"],
        rollup: [
          %{"name" => "lint", "conclusion" => "SUCCESS"},
          %{"name" => "phantom-a", "conclusion" => nil, "status" => "in_progress"},
          %{"name" => "phantom-b", "conclusion" => nil, "status" => "in_progress"}
        ]
      }

      assert AM.am010_phantom_only_blocker?(pr_state) == :not_phantom_only
    end

    test "FAILURE on phantom is NOT eligible — it ran and failed here" do
      # A BP008-flagged phantom that actually ran and failed on this PR
      # is NOT phantom for this commit. AM010 declines.
      pr_state = %{
        required_contexts: ["lint", "phantom-thing"],
        phantom_contexts: ["phantom-thing"],
        rollup: [
          %{"name" => "lint", "conclusion" => "SUCCESS"},
          %{"name" => "phantom-thing", "conclusion" => "FAILURE"}
        ]
      }

      assert AM.am010_phantom_only_blocker?(pr_state) == :not_phantom_only
    end

    test "SUCCESS on phantom is just passing — eligible regardless" do
      # The phantom set was a main-branch sample. If THIS PR has the
      # context with SUCCESS, it's passing — eligibility holds for the
      # standard "all passing" reason.
      pr_state = %{
        required_contexts: ["lint", "phantom-thing"],
        phantom_contexts: ["phantom-thing"],
        rollup: [
          %{"name" => "lint", "conclusion" => "SUCCESS"},
          %{"name" => "phantom-thing", "conclusion" => "SUCCESS"}
        ]
      }

      # SUCCESS-on-phantom-on-this-PR collapses the phantoms_on_this_pr
      # set to empty (phantom-thing is now in rollup_names).
      # phantoms_on_this_pr = {} → :not_phantom_only.
      # Auto-merge handles this fine.
      assert AM.am010_phantom_only_blocker?(pr_state) == :not_phantom_only
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
