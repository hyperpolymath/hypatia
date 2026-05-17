# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Hypatia.ScorecardReconcilerTest do
  use ExUnit.Case, async: true

  alias Hypatia.ScorecardReconciler, as: R
  alias Hypatia.ScorecardReconciler.Registry

  defp alert(rule, opts \\ []) do
    %{
      "number" => Keyword.get(opts, :number, 1),
      "rule" => %{"id" => rule},
      "tool" => %{"name" => Keyword.get(opts, :tool, "Scorecard")},
      "most_recent_instance" => %{
        "location" => %{
          "path" => Keyword.get(opts, :path, ".github/workflows/x.yml"),
          "start_line" => Keyword.get(opts, :line, 1)
        }
      }
    }
  end

  describe "classify/2 — the 4-axis taxonomy" do
    test "Maintained is informational -> dismiss_info / won't fix" do
      {action, code, why} = R.classify(alert("MaintainedID"))
      assert action == :dismiss_info
      assert code == "won't fix"
      assert why =~ "no remediation"
    end

    test "Contributors is informational -> dismiss_info" do
      assert {:dismiss_info, _, _} = R.classify(alert("ContributorsID"))
    end

    test "Pinned-Deps on a pin-EXEMPT ref -> dismiss_accept with SLSA rationale" do
      ctx = %{
        located_action_ref:
          "slsa-framework/slsa-github-generator/.github/workflows/generator_generic_slsa3.yml@v2.1.0"
      }

      {action, code, why} = R.classify(alert("PinnedDependenciesID"), ctx)
      assert action == :dismiss_accept
      assert code == "won't fix"
      assert why =~ "provenance"
    end

    test "Pinned-Deps on a NON-exempt ref -> fix (still pin it)" do
      ctx = %{located_action_ref: "actions/checkout@v4"}
      assert {:fix, nil, _} = R.classify(alert("PinnedDependenciesID"), ctx)
    end

    test "SAST -> fix (recommend actions matrix)" do
      {action, _, why} = R.classify(alert("SASTID"))
      assert action == :fix
      assert why =~ "actions"
    end

    test "unknown rule -> open_escalate (never silently dropped)" do
      assert {:open_escalate, nil, _} = R.classify(alert("SomeBrandNewRuleID"))
    end

    test "BranchProtection -> fix_settings (#265, not escalate, not code fix)" do
      {action, code, why} = R.classify(alert("BranchProtectionID"))
      assert action == :fix_settings
      assert code == nil
      assert why =~ "settings API"
    end

    test "CodeReview -> fix_settings (#265)" do
      assert {:fix_settings, nil, _} = R.classify(alert("CodeReviewID"))
    end

    test "fix_settings is distinct from :fix and :open_escalate" do
      assert {:fix_settings, _, _} = R.classify(alert("BranchProtectionID"))
      assert {:fix, _, _} = R.classify(alert("SASTID"))
      assert {:open_escalate, _, _} = R.classify(alert("SomeBrandNewRuleID"))
    end
  end

  describe "fingerprint/3 — stable across line drift" do
    test "identical except for line number => identical fingerprint" do
      a = alert("SASTID", line: 1)
      b = alert("SASTID", line: 999)
      assert R.fingerprint("o", "r", a) == R.fingerprint("o", "r", b)
    end

    test "different rule => different fingerprint" do
      refute R.fingerprint("o", "r", alert("SASTID")) ==
               R.fingerprint("o", "r", alert("MaintainedID"))
    end

    test "different repo => different fingerprint" do
      refute R.fingerprint("o", "r1", alert("SASTID")) ==
               R.fingerprint("o", "r2", alert("SASTID"))
    end
  end

  describe "Registry — learning substrate round-trip" do
    test "record is pure and threads through a map" do
      reg = Registry.record(%{}, "fp1", %{"action" => "dismiss_info"})
      assert Registry.known?(reg, "fp1")
      assert Registry.get(reg, "fp1")["action"] == "dismiss_info"
      refute Registry.known?(reg, "missing")
    end
  end
end
