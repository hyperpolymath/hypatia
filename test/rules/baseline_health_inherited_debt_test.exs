# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.BaselineHealth.InheritedDebtTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.BaselineHealth

  # BH008 — when ≥ threshold (default 3) open PRs share the same failing
  # check name the cause is almost always inherited from a degraded main
  # baseline. Flag (warn) — never auto-fix; owner must direct the root fix.

  describe "bh008_inherited_main_debt/4 — threshold semantics" do
    test "fires at exactly 3 PRs sharing a failing check (default threshold)" do
      data = [
        %{number: 101, failed_checks: ["governance/security-policy"]},
        %{number: 102, failed_checks: ["governance/security-policy"]},
        %{number: 103, failed_checks: ["governance/security-policy"]}
      ]

      assert [finding] = BaselineHealth.bh008_inherited_main_debt("foo", "bar", data)
      assert finding.rule == "BH008"
      assert finding.severity == :warn
      assert finding.auto_fixable == false
      assert finding.check_name == "governance/security-policy"
      assert finding.pr_count == 3
      assert finding.affected_prs == [101, 102, 103]
      assert finding.action == :file_root_fix_pr_on_main
    end

    test "does NOT fire at 2 PRs (below threshold)" do
      data = [
        %{number: 101, failed_checks: ["governance/security-policy"]},
        %{number: 102, failed_checks: ["governance/security-policy"]}
      ]

      assert [] = BaselineHealth.bh008_inherited_main_debt("foo", "bar", data)
    end

    test "honors custom :threshold opt" do
      data = [
        %{number: 1, failed_checks: ["ci/main"]},
        %{number: 2, failed_checks: ["ci/main"]}
      ]

      # threshold: 2 → should fire on 2 PRs
      assert [finding] =
               BaselineHealth.bh008_inherited_main_debt("foo", "bar", data, threshold: 2)

      assert finding.pr_count == 2
      assert finding.detail.threshold == 2
    end

    test "ignores per-PR-unique failures (not inherited)" do
      data = [
        %{number: 1, failed_checks: ["tests/pr-1-only"]},
        %{number: 2, failed_checks: ["tests/pr-2-only"]},
        %{number: 3, failed_checks: ["tests/pr-3-only"]}
      ]

      assert [] = BaselineHealth.bh008_inherited_main_debt("foo", "bar", data)
    end

    test "emits one finding per distinct shared check name" do
      data = [
        %{number: 1, failed_checks: ["a", "b"]},
        %{number: 2, failed_checks: ["a", "b"]},
        %{number: 3, failed_checks: ["a", "b"]}
      ]

      findings = BaselineHealth.bh008_inherited_main_debt("foo", "bar", data)
      assert length(findings) == 2
      checks = findings |> Enum.map(& &1.check_name) |> Enum.sort()
      assert checks == ["a", "b"]
    end

    test "deduplicates same PR + same check appearing twice in input" do
      data = [
        %{number: 1, failed_checks: ["x"]},
        %{number: 1, failed_checks: ["x"]},
        %{number: 2, failed_checks: ["x"]}
      ]

      # Only 2 distinct PRs → below default threshold of 3 → no finding
      assert [] = BaselineHealth.bh008_inherited_main_debt("foo", "bar", data)
    end

    test "finding never sets auto_fixable: true" do
      data =
        for i <- 1..5,
            do: %{number: i, failed_checks: ["x"]}

      assert [finding] = BaselineHealth.bh008_inherited_main_debt("foo", "bar", data)
      refute finding.auto_fixable
    end
  end
end
