# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Tests for branch-protection hygiene rules (BP001-BP007). Every BP rule
# is API-backed; the suite exercises the no-token path (returns `[]`
# cleanly) for each rule, plus the scan/2 facade shape and the
# dispatch-recommendation routing/confidence calibration.

defmodule Hypatia.Rules.BranchProtectionTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.BranchProtection

  @tmp_dir System.tmp_dir!()

  defp create_repo do
    repo = Path.join(@tmp_dir, "bp_test_#{System.unique_integer([:positive])}")
    File.mkdir_p!(repo)
    repo
  end

  # Wipe token env for the duration of a test block so the no-token
  # path is what runs. on_exit restores whatever was there.
  defp scrub_tokens do
    old_t = System.get_env("GITHUB_TOKEN")
    old_p = System.get_env("HYPATIA_DISPATCH_PAT")
    System.delete_env("GITHUB_TOKEN")
    System.delete_env("HYPATIA_DISPATCH_PAT")

    on_exit(fn ->
      if old_t, do: System.put_env("GITHUB_TOKEN", old_t)
      if old_p, do: System.put_env("HYPATIA_DISPATCH_PAT", old_p)
    end)

    :ok
  end

  describe "no-token regression (each rule returns [] cleanly)" do
    setup do
      scrub_tokens()
      :ok
    end

    test "bp001 returns [] without token" do
      assert BranchProtection.bp001_required_signatures_missing("o", "r") == []
    end

    test "bp002 returns [] without token" do
      assert BranchProtection.bp002_required_linear_history_missing("o", "r") == []
    end

    test "bp003 returns [] without token" do
      assert BranchProtection.bp003_required_approving_reviews_below_one("o", "r") == []
    end

    test "bp004 returns [] without token" do
      assert BranchProtection.bp004_dismiss_stale_reviews_off("o", "r") == []
    end

    test "bp005 returns [] without token" do
      assert BranchProtection.bp005_codeowners_required_but_missing("o", "r") == []
    end

    test "bp006 returns [] without token" do
      assert BranchProtection.bp006_enforce_admins_off("o", "r") == []
    end

    test "bp007 returns [] without token" do
      assert BranchProtection.bp007_force_push_or_delete_allowed("o", "r") == []
    end
  end

  describe "scan/2 facade shape" do
    setup do
      scrub_tokens()
      :ok
    end

    test "returns standard {findings, total, by_severity, dispatch} map" do
      repo = create_repo()
      on_exit(fn -> File.rm_rf!(repo) end)

      result = BranchProtection.scan(repo, owner_repo: {"hyperpolymath", "nonexistent"})

      assert is_map(result)
      assert Map.has_key?(result, :findings)
      assert Map.has_key?(result, :total)
      assert Map.has_key?(result, :by_severity)
      assert Map.has_key?(result, :dispatch)
    end

    test "returns empty findings with no token + no owner_repo + no git origin" do
      repo = create_repo()
      on_exit(fn -> File.rm_rf!(repo) end)

      result = BranchProtection.scan(repo)
      assert result.findings == []
      assert result.total == 0
      assert result.by_severity == %{}
      assert result.dispatch == []
    end

    test "returns empty findings with no token even when owner_repo passed" do
      repo = create_repo()
      on_exit(fn -> File.rm_rf!(repo) end)

      result = BranchProtection.scan(repo, owner_repo: {"o", "r"})
      assert result.findings == []
      assert result.total == 0
    end
  end

  describe "dispatch_recommendations/1" do
    test "routes every BP rule to sustainabot with calibrated confidence" do
      findings = [
        %{rule: "BP001", severity: :high, action: :report, reason: "x"},
        %{rule: "BP002", severity: :warn, action: :report, reason: "x"},
        %{rule: "BP003", severity: :critical, action: :report, reason: "x"},
        %{rule: "BP004", severity: :warn, action: :report, reason: "x"},
        %{rule: "BP005", severity: :high, action: :report, reason: "x"},
        %{rule: "BP006", severity: :high, action: :report, reason: "x"},
        %{rule: "BP007", severity: :critical, action: :report, reason: "x"}
      ]

      recs = BranchProtection.dispatch_recommendations(findings)

      assert Enum.all?(recs, &(&1.bot == :sustainabot))
    end

    test "maps severity to confidence per the documented table" do
      findings = [
        %{rule: "BP003", severity: :critical, action: :report, reason: "x"},
        %{rule: "BP001", severity: :high, action: :report, reason: "x"},
        %{rule: "BP002", severity: :warn, action: :report, reason: "x"},
        %{rule: "BPxxx", severity: :info, action: :report, reason: "x"}
      ]

      [r1, r2, r3, r4] = BranchProtection.dispatch_recommendations(findings)

      assert r1.confidence == 0.92
      assert r2.confidence == 0.85
      assert r3.confidence == 0.75
      assert r4.confidence == 0.60
    end

    test "returns [] when no findings to dispatch" do
      assert BranchProtection.dispatch_recommendations([]) == []
    end
  end

  describe "module surface" do
    test "all seven BP rule functions are exported with arity 2" do
      exports = BranchProtection.__info__(:functions) |> Map.new()

      assert exports[:bp001_required_signatures_missing] == 2
      assert exports[:bp002_required_linear_history_missing] == 2
      assert exports[:bp003_required_approving_reviews_below_one] == 2
      assert exports[:bp004_dismiss_stale_reviews_off] == 2
      assert exports[:bp005_codeowners_required_but_missing] == 2
      assert exports[:bp006_enforce_admins_off] == 2
      assert exports[:bp007_force_push_or_delete_allowed] == 2
    end

    test "scan/2 and dispatch_recommendations/1 are exported" do
      exports = BranchProtection.__info__(:functions) |> Map.new()
      assert exports[:scan] == 2
      assert exports[:dispatch_recommendations] == 1
    end
  end
end
