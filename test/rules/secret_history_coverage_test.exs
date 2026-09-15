# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.SecretHistoryCoverageTest do
  @moduledoc """
  Phase 1c — the secret-history coverage rule and its companion shallow-clone
  instrument-health probe.

  The governing requirement was that a history rule must "emit a loud, distinct
  finding when `.git/shallow` exists -- never silently return `[]`. A history
  rule that sees no history is a fake gate by construction." Most of the value
  in this file is therefore in the NEGATIVE-shaped assertions: that the rule
  fires when it should, not merely that it stays quiet when it should.
  """
  use ExUnit.Case, async: true

  alias Hypatia.Rules.GitState
  alias Hypatia.Rules.WorkflowAudit

  @rail "secret-scanner-reusable.yml"

  defp types(findings), do: Enum.map(findings, & &1.rule)

  # ─── Coverage: the repo has no secret-history gate at all ──────────────

  test "a repo with workflows but no secret-history gate is flagged" do
    contents = %{
      "ci.yml" => """
      name: CI
      on: [push]
      jobs:
        build:
          runs-on: ubuntu-latest
          steps:
            - uses: actions/checkout@v4
      """
    }

    findings = WorkflowAudit.check_secret_history_coverage(contents)

    assert types(findings) == ["missing_secret_history_scan"]
    [f] = findings
    assert f.severity == :medium
    assert f.action == :add_secret_history_scan
  end

  test "calling the estate rail counts as coverage and emits nothing" do
    contents = %{
      "secret-scanner.yml" => """
      jobs:
        scan:
          uses: hyperpolymath/standards/.github/workflows/#{@rail}@main
          secrets: inherit
      """
    }

    assert WorkflowAudit.check_secret_history_coverage(contents) == []
  end

  test "the rail short-circuits even when another workflow looks weak" do
    # A repo may call the rail AND run its own shallow working-tree scan. The
    # rail already guarantees a full-history pass, so the weak one is not a
    # coverage gap and must not be reported as one.
    contents = %{
      "secret-scanner.yml" => "uses: ./.github/workflows/#{@rail}@main",
      "extra.yml" => "run: gitleaks detect --source . --no-git"
    }

    assert WorkflowAudit.check_secret_history_coverage(contents) == []
  end

  # ─── Coverage: a direct scanner exists but cannot see history ──────────

  test "a direct scanner restricted to --no-git is flagged as working-tree only" do
    contents = %{
      "sec.yml" => """
      steps:
        - uses: actions/checkout@v4
          with:
            fetch-depth: 0
        - run: gitleaks detect --source . --no-git --redact
      """
    }

    findings = WorkflowAudit.check_secret_history_coverage(contents)

    assert types(findings) == ["secret_scan_without_history"]
    assert hd(findings).action == :add_history_pass
  end

  test "a direct scanner without fetch-depth: 0 is flagged as shallow-fed" do
    contents = %{
      "sec.yml" => """
      steps:
        - uses: actions/checkout@v4
        - run: gitleaks detect --source . --redact
      """
    }

    findings = WorkflowAudit.check_secret_history_coverage(contents)

    assert types(findings) == ["secret_scan_without_history"]
    assert hd(findings).action == :set_fetch_depth_zero
  end

  test "a direct scanner in history mode on a deep checkout is accepted" do
    contents = %{
      "sec.yml" => """
      steps:
        - uses: actions/checkout@v4
          with:
            fetch-depth: 0
        - run: gitleaks detect --source . --redact
      """
    }

    assert WorkflowAudit.check_secret_history_coverage(contents) == []
  end

  test "both gitleaks and trufflehog count as a direct scanner" do
    contents = %{
      "sec.yml" => """
      steps:
        - uses: actions/checkout@v4
          with:
            fetch-depth: 0
        - run: trufflehog filesystem . --no-git
      """
    }

    assert types(WorkflowAudit.check_secret_history_coverage(contents)) ==
             ["secret_scan_without_history"]
  end

  test "a non-map input is tolerated rather than raising" do
    # hypatia is resolved by `git ls-remote ... HEAD` at runtime by the scan
    # reusable, so an unhandled raise here reaches ~449 consumers with no PR.
    assert WorkflowAudit.check_secret_history_coverage(nil) == []
  end

  # ─── Instrument health: GS008 shallow clone ────────────────────────────

  describe "GS008 shallow clone" do
    setup do
      dir = Path.join(System.tmp_dir!(), "hypatia-gs008-#{System.unique_integer([:positive])}")
      File.mkdir_p!(Path.join(dir, ".git"))
      on_exit(fn -> File.rm_rf(dir) end)
      {:ok, dir: dir}
    end

    test "fires when .git/shallow is present", %{dir: dir} do
      File.write!(Path.join(dir, ".git/shallow"), "deadbeef\n")

      findings = GitState.gs008_shallow_clone(dir)

      assert types(findings) == ["GS008"]
      [f] = findings
      assert f.severity == :medium
      assert f.action == :deepen_checkout
      # The message must say what is unsafe, not merely that a file exists.
      assert f.reason =~ "vacuous"
    end

    test "is silent when the clone has full history", %{dir: dir} do
      refute File.exists?(Path.join(dir, ".git/shallow"))
      assert GitState.gs008_shallow_clone(dir) == []
    end

    test "the finding is NOT anchored inside .git/", %{dir: dir} do
      # REGRESSION FLOOR. `.git/` is in `@universal_excludes`
      # (scanner_suppression.ex), so a finding whose `file:` sits under `.git/`
      # is silently deleted by the path filter between `GitState.scan/1` and the
      # CLI's output. Measured 2026-09-15: anchored at `.git/shallow`, this rule
      # returned a finding from `scan/1` and produced NOTHING in the report on a
      # real `git clone --depth 1` -- a complete no-op that still passed a
      # full-clone test. Anchor stays at the repository root.
      File.write!(Path.join(dir, ".git/shallow"), "deadbeef\n")

      [f] = GitState.gs008_shallow_clone(dir)

      refute String.starts_with?(f.file, ".git/")
      assert f.file == "."
    end

    test "scan/1 includes GS008 in its findings", %{dir: dir} do
      # Guards the wiring, not the predicate: the rule existing but never being
      # summed into `scan/1` is the same defect class as `flawed_regex`, which
      # was computed and counted for its entire life without ever being added
      # to the findings list.
      File.write!(Path.join(dir, ".git/shallow"), "deadbeef\n")

      %{findings: findings} = GitState.scan(dir)

      assert "GS008" in types(findings)
    end
  end

  # ─── Wiring: the restored flawed_regex findings ────────────────────────

  test "flawed_regex findings reach audit/3's findings list" do
    # They were computed and counted but never concatenated, so the rule ran on
    # every scan in the estate and discarded everything it found.
    contents = %{"ci.yml" => ~s(      - run: grep "foo.bar" README.md\n)}

    %{findings: findings, flawed_regex_count: count} =
      WorkflowAudit.audit(["ci.yml"], contents)

    assert count > 0
    assert "flawed_regex" in Enum.map(findings, &(&1[:rule] || &1[:type]))
  end
end
