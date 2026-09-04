# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.CicdRules.ContentScannerTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.CicdRules

  # `scan_content_patterns/1` activates the previously-dormant
  # %{pattern: regex, applies_to: globs} entries in @blocked_patterns.
  # Per-rule positive + negative + pragma + exception coverage.

  setup do
    dir = Path.join(System.tmp_dir!(), "hyp-scan-test-#{:erlang.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf!(dir) end)
    {:ok, dir: dir}
  end

  describe "positive cases — each rule fires on a matching file" do
    test "innerhtml_usage on .js", %{dir: dir} do
      File.write!(Path.join(dir, "evil.js"), ~s|el.innerHTML = "<b>x</b>"|)
      findings = CicdRules.scan_content_patterns(dir)
      assert Enum.any?(findings, &(&1.rule == :innerhtml_usage))
    end

    test "eval_in_shell on .sh", %{dir: dir} do
      File.write!(Path.join(dir, "bad.sh"), "eval \"$user_input\"\n")
      findings = CicdRules.scan_content_patterns(dir)
      assert Enum.any?(findings, &(&1.rule == :eval_in_shell))
    end

    test "download_then_run_shell on curl|sh", %{dir: dir} do
      File.write!(Path.join(dir, "i.sh"), "curl -sSL https://example.com/i.sh | sh\n")
      findings = CicdRules.scan_content_patterns(dir)
      assert Enum.any?(findings, &(&1.rule == :download_then_run_shell))
    end

    test "hardcoded_tmp on .sh", %{dir: dir} do
      File.write!(Path.join(dir, "x.sh"), "cp file /tmp/staging\n")
      findings = CicdRules.scan_content_patterns(dir)
      assert Enum.any?(findings, &(&1.rule == :hardcoded_tmp))
    end
  end

  describe "applies_to filter" do
    test "innerhtml_usage does NOT fire on .md (applies_to is *.js/*.res)", %{dir: dir} do
      File.write!(Path.join(dir, "README.md"), ~s|Just text with .innerHTML = "x"|)
      findings = CicdRules.scan_content_patterns(dir)
      refute Enum.any?(findings, &(&1.rule == :innerhtml_usage))
    end

    test "eval_in_shell does NOT fire on .yml", %{dir: dir} do
      File.write!(Path.join(dir, "ci.yml"), "steps:\n  - run: eval xyz\n")
      findings = CicdRules.scan_content_patterns(dir)
      refute Enum.any?(findings, &(&1.rule == :eval_in_shell))
    end
  end

  describe "inline pragma — # hypatia:ignore <rule_id>" do
    test "same-line pragma suppresses", %{dir: dir} do
      File.write!(Path.join(dir, "ok.sh"), "eval \"$safe\" # hypatia:ignore eval_in_shell\n")
      findings = CicdRules.scan_content_patterns(dir)
      refute Enum.any?(findings, &(&1.rule == :eval_in_shell))
    end

    test "preceding-line pragma suppresses", %{dir: dir} do
      File.write!(Path.join(dir, "ok2.sh"), "# hypatia:ignore eval_in_shell\neval \"$safe\"\n")
      findings = CicdRules.scan_content_patterns(dir)
      refute Enum.any?(findings, &(&1.rule == :eval_in_shell))
    end
  end

  describe "exception substring" do
    test "hardcoded_tmp exempts Containerfile per rule's `exception:` field", %{dir: dir} do
      File.write!(Path.join(dir, "Containerfile"), "RUN /tmp/foo\n")
      findings = CicdRules.scan_content_patterns(dir)
      refute Enum.any?(findings, &(&1.rule == :hardcoded_tmp))
    end
  end

  # ── Regression guard: the engine must be able to SEE `.github/` ───────
  #
  # `matching_files/2` enumerated with `Path.wildcard(..., match_dot: false)`,
  # which never matches a dot-prefixed segment. Every workflow lives under
  # `.github/`, so no workflow was reachable and the only two YAML-scoped
  # rules could never fire on one. Proven with a byte-identical file: at
  # `.github/workflows/ci.yml` it produced nothing; at `root-ci.yml` it fired.
  # If this test ever goes red, the scanner has gone blind to CI again.
  describe "dot-directory reachability" do
    test "a rule fires on a file under .github/", %{dir: dir} do
      wf = Path.join(dir, ".github/workflows")
      File.mkdir_p!(wf)
      File.write!(Path.join(wf, "ci.yml"), "steps:\n  - run: npx prettier .\n")
      findings = CicdRules.scan_content_patterns(dir)
      assert Enum.any?(findings, &(&1.rule == :npx_in_workflow))
    end
  end

  # ── Scanner-derived rule: --frozen-lockfile ───────────────────────────
  #
  # Positive, canonical-fix negative, and a C4 comment case. The trio is the
  # house contract: a rule that fires but cannot be satisfied by the fix it
  # names is a gate that cannot pass, and one that matches commented-out
  # code repeats a defect this repo has already shipped once.
  describe "install_without_frozen_lockfile" do
    setup %{dir: dir} do
      wf = Path.join(dir, ".github/workflows")
      File.mkdir_p!(wf)
      {:ok, wf: wf}
    end

    test "fires on a bare `bun install`, at the right line", %{dir: dir, wf: wf} do
      File.write!(Path.join(wf, "ci.yml"), "steps:\n  - run: echo hi\n  - run: bun install\n")
      findings = CicdRules.scan_content_patterns(dir)
      finding = Enum.find(findings, &(&1.rule == :install_without_frozen_lockfile))
      assert finding
      # Line 3, not 1 -- the content engine is the only source of a real
      # `:line`, and it is what makes SARIF `startLine` non-degenerate.
      assert finding.line == 3
    end

    test "does NOT fire on the canonical fix", %{dir: dir, wf: wf} do
      File.write!(Path.join(wf, "ok.yml"), "steps:\n  - run: bun install --frozen-lockfile\n")
      findings = CicdRules.scan_content_patterns(dir)
      refute Enum.any?(findings, &(&1.rule == :install_without_frozen_lockfile))
    end

    test "C4: does NOT fire on a commented-out install", %{dir: dir, wf: wf} do
      File.write!(Path.join(wf, "c.yml"), "steps:\n  # - run: bun install\n  - run: echo ok\n")
      findings = CicdRules.scan_content_patterns(dir)
      refute Enum.any?(findings, &(&1.rule == :install_without_frozen_lockfile))
    end

    test "still fires when the comment marker is TRAILING, not leading", %{dir: dir, wf: wf} do
      File.write!(Path.join(wf, "t.yml"), "steps:\n  - run: bun install  # TODO pin this\n")
      findings = CicdRules.scan_content_patterns(dir)
      assert Enum.any?(findings, &(&1.rule == :install_without_frozen_lockfile))
    end
  end
end
