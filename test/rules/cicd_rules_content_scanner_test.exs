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
end
