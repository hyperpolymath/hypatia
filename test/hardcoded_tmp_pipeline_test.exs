# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.HardcodedTmpPipelineTest do
  use ExUnit.Case, async: true

  alias Hypatia.CLI
  alias Hypatia.Rules.CicdRules

  # hardcoded_tmp end to end: the rule, its own-cure skips, and the
  # training-corpus exemption — through the REAL pipeline (scan →
  # cli.ex normalisation → ScannerSuppression), never a hand-built
  # finding. A hand-built `%{rule_module: "content_patterns"}` fed to the
  # suppressor would derive both legs from one source, so a rename on
  # either side would leave it green.

  @pid_line ~s|PID_FILE="/tmp/stapeln-server.pid"\n|

  setup do
    dir = Path.join(System.tmp_dir!(), "hyp-tmp-test-#{:erlang.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf!(dir) end)
    {:ok, dir: dir}
  end

  defp write!(dir, rel, content) do
    path = Path.join(dir, rel)
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, content)
  end

  defp tmp_findings(dir) do
    dir
    |> CLI.collect_findings([:content_patterns])
    |> Enum.filter(&(&1.type == "hardcoded_tmp"))
  end

  describe "the rule fires on a predictable path (positive control)" do
    test "a /tmp pid file outside the training corpus is reported", %{dir: dir} do
      write!(dir, "scripts/launcher.sh", @pid_line)

      assert [finding] = tmp_findings(dir)
      assert finding.file =~ "scripts/launcher.sh"
      assert finding.line == 1
    end

    test "the emitted rule_module is literally \"content_patterns\"", %{dir: dir} do
      # Trap: CicdRules emits under TWO spellings — banned_language_file as
      # "cicd_rules", the content engine as "content_patterns". Every
      # suppression key and .hypatia-ignore line depends on this string.
      write!(dir, "scripts/launcher.sh", @pid_line)

      assert [%{rule_module: "content_patterns"}] = tmp_findings(dir)
    end

    test "the remediation distinguishes pid files from scratch files" do
      rule = Enum.find(CicdRules.blocked_patterns(), &(&1[:id] == :hardcoded_tmp))

      assert rule.reason =~ "XDG_RUNTIME_DIR"
      assert rule.reason =~ "mktemp is wrong here"
      assert rule.reason =~ "mktemp -d with no /tmp template"
    end
  end

  describe "the rule does not fire on its own cure" do
    test "the XDG ladder is clean", %{dir: dir} do
      write!(
        dir,
        "scripts/launcher.sh",
        ~s|PID_FILE="${XDG_RUNTIME_DIR:-${XDG_STATE_HOME:-$HOME/.local/state}}/app/server.pid"\n|
      )

      assert tmp_findings(dir) == []
    end

    test "mktemp, even with an explicit /tmp template, is clean", %{dir: dir} do
      write!(dir, "scripts/scratch.sh", ~s|d=$(mktemp -d /tmp/foo.XXXXXX)\n|)

      assert tmp_findings(dir) == []
    end

    test "a comment naming /tmp is clean", %{dir: dir} do
      write!(dir, "scripts/doc.sh", ~s|# never write to "/tmp/x" here\n|)

      assert tmp_findings(dir) == []
    end
  end

  describe "training-corpus exemption (content_patterns)" do
    for fragment <- ["tests/fixtures/", "test/", "lib/rules/", "scripts/fix-scripts/"] do
      test "a /tmp line under #{fragment} is suppressed", %{dir: dir} do
        write!(dir, unquote(fragment) <> "minted-launcher.sh", @pid_line)

        assert tmp_findings(dir) == []
      end
    end
  end

  describe "inline directives (both suppression spellings)" do
    for directive <- [
          "# hypatia: allow content_patterns/hardcoded_tmp -- pid must be re-findable",
          "# hypatia: allow cicd_rules/hardcoded_tmp -- pid must be re-findable",
          "# hypatia: allow hardcoded_tmp -- pid must be re-findable",
          "# hypatia:ignore hardcoded_tmp -- pid must be re-findable"
        ] do
      test "#{directive} on the previous line suppresses", %{dir: dir} do
        write!(dir, "scripts/launcher.sh", unquote(directive) <> "\n" <> @pid_line)
        assert tmp_findings(dir) == []
      end
    end

    test "a directive for a DIFFERENT rule does not suppress", %{dir: dir} do
      write!(
        dir,
        "scripts/launcher.sh",
        "# hypatia: allow content_patterns/http_in_docs -- unrelated\n" <> @pid_line
      )

      assert [_] = tmp_findings(dir)
    end

    test "a directive on the LAST line does not reach a hit on line 1", %{dir: dir} do
      # Enum.at(lines, -1) used to wrap line 1's "previous line" to the end.
      write!(
        dir,
        "scripts/launcher.sh",
        @pid_line <> "echo done\n# hypatia:ignore hardcoded_tmp"
      )

      assert [%{line: 1}] = tmp_findings(dir)
    end
  end
end
