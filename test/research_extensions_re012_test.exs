# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.ResearchExtensionsRE012Test do
  @moduledoc """
  RE012: a here-string fed by a command substitution (`consumer <<<"$(cmd)"`).

  Bash keeps only the status of the command the here-string feeds, so `cmd`'s
  failure is discarded. Each spelling of the form is flagged once; what is not
  the form (a here-string from a variable, a `<<<` in quotes or a comment, an
  arithmetic expansion) is not. The first describe block runs bash itself, so
  the claim the rule rests on is checked, not assumed.
  """
  use ExUnit.Case, async: true

  alias Hypatia.CLI
  alias Hypatia.Rules.ResearchExtensions

  @tmp_dir System.tmp_dir!()

  defp repo(files) do
    root = Path.join(@tmp_dir, "re012_#{System.unique_integer([:positive])}")
    File.rm_rf!(root)
    File.mkdir_p!(root)

    for {rel, content} <- files do
      path = Path.join(root, rel)
      File.mkdir_p!(Path.dirname(path))
      File.write!(path, content)
    end

    on_exit(fn -> File.rm_rf!(root) end)
    root
  end

  defp script(lines), do: Enum.join(["#!/usr/bin/env bash", "set -euo pipefail" | lines], "\n")

  defp scan(files), do: files |> repo() |> ResearchExtensions.re012_here_string_substitution()

  defp lines(text),
    do: text |> ResearchExtensions.here_string_substitution_lines() |> Enum.flat_map(&elem(&1, 3))

  # ─── The claim, checked against bash ────────────────────────────────

  describe "bash discards the substituted command's status" do
    @describetag :bash

    for {label, body} <- [
          {"read", ~S/read -r v <<<"$(exit 3)"; echo "status=$?"/},
          {"a while loop", ~S/while read -r l; do :; done <<<"$(exit 4)"; echo "status=$?"/},
          {"cat", ~S/cat <<<"$(exit 7)"; echo "status=$?"/}
        ] do
      test "#{label}: set -euo pipefail and inherit_errexit do not see it" do
        {out, 0} =
          System.cmd("bash", [
            "-c",
            "set -euo pipefail; shopt -s inherit_errexit; " <> unquote(body)
          ])

        assert out =~ "status=0"
      end
    end

    test "the fix, capture then feed, does see it" do
      {_out, status} =
        System.cmd("bash", ["-c", ~S/set -e; out=$(exit 5); cat <<<"$out"; echo reached/])

      assert status == 5
    end
  end

  # ─── What is flagged ────────────────────────────────────────────────

  describe "each spelling is flagged" do
    for {label, line, producer} <- [
          {"grep -q", ~S/grep -q x <<<"$(cmd)"/, "cmd"},
          {"read", ~S/IFS='|' read -r a b <<<"$(parse "$m")"/, ~S/parse "$m"/},
          {"done", ~S/done <<<"$(list | sort)"/, "list | sort"},
          {"unquoted, after a blank", ~S/read -r v <<< $(get a)/, "get a"},
          {"text before the substitution", ~S/cat <<<"pre $(cmd) post"/, "cmd"},
          {"after a ${var}", ~S/cat <<<"${v}$(z)"/, "z"},
          {"a backquote", ~S/cat <<<"pre `cmd`"/, "cmd"},
          {"inside an assignment's substitution", ~S/n=$(grep -c . <<<"$(cmd)")/, "cmd"},
          {"after an earlier here-string", ~S/y=$(a <<<b) && grep x <<<"$(z)"/, "z"},
          {"a quoted ) inside", ~S/cat <<<"a$(b "c)d" e)"/, ~S/b "c)d" e/},
          {"a substitution left open at the end of the line", ~S/cat <<<"$(/, nil},
          {"a carriage return at the end", "grep -q x <<<\"$(cmd)\"\r", "cmd"},
          {"after a quoted word with ''", ~S/echo 'it''s' <<<"$(a)"/, "a"},
          # Codex on #1232: a backslash escapes nothing inside single quotes,
          # so the quote after it closes them and the substitution runs.
          {"a backslash at the end of a single-quoted part", ~S/cat <<<'foo\'$(cmd)/, "cmd"},
          # Codex on #1232: arithmetic runs no command, but one inside it does.
          {"a substitution inside an arithmetic expansion", ~S/cat <<<"$(( $(count) + 1 ))"/,
           "count"},
          # CodeRabbit on #1232: a backquote body is a command.
          {"a here-string inside a backquote body", ~S/r="`cat <<<"$(cmd)"`"/, "cmd"}
        ] do
      test label do
        assert [unquote(producer)] == lines(unquote(line))
      end
    end

    test "every here-string on one line, and every substitution in a word (#1232 review)" do
      assert ["echo safe", "scan"] == lines(~S/cat <<<"$(echo safe)"; cat <<<"$(scan)"/)
      assert ["printf '%s' p", "scan"] == lines(~S/cat <<<"$(printf '%s' p)$(scan)"/)
    end

    test "every here-string of a file, each at its own line" do
      content =
        script([
          ~S/grep -q x <<<"$(a)"/,
          ~S/echo ok/,
          ~S/while read -r l; do echo "$l"; done <<<"$(b)"/
        ])

      assert [3, 5] == content |> then(&scan([{"s.sh", &1}])) |> Enum.map(& &1.detail.line)
    end
  end

  describe "what is not the form is not flagged" do
    for {label, line} <- [
          {"a here-string from a variable", ~S/grep -q x <<<"$out"/},
          {"a here-string from a literal", ~S/cat <<<"done"/},
          {"<<< inside double quotes", ~S/echo "<<<$(x)"/},
          {"<<< inside single quotes", ~S/grep -F '<<<"$(' f/},
          {"a substitution inside single quotes after <<<", ~S/cat <<<'$(x)'/},
          {"an arithmetic expansion", ~S/cat <<<"$((1 + 2))"/},
          {"a full-line comment", ~S/# grep -q x <<<"$(c)"/},
          {"a trailing comment", ~S/f  # grep <<<"$(c)"/},
          {"a heredoc", ~S/cat <<EOF/},
          {"an escaped <", ~S/echo \<<<"$(c)"/},
          {"a process substitution (not this rule)", ~S/done < <(cmd)/}
        ] do
      test label do
        assert [] == lines(unquote(line))
      end
    end
  end

  # ─── Severity ───────────────────────────────────────────────────────

  describe "severity" do
    test "a command is :warn" do
      assert [%{severity: :warn}] = scan([{"s.sh", script([~S/grep -q x <<<"$(cmd)"/])}])
    end

    test "a lone printf or echo cannot fail here: :info" do
      for line <- [
            ~S/grep -qx -- "$j" <<<"$(printf '%s\n' "${all[@]}")"/,
            ~S/cat <<<"$(echo "$a" $b)"/
          ] do
        assert [%{severity: :info} = f] = scan([{"s.sh", script([line])}])
        assert f.reason =~ "does not"
      end
    end

    for {label, line} <- [
          {"a redirection", ~S/cat <<<"$(printf x > f)"/},
          {"a pipe", ~S/cat <<<"$(printf x | cmd)"/},
          {"a list", ~S/cat <<<"$(printf x; cmd)"/},
          {"a nested substitution", ~S/cat <<<"$(echo "$(cmd)")"/},
          {"a backquote around a pipe", ~S/cat <<<"`echo x | cmd`"/}
        ] do
      test "printf or echo with #{label} is :warn" do
        assert [%{severity: :warn}] = scan([{"s.sh", script([unquote(line)])}])
      end
    end

    # Codex and CodeRabbit on #1232: printf fails on a bad conversion or option.
    for {label, line} <- [
          {"a numeric conversion", ~S/cat <<<"$(printf '%d\n' "$n")"/},
          {"an option", ~S/cat <<<"$(printf -Z)"/},
          {"-v", ~S/cat <<<"$(printf -v x '%s' y)"/},
          {"an unquoted format", ~S/cat <<<"$(printf $fmt x)"/},
          {"a format from a variable", ~S/cat <<<"$(printf "$fmt" x)"/}
        ] do
      test "printf with #{label} can fail: :warn" do
        assert [%{severity: :warn}] = scan([{"s.sh", script([unquote(line)])}])
      end
    end

    test "one failing substitution on the line makes it :warn, and is the one named" do
      for line <- [
            ~S/cat <<<"$(echo safe)"; cat <<<"$(scan)"/,
            ~S/cat <<<"$(printf '%s' p)$(scan)"/
          ] do
        assert [%{severity: :warn, detail: %{producer: "scan", producers: [_, "scan"]}}] =
                 scan([{"s.sh", script([line])}])
      end
    end

    test "set -e and pipefail do not change it" do
      bare = "#!/bin/bash\ngrep -q x <<<\"$(cmd)\"\n"
      assert [%{severity: :warn}] = scan([{"s.sh", bare}])
      assert [%{severity: :warn}] = scan([{"s.sh", script([~S/grep -q x <<<"$(cmd)"/])}])
    end
  end

  # ─── Workflows ──────────────────────────────────────────────────────

  describe "a workflow run: block" do
    defp workflow(text), do: scan([{".github/workflows/w.yml", text}])

    test "is flagged at its source line" do
      wf = """
      on: push
      jobs:
        t:
          runs-on: ubuntu-latest
          steps:
            - shell: bash
              run: |
                echo start
                grep -q x <<<"$(cmd)"
      """

      assert [%{file: ".github/workflows/w.yml", detail: %{line: 9}}] = workflow(wf)
    end

    test "a pwsh or python step is not read" do
      for shell <- ["pwsh", "powershell", "python", "cmd", "/usr/bin/python {0}"] do
        wf = """
        on: push
        jobs:
          t:
            runs-on: ubuntu-latest
            steps:
              - shell: #{shell}
                run: grep -q x <<<"$(cmd)"
        """

        assert [] == workflow(wf), shell
      end
    end

    test "an empty shell: is not a reason to skip, nor to crash (#1232 review)" do
      for shell <- ["''", "' '"] do
        wf = """
        on: push
        jobs:
          t:
            runs-on: ubuntu-latest
            steps:
              - shell: #{shell}
                run: grep -q x <<<"$(cmd)"
        """

        assert [%{detail: %{line: 7}}] = workflow(wf), shell
      end
    end

    test "text outside run: is not read" do
      wf = """
      on: push
      jobs:
        t:
          runs-on: ubuntu-latest
          env:
            HINT: 'grep -q x <<<"$(cmd)"'
          steps:
            - name: 'grep -q x <<<"$(cmd)"'
              run: echo ok
      """

      assert [] == workflow(wf)
    end

    test "a workflow the reader refuses is still scanned" do
      wf = """
      on: push
      x: &x
        runs-on: ubuntu-latest
      jobs:
        t:
          <<: *x
          steps:
            - run: grep -q x <<<"$(cmd)"
      """

      assert [%{detail: %{line: 8}}] = workflow(wf)
    end
  end

  # ─── The reviewed escape ────────────────────────────────────────────

  describe "a reviewed line" do
    test "hypatia: allow on the line above" do
      lines = [
        "# hypatia: allow research_extensions/RE012 -- cmd cannot fail here",
        ~S/grep -q x <<<"$(cmd)"/
      ]

      assert [] == scan([{"s.sh", script(lines)}])
    end

    test "hypatia:ignore at the end of the line" do
      lines = [~S/grep -q x <<<"$(cmd)"  # hypatia:ignore RE012 -- cmd cannot fail here/]
      assert [] == scan([{"s.sh", script(lines)}])
    end

    test "a directive for RE011 does not cover it" do
      lines = ["# hypatia: allow research_extensions/RE011 -- other rule", ~S/cat <<<"$(c)"/]
      assert [%{rule: "RE012"}] = scan([{"s.sh", script(lines)}])
    end
  end

  # ─── Flag-only, and through the CLI ─────────────────────────────────

  describe "the finding" do
    test "says what was found, why, and the fix; flag-only and advisory" do
      assert [f] =
               scan([
                 {"scripts/g.sh", script([~S/if grep -q BAD <<<"$(scan)"; then exit 1; fi/])}
               ])

      assert f.rule == "RE012"
      assert f.advisory == true
      assert f.action == :review
      assert f.detail.strategy == :review
      assert f.detail.producer == "scan"
      assert f.reason =~ "scripts/g.sh:3"
      assert f.reason =~ "here-string"
      assert f.reason =~ ~s(out=$(cmd\) || <handle the failure>; consumer <<<"$out")
      refute Map.has_key?(f, :fix_script)
    end

    test "scan/2 runs RE012 and never recommends auto-execution for it" do
      root = repo([{"s.sh", script([~S/cat <<<"$(c)"/])}])
      result = ResearchExtensions.scan(root)

      assert [%{rule: "RE012"}] = result.findings
      assert [%{rule: "RE012", action: :review, confidence: confidence}] = result.dispatch
      assert confidence < 0.95
    end

    test "the CLI carries it as advisory, so it never fails a scan" do
      root = repo([{"scripts/s.sh", script([~S/cat <<<"$(c)"/])}])

      assert [finding] =
               root
               |> CLI.collect_findings([:research_extensions])
               |> Enum.filter(&(&1.type == "RE012"))

      assert finding.line == 3
      assert finding.severity == "warn"
      assert finding.advisory == true
      assert {[_], []} = CLI.report_sets([finding], %{severity: "medium", advisory: false})
    end
  end
end
