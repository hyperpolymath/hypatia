# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.ResearchExtensionsRE011Test do
  @moduledoc """
  RE011: a pipe into a quiet grep (`writer | grep -q PATTERN`).

  The spellings are the ones standards' `scripts/tests/check-pipe-grep-q-test.sh`
  proves for `scripts/check-pipe-grep-q.sh`: each one flagged, each
  non-defect left alone. On top of that gate the rule says whether `pipefail`
  is in force (the finding is *exposed*) or not (*latent*), so each of those
  has a positive and a negative control here too.
  """
  use ExUnit.Case, async: true

  alias Hypatia.CLI
  alias Hypatia.Rules.ResearchExtensions
  alias Hypatia.SARIF

  @tmp_dir System.tmp_dir!()

  # A repo holding `files`, a list of `{relative path, content}`.
  defp repo(files) do
    root = Path.join(@tmp_dir, "re011_#{System.unique_integer([:positive])}")
    # unique_integer restarts with each VM, so clear what an earlier run left.
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

  # A bash script that sets pipefail, then `lines`.
  defp strict(lines), do: Enum.join(["#!/usr/bin/env bash", "set -euo pipefail" | lines], "\n")

  # The same script with pipefail left off.
  defp lax(lines), do: Enum.join(["#!/usr/bin/env bash", "set -eu" | lines], "\n")

  defp scan(files), do: files |> repo() |> ResearchExtensions.re011_pipe_into_quiet_grep()

  # ─── Each spelling the standards gate refuses ───────────────────────

  describe "each spelling is flagged (pipefail in force)" do
    for {label, lines, at} <- [
          {"-q", ["writer | grep -q x"], 3},
          {"-qF", ["writer | grep -qF x"], 3},
          {"-Eq", ["writer | grep -Eq 'a|b'"], 3},
          {"-qxF", ["writer | grep -qxF -- x"], 3},
          {"-iqE", ["writer | grep -iqE x"], 3},
          {"--quiet", ["writer | grep --quiet x"], 3},
          {"--silent", ["writer | grep --silent x"], 3},
          {"egrep -q", ["writer | egrep -q x"], 3},
          {"fgrep -q", ["writer | fgrep -q x"], 3},
          {"a path to grep", ["writer | /usr/bin/grep -q x"], 3},
          {"an env prefix", ["writer | LC_ALL=C grep -q x"], 3},
          {"a command prefix", ["writer | command grep -q x"], 3},
          {"a negation", ["if ! writer | grep -q x; then echo no; fi"], 3},
          {"|& into grep -q", ["writer |& grep -q x"], 3},
          {"the flag after the pattern", ["writer | grep x -q"], 3},
          {"the pipe at the end of the previous line", ["writer |", "grep -q x"], 4},
          {"a pipe, a backslash, then the grep", ["writer | \\", "  grep -q x"], 4},
          {"a comment between the pipe and the grep", ["writer |", "# note", "grep -q x"], 5},
          {"a blank line between the pipe and the grep", ["writer |", "", "grep -q x"], 5},
          {"the pipe on the line after a backslash", ["writer \\", "  | grep -q x"], 4},
          {"a second pipe on one line", ["a | b | grep -q x"], 3},
          {"a carriage return at the end of the line", ["writer | grep -q x\r"], 3}
        ] do
      test label do
        assert [finding] = scan([{"a.sh", strict(unquote(lines))}])
        assert finding.rule == "RE011"
        assert finding.file == "a.sh"
        assert finding.detail.line == unquote(at)
        assert finding.detail.exposure == :exposed
      end
    end

    test "two findings on two lines are two findings" do
      assert [%{detail: %{line: 3}}, %{detail: %{line: 4}}] =
               scan([{"a.sh", strict(["w | grep -q x", "w | grep -q y"])}])
    end
  end

  describe "which files are read" do
    test "a file under .githooks with no extension" do
      assert [%{file: ".githooks/pre-x"}] =
               scan([{".githooks/pre-x", "set -o pipefail\nwriter | grep -q x\n"}])
    end

    test "a file found by its shebang" do
      assert [%{file: "bin/tool"}] = scan([{"bin/tool", strict(["writer | grep -q x"])}])
    end

    for shebang <- [
          "#!/bin/sh",
          "#!/bin/bash",
          "#! /usr/bin/env zsh",
          "#!/usr/bin/env -S bash -e"
        ] do
      test "the shebang #{shebang}" do
        content = unquote(shebang) <> "\nset -o pipefail\nwriter | grep -q x\n"
        assert [%{file: "tool"}] = scan([{"tool", content}])
      end
    end

    test "a file that is not a shell source is not read" do
      assert [] == scan([{"notes.txt", "set -o pipefail\nwriter | grep -q x\n"}])
      assert [] == scan([{"tool.py", "#!/usr/bin/env python3\nwriter | grep -q x\n"}])
    end

    test "vendored and build trees are not walked" do
      body = strict(["writer | grep -q x"])

      assert [] ==
               scan([
                 {"node_modules/p/a.sh", body},
                 {"deps/p/a.sh", body},
                 {"_build/a.sh", body},
                 {"target/a.sh", body}
               ])
    end

    test "a symbolic link is not followed" do
      root = repo([{"real/a.sh", strict(["writer | grep -q x"])}])
      File.ln_s!(Path.join(root, "real/a.sh"), Path.join(root, "link.sh"))

      assert ["real/a.sh"] ==
               root |> ResearchExtensions.re011_pipe_into_quiet_grep() |> Enum.map(& &1.file)
    end

    test "a file that is not valid UTF-8 is still read" do
      content = "#!/bin/sh\nset -o pipefail\n# \xff\xfe\nwriter | grep -q x\n"
      assert [%{detail: %{line: 4}}] = scan([{"a.sh", content}])
    end
  end

  # ─── What is not a defect ───────────────────────────────────────────

  describe "what is not a defect is not flagged" do
    for {label, lines} <- [
          {"a full-line comment", ["# writer | grep -q x"]},
          {"an indented comment", ["    # writer | grep -q x"]},
          {"-eq is the pattern q", ["writer | grep -eq x"]},
          {"-e then a pattern", ["writer | grep -e -q x"]},
          {"--regexp then a pattern", ["writer | grep --regexp -q x"]},
          {"a non-quiet grep", ["writer | grep -c x", "writer | grep x"]},
          {"a here-string", ["grep -q x <<<\"$out\""]},
          {"|| is not a pipe", ["writer || grep -q x"]},
          {"a quoted -q in a pattern", ["writer | grep 'a -q b'"]},
          {"a -q after --", ["writer | grep -- -q"]},
          {"a -q that belongs to the next command", ["writer | grep x; other -q"]},
          {"a quiet grep after a line that is not a pipe", ["writer", "grep -q x"]},
          {"a quiet grep after a line that ends in ||", ["writer ||", "grep -q x"]},
          {"a command whose name ends in grep", ["writer | mygrep -q x", "writer | pgrep -q x"]}
        ] do
      test label do
        assert [] == scan([{"a.sh", strict(unquote(lines))}])
      end
    end
  end

  # ─── Pipefail: exposed or latent ────────────────────────────────────

  describe "a shell file: is pipefail in force?" do
    for set <- [
          "set -o pipefail",
          "set -euo pipefail",
          "set -eo pipefail",
          "set -Eeuo pipefail",
          "set -e -o pipefail",
          "set -o errexit -o nounset -o pipefail",
          "  set -uo pipefail",
          "set -e; set -o pipefail",
          "shopt -so pipefail"
        ] do
      test "`#{set}` exposes the pipe" do
        content = "#!/bin/bash\n" <> unquote(set) <> "\nwriter | grep -q x\n"
        assert [%{severity: :warn, detail: %{exposure: :exposed}}] = scan([{"a.sh", content}])
      end
    end

    for {label, set} <- [
          {"no set at all", ""},
          {"set -eu", "set -eu"},
          {"set +o pipefail", "set +o pipefail"},
          {"pipefail only in a comment", "# set -o pipefail"},
          {"pipefail only in a message", "echo 'run with set -o pipefail'"}
        ] do
      test "#{label}: the pipe is latent, at a lower severity" do
        content = "#!/bin/bash\n" <> unquote(set) <> "\nwriter | grep -q x\n"
        assert [finding] = scan([{"a.sh", content}])
        assert finding.detail.exposure == :latent
        assert finding.severity == :info
      end
    end

    test "the same line is exposed with pipefail and latent without" do
      assert [%{severity: :warn}] = scan([{"a.sh", strict(["writer | grep -q x"])}])
      assert [%{severity: :info}] = scan([{"a.sh", lax(["writer | grep -q x"])}])
    end
  end

  describe "a workflow run: block: is pipefail in force?" do
    defp workflow(step), do: [{".github/workflows/w.yml", step}]

    test "a run: line with no shell: is latent (the runner's default is bash -e)" do
      wf = """
      on: push
      jobs:
        t:
          runs-on: ubuntu-latest
          steps:
            - run: writer | grep -q x
      """

      assert [finding] = scan(workflow(wf))
      assert finding.file == ".github/workflows/w.yml"
      assert finding.detail.line == 6
      assert finding.detail.exposure == :latent
      assert finding.severity == :info
    end

    test "shell: bash on the step exposes it (bash -eo pipefail)" do
      wf = """
      on: push
      jobs:
        t:
          runs-on: ubuntu-latest
          steps:
            - name: check
              shell: bash
              run: |
                echo start
                writer | grep -q x
      """

      assert [finding] = scan(workflow(wf))
      assert finding.detail.line == 10
      assert finding.detail.exposure == :exposed
      assert finding.severity == :warn
      assert finding.detail.pipefail =~ "shell: bash"
    end

    test "shell: bash from the job's defaults exposes it" do
      wf = """
      on: push
      jobs:
        t:
          runs-on: ubuntu-latest
          defaults:
            run:
              shell: bash
          steps:
            - run: writer | grep -q x
      """

      assert [%{detail: %{exposure: :exposed, line: 9}}] = scan(workflow(wf))
    end

    test "shell: bash from the workflow's defaults exposes it" do
      wf = """
      on: push
      defaults:
        run:
          shell: bash
      jobs:
        t:
          runs-on: ubuntu-latest
          steps:
            - run: writer | grep -q x
      """

      assert [%{detail: %{exposure: :exposed, line: 9}}] = scan(workflow(wf))
    end

    test "the step's shell: wins over the defaults" do
      wf = """
      on: push
      defaults:
        run:
          shell: bash
      jobs:
        t:
          runs-on: ubuntu-latest
          steps:
            - shell: sh
              run: writer | grep -q x
      """

      assert [%{detail: %{exposure: :latent}}] = scan(workflow(wf))
    end

    test "a custom shell that names pipefail exposes it" do
      wf = """
      on: push
      jobs:
        t:
          runs-on: ubuntu-latest
          steps:
            - shell: bash -euo pipefail {0}
              run: writer | grep -q x
      """

      assert [%{detail: %{exposure: :exposed}}] = scan(workflow(wf))
    end

    test "set -o pipefail in the script exposes it" do
      wf = """
      on: push
      jobs:
        t:
          runs-on: ubuntu-latest
          steps:
            - run: |
                set -euo pipefail
                writer | grep -q x
      """

      assert [finding] = scan(workflow(wf))
      assert finding.detail.line == 8
      assert finding.detail.exposure == :exposed
      assert finding.detail.pipefail =~ "set in the script"
    end

    test "pipefail set in one step does not expose another step" do
      wf = """
      on: push
      jobs:
        t:
          runs-on: ubuntu-latest
          steps:
            - run: |
                set -euo pipefail
                echo ok
            - run: writer | grep -q x
      """

      assert [%{detail: %{exposure: :latent, line: 9}}] = scan(workflow(wf))
    end

    test "the pipe on the previous line of a block is found at the grep's line" do
      wf = """
      on: push
      jobs:
        t:
          runs-on: ubuntu-latest
          steps:
            - shell: bash
              run: |
                writer |
                  grep -q x
      """

      assert [%{detail: %{exposure: :exposed, line: 9}}] = scan(workflow(wf))
    end

    test "a shell comment inside the script is not flagged" do
      wf = """
      on: push
      jobs:
        t:
          runs-on: ubuntu-latest
          steps:
            - shell: bash
              run: |
                # writer | grep -q x
                echo ok
      """

      assert [] == scan(workflow(wf))
    end

    test "text outside run: is not read (a name, an env value)" do
      wf = """
      on: push
      jobs:
        t:
          runs-on: ubuntu-latest
          env:
            HINT: "writer | grep -q x"
          steps:
            - name: writer | grep -q x
              shell: bash
              run: echo ok
      """

      assert [] == scan(workflow(wf))
    end

    test "a KYAML workflow is read like a block one" do
      wf = """
      {
        "on": "push",
        jobs: {
          t: {
            "runs-on": "ubuntu-latest",
            steps: [
              {
                shell: "bash",
                run: "writer | grep -q x",
              },
            ],
          },
        },
      }
      """

      assert [%{detail: %{exposure: :exposed, line: 9}}] = scan(workflow(wf))
    end

    test "a workflow the reader refuses is still scanned, as latent" do
      # A `<<` merge key: Hypatia.Workflow refuses it, as GitHub Actions does.
      wf = """
      on: push
      x: &x
        runs-on: ubuntu-latest
      jobs:
        t:
          <<: *x
          steps:
            - shell: bash
              run: writer | grep -q x
      """

      assert [finding] = scan(workflow(wf))
      assert finding.detail.line == 9
      assert finding.detail.exposure == :latent
      assert finding.detail.pipefail =~ "could not be read"
    end
  end

  # ─── The reviewed escape ────────────────────────────────────────────

  describe "a reviewed line" do
    test "hypatia: allow on the line above" do
      lines = [
        "# hypatia: allow research_extensions/RE011 -- writer is one printf",
        "w | grep -q x"
      ]

      assert [] == scan([{"a.sh", strict(lines)}])
    end

    test "hypatia:ignore at the end of the line" do
      lines = ["w | grep -q x  # hypatia:ignore RE011 -- writer is one printf"]
      assert [] == scan([{"a.sh", strict(lines)}])
    end

    test "a directive for another rule does not cover it" do
      lines = ["# hypatia: allow research_extensions/RE005 -- unrelated", "w | grep -q x"]
      assert [%{rule: "RE011"}] = scan([{"a.sh", strict(lines)}])
    end

    test "a directive two lines above does not cover it" do
      lines = ["# hypatia: allow research_extensions/RE011 -- too far", "echo", "w | grep -q x"]
      assert [%{rule: "RE011"}] = scan([{"a.sh", strict(lines)}])
    end

    test "a directive in a workflow run: block" do
      wf = """
      on: push
      jobs:
        t:
          runs-on: ubuntu-latest
          steps:
            - shell: bash
              run: |
                # hypatia: allow research_extensions/RE011 -- writer is one printf
                writer | grep -q x
      """

      assert [] == scan([{".github/workflows/w.yml", wf}])
    end
  end

  # ─── What the finding says, and that it is flag-only ────────────────

  describe "the finding" do
    test "says what was found, why it is unsound, and the fix" do
      assert [finding] =
               scan([{"scripts/gate.sh", strict(["if w | grep -q BAD; then exit 1; fi"])}])

      assert finding.reason =~ "scripts/gate.sh:3"
      assert finding.reason =~ "pipe into a quiet grep"
      assert finding.reason =~ "pipefail"
      assert finding.reason =~ "SIGPIPE"
      assert finding.reason =~ "141"
      assert finding.reason =~ ~s(out=$(writer\); grep -q PATTERN <<<"$out")
      assert finding.detail.text == "if w | grep -q BAD; then exit 1; fi"
      assert finding.detail.fix =~ "capture, then match"
    end

    test "a latent finding says pipefail is not in force" do
      assert [finding] = scan([{"a.sh", lax(["w | grep -q x"])}])
      assert finding.reason =~ "not in force"
    end

    test "is advisory at every exposure: warn when exposed, info when latent" do
      findings = scan([{"a.sh", strict(["w | grep -q x"])}, {"b.sh", lax(["w | grep -q x"])}])
      assert [{:warn, true}, {:info, true}] == Enum.map(findings, &{&1.severity, &1.advisory})
    end

    test "is flag-only: the :review strategy, at every exposure" do
      findings = scan([{"a.sh", strict(["w | grep -q x"])}, {"b.sh", lax(["w | grep -q x"])}])
      assert length(findings) == 2
      assert Enum.all?(findings, &(&1.action == :review))
      assert Enum.all?(findings, &(&1.detail.strategy == :review))
      refute Enum.any?(findings, &Map.has_key?(&1, :fix_script))
      refute Enum.any?(findings, &Map.has_key?(&1, :recipe_id))
      refute Enum.any?(findings, &(Map.get(&1, :auto_fixable) == true))
    end

    test "is never above RE005's severity" do
      rank = %{critical: 4, high: 3, warn: 2, medium: 2, low: 1, info: 0}
      findings = scan([{"a.sh", strict(["w | grep -q x"])}, {"b.sh", lax(["w | grep -q x"])}])
      assert Enum.all?(findings, &(rank[&1.severity] <= rank[:warn]))
    end
  end

  describe "scan/2 and the CLI" do
    test "scan/2 runs RE011 and never recommends auto-execution for it" do
      root = repo([{"a.sh", strict(["w | grep -q x"])}])
      result = ResearchExtensions.scan(root)

      assert [%{rule: "RE011"}] = result.findings
      assert [%{rule: "RE011", action: :review, confidence: confidence}] = result.dispatch
      # Hypatia's auto_execute tier starts at 0.95 (TriangleRouter).
      assert confidence < 0.95
    end

    test "collect_findings/2 carries the type, the line and the action" do
      root = repo([{"scripts/a.sh", strict(["w | grep -q x"])}])

      assert [finding] =
               root
               |> CLI.collect_findings([:research_extensions])
               |> Enum.filter(&(&1.type == "RE011"))

      assert finding.rule_module == "research_extensions"
      assert finding.file == "scripts/a.sh"
      assert finding.line == 3
      assert finding.action == "review"
      assert finding.severity == "warn"

      [result] =
        [finding]
        |> SARIF.from_findings(root)
        |> Map.fetch!("runs")
        |> hd()
        |> Map.fetch!("results")

      assert result["level"] == "warning"

      assert get_in(result, ["locations", Access.at(0), "physicalLocation", "region", "startLine"]) ==
               3
    end

    test "an exposed finding is shown by a default scan and never fails it" do
      root = repo([{"a.sh", strict(["w | grep -q x"])}, {"b.sh", lax(["w | grep -q x"])}])

      findings =
        root |> CLI.collect_findings([:research_extensions]) |> Enum.filter(&(&1.type == "RE011"))

      assert Enum.all?(findings, &(&1.advisory == true))

      # The default scan: the exposed line is shown, the latent one is not,
      # and neither is in the set that decides exit 1.
      assert {[%{file: "a.sh", severity: "warn"}], []} =
               CLI.report_sets(findings, %{severity: "medium", advisory: false})

      # With --advisory the latent line is shown too; still nothing blocks.
      assert {[_, _], []} = CLI.report_sets(findings, %{severity: "medium", advisory: true})
    end

    test "the same finding without the advisory class would fail the scan" do
      root = repo([{"a.sh", strict(["w | grep -q x"])}])

      findings =
        root
        |> CLI.collect_findings([:research_extensions])
        |> Enum.map(&Map.delete(&1, :advisory))

      assert {[_], [_]} = CLI.report_sets(findings, %{severity: "medium", advisory: false})
    end

    test ".hypatia-ignore suppresses a path for RE011" do
      root =
        repo([
          {"vendor/a.sh", strict(["w | grep -q x"])},
          {"scripts/a.sh", strict(["w | grep -q x"])},
          {".hypatia-ignore", "research_extensions/RE011:vendor/\n"}
        ])

      assert ["scripts/a.sh"] ==
               root
               |> CLI.collect_findings([:research_extensions])
               |> Enum.filter(&(&1.type == "RE011"))
               |> Enum.map(& &1.file)
    end
  end
end
