# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.Rules.PinIntegrityTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.PinIntegrity, as: PI

  # The two anchors of the September 2026 incident, spelled once.
  @poison "1c5b675653bb5c22dbe9b12b556ec555138e09fd"
  @good "b96794f015dfd88f77b49b1c93e0fa7110f94c63"

  # The shape `pr-automerge-policy.json` carries for PIN-001, trimmed to the
  # fields the rules read.
  @policy %{
    "pin_denylist" => [
      %{
        "id" => "PIN-001",
        "action" => "github/codeql-action",
        "severity" => "critical",
        "blocked_versions" => ["4.38.1"],
        "blocked_shas" => [@poison],
        "blocked_refs" => ["v4.38.1"],
        "reason" =>
          "GitHub rejects this commit at workflow start-up: `startup_failure`, " <>
            "zero jobs, no logs, wherever it is used.",
        "known_good_version" => "4.38.0",
        "known_good_sha" => @good
      },
      %{
        "id" => "PIN-002",
        "action" => "dtolnay/rust-toolchain",
        "severity" => "high",
        "blocked_versions" => [],
        "blocked_shas" => [],
        "blocked_refs" => []
      }
    ],
    "moving_refs_never_lockable" => [
      "stable",
      "beta",
      "nightly",
      "latest",
      "main",
      "master",
      "HEAD"
    ]
  }

  @poisoned_file """
  name: codeql
  jobs:
    analyze:
      steps:
        - uses: actions/checkout@v7.0.1
        - uses: github/codeql-action/init@#{@poison} # v3
        - uses: github/codeql-action/analyze@#{@poison} # v4.38.0 (4.38.1 blocked estate-wide; nexia-list#100)
  """

  describe "pin_sites/1" do
    test "parses an indented, dash-led uses line and keeps the trailing comment" do
      content = "      - uses: github/codeql-action/init@#{@poison} # v4.38.0\n"

      assert [site] = PI.pin_sites(content)
      assert site.line == 1
      assert site.action == "github/codeql-action/init"
      assert site.action_base == "github/codeql-action"
      assert site.ref == @poison
      assert site.comment == "v4.38.0"
    end

    test "ignores lines that are not uses pins" do
      content = "name: ci\non: push\njobs:\n  build:\n    steps:\n      - run: echo hi\n"

      assert PI.pin_sites(content) == []
    end

    test "a pin with no comment has an empty comment, not a missing one" do
      assert [site] = PI.pin_sites("- uses: actions/checkout@v7.0.1\n")
      assert site.comment == ""
    end
  end

  # Read one line at a time, a `uses:` written inside a `run: |` script or a
  # quoted scalar continued from an earlier line looked like a key: PI001
  # reported script text and substitute_denylisted_pins/3 rewrote it as an
  # automatic repair. The file is now read through PinSites.mask_lines/1.
  describe "pin_sites/1 across lines — block and quoted scalars are text" do
    @real_step "      - uses: github/codeql-action/init@#{@poison} # v4.38.0\n"

    test "a uses: line inside a run: | script is no site, the step after it is" do
      content =
        "jobs:\n  b:\n    steps:\n      - run: |\n" <>
          "          cat > fixture.yml <<'EOF'\n" <>
          "          uses: github/codeql-action/init@#{@poison}\n" <>
          "          - uses: github/codeql-action/analyze@#{@poison} # v4.38.1\n" <>
          "          EOF\n\n" <>
          "        shell: bash\n" <> @real_step

      assert [%{line: 11}] = PI.pi001_denylisted_pin(content, @policy)

      assert {:ok, new_content, [_]} = PI.substitute_denylisted_pins(content, @policy)
      assert new_content =~ "          uses: github/codeql-action/init@#{@poison}\n"
      assert new_content =~ "analyze@#{@poison} # v4.38.1\n"
      assert new_content =~ "      - uses: github/codeql-action/init@#{@good} # v4.38.0\n"
    end

    test "a script that is all text leaves the file untouched" do
      content =
        "steps:\n  - name: write\n    run: >-\n      uses: github/codeql-action/init@#{@poison}\n"

      assert PI.pi001_denylisted_pin(content, @policy) == []
      assert {:ok, ^content, []} = PI.substitute_denylisted_pins(content, @policy)
    end

    test "a bare block-scalar sequence entry is text until the dash's level" do
      content =
        "      - args:\n          - |\n            uses: github/codeql-action/init@#{@poison}\n" <>
          @real_step

      assert [%{line: 4}] = PI.pi001_denylisted_pin(content, @policy)
    end

    test "the continuation of a multi-line double-quoted run: is text" do
      content =
        "      - run: \"echo start\n" <>
          "          uses: github/codeql-action/init@#{@poison}\n" <>
          "          echo \\\"end\\\"\"\n" <> @real_step

      assert [%{line: 4}] = PI.pi001_denylisted_pin(content, @policy)
      assert {:ok, new_content, [_]} = PI.substitute_denylisted_pins(content, @policy)
      assert new_content =~ "          uses: github/codeql-action/init@#{@poison}\n"
    end

    test "a single-quoted one, with an escaped quote inside, is text too" do
      content =
        "      - run: 'echo it''s\n" <>
          "          uses: github/codeql-action/init@#{@poison}\n" <>
          "          done'\n" <> @real_step

      assert [%{line: 4}] = PI.pi001_denylisted_pin(content, @policy)
    end

    test "a value pending from the line above may open the quote" do
      content =
        "      - run:\n          \"echo start\n" <>
          "          uses: github/codeql-action/init@#{@poison}\"\n" <> @real_step

      assert [%{line: 4}] = PI.pi001_denylisted_pin(content, @policy)
    end

    test "an apostrophe in a plain scalar opens nothing" do
      content = "      - run: echo it's here\n" <> @real_step <> "      - run: echo don't\n"

      assert [%{line: 2}] = PI.pi001_denylisted_pin(content, @policy)
    end

    test "a quote inside a run: | script opens nothing after the script" do
      content = "      - run: |\n          echo \"unterminated\n" <> @real_step

      assert [%{line: 3}] = PI.pi001_denylisted_pin(content, @policy)
    end

    test "a quote never closed by the end of the file hides nothing" do
      content = "      - run: \"echo start\n" <> @real_step

      assert [%{line: 2}] = PI.pi001_denylisted_pin(content, @policy)
    end

    test "the file mask keeps every line's bytes where they were" do
      content =
        "      - run: |\n          uses: a/b@#{@poison}\n      - run: \"x\n  y\" # c\n" <>
          @real_step

      masked = Hypatia.Rules.PinSites.mask_lines(content)
      lines = String.split(content, "\n")

      assert Enum.map(masked, &byte_size/1) == Enum.map(lines, &byte_size/1)
      assert Enum.at(masked, 1) == String.duplicate("x", byte_size(Enum.at(lines, 1)))
      assert Enum.at(masked, 3) == "xxxx # c"
      assert Enum.at(masked, 4) == String.trim_trailing(@real_step, "\n")
    end

    test "a JSON-style flow value after a bare colon opens its quote too" do
      content =
        "      - {\"run\":\"echo start\n" <>
          "          uses: github/codeql-action/init@#{@poison}\n" <>
          "          echo end\"}\n" <> @real_step

      assert [%{line: 4}] = PI.pi001_denylisted_pin(content, @policy)
      assert {:ok, new_content, [_]} = PI.substitute_denylisted_pins(content, @policy)
      assert new_content =~ "          uses: github/codeql-action/init@#{@poison}\n"
    end

    test "an anchor or tag before the quote does not hide where the scalar starts" do
      for opener <- [
            "      - {\"run\": &cmd \"echo start\n",
            "      - {\"run\":&cmd \"echo start\n",
            "      - run: !!str \"echo start\n"
          ] do
        content =
          opener <>
            "          uses: github/codeql-action/init@#{@poison}\n" <>
            "          echo end\"#{if String.contains?(opener, "{"), do: "}", else: ""}\n" <>
            @real_step

        assert [%{line: 4}] = PI.pi001_denylisted_pin(content, @policy)
      end
    end

    test "a uses: whose reference continues on the next line is a site there" do
      content =
        "      - uses:\n          github/codeql-action/init@#{@poison} # v4.38.0\n" <>
          "      - uses:\n          \"github/codeql-action/analyze@#{@poison}\"\n" <> @real_step

      assert [%{line: 2}, %{line: 4}, %{line: 5}] = PI.pi001_denylisted_pin(content, @policy)

      assert {:ok, new_content, [_, _, _]} = PI.substitute_denylisted_pins(content, @policy)
      refute new_content =~ @poison
      assert new_content =~ "          github/codeql-action/init@#{@good} # v4.38.0\n"
      assert new_content =~ "          \"github/codeql-action/analyze@#{@good}\"\n"

      # Controls: another key's continued value, and a `uses:` whose next
      # line is no deeper, hold no site.
      assert PI.pi001_denylisted_pin(
               "      - run:\n          github/codeql-action/init@#{@poison}\n" <>
                 "      - uses:\n      - name: github/codeql-action/init@#{@poison}\n",
               @policy
             ) == []
    end

    test "blank and comment-only lines between a uses: key and its reference are skipped" do
      # YAML lets them stand between a key and its scalar: the reference is
      # still the `uses:` value, so it is still a site — reported and repaired.
      content =
        "      - uses:\n\n          # pinned for the 4.38 line\n" <>
          "          github/codeql-action/init@#{@poison}\n" <> @real_step

      assert [%{line: 4}, %{line: 5}] = PI.pi001_denylisted_pin(content, @policy)
      assert {:ok, new_content, [_, _]} = PI.substitute_denylisted_pins(content, @policy)
      assert new_content =~ "          github/codeql-action/init@#{@good}\n"

      # Control: a comment and then a line no deeper than the key end it.
      assert PI.pi001_denylisted_pin(
               "      - uses:\n          # note\n      - name: github/codeql-action/init@#{@poison}\n",
               @policy
             ) == []
    end

    test "a uses: value carrying an anchor or tag is the pin all the same" do
      # YAML node properties name the node; the scalar after them is the
      # action reference, so it is a site — reported and repaired.
      content =
        "      - uses: &init github/codeql-action/init@#{@poison}\n" <>
          "      - uses: !!str \"github/codeql-action/analyze@#{@poison}\"\n" <>
          "      - { name: x, uses: !!str github/codeql-action/upload-sarif@#{@poison} }\n" <>
          "      - uses: !!str\n          github/codeql-action/autobuild@#{@poison}\n"

      assert [%{line: 1}, %{line: 2}, %{line: 3}, %{line: 5}] =
               PI.pi001_denylisted_pin(content, @policy)

      assert {:ok, new_content, [_, _, _, _]} = PI.substitute_denylisted_pins(content, @policy)
      refute new_content =~ @poison
      assert new_content =~ "      - uses: &init github/codeql-action/init@#{@good}\n"
      assert new_content =~ "      - uses: !!str \"github/codeql-action/analyze@#{@good}\"\n"

      # Control: `&` or `!` inside a quoted name is text, not a property.
      assert PI.pi001_denylisted_pin(
               "      - name: \"uses: !!str github/codeql-action/init@#{@poison}\"\n",
               @policy
             ) == []
    end

    test "a uses: entry of a flow step whose reference continues on the next line is a site" do
      content =
        "      - { name: x, uses:\n          github/codeql-action/init@#{@poison} }\n" <>
          "    steps: [ { uses:\n        github/codeql-action/analyze@#{@poison}, name: y } ]\n"

      assert [%{line: 2}, %{line: 4}] = PI.pi001_denylisted_pin(content, @policy)
      assert {:ok, new_content, [_, _]} = PI.substitute_denylisted_pins(content, @policy)
      assert new_content =~ "          github/codeql-action/init@#{@good} }\n"
      assert new_content =~ "        github/codeql-action/analyze@#{@good}, name: y } ]\n"

      # Control: an input's mapping is not a step — `with: { uses:` is data.
      assert PI.pi001_denylisted_pin(
               "        with: { uses:\n          github/codeql-action/init@#{@poison} }\n",
               @policy
             ) == []
    end

    test "line_states/1 says which lines are text and which a cut-off quote leaves open" do
      states =
        Hypatia.Rules.PinSites.line_states(
          "- run: |\n    uses: a/b@x\n- uses: c/d@y\n- run: \"open\n  uses: e/f@z"
        )

      assert Enum.map(states, &elem(&1, 1)) == [:yaml, :text, :yaml, :yaml, :open]
      assert List.last(states) == {"  uses: e/f@z", :open}
    end

    test "a KYAML flow step on its own line is still a site" do
      content =
        "steps: [\n  { name: \"x\", run: \"echo 'a\" },\n" <>
          "  { uses: \"github/codeql-action/init@#{@poison}\" },\n]\n"

      assert [%{line: 3}] = PI.pi001_denylisted_pin(content, @policy)
    end
  end

  describe "action_base/1" do
    test "keys on the release unit, not the sub-action" do
      assert PI.action_base("github/codeql-action/init") == "github/codeql-action"
      assert PI.action_base("actions/checkout") == "actions/checkout"
      assert PI.action_base("not-a-path") == "not-a-path"
    end
  end

  describe "denylisted?/3" do
    test "matches the SHA, the tag and the bare version — all three spellings" do
      assert %{"id" => "PIN-001"} = PI.denylisted?(@policy, "github/codeql-action/init", @poison)

      assert %{"id" => "PIN-001"} =
               PI.denylisted?(@policy, "github/codeql-action/analyze", "v4.38.1")

      assert %{"id" => "PIN-001"} = PI.denylisted?(@policy, "github/codeql-action/init", "4.38.1")

      # Upper-case hex is the same commit; the table is written in lower case.
      assert %{"id" => "PIN-001"} =
               PI.denylisted?(@policy, "github/codeql-action/init", String.upcase(@poison))
    end

    test "does not match a good ref, another action, or another repo's entry" do
      refute PI.denylisted?(@policy, "github/codeql-action/init", @good)
      assert PI.denylisted?(@policy, "dtolnay/rust-toolchain", "stable") == nil
      assert PI.denylisted?(@policy, "actions/checkout", "v4.38.1") == nil
    end
  end

  describe "pi001_denylisted_pin/2 — a poisoned pin wearing a good label" do
    test "fires on the SHA regardless of what the comment claims" do
      content = """
      jobs:
        analyze:
          steps:
            - uses: github/codeql-action/init@#{@poison} # v4.38.0
      """

      assert [finding] =
               PI.pi001_denylisted_pin(content, @policy, path: ".github/workflows/codeql.yml")

      assert finding.rule_id == "PI001"
      assert finding.severity == :critical
      assert finding.line == 4
      assert finding.path == ".github/workflows/codeql.yml"
      assert finding.detail.denylist_id == "PIN-001"
      assert finding.detail.known_good_sha == @good
      assert finding.detail.repair == "substitution"
      assert finding.description =~ "startup_failure"
    end

    test "silent on a good pin and on an action the policy does not deny" do
      content = """
      jobs:
        analyze:
          steps:
            - uses: github/codeql-action/init@#{@good} # v4.38.0
            - uses: dtolnay/rust-toolchain@stable
      """

      assert PI.pi001_denylisted_pin(content, @policy) == []
    end
  end

  describe "pi002_mislabelled_pin/2 — the comment against the resolved ref" do
    test "the rollback that relabelled without re-pinning" do
      poison = @poison
      content = "- uses: github/codeql-action/init@#{@poison} # v4.38.0\n"
      resolution = %{poison => "4.38.1"}

      assert [finding] = PI.pi002_mislabelled_pin(content, resolution)
      assert finding.rule_id == "PI002"
      assert finding.severity == :high
      assert finding.detail.claimed == "4.38.0"
      assert finding.detail.resolved == "4.38.1"
    end

    test "silent when the ref cannot be resolved — an unjudgeable claim is not a finding" do
      content = "- uses: github/codeql-action/init@#{@poison} # v4.38.0\n"

      assert PI.pi002_mislabelled_pin(content, %{}) == []
    end

    test "a coarse claim that does not contradict the resolved version is silent" do
      ref = "aaaa1111bbbb2222cccc3333dddd4444eeee5555"
      content = "- uses: actions/checkout@#{ref} # v3\n"

      assert PI.pi002_mislabelled_pin(content, %{ref => "3.1.0"}) == []
    end

    test "dictask's mislabel: a 4.38.1 pin annotated v3" do
      poison = @poison
      content = "- uses: github/codeql-action/init@#{@poison} # v3\n"

      assert [finding] = PI.pi002_mislabelled_pin(content, %{poison => "4.38.1"})
      assert finding.detail.claimed == "3"
      assert finding.detail.resolved == "4.38.1"
    end

    test "a comment with no version claim at all is not a finding" do
      poison = @poison
      content = "- uses: github/codeql-action/init@#{@poison} # do not move\n"

      assert PI.pi002_mislabelled_pin(content, %{poison => "4.38.1"}) == []
    end
  end

  describe "claimed_version/1" do
    test "reads the estate's real comment shapes" do
      assert PI.claimed_version("v4.38.0") == "4.38.0"

      assert PI.claimed_version("v4.38.0 (4.38.1 blocked estate-wide; nexia-list#100)") ==
               "4.38.0"

      assert PI.claimed_version("v3") == "3"
      assert PI.claimed_version("Pinned to v1.2.3 — do not move") == "1.2.3"
    end

    test "makes no claim out of prose" do
      assert PI.claimed_version("do not move") == nil
      assert PI.claimed_version("2 jobs") == nil
      assert PI.claimed_version("") == nil
      assert PI.claimed_version(nil) == nil
    end
  end

  describe "relabel/2 — byte-for-byte with relabel_comment in estate-pin-integrity.sh" do
    test "relabels a comment that leads with a version claim" do
      # The comment arrives from pin_sites/1 without its leading `#`.
      assert PI.relabel("v3", "4.38.0") == "# v4.38.0"
      assert PI.relabel("# v3", "4.38.0") == "# v4.38.0"
      # A bare claim without the `v` must keep its first digit.
      assert PI.relabel("4.38.1", "4.38.0") == "# v4.38.0"
    end

    test "leaves a comment that already carries the good version byte-identical" do
      comment = "# v4.38.0 (4.38.1 blocked estate-wide; nexia-list#100)"

      assert PI.relabel(comment, "4.38.0") == comment
    end

    test "leaves prose alone, whatever version it mentions mid-sentence" do
      assert PI.relabel("# Pinned to v1.2.3 — do not move", "4.38.0") ==
               "# Pinned to v1.2.3 — do not move"
    end

    test "empty stays empty, and a nil version is a no-op" do
      assert PI.relabel("", "4.38.0") == ""
      assert PI.relabel("# v3", nil) == "# v3"
    end
  end

  describe "substitute_denylisted_pins/2 — the whole of the mechanical repair" do
    test "rewrites every denylisted site and relabels only leading version claims" do
      assert {:ok, new_content, excisions} =
               PI.substitute_denylisted_pins(@poisoned_file, @policy)

      assert length(excisions) == 2
      refute new_content =~ "1c5b6756"
      assert length(Regex.scan(~r/b96794f0/, new_content)) == 2
      assert new_content =~ "codeql-action/init@#{@good}"
      assert new_content =~ "codeql-action/analyze@#{@good}"

      [first, second] = excisions
      assert first.action == "github/codeql-action/init"
      assert first.from_ref == @poison
      assert first.to_ref == @good
      assert first.to_version == "4.38.0"
      assert String.trim(first.comment_after) == "v4.38.0"

      # The rollback comment is a sentence, not a label: it must survive
      # byte-identical, and the `# v3` mislabel must not survive at all.
      assert second.comment_after == " v4.38.0 (4.38.1 blocked estate-wide; nexia-list#100)"
      assert new_content =~ "# v4.38.0 (4.38.1 blocked estate-wide; nexia-list#100)"
      refute new_content =~ "# v3"
    end

    test "KYAML: a quoted poisoned pin is caught and repaired in place" do
      kyaml = """
      {
        jobs: {
          a: {
            steps: [
              { uses: "actions/checkout@v7.0.1" },
              {
                uses: "github/codeql-action/init@#{@poison}", # v4.38.1
              },
            ],
          },
        },
      }
      """

      assert [%{line: 7}] = PI.pi001_denylisted_pin(kyaml, @policy)

      assert {:ok, new_content, [_]} = PI.substitute_denylisted_pins(kyaml, @policy)
      assert new_content =~ ~s(uses: "github/codeql-action/init@#{@good}", # v4.38.0)
      assert PI.pi001_denylisted_pin(new_content, @policy) == []
    end

    test "a quoted \"uses\" key is the same key: caught by PI001 and repaired in place" do
      content = """
      jobs:
        a:
          steps:
            - "uses": "github/codeql-action/init@#{@poison}" # v4.38.1
      """

      assert [%{line: 4}] = PI.pi001_denylisted_pin(content, @policy)

      assert {:ok, new_content, [_]} = PI.substitute_denylisted_pins(content, @policy)
      assert new_content =~ ~s("uses": "github/codeql-action/init@#{@good}" # v4.38.0)
      assert PI.pi001_denylisted_pin(new_content, @policy) == []
    end

    test "a compact flow step is a site: caught by PI001 and spliced at its own offset" do
      # `- { name: ..., uses: ... }` carries the dash, the brace and a sibling
      # key; the old whole-line pattern allowed only one of the first two and
      # nothing after the ref, so this pin was invisible to the file scan.
      content = """
      jobs:
        a:
          steps:
            - { name: "scan #{@poison}", uses: github/codeql-action/init@#{@poison} } # v4.38.1
            - uses: actions/checkout@v7.0.1
      """

      assert [%{line: 4, detail: %{denylist_id: "PIN-001"}}] =
               PI.pi001_denylisted_pin(content, @policy)

      assert {:ok, new_content, [excision]} = PI.substitute_denylisted_pins(content, @policy)
      assert excision.from_ref == @poison

      # Only the site moves: the same 40 hex characters inside `name:` are
      # text and stay as written, and the one-site line's label is relabelled.
      assert new_content =~
               ~s(- { name: "scan #{@poison}", uses: github/codeql-action/init@#{@good} } # v4.38.0)

      assert PI.pi001_denylisted_pin(new_content, @policy) == []
    end

    test "a multi-site flow line is repaired site by site and its shared label is kept" do
      content =
        "    steps: [ { uses: github/codeql-action/init@#{@poison} }, " <>
          "{ uses: actions/checkout@v7.0.1 } ] # v7.0.1\n"

      assert [_] = PI.pi001_denylisted_pin(content, @policy)
      assert {:ok, new_content, [_]} = PI.substitute_denylisted_pins(content, @policy)
      assert new_content =~ "init@#{@good} }, { uses: actions/checkout@v7.0.1 } ] # v7.0.1"
    end

    test "a mixed-case action spelling meets its lower-case denylist entry" do
      # GitHub resolves `GitHub/CodeQL-Action` to `github/codeql-action`.
      content = "      - uses: GitHub/CodeQL-Action/init@#{@poison} # v4.38.1\n"

      assert [%{rule_id: "PI001"}] = PI.pi001_denylisted_pin(content, @policy)

      assert {:ok, new_content, [_]} = PI.substitute_denylisted_pins(content, @policy)
      assert new_content == "      - uses: GitHub/CodeQL-Action/init@#{@good} # v4.38.0\n"
    end

    test "a file with no denylisted site is returned untouched" do
      clean = "- uses: actions/checkout@v7.0.1\n"

      assert {:ok, ^clean, []} = PI.substitute_denylisted_pins(clean, @policy)
    end

    test "refuses rather than guesses when the policy carries no known-good SHA" do
      policy = %{
        "pin_denylist" => [
          %{
            "id" => "PIN-001",
            "action" => "github/codeql-action",
            "severity" => "critical",
            "blocked_shas" => [@poison]
          }
        ]
      }

      content = "- uses: github/codeql-action/init@#{@poison} # v3\n"

      assert {:error, reason} = PI.substitute_denylisted_pins(content, policy)
      assert reason =~ "no known_good_sha"
    end
  end

  describe "pin_only_edit?/3 — the MGX-001 proof obligation" do
    test "a token substitution on the same action across both sides" do
      removed = ["      - uses: github/codeql-action/init@#{@poison} # v4.38.1"]
      added = ["      - uses: github/codeql-action/init@#{@good} # v4.38.0"]

      assert PI.pin_only_edit?(removed, added, @policy)
    end

    test "false when any changed line is not a uses pin" do
      removed = ["      - uses: github/codeql-action/init@#{@poison}"]
      added = ["      - uses: github/codeql-action/init@#{@good}", "      permissions: {}"]

      refute PI.pin_only_edit?(removed, added, @policy)
    end

    test "false when one action is swapped for another" do
      removed = ["      - uses: actions/checkout@v7.0.1"]
      added = ["      - uses: actions/setup-node@v7.0.1"]

      refute PI.pin_only_edit?(removed, added, @policy)
    end

    test "false when the substitution moves onto the denylist" do
      removed = ["      - uses: github/codeql-action/init@#{@good}"]
      added = ["      - uses: github/codeql-action/init@#{@poison}"]

      refute PI.pin_only_edit?(removed, added, @policy)
    end

    test "false when nothing actually changed" do
      line = "      - uses: actions/checkout@v7.0.1"

      refute PI.pin_only_edit?([line], [line], @policy)
    end

    test "false when either side is empty" do
      refute PI.pin_only_edit?([], ["      - uses: actions/checkout@v7.0.1"], @policy)
      refute PI.pin_only_edit?(["      - uses: actions/checkout@v7.0.1"], [], @policy)
    end
  end

  describe "pi003_locked_moving_ref/2" do
    test "a moving ref in a lockfile is a finding with a structural repair" do
      lock = """
      '.github/workflows/security-policy.yml':
          - 'dtolnay/rust-toolchain@stable'
          - 'actions/checkout@v7.0.1'
      """

      assert [finding] = PI.pi003_locked_moving_ref(lock, @policy)
      assert finding.rule_id == "PI003"
      assert finding.severity == :high
      assert finding.line == 2
      assert finding.detail.ref == "stable"
      assert finding.detail.action == "dtolnay/rust-toolchain"
      assert finding.detail.repair == "unpin_moving_ref"
    end

    test "a non-moving ref is never a PI003" do
      lock = "    - 'actions/checkout@v7.0.1'\n"

      assert PI.pi003_locked_moving_ref(lock, @policy) == []
    end

    test "two actions locked at one moving ref are two findings, each pair once" do
      lock = """
      '.github/workflows/ci.yml':
          - 'dtolnay/rust-toolchain@stable'
          - 'some/other-action@stable'
      dependencies:
          'dtolnay/rust-toolchain@stable':
      """

      assert [a, b] = PI.pi003_locked_moving_ref(lock, @policy)
      assert {a.detail.action, a.line} == {"some/other-action", 3}
      assert {b.detail.action, b.line} == {"dtolnay/rust-toolchain", 5}
    end
  end

  describe "pi004_lock_divergence/3" do
    test "workflow and lockfile disagreeing about the same action" do
      workflow = "- uses: github/codeql-action/init@#{@poison} # v4.38.1\n"

      lock = """
      '.github/workflows/codeql.yml':
          - 'github/codeql-action@#{@good}'
      """

      assert [finding] = PI.pi004_lock_divergence(workflow, lock)
      assert finding.rule_id == "PI004"
      assert finding.severity == :medium
      assert finding.detail.workflow_ref == @poison
      assert finding.detail.lock_refs == [@good]
      assert finding.detail.repair == "reconcile_lock_and_workflow"
    end

    test "agreement is silence" do
      workflow = "- uses: github/codeql-action/init@#{@good}\n"
      lock = "    - 'github/codeql-action@#{@good}'\n"

      assert PI.pi004_lock_divergence(workflow, lock) == []
    end

    test "an action the lockfile does not mention is not a divergence" do
      workflow = "- uses: actions/checkout@v7.0.1\n"
      lock = "    - 'github/codeql-action@#{@good}'\n"

      assert PI.pi004_lock_divergence(workflow, lock) == []
    end

    test "an action sharing its locked ref with another action is still compared" do
      # Keyed by ref alone, `github/codeql-action@v4` replaced the checkout
      # entry and the divergence below went unreported.
      workflow = "- uses: actions/checkout@v3\n"
      lock = "    - 'actions/checkout@v4'\n    - 'github/codeql-action@v4'\n"

      assert [finding] = PI.pi004_lock_divergence(workflow, lock)
      assert finding.detail.lock_refs == ["v4"]
    end

    test "a commit ID in upper case agrees with the same commit locked in lower case" do
      workflow = "- uses: github/codeql-action/init@#{String.upcase(@good)}\n"
      lock = "    - 'github/codeql-action@#{@good}'\n"

      assert PI.pi004_lock_divergence(workflow, lock) == []

      # Control: a symbolic ref is a name — `V4` is not the tag `v4`.
      assert [_] =
               PI.pi004_lock_divergence(
                 "- uses: actions/checkout@V4\n",
                 "    - 'actions/checkout@v4'\n"
               )
    end
  end

  describe "pi005_stale_pin/2 — advisory, never an action" do
    test "an old pin with a versioned comment, reported as information" do
      content = "- uses: actions/checkout@v4.1.7 # v4.1.7\n"

      assert [finding] = PI.pi005_stale_pin(content, %{"actions/checkout" => "4.2.0"})
      assert finding.rule_id == "PI005"
      assert finding.severity == :info
      assert finding.detail.latest == "4.2.0"
      assert finding.detail.repair == "advisory"
    end

    test "silent when the comment makes no version claim" do
      content = "- uses: actions/checkout@v4.1.7 # pinned deliberately\n"

      assert PI.pi005_stale_pin(content, %{"actions/checkout" => "4.2.0"}) == []
    end

    test "silent when there is no upstream information" do
      content = "- uses: actions/checkout@v4.1.7 # v4.1.7\n"

      assert PI.pi005_stale_pin(content, %{}) == []
    end
  end

  describe "PIN-003 in the committed policy (standards revisions whose lock GitHub rejects)" do
    @policy_path Path.expand(
                   "../../.machine_readable/merge-orchestration/pr-automerge-policy.json",
                   __DIR__
                 )
    @rejected "8f2ee50841e216cd8c192eeb68953118190f105c"
    @accepted "75bd5994470661a099df562d6d5ad2fe744dbd66"
    @governance "hyperpolymath/standards/.github/workflows/governance-reusable.yml"

    defp committed_policy, do: Jason.decode!(File.read!(@policy_path))

    test "the entry exists, is keyed on SHA only, and names the known-good revision" do
      policy = committed_policy()
      entry = Enum.find(policy["pin_denylist"], &(&1["id"] == "PIN-003"))
      assert entry["action"] == "hyperpolymath/standards"
      assert entry["severity"] == "critical"
      assert entry["blocked_versions"] == []
      assert entry["blocked_refs"] == []
      assert entry["known_good_sha"] == @accepted
      assert entry["replacement"] == "pin_to_known_good"
      assert length(entry["blocked_shas"]) > 500
      assert Enum.all?(entry["blocked_shas"], &Regex.match?(~r/^[0-9a-f]{40}$/, &1))
      refute @accepted in entry["blocked_shas"]
    end

    test "a caller pinned inside the window is denylisted through the reusable path; the replacement is not" do
      policy = committed_policy()
      assert %{"id" => "PIN-003"} = PI.denylisted?(policy, @governance, @rejected)
      assert PI.denylisted?(policy, @governance, @accepted) == nil
      # The June toolchain window is WF027's, not this entry's: a June revision
      # has no lock and must not be denylisted here.
      assert PI.denylisted?(policy, @governance, "d135b05bfc647d0c0fbfedc7e80f37ea50f49236") ==
               nil
    end
  end
end
