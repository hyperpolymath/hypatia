# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.CLITest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureIO

  # Regression coverage for hyperpolymath/hypatia#213 -- the `scan` step
  # in consumer-repo workflows was failing under `set -e` because the
  # CLI halted with exit 1 whenever findings existed, and there was no
  # stderr diagnostic to explain why. The fix adds `--exit-zero` /
  # `HYPATIA_EXIT_ZERO` plus an always-emitted summary line; this test
  # locks in the flag plumbing and the documented exit-code semantics.

  describe "--exit-zero plumbing" do
    test "OptionParser accepts --exit-zero as a strict boolean" do
      {opts, _args, invalid} =
        OptionParser.parse(
          ["scan", ".", "--exit-zero"],
          strict: [
            rules: :string,
            format: :string,
            severity: :string,
            path: :string,
            help: :boolean,
            version: :boolean,
            exit_zero: :boolean
          ]
        )

      assert opts[:exit_zero] == true
      assert invalid == []
    end
  end

  describe "help output" do
    test "documents --exit-zero, HYPATIA_EXIT_ZERO, and exit codes" do
      help = capture_io(fn -> Hypatia.CLI.main(["help"]) end)

      assert help =~ "--exit-zero"
      assert help =~ "HYPATIA_EXIT_ZERO"
      assert help =~ "EXIT CODES"
    end
  end

  # Regression coverage for the placeholder-message defect.
  #
  # `cli.ex`'s workflow_audit normalizer read only `:detail`, while
  # `workflow_audit.ex` authors its message under three keys (`reason:` 18,
  # `detail:` 11, `description:` 3 at 71d9b19). Every finding using the other
  # two fell through to `describe_workflow_finding/1` and shipped a GENERATED
  # placeholder -- including `missing_timeout_minutes`, the estate's widest
  # class at 2,526 alerts across 292 repos, which rendered as "Issue in ci.yml"
  # in the JSON and in the SARIF `message.text` alike.
  #
  # ⚠ Each test asserts the rendered message DIFFERS from the placeholder the
  # broken version produced. Asserting only "a non-empty string" passes against
  # the defect -- that is precisely why it survived this long.
  describe "workflow_finding_message/1 -- the authored message must survive" do
    @placeholder "Issue in ci.yml"

    test "reads :description, the key the widest alert class uses" do
      f = %{
        rule: "missing_timeout_minutes",
        file: "ci.yml",
        job: "build",
        description: "Job `build` in ci.yml has no `timeout-minutes:` declaration."
      }

      msg = Hypatia.CLI.workflow_finding_message(f)

      refute msg == @placeholder
      assert msg =~ "timeout-minutes"
    end

    test "reads :reason, the key 18 workflow_audit findings use" do
      f = %{rule: "wf_thing", file: "ci.yml", reason: "Pin this action to a full SHA."}

      msg = Hypatia.CLI.workflow_finding_message(f)

      refute msg == @placeholder
      assert msg =~ "full SHA"
    end

    test "still reads :detail, which already worked" do
      f = %{rule: "wf_thing", file: "ci.yml", detail: "Detail text."}

      assert Hypatia.CLI.workflow_finding_message(f) == "Detail text."
    end

    test "a map-valued :detail does not become the alert message" do
      # Several rule modules emit `:detail` as structured metadata. None reach
      # this normalizer today, but an unguarded limb would render the map.
      f = %{
        rule: "wf_thing",
        file: "ci.yml",
        detail: %{fix: "structured", count: 3},
        reason: "The human-readable message."
      }

      assert Hypatia.CLI.workflow_finding_message(f) == "The human-readable message."
    end

    test "falls back only when the finding authored no message at all" do
      assert Hypatia.CLI.workflow_finding_message(%{rule: "x", file: "ci.yml"}) == @placeholder

      assert Hypatia.CLI.workflow_finding_message(%{rule: "x", action_ref: "foo/bar@v1"}) ==
               "Action foo/bar@v1 needs attention"

      assert Hypatia.CLI.workflow_finding_message(%{rule: "x"}) == "Workflow issue detected"
    end
  end
end
