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

    test "documents --advisory and HYPATIA_ADVISORY" do
      help = capture_io(fn -> Hypatia.CLI.main(["help"]) end)

      assert help =~ "--advisory"
      assert help =~ "HYPATIA_ADVISORY"
    end
  end

  # WF026/WF027 name a repair the fleet can route and must never fail a gate
  # (AGENTIC must-not-promote-to-blocking). At `:low` the default threshold
  # hid them from every consumer; lowering `--severity` to see them made them,
  # and every other low finding, fail the scan. `report_sets/2` separates
  # what is shown from what decides exit 1.
  describe "report_sets/2 -- advisory findings are shown on request, never blocking" do
    @advisory %{rule: "WF027", severity: "low", advisory: true, fix_script: "x.sh"}
    @plain_low %{rule: "flawed_regex", severity: "low"}
    @medium %{rule: "unpinned_action", severity: "medium"}
    @base %{severity: "medium", exit_zero: false, advisory: false}

    test "default threshold: an advisory low finding is neither shown nor blocking" do
      assert {[@medium], [@medium]} =
               Hypatia.CLI.report_sets([@advisory, @plain_low, @medium], @base)
    end

    test "--advisory shows it below the threshold, in order, without blocking" do
      config = %{@base | advisory: true}

      assert {[@advisory, @medium], [@medium]} =
               Hypatia.CLI.report_sets([@advisory, @plain_low, @medium], config)
    end

    test "an advisory finding alone never decides exit 1" do
      assert {[@advisory], []} =
               Hypatia.CLI.report_sets([@advisory], %{@base | advisory: true})
    end

    test "at --severity low it is shown, a plain low finding blocks, it does not" do
      config = %{@base | severity: "low"}

      assert {[@advisory, @plain_low, @medium], [@plain_low, @medium]} =
               Hypatia.CLI.report_sets([@advisory, @plain_low, @medium], config)
    end

    test "a config without the :advisory key behaves as before" do
      assert {[@medium], [@medium]} =
               Hypatia.CLI.report_sets([@advisory, @medium], %{severity: "medium"})
    end

    test "advisory survives the workflow_audit normalizer" do
      dir = Path.join(System.tmp_dir!(), "hyp-advisory-#{:erlang.unique_integer([:positive])}")
      File.mkdir_p!(Path.join(dir, ".github/workflows"))
      on_exit(fn -> File.rm_rf!(dir) end)

      File.write!(Path.join(dir, ".github/workflows/rust-ci.yml"), """
      name: rust-ci
      on: push
      permissions: {}
      jobs:
        rust-ci:
          uses: hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml@d135b05bfc647d0c0fbfedc7e80f37ea50f49236
      """)

      wf027 =
        dir
        |> Hypatia.CLI.collect_findings([:workflow_audit])
        |> Enum.filter(&(&1.type == "known_broken_reusable_revision"))

      assert [%{advisory: true, severity: "low", fix_script: "fix-stale-reusable-pin.sh"}] =
               wf027

      assert {[], []} = Hypatia.CLI.report_sets(wf027, %{@base | advisory: false})
      assert {^wf027, []} = Hypatia.CLI.report_sets(wf027, %{@base | advisory: true})
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
