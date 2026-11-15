# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# E0-05: the live neural path learns from qualified evidence only.
# FleetDispatcher.ingest_fix_outcome/2 is reached by dispatch_finding/1 for a
# :fix_outcome finding. Every field of that finding is a claim of the caller, so
# neither its `success` nor a `verification` that it carries qualifies an outcome.
defmodule Hypatia.FleetDispatcherNeuralQualificationTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  alias Hypatia.FleetDispatcher

  # A fake coordinator: a process under a name that forwards what it receives to
  # the test, in the shape that Coordinator.record_outcome/3 sends. The path
  # also appends a row to a learning file; a test gives it a temporary file and
  # never lets it write under the real home directory.
  setup do
    test = self()
    name = :"fake_coordinator_#{System.unique_integer([:positive])}"

    pid =
      spawn_link(fn ->
        loop = fn loop ->
          receive do
            {:source_bound, _source, {:outcome, finding, outcome}} ->
              send(test, {:coordinator_received, finding, outcome})
              loop.(loop)
          end
        end

        loop.(loop)
      end)

    Process.register(pid, name)

    dir = Path.join(System.tmp_dir!(), "fd_learning_#{System.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf!(dir) end)

    %{server: name, learning_file: Path.join(dir, "fix-outcomes.jsonl")}
  end

  defp finding(extra) do
    Map.merge(
      %{type: :fix_outcome, pattern: "PA001", repo: "owner/repo", success: true, fix_type: "t"},
      extra
    )
  end

  defp ingest(ctx, finding, extra \\ []) do
    FleetDispatcher.ingest_fix_outcome(
      finding,
      [server: ctx.server, learning_file: ctx.learning_file] ++ extra
    )
  end

  test "a finding with success: true and no verification does not reach the coordinator", ctx do
    ingest(ctx, finding(%{}))
    refute_receive {:coordinator_received, _, _}, 100
  end

  test "a verification that the caller put in the finding does not qualify it", ctx do
    ingest(ctx, finding(%{verification: "verified"}))
    ingest(ctx, finding(%{verification: :verified}))
    ingest(ctx, finding(%{"verification" => "verified"}))
    refute_receive {:coordinator_received, _, _}, 100
  end

  test "a failure claimed by the caller does not reach the coordinator either", ctx do
    ingest(ctx, finding(%{success: false, verification: "failed"}))
    refute_receive {:coordinator_received, _, _}, 100
  end

  test "an outcome that is not learned is logged once, with the pattern and the repo", ctx do
    log = capture_log(fn -> ingest(ctx, finding(%{})) end)

    assert log =~ "PA001"
    assert log =~ "owner/repo"
    assert length(String.split(log, "not learned")) == 2
  end

  test "the observation is still appended to the learning file, qualified or not", ctx do
    ingest(ctx, finding(%{}))
    assert File.read!(ctx.learning_file) =~ ~s("type":"PA001")
  end

  describe "with a trusted source of the verification" do
    # No production path supplies one yet (see trusted_verification/1 in
    # FleetDispatcher); the resolver is driven through the option that exists for
    # tests, so the rule that decides what reaches the coordinator is pinned.
    defp trusted(verification), do: [trusted_verification: fn _finding -> verification end]

    test "an outcome that the trusted source verifies reaches the coordinator once", ctx do
      ingest(ctx, finding(%{}), trusted("verified"))

      assert_receive {:coordinator_received, %{pattern: "PA001"}, "success"}, 500
      refute_receive {:coordinator_received, _, _}, 100
    end

    test "a failure that the trusted source qualifies reaches the coordinator once", ctx do
      for verification <- ["failed", "incomplete"] do
        ingest(ctx, finding(%{success: false}), trusted(verification))
        assert_receive {:coordinator_received, _, "failure"}, 500
      end

      refute_receive {:coordinator_received, _, _}, 100
    end

    test "an outcome that the trusted source does not qualify does not reach it", ctx do
      for verification <-
            ["unverified", "no_op", "scan_skipped", "scan_failed", "still_present", nil] do
        ingest(ctx, finding(%{}), trusted(verification))
      end

      refute_receive {:coordinator_received, _, _}, 100
    end

    test "the trusted source decides, not the finding", ctx do
      # The caller says verified; the trusted source says unverified.
      ingest(ctx, finding(%{verification: "verified"}), trusted("unverified"))
      refute_receive {:coordinator_received, _, _}, 100

      # And the other way: the caller says nothing, the trusted source verifies.
      ingest(ctx, finding(%{}), trusted("verified"))
      assert_receive {:coordinator_received, _, "success"}, 500
    end
  end
end
