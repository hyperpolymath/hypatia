# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Mix.Tasks.Hypatia.MergeOrchestrateTest do
  use ExUnit.Case, async: false
  import ExUnit.CaptureIO

  alias Hypatia.MergeOrchestration.TestObservationStore, as: Evidence
  alias Mix.Tasks.Hypatia.MergeOrchestrate

  test "the actual manifest task writes a decision artifact without claiming a merge" do
    store = Evidence.seed()
    on_exit(fn -> File.rm_rf!(store) end)

    output = capture_io(fn -> MergeOrchestrate.run(["--store", store]) end)
    path = Path.join(store, "merge-decisions.jsonl")
    entry = path |> File.read!() |> Jason.decode!()

    assert output =~ path
    refute output =~ "baton(s)"
    assert entry["strategy"] in ["review", "report_only"]
    assert entry["merge"]["head_sha"] == Evidence.head()
    assert entry["event_id"] == Evidence.event()
    assert File.ls!(Path.join(store, "leases")) == []
  end

  test "unavailable Baton modes fail the actual argv path even with no candidates" do
    for mode <- ["baton", "both"], observations <- [[], [Evidence.observation()]] do
      store = Evidence.seed(observations: observations)
      on_exit(fn -> File.rm_rf!(store) end)
      manifest = Path.join(store, "merge-decisions.jsonl")
      File.write!(manifest, "previous decision manifest")

      output =
        capture_io(fn ->
          assert_raise Mix.Error, ~r/qualified_merge_executor_unavailable/, fn ->
            MergeOrchestrate.run(["--store", store, "--actuation", mode])
          end
        end)

      assert output == ""
      assert File.read!(manifest) == "previous decision manifest"
      assert File.ls!(Path.join(store, "leases")) == []
    end
  end
end
