# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Mix.Tasks.Hypatia.WatchTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  alias Hypatia.Telemetry, as: T

  setup do
    case Process.whereis(Hypatia.Watcher) do
      nil ->
        {:ok, pid} = Hypatia.Watcher.start_link([])
        on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid) end)

      _ ->
        :ok
    end

    :ok
  end

  describe "render/3" do
    test "renders all event kinds with their dotted-string names" do
      T.scan_complete(50, 3, path: "/tmp/x", severity_floor: "low")
      T.dispatch_decision(0.95, strategy: :auto_execute, tier: :eliminate, recipe_id: "r1", repo: "x")
      T.outcome_recorded(recipe_id: "r1", repo: "x", outcome: "success", verification: "verified")
      T.quarantine_triggered(kind: :recipe, id: "bad", reason: "verification_rate", level: :auto)
      Process.sleep(50)

      snap = Hypatia.Watcher.snapshot()
      output = capture_io(fn -> Mix.Tasks.Hypatia.Watch.render(snap, true, []) end)

      assert output =~ "Hypatia Watcher"
      assert output =~ "Events / 5min"
      assert output =~ "Events / 1hr"
      assert output =~ "GenServer queue depths"

      # Each event kind should be present in the dotted-string format.
      assert output =~ "hypatia.scan.complete"
      assert output =~ "hypatia.dispatch.decision"
      assert output =~ "hypatia.outcome.recorded"
      assert output =~ "hypatia.quarantine.triggered"
    end

    test "renders :unavailable when the watcher snapshot is missing" do
      output = capture_io(fn -> Mix.Tasks.Hypatia.Watch.render(:unavailable, true, []) end)
      assert output =~ "Watcher unavailable"
    end

    test "warns when dropped_events > 0" do
      snap = %{
        counts: %{m5: %{}, h1: %{}, d1: %{}},
        queue_depths: %{},
        dropped_events: 42,
        uptime_seconds: 10,
        generated_at: "2026-05-24T00:00:00Z",
        recent_by_kind: %{}
      }

      output = capture_io(fn -> Mix.Tasks.Hypatia.Watch.render(snap, true, []) end)
      assert output =~ "Dropped 42 telemetry event"
    end
  end
end
