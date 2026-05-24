# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.WatcherTest do
  # async: false because the Watcher is a named singleton attached to
  # global telemetry handlers; concurrent tests would observe each
  # other's events.
  use ExUnit.Case, async: false

  alias Hypatia.Watcher
  alias Hypatia.Telemetry, as: T

  setup do
    # If the Application's Watcher is already running (production tests),
    # use it. Otherwise spin one up just for this test.
    pid =
      case Process.whereis(Watcher) do
        nil ->
          {:ok, p} = Watcher.start_link([])
          on_exit(fn -> if Process.alive?(p), do: GenServer.stop(p) end)
          p

        existing ->
          existing
      end

    {:ok, watcher: pid}
  end

  describe "telemetry → counters" do
    test "scan_complete event increments the m5 window counter" do
      before = Watcher.counts(:m5) |> Map.get([:hypatia, :scan, :complete], 0)

      T.scan_complete(123, 7, path: "/tmp/x", severity_floor: "low")

      # Give the cast a moment to be processed (cast is async).
      Process.sleep(50)

      after_ = Watcher.counts(:m5) |> Map.get([:hypatia, :scan, :complete], 0)
      assert after_ == before + 1
    end

    test "dispatch_decision event lands in all three windows" do
      before_5m = Watcher.counts(:m5) |> Map.get([:hypatia, :dispatch, :decision], 0)
      before_1h = Watcher.counts(:h1) |> Map.get([:hypatia, :dispatch, :decision], 0)
      before_1d = Watcher.counts(:d1) |> Map.get([:hypatia, :dispatch, :decision], 0)

      T.dispatch_decision(0.95,
        strategy: :auto_execute,
        tier: :eliminate,
        recipe_id: "test-recipe",
        repo: "test/repo"
      )

      Process.sleep(50)

      assert Watcher.counts(:m5) |> Map.get([:hypatia, :dispatch, :decision], 0) == before_5m + 1
      assert Watcher.counts(:h1) |> Map.get([:hypatia, :dispatch, :decision], 0) == before_1h + 1
      assert Watcher.counts(:d1) |> Map.get([:hypatia, :dispatch, :decision], 0) == before_1d + 1
    end
  end

  describe "snapshot/0" do
    test "returns a fully-shaped map" do
      T.outcome_recorded(recipe_id: "x", repo: "r", outcome: "success", verification: "verified")
      Process.sleep(50)

      snap = Watcher.snapshot()

      assert Map.has_key?(snap, :counts)
      assert Map.has_key?(snap, :queue_depths)
      assert Map.has_key?(snap, :dropped_events)
      assert Map.has_key?(snap, :recent_by_kind)
      assert Map.has_key?(snap, :uptime_seconds)
      assert Map.has_key?(snap, :generated_at)

      assert Map.has_key?(snap.counts, :m5)
      assert Map.has_key?(snap.counts, :h1)
      assert Map.has_key?(snap.counts, :d1)
    end
  end

  describe "recent_events/0" do
    test "captures the latest event per kind with measurements + metadata" do
      T.verification_result(recipe_id: "drilldown", repo: "r/x", verdict: :verified)
      Process.sleep(50)

      events = Watcher.recent_events()
      kind_events = Map.get(events, [:hypatia, :verification, :result], [])

      assert is_list(kind_events)
      assert length(kind_events) >= 1

      [latest | _] = kind_events
      assert latest.metadata.recipe_id == "drilldown"
      assert latest.metadata.verdict == :verified
      assert is_integer(latest.at)
    end
  end

  describe "queue_depths/0" do
    test "returns depths for supervised processes when supervisor exists" do
      # The depth map may be empty if Hypatia.Supervisor isn't started
      # (tests in isolation). Just assert the shape rather than content.
      depths = Watcher.queue_depths()
      assert is_map(depths)
    end
  end
end
