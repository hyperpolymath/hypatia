# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Watcher.AnomalyDetectorTest do
  use ExUnit.Case, async: false

  alias Hypatia.Watcher.AnomalyDetector
  alias Hypatia.Telemetry, as: T

  setup do
    case Process.whereis(Hypatia.Watcher.PubSub) do
      nil ->
        start_supervised!({Registry, keys: :duplicate, name: Hypatia.Watcher.PubSub})

      _ ->
        :ok
    end

    case Process.whereis(Hypatia.Watcher) do
      nil -> start_supervised!(Hypatia.Watcher)
      _ -> :ok
    end

    # Finish earlier watcher work before subscribing a fresh detector.
    assert %{counts: _} = Hypatia.Watcher.snapshot()

    # The application singleton retains its window and alert deduplication
    # state across tests. Exercise the real callbacks in an unnamed,
    # test-supervised instance instead of assuming that singleton is fresh.
    detector =
      start_supervised!(%{
        id: AnomalyDetector,
        start: {GenServer, :start_link, [AnomalyDetector, []]}
      })

    assert GenServer.call(detector, :window) == []
    %{detector: detector}
  end

  defp record_outcomes(outcomes) do
    Enum.each(outcomes, fn outcome ->
      T.outcome_recorded(
        recipe_id: "r",
        repo: "x",
        outcome: outcome,
        verification: if(outcome == "success", do: "verified", else: "unverified")
      )
    end)
  end

  defp delivered_window(detector) do
    caller = self()
    reference = make_ref()

    # Telemetry casts and this system request are sent by the test process
    # to the Watcher in order. Run the second barrier IN the Watcher: its
    # Registry broadcasts and this detector call then have the same sender.
    # A call directly from the test to the detector would not order the two
    # mailboxes. Return the Watcher's state unchanged.
    :sys.replace_state(Hypatia.Watcher, fn state ->
      window = GenServer.call(detector, :window)
      send(caller, {reference, window})
      state
    end)

    assert_receive {^reference, window}, 1000
    window
  end

  defp capture_anomalies(detector) do
    handler = {__MODULE__, make_ref()}

    :ok =
      :telemetry.attach(
        handler,
        [:hypatia, :anomaly, :detected],
        &__MODULE__.handle_anomaly/4,
        {self(), detector}
      )

    on_exit(fn -> :telemetry.detach(handler) end)
  end

  @doc false
  def handle_anomaly(_event, measurements, metadata, {caller, detector}) do
    # The supervised application detector can also emit events. Only this
    # test's detector and its statistical success-rate signal are evidence
    # for these tests; independent ESN drift is a different alert path.
    if self() == detector and Map.get(metadata, :kind) == :success_rate_drop do
      send(caller, {:caught, measurements, metadata})
    end
  end

  describe "outcome ingestion" do
    test "rolling window accumulates outcome.recorded events", %{detector: detector} do
      record_outcomes(List.duplicate("success", 5))
      assert delivered_window(detector) == List.duplicate(true, 5)
    end
  end

  describe "evaluate (insufficient data)" do
    test "below min_outcomes_for_alert, tick emits no statistical alert", %{detector: detector} do
      capture_anomalies(detector)
      assert delivered_window(detector) == []
      assert :ok = GenServer.call(detector, :tick_now)
      refute_receive {:caught, _, _}, 200
    end
  end

  describe "evaluate (clear regression)" do
    test "emits hypatia.anomaly.detected when recent rate drops 100% → 0%", %{
      detector: detector
    } do
      capture_anomalies(detector)

      record_outcomes(List.duplicate("success", 170) ++ List.duplicate("failure", 30))
      assert delivered_window(detector) == List.duplicate(false, 30) ++ List.duplicate(true, 170)

      :ok = GenServer.call(detector, :tick_now)
      assert_receive {:caught, measurements, metadata}, 1000

      assert measurements.recent_rate == 0.0
      assert measurements.baseline_rate == 1.0
      assert measurements.sigma_distance > 2.0
      assert metadata.kind == :success_rate_drop
      assert metadata.recent_count == 30
      assert metadata.baseline_count == 170
    end

    test "a repeated evaluation deduplicates the same observed regression", %{detector: detector} do
      capture_anomalies(detector)
      record_outcomes(List.duplicate("success", 170) ++ List.duplicate("failure", 30))
      expected = List.duplicate(false, 30) ++ List.duplicate(true, 170)
      assert delivered_window(detector) == expected

      assert :ok = GenServer.call(detector, :tick_now)
      assert_receive {:caught, _, %{kind: :success_rate_drop}}, 1000

      assert :ok = GenServer.call(detector, :tick_now)
      assert GenServer.call(detector, :window) == expected
      refute_receive {:caught, _, _}, 200
    end
  end

  describe "evaluate (stable history)" do
    test "does NOT emit when recent rate matches baseline", %{detector: detector} do
      capture_anomalies(detector)

      outcomes = for i <- 1..200, do: if(rem(i, 5) == 0, do: "failure", else: "success")
      record_outcomes(outcomes)
      expected = outcomes |> Enum.reverse() |> Enum.map(&(&1 == "success"))
      assert delivered_window(detector) == expected

      :ok = GenServer.call(detector, :tick_now)
      refute_receive {:caught, _, _}, 200
    end
  end
end
