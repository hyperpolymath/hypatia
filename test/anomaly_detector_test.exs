# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Watcher.AnomalyDetectorTest do
  use ExUnit.Case, async: false

  alias Hypatia.Watcher.AnomalyDetector
  alias Hypatia.Telemetry, as: T

  setup do
    case Process.whereis(Hypatia.Watcher.PubSub) do
      nil ->
        {:ok, pid} = Registry.start_link(keys: :duplicate, name: Hypatia.Watcher.PubSub)
        on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid) end)

      _ ->
        :ok
    end

    case Process.whereis(Hypatia.Watcher) do
      nil ->
        {:ok, pid} = Hypatia.Watcher.start_link([])
        on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid) end)

      _ ->
        :ok
    end

    case Process.whereis(AnomalyDetector) do
      nil ->
        {:ok, pid} = AnomalyDetector.start_link([])
        on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid) end)

      _ ->
        :ok
    end

    :ok
  end

  describe "outcome ingestion" do
    test "rolling window accumulates outcome.recorded events" do
      for _ <- 1..5 do
        T.outcome_recorded(recipe_id: "r", repo: "x", outcome: "success", verification: "verified")
      end

      Process.sleep(100)

      window = AnomalyDetector.window()
      assert length(window) >= 5
    end
  end

  describe "evaluate (insufficient data)" do
    test "below min_outcomes_for_alert, tick is a no-op" do
      # Fresh detector — fewer than 30 outcomes
      assert :ok = AnomalyDetector.tick_now()
    end
  end

  describe "evaluate (clear regression)" do
    test "emits hypatia.anomaly.detected when recent rate drops 100% → 0%" do
      :ok = :telemetry.attach("anomaly-test-handler",
        [:hypatia, :anomaly, :detected],
        fn _event, measurements, metadata, _config ->
          send(self(), {:caught, measurements, metadata})
        end,
        nil)

      on_exit(fn -> :telemetry.detach("anomaly-test-handler") end)

      # 170 baseline successes + 30 recent failures = clear regression
      for _ <- 1..170 do
        T.outcome_recorded(recipe_id: "r", repo: "x", outcome: "success", verification: "verified")
      end

      for _ <- 1..30 do
        T.outcome_recorded(recipe_id: "r", repo: "x", outcome: "failure", verification: "unverified")
      end

      # Let the cast queue drain
      Process.sleep(200)

      :ok = AnomalyDetector.tick_now()

      # Handler runs in the *emitting* process (the detector). It sends
      # to whatever pid is captured at attach time -- which was the
      # test process. So we receive in this test's mailbox.
      assert_receive {:caught, measurements, metadata}, 1000

      assert measurements.recent_rate < 0.5
      assert measurements.baseline_rate > 0.5
      assert measurements.sigma_distance > 2.0
      assert metadata.kind == :success_rate_drop
    end
  end

  describe "evaluate (stable history)" do
    test "does NOT emit when recent rate matches baseline" do
      caller = self()

      :ok = :telemetry.attach("anomaly-stable-handler",
        [:hypatia, :anomaly, :detected],
        fn _event, _measurements, _metadata, _config ->
          send(caller, :unexpected_anomaly)
        end,
        nil)

      on_exit(fn -> :telemetry.detach("anomaly-stable-handler") end)

      # Mixed but stable history: 80% success across both halves.
      for i <- 1..200 do
        outcome = if rem(i, 5) == 0, do: "failure", else: "success"
        T.outcome_recorded(recipe_id: "r", repo: "x", outcome: outcome, verification: "verified")
      end

      Process.sleep(200)

      :ok = AnomalyDetector.tick_now()

      refute_receive :unexpected_anomaly, 200
    end
  end
end
