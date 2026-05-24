# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Watcher.AnomalyDetector do
  @moduledoc """
  Statistical anomaly detector for outcome stream.

  Phase 3 closes the loop with a cheap, dependency-free anomaly check
  that fires telemetry when measured success rate diverges from its
  recent baseline by > 2σ. The Alerts module picks up
  `hypatia.anomaly.detected` and fans out via the configured sinks.

  Architecture choice (intentional): this detector is purely
  statistical on the outcome stream, not a wrapper around the ESN.
  Two reasons:

    1. The ESN is fed by `Hypatia.Neural.Coordinator` and depends on
       multi-network state we don't want to couple to. Wiring its
       forecast in directly would create a tight dependency that
       breaks if the Coordinator restarts or the network rebalances.

    2. The simple rolling-baseline approach catches the same kind of
       regression (sustained success-rate drop) without any of the
       hyperparameter sensitivity ESN forecasting carries.

  When the Coordinator IS healthy and reports a non-`:stable` drift
  state, we additionally emit an `:esn_drift_concurs` flag in the
  alert metadata. The neural signal becomes a corroborating piece of
  evidence rather than the gate.

  ## Detection

  Rolling window of the last `@window_size` outcomes. Every
  `@tick_interval_ms`:

    1. If we have at least `@min_outcomes_for_alert` outcomes,
       compute recent (last `@recent_size`) success rate vs baseline
       (older entries).
    2. Compute σ via the binomial-proportion standard error.
    3. If `(baseline - recent) / σ > @sigma_threshold`, emit
       `hypatia.anomaly.detected` with full context.
    4. Dedup per `kind` within `@dedup_window_ms` to avoid spamming
       while a degradation persists.

  ## Telemetry surface

  Emits `[:hypatia, :anomaly, :detected]` with:

      measurements:
        recent_rate            (0.0..1.0)
        baseline_rate          (0.0..1.0)
        sigma_distance         signed σ — negative means drop

      metadata:
        kind                   :success_rate_drop
        recent_count           int
        baseline_count         int
        esn_drift_concurs      boolean
        esn_state              :rising_drift | :falling_drift | :stable | :unavailable
  """

  use GenServer

  require Logger

  @window_size 200
  @recent_size 30
  @min_outcomes_for_alert 30
  @tick_interval_ms 60_000
  @sigma_threshold 2.0
  @dedup_window_ms 5 * 60 * 1000

  # ─── Public ────────────────────────────────────────────────────────────

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Force an immediate evaluation. Returns whatever anomaly verdict the
  detector produced. Useful in tests and for manual triage.
  """
  def tick_now do
    GenServer.call(__MODULE__, :tick_now, 5_000)
  end

  @doc """
  Current rolling window — newest first. Each entry is a boolean
  (success = true). Useful in tests.
  """
  def window do
    GenServer.call(__MODULE__, :window, 5_000)
  end

  # ─── GenServer ─────────────────────────────────────────────────────────

  @impl true
  def init(_opts) do
    Hypatia.Watcher.subscribe(events: [[:hypatia, :outcome, :recorded]])

    Process.send_after(self(), :tick, @tick_interval_ms)

    state = %{
      outcomes: [],
      last_alert_at: %{}
    }

    {:ok, state}
  end

  @impl true
  def handle_info({:hypatia_event, [:hypatia, :outcome, :recorded], _meas, metadata, _ts}, state) do
    outcome = Map.get(metadata, :outcome) || Map.get(metadata, "outcome") || "unknown"
    success? = outcome == "success"

    outcomes = [success? | state.outcomes] |> Enum.take(@window_size)

    {:noreply, %{state | outcomes: outcomes}}
  end

  def handle_info({:hypatia_event, _event, _meas, _meta, _ts}, state) do
    {:noreply, state}
  end

  def handle_info(:tick, state) do
    state = evaluate(state)
    Process.send_after(self(), :tick, @tick_interval_ms)
    {:noreply, state}
  end

  @impl true
  def handle_call(:tick_now, _from, state) do
    state = evaluate(state)
    {:reply, :ok, state}
  end

  def handle_call(:window, _from, state) do
    {:reply, state.outcomes, state}
  end

  # ─── Evaluation ────────────────────────────────────────────────────────

  defp evaluate(state) do
    state
    |> evaluate_statistical()
    |> evaluate_esn_drift()
  end

  defp evaluate_statistical(state) do
    outcomes = state.outcomes

    cond do
      length(outcomes) < @min_outcomes_for_alert ->
        state

      length(outcomes) < @recent_size + 5 ->
        # Not enough older history to form a meaningful baseline.
        state

      true ->
        do_evaluate(state, outcomes)
    end
  end

  # M15c — ESN tight integration. The statistical path uses ESN drift
  # as a corroborating signal that bumps severity. This second path
  # lets ESN drift be an INDEPENDENT alert source: if the ESN is
  # trained and reporting sustained directional change, we surface
  # that even if the rolling-baseline gate hasn't tripped — the
  # neural layer may catch a trend earlier than a 2σ statistical
  # threshold can.
  defp evaluate_esn_drift(state) do
    case Process.whereis(Hypatia.Neural.Coordinator) do
      nil ->
        state

      _ ->
        try do
          report = Hypatia.Neural.Coordinator.health_report()
          drift = get_in(report, [:esn, :drift])
          trained? = get_in(report, [:esn, :trained])
          accuracy = get_in(report, [:esn, :accuracy])

          cond do
            not trained? ->
              state

            drift in [:rising_drift, :falling_drift] ->
              key = :"esn_drift_#{drift}"
              now = System.system_time(:millisecond)
              last = Map.get(state.last_alert_at, key, 0)

              if now - last >= @dedup_window_ms do
                Hypatia.Telemetry.anomaly_detected(
                  measurements: %{
                    recent_rate: nil,
                    baseline_rate: nil,
                    sigma_distance: 0.0
                  },
                  metadata: %{
                    kind: key,
                    esn_drift_concurs: true,
                    esn_state: drift,
                    esn_accuracy: jsonable_accuracy(accuracy)
                  }
                )

                %{state | last_alert_at: Map.put(state.last_alert_at, key, now)}
              else
                state
              end

            true ->
              state
          end
        rescue
          _ -> state
        catch
          _, _ -> state
        end
    end
  end

  # The accuracy report from EchoStateNetwork is a map of floats
  # (or :insufficient_data); make sure the metadata stays JSON-safe
  # for the alerts pipeline.
  defp jsonable_accuracy(:insufficient_data), do: "insufficient_data"
  defp jsonable_accuracy(%{} = m), do: m
  defp jsonable_accuracy(other), do: inspect(other)

  defp do_evaluate(state, outcomes) do
    {recent, baseline} = Enum.split(outcomes, @recent_size)

    recent_count = length(recent)
    baseline_count = length(baseline)
    recent_rate = success_rate(recent)
    baseline_rate = success_rate(baseline)

    # Binomial-proportion standard error of the baseline. We compare
    # the recent rate against the baseline, treating the baseline as
    # the "true" distribution.
    sigma =
      :math.sqrt(
        max(baseline_rate * (1 - baseline_rate) / max(baseline_count, 1), 0.0001)
      )

    sigma_distance = (baseline_rate - recent_rate) / sigma

    if sigma_distance > @sigma_threshold do
      key = :success_rate_drop
      now = System.system_time(:millisecond)
      last = Map.get(state.last_alert_at, key, 0)

      if now - last >= @dedup_window_ms do
        {esn_state, drift_concurs} = esn_signal()

        Hypatia.Telemetry.anomaly_detected(
          measurements: %{
            recent_rate: recent_rate,
            baseline_rate: baseline_rate,
            sigma_distance: sigma_distance
          },
          metadata: %{
            kind: key,
            recent_count: recent_count,
            baseline_count: baseline_count,
            esn_drift_concurs: drift_concurs,
            esn_state: esn_state
          }
        )

        %{state | last_alert_at: Map.put(state.last_alert_at, key, now)}
      else
        state
      end
    else
      state
    end
  end

  defp success_rate([]), do: 0.0

  defp success_rate(list) do
    successes = Enum.count(list, & &1)
    successes / length(list)
  end

  # ─── ESN signal (optional, soft) ───────────────────────────────────────

  defp esn_signal do
    case Process.whereis(Hypatia.Neural.Coordinator) do
      nil ->
        {:unavailable, false}

      _ ->
        try do
          report = Hypatia.Neural.Coordinator.health_report()
          state = get_in(report, [:esn, :drift]) || :unavailable
          {state, state in [:rising_drift, :falling_drift]}
        rescue
          _ -> {:unavailable, false}
        catch
          _, _ -> {:unavailable, false}
        end
    end
  end
end
