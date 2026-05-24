# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Watcher.Alerts do
  @moduledoc """
  Alert evaluator + fan-out.

  Subscribes to live telemetry events from `Hypatia.Watcher` and, on
  a periodic tick, samples the watcher's snapshot to evaluate
  threshold-based rules. Matching alerts are dispatched to every
  enabled sink (Log, Webhook, File).

  ## Rules (Phase 3 baseline set)

    * `quarantine_triggered` -- any `hypatia.quarantine.triggered`
      event fires an immediate alert. Deduped per `{kind, id}` within
      the `@dedup_window_ms`.
    * `soundness_violation` -- any `hypatia.soundness.violation` event
      fires immediately. No dedup (every regression must be visible).
    * `queue_depth_high` -- on each tick, any supervised GenServer
      whose message_queue_len > `@queue_threshold` fires.
      Deduped per process within `@dedup_window_ms`.
    * `events_dropped` -- if the watcher's `dropped_events` counter
      has incremented since the last tick, fire. Tracks the delta,
      not the absolute value, so a single alert isn't suppressed by
      historical drops.

  ## Sinks

  Sinks are picked up from config / env. Each sink implements
  `handle_alert/1`. Built-in sinks:

    * Log     -- always on. Structured `Logger.warning/1` line.
    * Webhook -- on if `HYPATIA_ALERT_WEBHOOK_URL` is set. HTTP POST
                 of a Slack-compatible JSON payload. Failures are
                 swallowed (alerting must never crash the host).
    * File    -- on if `HYPATIA_ALERT_LOG_FILE` is set. Append-only
                 JSONL for historical replay.

  ## Lifecycle

  Supervised by `Hypatia.Application`. Subscribes to the watcher's
  Registry pub/sub on init; the subscription is dropped automatically
  if this process dies.
  """

  use GenServer

  require Logger

  @tick_interval_ms 30_000
  @dedup_window_ms 5 * 60 * 1000
  @queue_threshold 100

  # ─── Public ────────────────────────────────────────────────────────────

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Force an immediate threshold evaluation. Mostly useful in tests.
  """
  def tick_now do
    GenServer.call(__MODULE__, :tick_now, 5_000)
  end

  @doc """
  Most-recent alerts emitted (ring buffer, newest first). Backs the
  /api/alerts endpoint and the dashboard's alert ribbon.
  """
  def recent do
    GenServer.call(__MODULE__, :recent, 5_000)
  catch
    :exit, _ -> []
  end

  # ─── GenServer ─────────────────────────────────────────────────────────

  @impl true
  def init(_opts) do
    # Subscribe to live telemetry only on the kinds we actually act on.
    # Reduces wakeups vs. all-events subscription.
    Hypatia.Watcher.subscribe(
      events: [
        [:hypatia, :quarantine, :triggered],
        [:hypatia, :soundness, :violation]
      ]
    )

    Process.send_after(self(), :tick, @tick_interval_ms)

    state = %{
      recent: [],
      dedup: %{},
      last_dropped: 0
    }

    {:ok, state}
  end

  @impl true
  def handle_info({:hypatia_event, [:hypatia, :quarantine, :triggered], _meas, metadata, ts},
        state) do
    kind = metadata[:kind] || metadata["kind"]
    id = metadata[:id] || metadata["id"]
    key = "quarantine:#{inspect(kind)}:#{inspect(id)}"

    alert = %{
      rule: :quarantine_triggered,
      severity: :high,
      summary: "Recipe/bot auto-quarantined: #{kind}/#{id}",
      metadata: metadata,
      at: ts || System.system_time(:millisecond)
    }

    {:noreply, maybe_emit(state, key, alert)}
  end

  def handle_info({:hypatia_event, [:hypatia, :soundness, :violation], _meas, metadata, ts},
        state) do
    rule_module = metadata[:rule_module] || metadata["rule_module"]
    rule_id = metadata[:rule_id] || metadata["rule_id"]

    alert = %{
      rule: :soundness_violation,
      severity: :critical,
      summary: "Soundness gate violation: #{rule_module}/#{rule_id}",
      metadata: metadata,
      at: ts || System.system_time(:millisecond)
    }

    # No dedup for soundness — every regression must be visible.
    {:noreply, emit(state, alert)}
  end

  # Ignore other event kinds we may receive (e.g. via :all subscriptions
  # in tests).
  def handle_info({:hypatia_event, _event, _meas, _meta, _ts}, state) do
    {:noreply, state}
  end

  def handle_info(:tick, state) do
    state = run_threshold_rules(state)
    Process.send_after(self(), :tick, @tick_interval_ms)
    {:noreply, state}
  end

  @impl true
  def handle_call(:tick_now, _from, state) do
    state = run_threshold_rules(state)
    {:reply, :ok, state}
  end

  def handle_call(:recent, _from, state) do
    {:reply, state.recent, state}
  end

  # ─── Threshold evaluation (tick) ───────────────────────────────────────

  defp run_threshold_rules(state) do
    snap = safe_snapshot()

    state
    |> check_queue_depths(snap)
    |> check_dropped_events(snap)
  end

  defp check_queue_depths(state, snap) do
    Enum.reduce(snap[:queue_depths] || %{}, state, fn
      {process, depth}, acc when is_integer(depth) and depth > @queue_threshold ->
        key = "queue_depth:#{process}"

        alert = %{
          rule: :queue_depth_high,
          severity: :high,
          summary: "GenServer queue depth #{depth} > #{@queue_threshold} for #{process}",
          metadata: %{process: process, depth: depth, threshold: @queue_threshold},
          at: System.system_time(:millisecond)
        }

        maybe_emit(acc, key, alert)

      _, acc ->
        acc
    end)
  end

  defp check_dropped_events(state, snap) do
    current = Map.get(snap, :dropped_events, 0)

    if current > state.last_dropped do
      delta = current - state.last_dropped

      alert = %{
        rule: :events_dropped,
        severity: :medium,
        summary: "Watcher dropped #{delta} telemetry event(s) under back-pressure",
        metadata: %{delta: delta, total: current},
        at: System.system_time(:millisecond)
      }

      %{emit(state, alert) | last_dropped: current}
    else
      %{state | last_dropped: current}
    end
  end

  defp safe_snapshot do
    case Hypatia.Watcher.snapshot() do
      %{status: :unavailable} -> %{}
      snap when is_map(snap) -> snap
      _ -> %{}
    end
  rescue
    _ -> %{}
  catch
    _, _ -> %{}
  end

  # ─── Emit + dedup ──────────────────────────────────────────────────────

  defp maybe_emit(state, dedup_key, alert) do
    now = System.system_time(:millisecond)
    last = Map.get(state.dedup, dedup_key, 0)

    if now - last < @dedup_window_ms do
      state
    else
      state = %{state | dedup: Map.put(state.dedup, dedup_key, now)}
      emit(state, alert)
    end
  end

  defp emit(state, alert) do
    Enum.each(sinks(), fn sink ->
      try do
        sink.handle_alert(alert)
      rescue
        e -> Logger.error("Alert sink #{inspect(sink)} crashed: #{Exception.message(e)}")
      catch
        kind, reason ->
          Logger.error(
            "Alert sink #{inspect(sink)} threw #{inspect(kind)}: #{inspect(reason)}"
          )
      end
    end)

    %{state | recent: [alert | state.recent] |> Enum.take(100)}
  end

  defp sinks do
    base = [Hypatia.Watcher.Alerts.Sinks.Log]

    base =
      if System.get_env("HYPATIA_ALERT_WEBHOOK_URL") not in [nil, ""],
        do: [Hypatia.Watcher.Alerts.Sinks.Webhook | base],
        else: base

    base =
      if System.get_env("HYPATIA_ALERT_LOG_FILE") not in [nil, ""],
        do: [Hypatia.Watcher.Alerts.Sinks.File | base],
        else: base

    base
  end
end
