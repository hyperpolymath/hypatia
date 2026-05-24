# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Telemetry do
  @moduledoc """
  Centralised event-name registry for Hypatia's telemetry surface.

  Every observable decision in the pipeline emits a `:telemetry` event
  through one of the helpers in this module. Centralising event names
  here (rather than spreading magic atom lists across call sites) means
  the watcher / Prometheus exporter / future alerting layer can
  enumerate the full surface from a single source.

  All events follow the convention `[:hypatia, <area>, <verb>]`. The
  measurements map carries numeric values (counts, durations);
  metadata carries categorical context (recipe_id, repo, severity).

  Calling `:telemetry.execute/3` is safe with no handlers attached —
  it's a no-op, so instrumenting a code path costs nothing when the
  watcher isn't running (e.g. inside the escript scanner).

  ## Event catalogue

  | event                                | measurements              | metadata                                        |
  |--------------------------------------|---------------------------|-------------------------------------------------|
  | `[:hypatia, :scan, :complete]`       | `duration_ms, findings`   | `path, severity_floor`                          |
  | `[:hypatia, :dispatch, :decision]`   | `confidence`              | `strategy, tier, recipe_id, repo`               |
  | `[:hypatia, :outcome, :recorded]`    | `count`                   | `recipe_id, repo, outcome, verification`        |
  | `[:hypatia, :verification, :result]` | `count`                   | `recipe_id, repo, verdict`                      |
  | `[:hypatia, :quarantine, :triggered]`| `count`                   | `kind, id, reason, level`                       |
  | `[:hypatia, :rate_limit, :exceeded]` | `count`                   | `bot, scope`                                    |
  | `[:hypatia, :neural, :cycle]`        | `duration_ms`             | `networks_updated`                              |
  | `[:hypatia, :soundness, :violation]` | `count`                   | `rule_module, rule_id, fixture`                 |

  ## Subscribers

  Subscribe by attaching a handler with `:telemetry.attach_many/4`:

      :telemetry.attach_many(
        "my-handler",
        Hypatia.Telemetry.all_events(),
        fn event, measurements, metadata, _config ->
          # handle event
        end,
        nil
      )

  The watcher (`Hypatia.Watcher`) does this on startup and aggregates
  into rolling-window ETS tables.
  """

  @scan_complete [:hypatia, :scan, :complete]
  @dispatch_decision [:hypatia, :dispatch, :decision]
  @outcome_recorded [:hypatia, :outcome, :recorded]
  @verification_result [:hypatia, :verification, :result]
  @quarantine_triggered [:hypatia, :quarantine, :triggered]
  @rate_limit_exceeded [:hypatia, :rate_limit, :exceeded]
  @neural_cycle [:hypatia, :neural, :cycle]
  @soundness_violation [:hypatia, :soundness, :violation]
  @anomaly_detected [:hypatia, :anomaly, :detected]

  @all_events [
    @scan_complete,
    @dispatch_decision,
    @outcome_recorded,
    @verification_result,
    @quarantine_triggered,
    @rate_limit_exceeded,
    @neural_cycle,
    @soundness_violation,
    @anomaly_detected
  ]

  @doc "Every event the watcher should subscribe to."
  def all_events, do: @all_events

  # ─── Emit helpers ──────────────────────────────────────────────────────
  #
  # Hand-written rather than meta-programmed so each emit site shows
  # what it's saying. Each helper takes the metadata fields as a
  # keyword list to keep call sites self-documenting.

  def scan_complete(duration_ms, findings, metadata) when is_integer(duration_ms) do
    safe_execute(@scan_complete, %{duration_ms: duration_ms, findings: findings}, Map.new(metadata))
  end

  def dispatch_decision(confidence, metadata) when is_number(confidence) do
    safe_execute(@dispatch_decision, %{confidence: confidence}, Map.new(metadata))
  end

  def outcome_recorded(metadata) do
    safe_execute(@outcome_recorded, %{count: 1}, Map.new(metadata))
  end

  def verification_result(metadata) do
    safe_execute(@verification_result, %{count: 1}, Map.new(metadata))
  end

  def quarantine_triggered(metadata) do
    safe_execute(@quarantine_triggered, %{count: 1}, Map.new(metadata))
  end

  def rate_limit_exceeded(metadata) do
    safe_execute(@rate_limit_exceeded, %{count: 1}, Map.new(metadata))
  end

  def neural_cycle(duration_ms, metadata) when is_integer(duration_ms) do
    safe_execute(@neural_cycle, %{duration_ms: duration_ms}, Map.new(metadata))
  end

  def soundness_violation(metadata) do
    safe_execute(@soundness_violation, %{count: 1}, Map.new(metadata))
  end

  @doc """
  Emitted by `Hypatia.Watcher.AnomalyDetector` when the recent
  outcome stream diverges from its baseline. `measurements:` carries
  numeric context (rates, sigma); `metadata:` carries categorical
  context (kind, esn corroboration).
  """
  def anomaly_detected(opts) do
    measurements = Keyword.fetch!(opts, :measurements)
    metadata = Keyword.fetch!(opts, :metadata) |> Map.new()
    safe_execute(@anomaly_detected, Map.new(measurements), metadata)
  end

  # `:telemetry` is a transitive dep of phoenix/bandit, but if Hypatia
  # is consumed in an unusual build (escript-only, stripped releases)
  # the module may not be loaded. Wrap the call so a missing
  # `:telemetry` is a no-op rather than a crash. Instrumentation must
  # never break the host.
  defp safe_execute(event, measurements, metadata) do
    if Code.ensure_loaded?(:telemetry) do
      :telemetry.execute(event, measurements, metadata)
    end

    :ok
  end
end
