# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Web.Metrics do
  @moduledoc """
  Prometheus text-format `/metrics` exposition.

  Hand-rolled rather than via `:telemetry_metrics_prometheus` so we
  don't add a runtime dependency. The text format is simple and
  stable — `# HELP` + `# TYPE` + value rows.

  Surface:

      hypatia_events_total{event="hypatia.scan.complete", window="5m"}     N
      hypatia_events_total{event="hypatia.dispatch.decision", window="1h"} N
      ...
      hypatia_queue_depth{process="Hypatia.Watcher"}                       N
      hypatia_watcher_dropped_total                                        N
      hypatia_watcher_uptime_seconds                                       N
      hypatia_recipe_verification_rate{recipe="recipe-foo"}                0.93
      hypatia_recipe_dispatches_total{recipe="recipe-foo"}                 1234
      hypatia_recipe_quarantine_candidates                                 N
      hypatia_recipe_degraded                                              N

  Bound publicly (NOT loopback-only) because Prometheus scrapers
  routinely run on a different host than the app — there's no
  operational data in the metric body that isn't already implied by
  the dashboard. If this assumption changes, move under /api.

  ## Prometheus scrape config

      scrape_configs:
        - job_name: hypatia
          metrics_path: /metrics
          static_configs:
            - targets: ['hypatia.internal:9090']
  """

  import Plug.Conn

  def call(conn, _opts) do
    body = render()

    conn
    |> put_resp_content_type("text/plain; version=0.0.4; charset=utf-8")
    |> send_resp(200, body)
  end

  def init(opts), do: opts

  @doc """
  Render the Prometheus text-format exposition. Public so the unit
  test can assert against the body without going through Plug.
  """
  def render do
    [
      render_events_total(),
      render_queue_depths(),
      render_watcher_meta(),
      render_recipe_health()
    ]
    |> Enum.join("\n")
  end

  # ─── Event counters per window ─────────────────────────────────────────

  defp render_events_total do
    lines =
      for window <- [:m5, :h1, :d1],
          {event, count} <- safe_counts(window) do
        ~s|hypatia_events_total{event="#{Enum.join(event, ".")}",window="#{window_label(window)}"} #{count}|
      end

    [
      "# HELP hypatia_events_total Telemetry events counted in the named rolling window",
      "# TYPE hypatia_events_total gauge"
      | lines
    ]
    |> Enum.join("\n")
  end

  defp window_label(:m5), do: "5m"
  defp window_label(:h1), do: "1h"
  defp window_label(:d1), do: "1d"

  defp safe_counts(window) do
    Hypatia.Watcher.counts(window)
  rescue
    _ -> %{}
  catch
    _, _ -> %{}
  end

  # ─── GenServer queue depths ────────────────────────────────────────────

  defp render_queue_depths do
    depths = safe_queue_depths()

    lines =
      for {name, depth} <- depths, is_integer(depth) do
        # Strip module-inspect formatting (e.g. "Elixir.Hypatia.Watcher"
        # → "Hypatia.Watcher") so Prometheus labels are tidy.
        clean = name |> to_string() |> String.replace_leading("Elixir.", "")
        ~s|hypatia_queue_depth{process="#{clean}"} #{depth}|
      end

    [
      "# HELP hypatia_queue_depth GenServer mailbox depth for supervised processes",
      "# TYPE hypatia_queue_depth gauge"
      | lines
    ]
    |> Enum.join("\n")
  end

  defp safe_queue_depths do
    Hypatia.Watcher.queue_depths()
  rescue
    _ -> %{}
  catch
    _, _ -> %{}
  end

  # ─── Watcher meta (dropped events, uptime) ─────────────────────────────

  defp render_watcher_meta do
    snap = safe_snapshot()
    dropped = Map.get(snap, :dropped_events, 0)
    uptime = Map.get(snap, :uptime_seconds, 0)

    """
    # HELP hypatia_watcher_dropped_total Telemetry events dropped under back-pressure
    # TYPE hypatia_watcher_dropped_total counter
    hypatia_watcher_dropped_total #{dropped}
    # HELP hypatia_watcher_uptime_seconds Seconds since the Watcher started
    # TYPE hypatia_watcher_uptime_seconds gauge
    hypatia_watcher_uptime_seconds #{uptime}
    """
    |> String.trim_trailing()
  end

  defp safe_snapshot do
    Hypatia.Watcher.snapshot()
  rescue
    _ -> %{}
  catch
    _, _ -> %{}
  end

  # ─── Recipe health ─────────────────────────────────────────────────────

  defp render_recipe_health do
    rows = safe_recipe_health()

    rate_lines =
      for r <- rows, is_float(r.verification.rate) do
        ~s|hypatia_recipe_verification_rate{recipe="#{escape(r.recipe_id)}"} #{r.verification.rate}|
      end

    dispatch_lines =
      for r <- rows do
        ~s|hypatia_recipe_dispatches_total{recipe="#{escape(r.recipe_id)}"} #{r.dispatches}|
      end

    qc = Enum.count(rows, &(&1.status == :quarantine_candidate))
    deg = Enum.count(rows, &(&1.status == :degraded))

    sections = [
      [
        "# HELP hypatia_recipe_verification_rate Fraction of recipe successes confirmed clean by post-fix re-scan",
        "# TYPE hypatia_recipe_verification_rate gauge"
        | rate_lines
      ],
      [
        "# HELP hypatia_recipe_dispatches_total Total dispatch attempts recorded for the recipe",
        "# TYPE hypatia_recipe_dispatches_total counter"
        | dispatch_lines
      ],
      [
        "# HELP hypatia_recipe_quarantine_candidates Recipes auto-quarantined on verification-rate gate",
        "# TYPE hypatia_recipe_quarantine_candidates gauge",
        "hypatia_recipe_quarantine_candidates #{qc}",
        "# HELP hypatia_recipe_degraded Recipes below the degraded threshold (but above quarantine)",
        "# TYPE hypatia_recipe_degraded gauge",
        "hypatia_recipe_degraded #{deg}"
      ]
    ]

    sections
    |> Enum.map(&Enum.join(&1, "\n"))
    |> Enum.join("\n")
  end

  defp safe_recipe_health do
    Hypatia.OutcomeTracker.recipe_health()
  rescue
    _ -> []
  catch
    _, _ -> []
  end

  # Prometheus label values must escape backslash, double-quote, and
  # newline per the exposition spec.
  defp escape(value) when is_binary(value) do
    value
    |> String.replace("\\", "\\\\")
    |> String.replace("\"", "\\\"")
    |> String.replace("\n", "\\n")
  end

  defp escape(value), do: value |> to_string() |> escape()
end
