# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Watcher.Alerts.Sinks do
  @moduledoc """
  Behavior + built-in implementations for alert sinks.

  An alert sink's only contract is `handle_alert/1`. The Alerts
  GenServer wraps each invocation in rescue/catch so a broken sink
  can never take down alerting itself.

  Alert shape:

      %{
        rule: atom,           # :quarantine_triggered, :soundness_violation, ...
        severity: atom,       # :critical | :high | :medium | :low
        summary: String.t,    # one-line human-readable
        metadata: map,        # arbitrary structured context
        at: integer           # unix epoch ms
      }
  """

  @callback handle_alert(alert :: map) :: :ok
end

defmodule Hypatia.Watcher.Alerts.Sinks.Log do
  @moduledoc """
  Default sink: structured Logger.warning/1 line. Always enabled.

  Format: a single line with the rule, severity, summary, and the
  metadata inspect'd compactly so log aggregators (loki, journald,
  etc.) can parse the fixed prefix and still see the context.
  """

  @behaviour Hypatia.Watcher.Alerts.Sinks

  require Logger

  @impl true
  def handle_alert(%{rule: rule, severity: severity, summary: summary, metadata: metadata}) do
    Logger.warning(
      "[hypatia-alert] severity=#{severity} rule=#{rule} #{summary} " <>
        "meta=#{inspect(metadata, limit: :infinity)}"
    )

    :ok
  end
end

defmodule Hypatia.Watcher.Alerts.Sinks.File do
  @moduledoc """
  Append-only JSONL sink. Path comes from `HYPATIA_ALERT_LOG_FILE`.

  Each alert becomes one JSON line, ISO-8601 timestamped, suitable for
  rotation by logrotate / cron. Failure to write is logged but never
  raises — alerting must never crash the host.
  """

  @behaviour Hypatia.Watcher.Alerts.Sinks

  require Logger

  @impl true
  def handle_alert(alert) do
    path = System.get_env("HYPATIA_ALERT_LOG_FILE")

    if path do
      line =
        Jason.encode!(%{
          rule: alert.rule,
          severity: alert.severity,
          summary: alert.summary,
          metadata: jsonable(alert.metadata),
          at: alert.at,
          iso: alert.at |> DateTime.from_unix!(:millisecond) |> DateTime.to_iso8601()
        })

      case File.write(path, line <> "\n", [:append, :utf8]) do
        :ok ->
          :ok

        {:error, reason} ->
          Logger.error("Alert file sink failed (#{path}): #{inspect(reason)}")
          :ok
      end
    end

    :ok
  end

  defp jsonable(v) when is_binary(v) or is_number(v) or is_boolean(v) or is_atom(v) or is_nil(v),
    do: v

  defp jsonable(v) when is_list(v), do: Enum.map(v, &jsonable/1)
  defp jsonable(v) when is_map(v), do: Map.new(v, fn {k, val} -> {k, jsonable(val)} end)
  defp jsonable(v), do: inspect(v)
end

defmodule Hypatia.Watcher.Alerts.Sinks.Webhook do
  @moduledoc """
  HTTP POST sink. URL comes from `HYPATIA_ALERT_WEBHOOK_URL`.

  Payload is Slack-compatible (`{"text": ...}` top-level) so the URL
  can be a Slack incoming webhook directly. Additional `attachments`
  field carries the full structured alert for Slack's attachment
  rendering or generic webhook consumers.

  Uses `curl` shell-out (same pattern as the existing GitHub-API
  callers) rather than a new HTTP-client dep. Timeout 5s — alerting
  must not block the host on a hung webhook.
  """

  @behaviour Hypatia.Watcher.Alerts.Sinks

  require Logger

  @impl true
  def handle_alert(alert) do
    url = System.get_env("HYPATIA_ALERT_WEBHOOK_URL")

    if url do
      payload =
        Jason.encode!(%{
          text: "*[#{alert.severity}]* #{alert.summary} (#{alert.rule})",
          attachments: [
            %{
              color: color_for(alert.severity),
              fallback: alert.summary,
              fields: [
                %{title: "rule", value: to_string(alert.rule), short: true},
                %{title: "severity", value: to_string(alert.severity), short: true},
                %{
                  title: "metadata",
                  value: "```" <> inspect(alert.metadata, pretty: true) <> "```",
                  short: false
                }
              ],
              ts: div(alert.at, 1000)
            }
          ]
        })

      case System.cmd(
             "curl",
             [
               "-sS",
               "--max-time",
               "5",
               "-X",
               "POST",
               "-H",
               "Content-Type: application/json",
               "-d",
               payload,
               url
             ],
             stderr_to_stdout: true
           ) do
        {_body, 0} ->
          :ok

        {error, _code} ->
          Logger.error("Alert webhook POST failed: #{String.slice(error, 0, 200)}")
          :ok
      end
    end

    :ok
  end

  defp color_for(:critical), do: "danger"
  defp color_for(:high), do: "danger"
  defp color_for(:medium), do: "warning"
  defp color_for(_), do: "good"
end
