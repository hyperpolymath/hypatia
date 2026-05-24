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

defmodule Hypatia.Watcher.Alerts.Sinks.Peer do
  @moduledoc """
  Cross-host alert federation sink. On if `HYPATIA_FEDERATION_PEERS`
  is set (comma-separated URLs of other Hypatia instances).

  POSTs each local alert to every peer's `/api/alerts/ingest`
  endpoint. The receiving instance's ApiRouter injects the alert
  into ITS local Alerts ring buffer with a `federated_from` tag,
  so the receiving dashboard shows alerts from all federated peers
  in one view.

  Loop prevention: alerts whose metadata already carries
  `federated_from` are SKIPPED here. A federated alert that came
  from peer A is not re-broadcast from this instance to peer B,
  or it would ping-pong indefinitely. The same tag is also how
  the receiving side knows not to re-federate.

  Auth: peers MUST share `HYPATIA_API_BEARER_TOKEN`. The sink
  attaches `Authorization: Bearer <token>` to every POST. Without
  a shared token, federation is disabled — there's no point
  having peers reach each other if the receiving end's gate
  rejects them.

  Timeout 5s per peer — federation must not slow down local
  alerting.
  """

  @behaviour Hypatia.Watcher.Alerts.Sinks

  require Logger

  @impl true
  def handle_alert(alert) do
    peers_env = System.get_env("HYPATIA_FEDERATION_PEERS")
    token = System.get_env("HYPATIA_API_BEARER_TOKEN")

    cond do
      peers_env in [nil, ""] ->
        :ok

      token in [nil, ""] ->
        Logger.warning(
          "Peer sink: HYPATIA_FEDERATION_PEERS set but HYPATIA_API_BEARER_TOKEN " <>
            "is empty — federation requires a shared bearer token. Skipping."
        )

        :ok

      federated?(alert) ->
        # Loop prevention: incoming federated alert MUST NOT
        # be re-federated. Each instance broadcasts only its OWN alerts.
        :ok

      true ->
        peers = peers_env |> String.split(",", trim: true)

        payload =
          Jason.encode!(%{
            rule: alert.rule,
            severity: alert.severity,
            summary: alert.summary,
            metadata: alert.metadata,
            at: alert.at
          })

        Enum.each(peers, fn peer -> post_to_peer(peer, payload, token) end)

        :ok
    end
  end

  defp federated?(alert) do
    case alert.metadata do
      %{federated_from: _} -> true
      %{"federated_from" => _} -> true
      _ -> false
    end
  end

  defp post_to_peer(peer, payload, token) do
    url = peer |> String.trim() |> String.trim_trailing("/")
    target = "#{url}/api/alerts/ingest"

    args = [
      "-sS",
      "--max-time",
      "5",
      "-X",
      "POST",
      "-H",
      "Content-Type: application/json",
      "-H",
      "Authorization: Bearer #{token}",
      "-d",
      payload,
      target
    ]

    case System.cmd("curl", args, stderr_to_stdout: true) do
      {_body, 0} ->
        :ok

      {error, _code} ->
        Logger.warning("Peer sink: POST to #{target} failed: #{String.slice(error, 0, 200)}")
        :ok
    end
  end
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
