# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Web.ApiRouter do
  @moduledoc """
  Operational HTTP API. Forwarded from `Hypatia.Web.Router` at `/api`.

  All endpoints are loopback-only by default (operational data must
  not leak past the local machine). Set `HYPATIA_API_ALLOW_NONLOCAL=true`
  to bypass — the bypass is logged on each request so audit captures it.

  Endpoints:
    GET /status              full Watcher snapshot
    GET /counts/:window      event counts in window (5m | 1h | 1d)
    GET /recipes             recipe-health rows (?status=...)
  """

  use Plug.Router

  require Logger

  plug :match
  plug :loopback_only
  plug :dispatch

  get "/status" do
    snap = Hypatia.Watcher.snapshot()
    json(conn, 200, normalize_snapshot(snap))
  end

  get "/counts/:window" do
    case parse_window(window) do
      {:ok, atom} ->
        counts = Hypatia.Watcher.counts(atom)
        json(conn, 200, %{window: window, counts: flatten_event_keys(counts)})

      :error ->
        json(conn, 400, %{
          error: "unknown_window",
          got: window,
          valid: ["5m", "1h", "1d"]
        })
    end
  end

  get "/recipes" do
    conn = Plug.Conn.fetch_query_params(conn)
    rows = Hypatia.OutcomeTracker.recipe_health()

    case Map.get(conn.query_params, "status") do
      nil ->
        json(conn, 200, %{count: length(rows), rows: rows})

      statuses ->
        try do
          allowed = statuses |> String.split(",") |> Enum.map(&String.to_existing_atom/1)
          filtered = Enum.filter(rows, &(&1.status in allowed))
          json(conn, 200, %{count: length(filtered), rows: filtered})
        rescue
          ArgumentError -> json(conn, 400, %{error: "unknown_status_filter"})
        end
    end
  end

  match _ do
    json(conn, 404, %{error: "not_found"})
  end

  # ─── Plug ──────────────────────────────────────────────────────────────

  defp loopback_only(conn, _opts) do
    cond do
      System.get_env("HYPATIA_API_ALLOW_NONLOCAL") == "true" ->
        Logger.warning(
          "Hypatia /api access from #{inspect(conn.remote_ip)} allowed by " <>
            "HYPATIA_API_ALLOW_NONLOCAL env override"
        )

        conn

      loopback_ip?(conn.remote_ip) ->
        conn

      true ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(
          403,
          Jason.encode!(%{
            error: "loopback_only",
            path: conn.request_path,
            hint:
              "Hypatia /api is loopback-only. Set HYPATIA_API_ALLOW_NONLOCAL=true to " <>
                "permit non-local clients, or tunnel via SSH."
          })
        )
        |> halt()
    end
  end

  defp loopback_ip?({127, _, _, _}), do: true
  defp loopback_ip?({0, 0, 0, 0, 0, 0, 0, 1}), do: true
  defp loopback_ip?(_), do: false

  # ─── Helpers ───────────────────────────────────────────────────────────

  defp json(conn, status, body) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(status, Jason.encode!(body))
  end

  defp parse_window("5m"), do: {:ok, :m5}
  defp parse_window("1h"), do: {:ok, :h1}
  defp parse_window("1d"), do: {:ok, :d1}
  defp parse_window(_), do: :error

  defp flatten_event_keys(counts_map) do
    Map.new(counts_map, fn {k, v} -> {Enum.join(k, "."), v} end)
  end

  defp normalize_snapshot(snap) do
    %{
      counts: %{
        m5: flatten_event_keys(snap.counts.m5),
        h1: flatten_event_keys(snap.counts.h1),
        d1: flatten_event_keys(snap.counts.d1)
      },
      queue_depths: snap.queue_depths,
      dropped_events: snap.dropped_events,
      uptime_seconds: snap.uptime_seconds,
      generated_at: snap.generated_at,
      recent_by_kind:
        Map.new(snap.recent_by_kind, fn {event, entries} ->
          {Enum.join(event, "."),
           Enum.map(entries, fn entry -> %{entry | event: Enum.join(entry.event, ".")} end)}
        end)
    }
  end
end
