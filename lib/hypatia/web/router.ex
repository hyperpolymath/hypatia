# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Web.Router do
  @moduledoc """
  HTTP router for Hypatia's public endpoints.

  Serves well-known service discovery manifests and health checks.
  Listens on port 9090 via Bandit, supervised by the OTP application.

  Public:
    GET /health              liveness probe (no auth, no IP filter)
    GET /.well-known/groove  service discovery (via GroovePlug)

  Loopback-only (operational):
    GET /api/status          live Watcher snapshot
    GET /api/counts/:window  event counts in window
    GET /api/recipes         recipe-health roll-up
  """

  use Plug.Router

  # GroovePlug handles /.well-known/groove/* before the router's own
  # match/dispatch cycle.  It halts on groove paths, so the router
  # never sees them.
  plug Hypatia.Web.GroovePlug

  plug :match
  plug :dispatch

  @doc """
  GET / -- Single-page live operational dashboard. HTML + vanilla JS,
  polls /api/status and EventSource-streams /api/events. The dashboard
  itself is publicly reachable; the data endpoints it calls are
  loopback-only (gated in ApiRouter), so a non-local browser would
  render the chrome but get 403 from the XHR/SSE calls.
  """
  get "/" do
    Hypatia.Web.Dashboard.call(conn, [])
  end

  @doc """
  GET /health -- Basic health check for the HTTP endpoint.
  """
  get "/health" do
    health = %{
      status: "ok",
      service: "hypatia",
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
    }

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Jason.encode!(health))
  end

  @doc """
  GET /metrics -- Prometheus text-format exposition. Publicly
  reachable (NOT loopback-only) because scrapers routinely run on a
  different host; there's no operational data in the metric body
  that isn't already implied by the dashboard's existence.
  """
  get "/metrics" do
    Hypatia.Web.Metrics.call(conn, [])
  end

  @doc """
  GET /metrics/snapshot -- Compact JSON snapshot of estate-level
  counters: repos scanned, weak points, dispatched actions, outcomes,
  recipes, average confidence. Consumed by the optional Ada TUI
  (`lib/tui/port.ex`) on its 10s tick, and useful as a single-call
  status read for external dashboards.

  Reads from the verisim-data flat-file store via VerisimConnector;
  any failure returns a degraded snapshot with status="degraded"
  rather than 500, so the TUI keeps rendering.
  """
  get "/metrics/snapshot" do
    snapshot = Hypatia.Web.MetricsSnapshot.build()

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Jason.encode!(snapshot))
  end

  # /api/* is gated to loopback in Hypatia.Web.ApiRouter — keeps
  # operational data off the public surface while leaving /health
  # reachable for container orchestrators.
  forward "/api", to: Hypatia.Web.ApiRouter

  @doc """
  POST /graphql -- GraphQL-shaped query endpoint (M14).

  Minimal hand-rolled implementation; no introspection, no schema
  federation, no Absinthe dep. See lib/hypatia/web/graphql.ex for
  the supported field set and limitations. Loopback-only by sharing
  the bearer-auth gate when HYPATIA_API_BEARER_TOKEN is configured.
  """
  post "/graphql" do
    Hypatia.Web.GraphQL.call(conn, [])
  end

  match _ do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(404, Jason.encode!(%{error: "not_found"}))
  end
end
