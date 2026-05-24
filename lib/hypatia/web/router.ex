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

  # /api/* is gated to loopback in Hypatia.Web.ApiRouter — keeps
  # operational data off the public surface while leaving /health
  # reachable for container orchestrators.
  forward "/api", to: Hypatia.Web.ApiRouter

  match _ do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(404, Jason.encode!(%{error: "not_found"}))
  end
end
