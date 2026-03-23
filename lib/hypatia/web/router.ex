# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Web.Router do
  @moduledoc """
  HTTP router for Hypatia's public endpoints.

  Serves well-known service discovery manifests and health checks.
  Listens on port 9090 via Bandit, supervised by the OTP application.
  """

  use Plug.Router

  plug :match
  plug :dispatch

  @doc """
  GET /.well-known/groove — Groove capability manifest for service discovery.

  Returns Hypatia's scanning and analysis capabilities in the standard
  Groove manifest format. See Groove.idr hypatiaManifest for the canonical
  ABI definition.
  """
  get "/.well-known/groove" do
    manifest = %{
      service_id: "hypatia",
      groove_version: "1.0.0",
      capabilities: [
        %{
          name: "scanning",
          protocol: "http",
          endpoint: "/api/v1/scan"
        },
        %{
          name: "static-analysis",
          protocol: "http",
          endpoint: "/api/v1/analyze"
        }
      ],
      consumes: ["octad-storage", "workflow"],
      port: 9090
    }

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Jason.encode!(manifest))
  end

  @doc """
  GET /health — Basic health check for the HTTP endpoint.
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

  match _ do
    send_resp(conn, 404, Jason.encode!(%{error: "not_found"}))
  end
end
