# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.Application do
  @moduledoc """
  OTP Application for Hypatia.

  Starts the supervision tree with layered dependencies:
  1. Data layer (ArangoDB) — federated store alongside verisimdb-data
  2. Safety layer — rate limiter, quarantine
  3. Intelligence layer — learning scheduler, self-diagnostics
  4. Neural layer — 5 neural networks + coordinator
  """

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Layer 1: Data — ArangoDB federated store (graceful degradation if unavailable)
      Hypatia.Data.ArangoDB,
      # Layer 2: Safety — rate limiting and bot quarantine
      Hypatia.Safety.RateLimiter,
      Hypatia.Safety.Quarantine,
      # Layer 3: Intelligence — feedback loop and health monitoring
      Hypatia.LearningScheduler,
      Hypatia.SelfDiagnostics,
      # Layer 4: Neural — 5 networks orchestrated by coordinator
      Hypatia.Neural.Coordinator
    ]

    opts = [strategy: :one_for_one, name: Hypatia.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
