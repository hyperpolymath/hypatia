# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Application do
  @moduledoc """
  OTP Application for Hypatia.

  Starts the supervision tree with layered dependencies:
  0. HTTP layer (Bandit + Plug.Router) — groove endpoint, health on port 9090
  0. Query layer (VQL Client) — structured query interface to verisimdb-data
  1. Safety layer — rate limiter, quarantine
  2. Intelligence layer — learning scheduler, self-diagnostics
  3. Neural layer — blackboard + 8 networks + coordinator (phased execution)
  4. Kin layer — ecosystem coordination, watchdog, self-healing
  """

  use Application

  @impl true
  def start(_type, _args) do
    # Port is configurable via :hypatia, :http_port (default 9090).
    # Tests use a different port to avoid collisions with the running dev server.
    http_port = Application.get_env(:hypatia, :http_port, 9090)

    children = [
      # Layer 0: HTTP — public endpoints (groove, health)
      {Bandit, plug: Hypatia.Web.Router, port: http_port, scheme: :http},
      # Layer 0: Query — VQL client for structured data access
      Hypatia.VQL.Client,
      # Layer 0.5: Dispatch — GenStage pipeline for controlled parallel processing
      Hypatia.Dispatch.Pipeline,
      # Layer 0.7: Diagnostics — system health monitoring and auto-recovery
      Hypatia.Diagnostics.Monitor,
      # Layer 1: Safety — rate limiting and bot quarantine
      Hypatia.Safety.RateLimiter,
      Hypatia.Safety.Quarantine,
      # Layer 3: Intelligence — feedback loop, rules, and health monitoring
      Hypatia.Rules.Learning,
      Hypatia.LearningScheduler,
      Hypatia.SelfDiagnostics,
      # Layer 4: Neural — blackboard + 8 networks, phased execution
      # Blackboard MUST start before coordinator and network GenServers
      Hypatia.Neural.Blackboard,
      # New network GenServers (GNN, VAE, SequenceModel)
      Hypatia.Neural.GraphNeuralNetwork,
      Hypatia.Neural.VariationalAutoencoder,
      Hypatia.Neural.SequenceModel,
      # Coordinator orchestrates all 8 networks via blackboard
      Hypatia.Neural.Coordinator,
      # Layer 5: Kin — ecosystem coordination, watchdog, and self-healing
      Hypatia.Kin.Contingency,    # must start first (emergency state persisted)
      Hypatia.Kin.Arbiter,        # conflict resolution (stateless on startup)
      Hypatia.Kin.Gate,           # action review checkpoint
      Hypatia.Kin.Coordinator,    # sibling heartbeat polling
      Hypatia.Kin.Watchdog        # internal OTP process monitoring
    ]

    opts = [strategy: :one_for_one, name: Hypatia.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
