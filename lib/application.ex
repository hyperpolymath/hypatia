# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.Application do
  @moduledoc """
  OTP Application for Hypatia.

  Starts the supervision tree including:
  - LearningScheduler: automatic feedback loop (polls every 5 min)
  - SelfDiagnostics: health monitoring and self-healing
  - Neural.Coordinator: orchestrates 5 neural network subsystems
  """

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Feedback loop: polls outcomes and updates recipe confidence
      Hypatia.LearningScheduler,
      # Self-diagnostics and health monitoring
      Hypatia.SelfDiagnostics,
      # Neural network coordinator (Graph of Trust, MoE, LSM, ESN, RBF)
      Hypatia.Neural.Coordinator
    ]

    opts = [strategy: :one_for_one, name: Hypatia.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
