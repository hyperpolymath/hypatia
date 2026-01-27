# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Hypatia.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Learning engine for autonomous rule generation
      {Hypatia.Learning, []}
    ]

    opts = [strategy: :one_for_one, name: Hypatia.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
