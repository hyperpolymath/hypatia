# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.MixProject do
  use Mix.Project

  def project do
    [
      app: :hypatia,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      elixirc_paths: elixirc_paths(Mix.env()),
      deps: deps(),
      escript: escript()
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp escript do
    [
      main_module: Hypatia.CLI,
      name: :hypatia,
      # Do not start the OTP supervision tree for CLI scans.
      # The rule modules are pure functions that don't need GenServers.
      # This avoids the SelfDiagnostics/LearningScheduler timeout hang.
      app: nil
    ]
  end

  def application do
    [
      extra_applications: [:logger, :inets, :ssl],
      mod: {Hypatia.Application, []}
    ]
  end

  defp deps do
    [
      {:jason, "~> 1.4"},
      {:gen_stage, "~> 1.2"},
      {:phoenix, "~> 1.7"},
      {:bandit, "~> 1.0"},
      {:plug, "~> 1.14"}
    ]
  end
end
