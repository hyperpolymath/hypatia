# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.MixProject do
  use Mix.Project

  def project do
    [
      app: :hypatia,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      escript: escript()
    ]
  end

  defp escript do
    [
      main_module: Hypatia.CLI,
      name: :hypatia
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
      {:phoenix, "~> 1.7"}
    ]
  end
end
