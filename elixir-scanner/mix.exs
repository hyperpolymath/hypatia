# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Hypatia.MixProject do
  use Mix.Project

  def project do
    [
      app: :hypatia,
      version: "2.0.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      escript: escript(),
      aliases: aliases()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {Hypatia.Application, []}
    ]
  end

  defp deps do
    [
      {:jason, "~> 1.4"},
      {:typed_struct, "~> 0.3.0"}
    ]
  end

  defp escript do
    [main_module: Hypatia.CLI, name: "hypatia"]
  end

  defp aliases do
    [
      scan: "run -e 'Hypatia.CLI.main(System.argv())'"
    ]
  end
end
