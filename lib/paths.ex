# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Hypatia.Paths do
  @moduledoc "Central path resolution for hypatia's local verisimdb data store."

  def verisimdb_data do
    Application.get_env(:hypatia, :verisimdb_data_path,
      Path.expand("data/verisimdb", File.cwd!()))
  end

  def fleet do
    Application.get_env(:hypatia, :fleet_path,
      Path.expand("~/Documents/hyperpolymath-repos/gitbot-fleet"))
  end

  def patterns, do: Path.join(verisimdb_data(), "patterns")
  def recipes, do: Path.join(verisimdb_data(), "recipes")
  def outcomes, do: Path.join(verisimdb_data(), "outcomes")
  def scans, do: Path.join(verisimdb_data(), "scans")
  def dispatch, do: Path.join(verisimdb_data(), "dispatch")
  def neural_states, do: Path.join(verisimdb_data(), "neural-states")
end
