# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.Paths do
  @moduledoc "Central path resolution for hypatia's local verisim data store."

  @doc """
  Returns the root path for verisimdb data storage.

  Defaults to `data/verisim/` in the current working directory unless configured
  via `:verisimdb_data_path` application environment variable.
  """
  def verisimdb_data do
    Application.get_env(:hypatia, :verisimdb_data_path, Path.expand("data/verisim", File.cwd!()))
  end

  @doc """
  Returns the path to the gitbot-fleet directory.

  Defaults to `~/Documents/hyperpolymath-repos/gitbot-fleet` unless configured
  via `:fleet_path` application environment variable.
  """
  def fleet do
    Application.get_env(
      :hypatia,
      :fleet_path,
      Path.expand("~/Documents/hyperpolymath-repos/gitbot-fleet")
    )
  end

  @doc "Returns the patterns subdirectory within verisimdb data."
  def patterns, do: Path.join(verisimdb_data(), "patterns")

  @doc "Returns the recipes subdirectory within verisimdb data."
  def recipes, do: Path.join(verisimdb_data(), "recipes")

  @doc "Returns the outcomes subdirectory within verisimdb data."
  def outcomes, do: Path.join(verisimdb_data(), "outcomes")

  @doc "Returns the scans subdirectory within verisimdb data."
  def scans, do: Path.join(verisimdb_data(), "scans")

  @doc "Returns the dispatch subdirectory within verisimdb data."
  def dispatch, do: Path.join(verisimdb_data(), "dispatch")

  @doc "Returns the neural-states subdirectory within verisimdb data."
  def neural_states, do: Path.join(verisimdb_data(), "neural-states")

  @machine_tree_canonical "machine-readable"
  @machine_tree_legacy ".machine_readable"

  @doc """
  Name of a repository's machine tree directory.

  The canonical name is `machine-readable/`; `.machine_readable/` is the LEGACY
  form. Both are accepted: the canon, scaffoldia, the julia variant and ~300
  minted repos still carry the dotted name, so resolving to one form only would
  make the oracle unable to score whichever half of the estate had not migrated.

  Prefers the canonical name when both are present. Falls back to the canonical
  name when neither exists, so callers building a path for a repo that has no
  machine tree at all report against the name it *should* have.
  """
  def machine_tree(repo_path) do
    cond do
      File.dir?(Path.join(repo_path, @machine_tree_canonical)) -> @machine_tree_canonical
      File.dir?(Path.join(repo_path, @machine_tree_legacy)) -> @machine_tree_legacy
      true -> @machine_tree_canonical
    end
  end

  @doc "Join a path inside a repository's machine tree, whichever name it uses."
  def machine_tree_join(repo_path, parts) when is_list(parts) do
    Path.join([repo_path, machine_tree(repo_path) | parts])
  end

  def machine_tree_join(repo_path, part), do: machine_tree_join(repo_path, [part])
end
