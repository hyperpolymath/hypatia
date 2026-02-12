# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.VerisimdbConnector do
  @moduledoc """
  Reads scan results, pattern registry, and recipes from verisimdb-data repo.
  Transforms data into Logtalk facts and provides access to the expanded schema.
  """

  @verisimdb_data_path "~/Documents/hyperpolymath-repos/verisimdb-data"

  require Logger

  # ====================================================================
  # Scan Access
  # ====================================================================

  @doc "Fetch all scan results from verisimdb-data/scans/."
  def fetch_all_scans do
    scans_path = Path.join(expand_path(@verisimdb_data_path), "scans")

    case File.ls(scans_path) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".json"))
        |> Enum.map(&load_scan/1)
        |> Enum.reject(&is_nil/1)

      {:error, reason} ->
        Logger.error("Failed to read scans directory: #{inspect(reason)}")
        []
    end
  end

  # ====================================================================
  # Pattern Registry Access
  # ====================================================================

  @doc "Load the pattern registry from verisimdb-data/patterns/registry.json."
  def fetch_pattern_registry do
    path = Path.join(expand_path(@verisimdb_data_path), "patterns/registry.json")

    case File.read(path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, data} -> {:ok, data}
          {:error, reason} -> {:error, {:decode, reason}}
        end

      {:error, reason} ->
        {:error, {:read, reason}}
    end
  end

  # ====================================================================
  # Recipe Access
  # ====================================================================

  @doc "Load all recipe files from verisimdb-data/recipes/."
  def fetch_all_recipes do
    recipes_path = Path.join(expand_path(@verisimdb_data_path), "recipes")

    case File.ls(recipes_path) do
      {:ok, files} ->
        files
        |> Enum.filter(fn f ->
          String.starts_with?(f, "recipe-") and String.ends_with?(f, ".json")
        end)
        |> Enum.map(fn f -> load_json(Path.join(recipes_path, f)) end)
        |> Enum.reject(&is_nil/1)

      {:error, reason} ->
        Logger.error("Failed to read recipes directory: #{inspect(reason)}")
        []
    end
  end

  @doc "Load the proven-substitutions mapping."
  def fetch_substitutions do
    path = Path.join(expand_path(@verisimdb_data_path), "recipes/proven-substitutions.json")

    case load_json(path) do
      nil -> []
      data -> Map.get(data, "substitutions", [])
    end
  end

  # ====================================================================
  # Index Access
  # ====================================================================

  @doc "Load the master index from verisimdb-data/index.json."
  def fetch_index do
    path = Path.join(expand_path(@verisimdb_data_path), "index.json")

    case load_json(path) do
      nil -> {:error, :not_found}
      data -> {:ok, data}
    end
  end

  # ====================================================================
  # Outcome Access
  # ====================================================================

  @doc "Load all outcome records from verisimdb-data/outcomes/."
  def fetch_all_outcomes do
    outcomes_path = Path.join(expand_path(@verisimdb_data_path), "outcomes")

    case File.ls(outcomes_path) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
        |> Enum.flat_map(fn f ->
          path = Path.join(outcomes_path, f)

          case File.read(path) do
            {:ok, content} ->
              content
              |> String.split("\n", trim: true)
              |> Enum.map(fn line ->
                case Jason.decode(line) do
                  {:ok, record} -> record
                  {:error, _} -> nil
                end
              end)
              |> Enum.reject(&is_nil/1)

            {:error, _} ->
              []
          end
        end)

      {:error, _} ->
        []
    end
  end

  # ====================================================================
  # Logtalk Fact Generation
  # ====================================================================

  @doc "Transform a scan result into Logtalk facts for the rule engine."
  def to_logtalk_facts(scan_data) do
    weak_points = Map.get(scan_data.scan, "weak_points", [])

    weak_points
    |> Enum.map(fn wp ->
      # panic-attack uses "location" for file path
      file = Map.get(wp, "location", Map.get(wp, "file", "unknown"))
      category = Map.get(wp, "category", "unknown")
      severity = Map.get(wp, "severity", "unknown")

      """
      weak_point('#{scan_data.repo}', '#{file}', '#{category}', '#{severity}').
      """
    end)
    |> Enum.join("\n")
  end

  # ====================================================================
  # Private
  # ====================================================================

  defp load_scan(filename) do
    path = Path.join([expand_path(@verisimdb_data_path), "scans", filename])
    repo_name = String.replace(filename, ".json", "")

    with {:ok, content} <- File.read(path),
         {:ok, data} <- Jason.decode(content) do
      %{repo: repo_name, scan: data}
    else
      {:error, reason} ->
        Logger.error("Failed to load scan #{filename}: #{inspect(reason)}")
        nil
    end
  end

  defp load_json(path) do
    with {:ok, content} <- File.read(path),
         {:ok, data} <- Jason.decode(content) do
      data
    else
      {:error, reason} ->
        Logger.error("Failed to load JSON #{path}: #{inspect(reason)}")
        nil
    end
  end

  defp expand_path(path) do
    Path.expand(path)
  end
end
