# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.VerisimdbConnector do
  @moduledoc """
  Reads scan results, pattern registry, and recipes from verisimdb-data.

  Now powered by VQL — queries go through Hypatia.VQL.Client instead of raw
  file I/O. Falls back to direct file reads if VQL Client is unavailable
  (e.g., during testing or before OTP supervision tree starts).

  ## VQL Migration

  | Old (file I/O)           | New (VQL)                                         |
  |--------------------------|---------------------------------------------------|
  | File.ls + File.read      | SELECT DOCUMENT FROM STORE scans                  |
  | Jason.decode(registry)   | SELECT DOCUMENT FROM STORE patterns               |
  | File.ls(recipes)         | SELECT DOCUMENT FROM STORE recipes                |
  | JSONL line-by-line       | SELECT TEMPORAL, DOCUMENT FROM STORE outcomes      |

  Direct file reads preserved as fallback for graceful degradation.
  """

  alias Hypatia.VQL.Query, as: VQL

  @verisimdb_data_path "~/Documents/hyperpolymath-repos/verisimdb-data"

  require Logger

  # ====================================================================
  # Scan Access (VQL-powered)
  # ====================================================================

  @doc "Fetch all scan results from verisimdb-data/scans/ via VQL."
  def fetch_all_scans do
    case VQL.fetch_scans() do
      {:ok, scans} -> scans
      {:error, reason} ->
        Logger.warning("VQL scan fetch failed (#{inspect(reason)}), falling back to file I/O")
        fetch_all_scans_fallback()
    end
  end

  @doc "Fetch scan for a specific repo."
  def fetch_scan(repo_name) do
    case VQL.fetch_scan(repo_name) do
      {:ok, scan} -> scan
      {:error, _} -> load_scan_fallback("#{repo_name}.json")
    end
  end

  # ====================================================================
  # Pattern Registry Access (VQL-powered)
  # ====================================================================

  @doc "Load the pattern registry from verisimdb-data/patterns/registry.json via VQL."
  def fetch_pattern_registry do
    case VQL.fetch_pattern_registry() do
      {:ok, [registry | _]} -> {:ok, registry}
      {:ok, []} -> {:error, :empty}
      {:error, reason} ->
        Logger.warning("VQL pattern fetch failed (#{inspect(reason)}), falling back to file I/O")
        fetch_pattern_registry_fallback()
    end
  end

  # ====================================================================
  # Recipe Access (VQL-powered)
  # ====================================================================

  @doc "Load all recipe files from verisimdb-data/recipes/ via VQL."
  def fetch_all_recipes do
    case VQL.fetch_recipes() do
      {:ok, recipes} -> recipes
      {:error, reason} ->
        Logger.warning("VQL recipe fetch failed (#{inspect(reason)}), falling back to file I/O")
        fetch_all_recipes_fallback()
    end
  end

  @doc "Load the proven-substitutions mapping."
  def fetch_substitutions do
    case VQL.fetch_substitutions() do
      {:ok, subs} -> subs
      {:error, _} -> fetch_substitutions_fallback()
    end
  end

  # ====================================================================
  # Index Access (VQL-powered)
  # ====================================================================

  @doc "Load the master index from verisimdb-data/index.json via VQL."
  def fetch_index do
    case VQL.fetch_index() do
      {:ok, [index | _]} -> {:ok, index}
      {:ok, []} -> {:error, :not_found}
      {:error, reason} ->
        Logger.warning("VQL index fetch failed (#{inspect(reason)}), falling back to file I/O")
        fetch_index_fallback()
    end
  end

  # ====================================================================
  # Outcome Access (VQL-powered)
  # ====================================================================

  @doc "Load all outcome records from verisimdb-data/outcomes/ via VQL."
  def fetch_all_outcomes do
    case VQL.fetch_outcomes() do
      {:ok, outcomes} -> outcomes
      {:error, reason} ->
        Logger.warning("VQL outcome fetch failed (#{inspect(reason)}), falling back to file I/O")
        fetch_all_outcomes_fallback()
    end
  end

  @doc "Load outcomes for a specific recipe via VQL."
  def fetch_outcomes_for_recipe(recipe_id) do
    case VQL.outcomes_for_recipe(recipe_id) do
      {:ok, outcomes} -> outcomes
      {:error, _} -> []
    end
  end

  # ====================================================================
  # Cross-Repo Analytics (NEW — VQL-only, no file I/O equivalent)
  # ====================================================================

  @doc "Find patterns appearing across 3+ repos."
  def cross_repo_patterns(min_repos \\ 3) do
    VQL.cross_repo_patterns(min_repos)
  end

  @doc "Find correlated patterns (tend to appear together)."
  def pattern_correlations(min_shared \\ 2) do
    VQL.pattern_correlations(min_shared)
  end

  @doc "Get outcome timeline for a recipe."
  def outcome_timeline(recipe_id) do
    VQL.outcome_timeline(recipe_id)
  end

  @doc "Get recipe effectiveness metrics."
  def recipe_effectiveness(recipe_id) do
    VQL.recipe_effectiveness(recipe_id)
  end

  @doc "Get most vulnerable repos."
  def most_vulnerable_repos(limit \\ 20) do
    VQL.most_vulnerable_repos(limit)
  end

  @doc "Get category distribution across all scans."
  def category_distribution do
    VQL.category_distribution()
  end

  @doc "Get recipe coverage report."
  def recipe_coverage do
    VQL.recipe_coverage()
  end

  @doc "Get full pipeline health summary."
  def pipeline_health do
    VQL.pipeline_health()
  end

  # ====================================================================
  # Logtalk Fact Generation
  # ====================================================================

  @doc "Transform a scan result into Logtalk facts for the rule engine."
  def to_logtalk_facts(scan_data) do
    weak_points = Map.get(scan_data.scan, "weak_points", [])

    weak_points
    |> Enum.map(fn wp ->
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
  # Fallback: Direct File I/O (graceful degradation)
  # ====================================================================

  defp fetch_all_scans_fallback do
    scans_path = Path.join(expand_path(@verisimdb_data_path), "scans")

    case File.ls(scans_path) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".json"))
        |> Enum.map(&load_scan_fallback/1)
        |> Enum.reject(&is_nil/1)

      {:error, reason} ->
        Logger.error("Failed to read scans directory: #{inspect(reason)}")
        []
    end
  end

  defp load_scan_fallback(filename) do
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

  defp fetch_pattern_registry_fallback do
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

  defp fetch_all_recipes_fallback do
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

  defp fetch_substitutions_fallback do
    path = Path.join(expand_path(@verisimdb_data_path), "recipes/proven-substitutions.json")

    case load_json(path) do
      nil -> []
      data -> Map.get(data, "substitutions", [])
    end
  end

  defp fetch_index_fallback do
    path = Path.join(expand_path(@verisimdb_data_path), "index.json")

    case load_json(path) do
      nil -> {:error, :not_found}
      data -> {:ok, data}
    end
  end

  defp fetch_all_outcomes_fallback do
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
