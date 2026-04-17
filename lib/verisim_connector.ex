# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.VerisimConnector do
  @moduledoc """
  Reads scan results, pattern registry, and recipes from verisim-data.

  Now powered by VCL -- queries go through Hypatia.VCL.Client instead of raw
  file I/O. Falls back to direct file reads if VCL Client is unavailable
  (e.g., during testing or before OTP supervision tree starts).

  ## VCL Migration

  | Old (file I/O)           | New (VCL)                                         |
  |--------------------------|---------------------------------------------------|
  | File.ls + File.read      | SELECT DOCUMENT FROM STORE scans                  |
  | Jason.decode(registry)   | SELECT DOCUMENT FROM STORE patterns               |
  | File.ls(recipes)         | SELECT DOCUMENT FROM STORE recipes                |
  | JSONL line-by-line       | SELECT TEMPORAL, DOCUMENT FROM STORE outcomes      |

  Direct file reads preserved as fallback for graceful degradation.
  """

  alias Hypatia.VCL.Query, as: VCL

  @verisimdb_data_path Application.compile_env(:hypatia, :verisimdb_data_path, "data/verisim")

  require Logger

  # ====================================================================
  # Scan Access (VCL-powered)
  # ====================================================================

  @doc "Fetch all scan results from verisim-data/scans/ via VCL."
  def fetch_all_scans do
    case VCL.fetch_scans() do
      {:ok, scans} -> scans
      {:error, reason} ->
        Logger.warning("VCL scan fetch failed (#{inspect(reason)}), falling back to file I/O")
        fetch_all_scans_fallback()
    end
  end

  @doc "Fetch scan for a specific repo."
  def fetch_scan(repo_name) do
    case VCL.fetch_scan(repo_name) do
      {:ok, scan} -> scan
      {:error, _} -> load_scan_fallback("#{repo_name}.json")
    end
  end

  # ====================================================================
  # Pattern Registry Access (VCL-powered)
  # ====================================================================

  @doc "Load the pattern registry from verisim-data/patterns/registry.json via VCL."
  def fetch_pattern_registry do
    case VCL.fetch_pattern_registry() do
      {:ok, [registry | _]} -> {:ok, registry}
      {:ok, []} -> {:error, :empty}
      {:error, reason} ->
        Logger.warning("VCL pattern fetch failed (#{inspect(reason)}), falling back to file I/O")
        fetch_pattern_registry_fallback()
    end
  end

  # ====================================================================
  # Recipe Access (VCL-powered)
  # ====================================================================

  @doc "Load all recipe files from verisim-data/recipes/ via VCL."
  def fetch_all_recipes do
    case VCL.fetch_recipes() do
      {:ok, recipes} -> recipes
      {:error, reason} ->
        Logger.warning("VCL recipe fetch failed (#{inspect(reason)}), falling back to file I/O")
        fetch_all_recipes_fallback()
    end
  end

  @doc "Load the proven-substitutions mapping."
  def fetch_substitutions do
    case VCL.fetch_substitutions() do
      {:ok, subs} -> subs
      {:error, _} -> fetch_substitutions_fallback()
    end
  end

  # ====================================================================
  # Index Access (VCL-powered)
  # ====================================================================

  @doc "Load the master index from verisim-data/index.json via VCL."
  def fetch_index do
    case VCL.fetch_index() do
      {:ok, [index | _]} -> {:ok, index}
      {:ok, []} -> {:error, :not_found}
      {:error, reason} ->
        Logger.warning("VCL index fetch failed (#{inspect(reason)}), falling back to file I/O")
        fetch_index_fallback()
    end
  end

  # ====================================================================
  # Outcome Access (VCL-powered)
  # ====================================================================

  @doc "Load all outcome records from verisim-data/outcomes/ via VCL."
  def fetch_all_outcomes do
    case VCL.fetch_outcomes() do
      {:ok, outcomes} -> outcomes
      {:error, reason} ->
        Logger.warning("VCL outcome fetch failed (#{inspect(reason)}), falling back to file I/O")
        fetch_all_outcomes_fallback()
    end
  end

  @doc "Load outcomes for a specific recipe via VCL."
  def fetch_outcomes_for_recipe(recipe_id) do
    case VCL.outcomes_for_recipe(recipe_id) do
      {:ok, outcomes} -> outcomes
      {:error, _} -> []
    end
  end

  # ====================================================================
  # Cross-Repo Analytics (NEW -- VCL-only, no file I/O equivalent)
  # ====================================================================

  @doc "Find patterns appearing across 3+ repos."
  def cross_repo_patterns(min_repos \\ 3) do
    VCL.cross_repo_patterns(min_repos)
  end

  @doc "Find correlated patterns (tend to appear together)."
  def pattern_correlations(min_shared \\ 2) do
    VCL.pattern_correlations(min_shared)
  end

  @doc "Get outcome timeline for a recipe."
  def outcome_timeline(recipe_id) do
    VCL.outcome_timeline(recipe_id)
  end

  @doc "Get recipe effectiveness metrics."
  def recipe_effectiveness(recipe_id) do
    VCL.recipe_effectiveness(recipe_id)
  end

  @doc "Get most vulnerable repos."
  def most_vulnerable_repos(limit \\ 20) do
    VCL.most_vulnerable_repos(limit)
  end

  @doc "Get category distribution across all scans."
  def category_distribution do
    VCL.category_distribution()
  end

  @doc "Get recipe coverage report."
  def recipe_coverage do
    VCL.recipe_coverage()
  end

  @doc "Get full pipeline health summary."
  def pipeline_health do
    VCL.pipeline_health()
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
