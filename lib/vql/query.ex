# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.VQL.Query do
  @moduledoc """
  High-level VQL query functions for the Hypatia pipeline.

  Replaces raw file I/O in pipeline modules with structured VQL queries,
  enabling cross-modal analytics, temporal trending, and graph-based
  pattern detection that weren't possible with direct JSON reads.

  ## Usage

      # Simple queries
      {:ok, scans} = Hypatia.VQL.Query.fetch_scans()
      {:ok, patterns} = Hypatia.VQL.Query.fetch_patterns()
      {:ok, recipes} = Hypatia.VQL.Query.fetch_recipes()

      # Filtered queries
      {:ok, critical} = Hypatia.VQL.Query.scans_by_severity("Critical")
      {:ok, shell_recipes} = Hypatia.VQL.Query.recipes_by_language("shell")

      # Cross-repo analytics (NEW — not possible with raw JSON)
      {:ok, trending} = Hypatia.VQL.Query.trending_patterns(:increasing)
      {:ok, correlations} = Hypatia.VQL.Query.cross_repo_pattern_correlation("PA009")
      {:ok, timeline} = Hypatia.VQL.Query.outcome_timeline("recipe-shell-quote-vars")
  """

  alias Hypatia.VQL.Client

  require Logger

  # ---------------------------------------------------------------------------
  # Core Data Access (replaces raw file reads)
  # ---------------------------------------------------------------------------

  @doc "Fetch all scan results. Replaces VerisimdbConnector.fetch_all_scans/0."
  def fetch_scans(opts \\ []) do
    limit = Keyword.get(opts, :limit)
    query = if limit do
      "SELECT DOCUMENT FROM STORE scans LIMIT #{limit}"
    else
      "SELECT DOCUMENT FROM STORE scans"
    end

    case Client.query(query) do
      {:ok, results} ->
        scans = Enum.map(results, fn data ->
          repo_name = data
            |> Map.get("_source", "unknown.json")
            |> String.replace(".json", "")
          %{repo: repo_name, scan: Map.delete(data, "_source")}
        end)
        {:ok, scans}

      {:error, _} = error -> error
    end
  end

  @doc "Fetch scan for a specific repo. Replaces VerisimdbConnector.load_scan/1."
  def fetch_scan(repo_name) do
    case Client.query("SELECT DOCUMENT FROM HEXAD #{repo_name}") do
      {:ok, [scan | _]} -> {:ok, %{repo: repo_name, scan: scan}}
      {:ok, []} -> {:error, :not_found}
      {:error, _} = error -> error
    end
  end

  @doc "Fetch the pattern registry. Replaces VerisimdbConnector.fetch_pattern_registry/0."
  def fetch_pattern_registry do
    Client.query("SELECT DOCUMENT FROM STORE patterns")
  end

  @doc "Fetch all patterns as a flat list."
  def fetch_patterns do
    case fetch_pattern_registry() do
      {:ok, [%{"patterns" => patterns} | _]} ->
        {:ok, Map.values(patterns)}

      {:ok, _} ->
        {:ok, []}

      {:error, _} = error -> error
    end
  end

  @doc "Fetch all recipes. Replaces RecipeMatcher.all_recipes/0."
  def fetch_recipes(opts \\ []) do
    language = Keyword.get(opts, :language)

    query = if language do
      ~s(SELECT DOCUMENT FROM STORE recipes WHERE FIELD languages CONTAINS "#{language}")
    else
      "SELECT DOCUMENT FROM STORE recipes"
    end

    case Client.query(query) do
      {:ok, results} ->
        recipes = Enum.map(results, &Map.delete(&1, "_source"))
        {:ok, recipes}

      {:error, _} = error -> error
    end
  end

  @doc "Fetch a specific recipe by ID."
  def fetch_recipe(recipe_id) do
    case Client.query("SELECT DOCUMENT FROM HEXAD #{recipe_id}") do
      {:ok, [recipe | _]} -> {:ok, recipe}
      {:ok, []} -> {:error, :not_found}
      {:error, _} = error -> error
    end
  end

  @doc "Fetch proven substitutions."
  def fetch_substitutions do
    path = Path.expand("~/Documents/hyperpolymath-repos/verisimdb-data/recipes/proven-substitutions.json")
    case File.read(path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, data} -> {:ok, Map.get(data, "substitutions", [])}
          {:error, reason} -> {:error, reason}
        end
      {:error, reason} -> {:error, reason}
    end
  end

  @doc "Fetch all outcomes. Replaces VerisimdbConnector.fetch_all_outcomes/0."
  def fetch_outcomes(opts \\ []) do
    recipe_id = Keyword.get(opts, :recipe_id)
    limit = Keyword.get(opts, :limit)

    query = cond do
      recipe_id && limit ->
        ~s(SELECT TEMPORAL, DOCUMENT FROM STORE outcomes WHERE FIELD recipe_id == "#{recipe_id}" LIMIT #{limit})
      recipe_id ->
        ~s(SELECT TEMPORAL, DOCUMENT FROM STORE outcomes WHERE FIELD recipe_id == "#{recipe_id}")
      limit ->
        "SELECT TEMPORAL, DOCUMENT FROM STORE outcomes LIMIT #{limit}"
      true ->
        "SELECT TEMPORAL, DOCUMENT FROM STORE outcomes"
    end

    Client.query(query)
  end

  @doc "Fetch the master index."
  def fetch_index do
    Client.query("SELECT DOCUMENT FROM STORE index")
  end

  # ---------------------------------------------------------------------------
  # Filtered Queries
  # ---------------------------------------------------------------------------

  @doc "Fetch scans filtered by severity."
  def scans_by_severity(severity) do
    case fetch_scans() do
      {:ok, scans} ->
        filtered = Enum.filter(scans, fn %{scan: scan} ->
          weak_points = Map.get(scan, "weak_points", [])
          Enum.any?(weak_points, fn wp ->
            Map.get(wp, "severity") == severity
          end)
        end)
        {:ok, filtered}

      error -> error
    end
  end

  @doc "Fetch scans filtered by category (PA rule)."
  def scans_by_category(category) do
    case fetch_scans() do
      {:ok, scans} ->
        filtered = Enum.filter(scans, fn %{scan: scan} ->
          weak_points = Map.get(scan, "weak_points", [])
          Enum.any?(weak_points, fn wp ->
            Map.get(wp, "category") == category
          end)
        end)
        {:ok, filtered}

      error -> error
    end
  end

  @doc "Fetch recipes filtered by triangle tier."
  def recipes_by_tier(tier) do
    Client.query(~s(SELECT DOCUMENT FROM STORE recipes WHERE FIELD triangle_tier == "#{tier}"))
  end

  @doc "Fetch recipes filtered by language."
  def recipes_by_language(language) do
    fetch_recipes(language: language)
  end

  @doc "Fetch recipes above a confidence threshold."
  def recipes_above_confidence(threshold) do
    case fetch_recipes() do
      {:ok, recipes} ->
        filtered = Enum.filter(recipes, fn recipe ->
          Map.get(recipe, "confidence", 0.0) >= threshold
        end)
        {:ok, filtered}

      error -> error
    end
  end

  @doc "Fetch outcomes for a specific recipe."
  def outcomes_for_recipe(recipe_id) do
    fetch_outcomes(recipe_id: recipe_id)
  end

  @doc "Fetch outcomes filtered by result type."
  def outcomes_by_result(outcome_type) do
    Client.query(~s(SELECT DOCUMENT FROM STORE outcomes WHERE FIELD outcome == "#{outcome_type}"))
  end

  # ---------------------------------------------------------------------------
  # Cross-Repo Analytics (NEW — not possible with raw JSON reads)
  # ---------------------------------------------------------------------------

  @doc """
  Find patterns that appear across multiple repos.
  Returns patterns sorted by repos_affected count (descending).
  """
  def cross_repo_patterns(min_repos \\ 3) do
    case fetch_patterns() do
      {:ok, patterns} ->
        cross_repo = patterns
          |> Enum.filter(fn p -> (Map.get(p, "repos_affected", 0)) >= min_repos end)
          |> Enum.sort_by(fn p -> Map.get(p, "repos_affected", 0) end, :desc)
        {:ok, cross_repo}

      error -> error
    end
  end

  @doc """
  Analyze pattern correlation — which patterns tend to appear together in the same repos.
  Returns a list of {pattern_a, pattern_b, shared_repo_count} tuples.
  """
  def pattern_correlations(min_shared \\ 2) do
    case fetch_patterns() do
      {:ok, patterns} ->
        # Build repo → pattern map
        repo_patterns = Enum.reduce(patterns, %{}, fn pattern, acc ->
          repos = Map.get(pattern, "repos_affected_list", [])
          pattern_id = Map.get(pattern, "id", "")

          Enum.reduce(repos, acc, fn repo, inner_acc ->
            Map.update(inner_acc, repo, [pattern_id], &[pattern_id | &1])
          end)
        end)

        # Find co-occurring patterns
        correlations = repo_patterns
          |> Map.values()
          |> Enum.filter(fn pids -> length(pids) >= 2 end)
          |> Enum.flat_map(fn pids ->
            for a <- pids, b <- pids, a < b, do: {a, b}
          end)
          |> Enum.frequencies()
          |> Enum.filter(fn {_, count} -> count >= min_shared end)
          |> Enum.sort_by(fn {_, count} -> count end, :desc)
          |> Enum.map(fn {{a, b}, count} -> %{pattern_a: a, pattern_b: b, shared_repos: count} end)

        {:ok, correlations}

      error -> error
    end
  end

  @doc """
  Get outcome timeline for a recipe — success/failure rates over time.
  Returns a list of %{date, successes, failures, total} per day.
  """
  def outcome_timeline(recipe_id) do
    case outcomes_for_recipe(recipe_id) do
      {:ok, outcomes} ->
        timeline = outcomes
          |> Enum.group_by(fn o ->
            timestamp = Map.get(o, "timestamp", "")
            String.slice(timestamp, 0, 10)
          end)
          |> Enum.map(fn {date, day_outcomes} ->
            %{
              date: date,
              successes: Enum.count(day_outcomes, &(Map.get(&1, "outcome") == "success")),
              failures: Enum.count(day_outcomes, &(Map.get(&1, "outcome") == "failure")),
              false_positives: Enum.count(day_outcomes, &(Map.get(&1, "outcome") == "false_positive")),
              total: length(day_outcomes)
            }
          end)
          |> Enum.sort_by(& &1.date)

        {:ok, timeline}

      error -> error
    end
  end

  @doc """
  Compute recipe effectiveness — success rate, total attempts, trend.
  """
  def recipe_effectiveness(recipe_id) do
    case outcomes_for_recipe(recipe_id) do
      {:ok, outcomes} when outcomes != [] ->
        total = length(outcomes)
        successes = Enum.count(outcomes, &(Map.get(&1, "outcome") == "success"))
        failures = Enum.count(outcomes, &(Map.get(&1, "outcome") == "failure"))
        false_positives = Enum.count(outcomes, &(Map.get(&1, "outcome") == "false_positive"))

        # Recent trend (last 10)
        recent = Enum.take(outcomes, -10)
        recent_success_rate = if recent != [] do
          Enum.count(recent, &(Map.get(&1, "outcome") == "success")) / length(recent)
        else
          0.0
        end

        overall_success_rate = successes / total

        trend = cond do
          recent_success_rate > overall_success_rate + 0.1 -> :improving
          recent_success_rate < overall_success_rate - 0.1 -> :declining
          true -> :stable
        end

        {:ok, %{
          recipe_id: recipe_id,
          total_attempts: total,
          successes: successes,
          failures: failures,
          false_positives: false_positives,
          success_rate: Float.round(overall_success_rate, 4),
          recent_success_rate: Float.round(recent_success_rate, 4),
          trend: trend
        }}

      {:ok, []} ->
        {:ok, %{recipe_id: recipe_id, total_attempts: 0, trend: :no_data}}

      error -> error
    end
  end

  @doc """
  Find repos with the most unresolved weak points.
  Returns [{repo_name, weak_point_count, categories}] sorted by count.
  """
  def most_vulnerable_repos(limit \\ 20) do
    case fetch_scans() do
      {:ok, scans} ->
        ranked = scans
          |> Enum.map(fn %{repo: repo, scan: scan} ->
            weak_points = Map.get(scan, "weak_points", [])
            categories = weak_points
              |> Enum.map(&Map.get(&1, "category", "unknown"))
              |> Enum.uniq()
            %{repo: repo, count: length(weak_points), categories: categories}
          end)
          |> Enum.filter(fn %{count: c} -> c > 0 end)
          |> Enum.sort_by(fn %{count: c} -> c end, :desc)
          |> Enum.take(limit)

        {:ok, ranked}

      error -> error
    end
  end

  @doc """
  Category distribution — how many findings per PA category across all repos.
  """
  def category_distribution do
    case fetch_scans() do
      {:ok, scans} ->
        distribution = scans
          |> Enum.flat_map(fn %{scan: scan} ->
            Map.get(scan, "weak_points", [])
          end)
          |> Enum.group_by(&Map.get(&1, "category", "unknown"))
          |> Enum.map(fn {category, findings} ->
            severities = Enum.frequencies_by(findings, &Map.get(&1, "severity", "Medium"))
            %{
              category: category,
              total: length(findings),
              severities: severities,
              repos: findings
                |> Enum.map(&Map.get(&1, "location", ""))
                |> Enum.uniq()
                |> length()
            }
          end)
          |> Enum.sort_by(& &1.total, :desc)

        {:ok, distribution}

      error -> error
    end
  end

  @doc """
  Recipe coverage — what percentage of patterns have automated fix recipes.
  """
  def recipe_coverage do
    with {:ok, patterns} <- fetch_patterns(),
         {:ok, recipes} <- fetch_recipes() do
      total_patterns = length(patterns)

      recipe_pattern_ids = recipes
        |> Enum.flat_map(&Map.get(&1, "pattern_ids", []))
        |> MapSet.new()

      covered = Enum.count(patterns, fn p ->
        pattern_id = Map.get(p, "id", "")
        MapSet.member?(recipe_pattern_ids, pattern_id)
      end)

      pa_rule_coverage = recipes
        |> Enum.flat_map(&Map.get(&1, "pattern_ids", []))
        |> Enum.map(fn pid -> String.split(pid, "-") |> List.first() end)
        |> Enum.uniq()

      {:ok, %{
        total_patterns: total_patterns,
        patterns_with_recipe: covered,
        coverage_pct: if(total_patterns > 0, do: Float.round(covered / total_patterns * 100, 1), else: 0.0),
        total_recipes: length(recipes),
        pa_rules_covered: pa_rule_coverage
      }}
    end
  end

  @doc """
  Pipeline health summary — comprehensive status of the entire system.
  """
  def pipeline_health do
    with {:ok, scans} <- fetch_scans(),
         {:ok, patterns} <- fetch_patterns(),
         {:ok, recipes} <- fetch_recipes(),
         {:ok, outcomes} <- fetch_outcomes() do
      total_weak_points = scans
        |> Enum.map(fn %{scan: scan} -> length(Map.get(scan, "weak_points", [])) end)
        |> Enum.sum()

      success_count = Enum.count(outcomes, &(Map.get(&1, "outcome") == "success"))
      failure_count = Enum.count(outcomes, &(Map.get(&1, "outcome") == "failure"))

      {:ok, %{
        repos_scanned: length(scans),
        total_weak_points: total_weak_points,
        canonical_patterns: length(patterns),
        fix_recipes: length(recipes),
        outcomes_recorded: length(outcomes),
        success_rate: if(outcomes != [], do: Float.round(success_count / length(outcomes) * 100, 1), else: 0.0),
        failure_count: failure_count,
        high_confidence_recipes: Enum.count(recipes, &(Map.get(&1, "confidence", 0) >= 0.95)),
        auto_executable_recipes: Enum.count(recipes, fn r ->
          Map.get(r, "confidence", 0) >= 0.95 and Map.get(r, "auto_fixable", false)
        end)
      }}
    end
  end
end
