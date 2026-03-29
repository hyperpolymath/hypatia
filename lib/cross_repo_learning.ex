# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Cross-Repo Learning — pattern transfer and ecosystem-level intelligence
#
# When repo A fixes a pattern successfully, repo B shouldn't have to
# relearn the same lesson from scratch. This module enables confidence
# transfer across repositories, modulated by:
#
# 1. Language family similarity (Rust fixes transfer to Rust repos)
# 2. Annealing temperature (cross-repo = higher effective temperature)
# 3. Drift policies (conservative, aggressive, language-aware)
# 4. Ecosystem-level anomaly detection (CVE waves, pattern spikes)
#
# Data flows:
#   outcomes/*.jsonl → per-repo outcome index → cross-repo transfer matrix
#                                             → ecosystem anomaly detector

defmodule Hypatia.CrossRepoLearning do
  @moduledoc """
  Cross-repository learning for Hypatia's confidence and dispatch system.

  ## Architecture

  Cross-repo learning operates on three levels:

  ### Level 1: Repo-Contextual Confidence

  Each recipe tracks confidence per-repo, not just globally. When a recipe
  has 50 successes in Rust repos but 2 failures in Python repos, the
  per-repo confidence reflects that divergence.

  ### Level 2: Pattern Transfer

  When a recipe succeeds in repo A, that success can boost confidence for
  the same recipe in similar repos. Transfer is modulated by:

  - **Language family**: repos sharing a primary language transfer at full
    strength; cross-language transfer is discounted
  - **Annealing temperature**: cross-repo evidence uses higher effective
    temperature (more conservative)
  - **Drift policy**: controls how aggressively cross-repo evidence is
    weighted

  ### Level 3: Ecosystem Anomaly Detection

  Monitors the outcome stream across all 300+ repos for ecosystem-wide
  anomalies:

  - CVE waves (many repos suddenly failing the same pattern)
  - Recipe degradation (a recipe that was reliable starts failing everywhere)
  - Language-specific trends (new vulnerability class in one language family)

  ## Drift Policies

  | Policy          | Cross-repo discount | Description                        |
  |-----------------|--------------------|------------------------------------|
  | :conservative   | 0.5                | Heavy discount, trust local data   |
  | :moderate       | 0.75               | Balanced transfer                  |
  | :aggressive     | 1.0                | Trust cross-repo equally           |
  | :language_aware | 1.0 same / 0.3 diff | Full within language family        |

  ## VQL Integration

  Cross-repo queries use the FEDERATION modality with drift_policy:

      FEDERATION /outcomes/* WITH drift_policy "language_aware"
  """

  require Logger

  alias Hypatia.ConfidenceAnnealing

  @verisimdb_data_path Application.compile_env(:hypatia, :verisimdb_data_path, "data/verisimdb")
  @cross_repo_index_path Application.compile_env(
    :hypatia, :cross_repo_index_path,
    "data/verisimdb/cross-repo-index"
  )

  # Drift policy discount factors for cross-repo confidence transfer
  @drift_policies %{
    conservative: 0.5,
    moderate: 0.75,
    aggressive: 1.0,
    language_aware: :language_dependent
  }

  # Language family groupings — repos in the same family transfer at
  # full strength; cross-family transfers are discounted
  @language_families %{
    "rust" => :systems,
    "zig" => :systems,
    "c" => :systems,
    "cpp" => :systems,
    "ada" => :systems,
    "rescript" => :ml_family,
    "ocaml" => :ml_family,
    "haskell" => :ml_family,
    "idris" => :ml_family,
    "gleam" => :beam,
    "elixir" => :beam,
    "erlang" => :beam,
    "javascript" => :scripting,
    "shell" => :scripting,
    "julia" => :scientific,
    "nickel" => :config
  }

  # Cross-family discount factor (used in :language_aware policy)
  @cross_family_discount 0.3

  # Minimum outcome count before a repo's data can be transferred
  @min_transfer_outcomes 3

  # Ecosystem anomaly detection thresholds
  @anomaly_failure_rate_threshold 0.3
  @anomaly_min_repos 5
  @anomaly_time_window_hours 24

  # --- Types ---

  @type drift_policy :: :conservative | :moderate | :aggressive | :language_aware
  @type repo_confidence :: %{
    repo: String.t(),
    recipe_id: String.t(),
    local_confidence: float(),
    transferred_confidence: float(),
    blended_confidence: float(),
    local_outcomes: non_neg_integer(),
    transfer_sources: [String.t()]
  }

  # --- Public API ---

  @doc """
  Get blended confidence for a recipe in a specific repo.

  Blends local evidence (outcomes for this recipe in this repo) with
  transferred evidence (outcomes for the same recipe in similar repos),
  modulated by drift policy and annealing temperature.

  Returns a repo_confidence struct with local, transferred, and blended
  confidence values for full transparency.
  """
  @spec repo_confidence(String.t(), String.t(), drift_policy()) :: repo_confidence()
  def repo_confidence(recipe_id, repo, policy \\ :moderate) do
    # Load local outcomes for this recipe in this repo
    local_outcomes = load_repo_outcomes(recipe_id, repo)
    local_count = length(local_outcomes)

    local_confidence =
      if local_count > 0 do
        successes = Enum.count(local_outcomes, &(Map.get(&1, "outcome") == "success"))
        Hypatia.OutcomeTracker.bayesian_update(0.5, successes, local_count - successes)
      else
        nil
      end

    # Load transferable outcomes from similar repos
    {transferred_confidence, transfer_sources} =
      compute_transferred_confidence(recipe_id, repo, policy)

    # Blend local and transferred confidence
    blended = blend_confidence(local_confidence, local_count, transferred_confidence, policy)

    # Apply annealing to the blended result
    annealing_state = Hypatia.OutcomeTracker.get_annealing_state(recipe_id)
    annealed = ConfidenceAnnealing.anneal(blended, annealing_state)

    %{
      repo: repo,
      recipe_id: recipe_id,
      local_confidence: local_confidence,
      transferred_confidence: transferred_confidence,
      blended_confidence: annealed,
      local_outcomes: local_count,
      transfer_sources: transfer_sources
    }
  end

  @doc """
  Find repos that have successfully fixed a given pattern/recipe.

  Returns a list of {repo, success_count, total_count} tuples,
  ordered by success rate descending. Useful for cross-repo learning
  queries: "Who has fixed this before?"
  """
  @spec repos_with_fix(String.t()) :: [{String.t(), non_neg_integer(), non_neg_integer()}]
  def repos_with_fix(recipe_id) do
    all_outcomes = load_all_outcomes_for_recipe(recipe_id)

    all_outcomes
    |> Enum.group_by(&Map.get(&1, "repo"))
    |> Enum.map(fn {repo, outcomes} ->
      successes = Enum.count(outcomes, &(Map.get(&1, "outcome") == "success"))
      {repo, successes, length(outcomes)}
    end)
    |> Enum.filter(fn {_repo, _s, total} -> total >= @min_transfer_outcomes end)
    |> Enum.sort_by(fn {_repo, s, total} -> -(s / max(total, 1)) end)
  end

  @doc """
  Detect ecosystem-level anomalies across all repos.

  Scans recent outcomes for patterns that suggest ecosystem-wide issues:
  - A recipe suddenly failing across multiple repos
  - A language family experiencing elevated failure rates
  - A new pattern appearing in many repos simultaneously

  Returns a list of anomaly descriptors.
  """
  @spec detect_ecosystem_anomalies() :: [map()]
  def detect_ecosystem_anomalies do
    recent_outcomes = load_recent_outcomes(@anomaly_time_window_hours)

    recipe_anomalies = detect_recipe_anomalies(recent_outcomes)
    language_anomalies = detect_language_anomalies(recent_outcomes)

    anomalies = recipe_anomalies ++ language_anomalies

    if length(anomalies) > 0 do
      Logger.warning(
        "CrossRepoLearning: #{length(anomalies)} ecosystem anomalies detected"
      )
    end

    anomalies
  end

  @doc """
  Build the cross-repo outcome index.

  Indexes outcomes by {recipe_id, repo} for fast cross-repo lookups.
  Called periodically by LearningScheduler or on-demand.
  """
  @spec rebuild_index() :: {:ok, non_neg_integer()}
  def rebuild_index do
    outcomes_dir = Path.join(Path.expand(@verisimdb_data_path), "outcomes")
    index_dir = Path.expand(@cross_repo_index_path)
    File.mkdir_p!(index_dir)

    # Build repo→recipe→outcomes index from all JSONL files
    index =
      case File.ls(outcomes_dir) do
        {:ok, files} ->
          files
          |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
          |> Enum.reduce(%{}, fn file, acc ->
            path = Path.join(outcomes_dir, file)

            path
            |> File.stream!()
            |> Stream.map(fn line ->
              case Jason.decode(String.trim(line)) do
                {:ok, record} -> record
                {:error, _} -> nil
              end
            end)
            |> Stream.reject(&is_nil/1)
            |> Enum.reduce(acc, fn record, idx ->
              repo = Map.get(record, "repo", "unknown")
              recipe = Map.get(record, "recipe_id", Map.get(record, "pattern", "unknown"))
              outcome = Map.get(record, "outcome", "unknown")

              key = "#{recipe}:#{repo}"
              entry = Map.get(idx, key, %{"successes" => 0, "failures" => 0, "false_positives" => 0, "total" => 0})

              updated = case outcome do
                "success" -> %{entry | "successes" => entry["successes"] + 1, "total" => entry["total"] + 1}
                "failure" -> %{entry | "failures" => entry["failures"] + 1, "total" => entry["total"] + 1}
                "false_positive" -> %{entry | "false_positives" => entry["false_positives"] + 1, "total" => entry["total"] + 1}
                _ -> %{entry | "total" => entry["total"] + 1}
              end

              Map.put(idx, key, Map.merge(updated, %{"repo" => repo, "recipe_id" => recipe}))
            end)
          end)

        {:error, _} -> %{}
      end

    # Write index as JSON
    index_path = Path.join(index_dir, "repo-recipe-index.json")
    case Jason.encode(index, pretty: true) do
      {:ok, json} ->
        File.write!(index_path, json <> "\n")
        Logger.info("CrossRepoLearning: rebuilt index with #{map_size(index)} entries")
        {:ok, map_size(index)}

      {:error, reason} ->
        Logger.error("CrossRepoLearning: failed to write index: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Get the drift policy discount factor for a given policy and repo pair.

  For :language_aware policy, detects the primary language of both repos
  and applies full discount if they're in different language families.
  """
  @spec drift_discount(drift_policy(), String.t(), String.t()) :: float()
  def drift_discount(:language_aware, source_repo, target_repo) do
    source_lang = detect_repo_language(source_repo)
    target_lang = detect_repo_language(target_repo)

    source_family = Map.get(@language_families, source_lang, :unknown)
    target_family = Map.get(@language_families, target_lang, :unknown)

    if source_family == target_family and source_family != :unknown do
      1.0
    else
      @cross_family_discount
    end
  end

  def drift_discount(policy, _source_repo, _target_repo) do
    case Map.get(@drift_policies, policy) do
      :language_dependent -> 0.75  # Fallback if called incorrectly
      factor when is_number(factor) -> factor
      _ -> 0.75
    end
  end

  @doc "Get the list of supported drift policies."
  @spec available_policies() :: [drift_policy()]
  def available_policies, do: Map.keys(@drift_policies)

  @doc "Get the language family groupings."
  @spec language_families() :: map()
  def language_families, do: @language_families

  # --- Private: Confidence Transfer ---

  # Compute transferred confidence from similar repos for a given recipe.
  # Returns {confidence, source_repos} or {nil, []} if no transfer data.
  defp compute_transferred_confidence(recipe_id, target_repo, policy) do
    # Find repos that have outcomes for this recipe
    source_repos = repos_with_fix(recipe_id)

    # Exclude the target repo itself
    transferable =
      source_repos
      |> Enum.reject(fn {repo, _s, _t} -> repo == target_repo end)
      |> Enum.filter(fn {_repo, _s, total} -> total >= @min_transfer_outcomes end)

    if Enum.empty?(transferable) do
      {nil, []}
    else
      # Weighted average of source repo confidences, discounted by policy
      {weighted_sum, total_weight, sources} =
        Enum.reduce(transferable, {0.0, 0.0, []}, fn {repo, successes, total}, {ws, tw, srcs} ->
          raw_conf = successes / max(total, 1)
          discount = drift_discount(policy, repo, target_repo)
          weight = total * discount  # More outcomes = more weight

          {ws + raw_conf * weight, tw + weight, [repo | srcs]}
        end)

      if total_weight > 0 do
        {weighted_sum / total_weight, Enum.reverse(sources)}
      else
        {nil, []}
      end
    end
  end

  # Blend local and transferred confidence.
  # Local evidence always dominates when available; transferred
  # evidence fills in when local data is sparse.
  defp blend_confidence(nil, _local_count, nil, _policy), do: 0.5
  defp blend_confidence(nil, _local_count, transferred, _policy), do: transferred
  defp blend_confidence(local, _local_count, nil, _policy), do: local

  defp blend_confidence(local, local_count, transferred, _policy) do
    # Local weight grows with outcome count; transferred weight
    # is capped. At 15+ local outcomes, local dominates ~85%.
    local_weight = min(local_count, 30) / 30.0
    transfer_weight = 1.0 - local_weight

    local * local_weight + transferred * transfer_weight
  end

  # --- Private: Outcome Loading ---

  defp load_repo_outcomes(recipe_id, repo) do
    outcomes_dir = Path.join(Path.expand(@verisimdb_data_path), "outcomes")

    case File.ls(outcomes_dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
        |> Enum.flat_map(fn f ->
          path = Path.join(outcomes_dir, f)

          path
          |> File.stream!()
          |> Stream.filter(&(String.contains?(&1, recipe_id) and String.contains?(&1, repo)))
          |> Stream.map(fn line ->
            case Jason.decode(String.trim(line)) do
              {:ok, record} -> record
              {:error, _} -> nil
            end
          end)
          |> Stream.reject(&is_nil/1)
          |> Stream.filter(fn r ->
            (Map.get(r, "recipe_id") == recipe_id or Map.get(r, "pattern") == recipe_id) and
              Map.get(r, "repo") == repo
          end)
          |> Enum.to_list()
        end)

      {:error, _} -> []
    end
  end

  defp load_all_outcomes_for_recipe(recipe_id) do
    outcomes_dir = Path.join(Path.expand(@verisimdb_data_path), "outcomes")

    case File.ls(outcomes_dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
        |> Enum.flat_map(fn f ->
          path = Path.join(outcomes_dir, f)

          path
          |> File.stream!()
          |> Stream.filter(&String.contains?(&1, recipe_id))
          |> Stream.map(fn line ->
            case Jason.decode(String.trim(line)) do
              {:ok, record} -> record
              {:error, _} -> nil
            end
          end)
          |> Stream.reject(&is_nil/1)
          |> Stream.filter(fn r ->
            Map.get(r, "recipe_id") == recipe_id or Map.get(r, "pattern") == recipe_id
          end)
          |> Enum.to_list()
        end)

      {:error, _} -> []
    end
  end

  defp load_recent_outcomes(hours) do
    outcomes_dir = Path.join(Path.expand(@verisimdb_data_path), "outcomes")
    cutoff = DateTime.utc_now() |> DateTime.add(-hours * 3600, :second)
    cutoff_str = DateTime.to_iso8601(cutoff)

    case File.ls(outcomes_dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
        |> Enum.flat_map(fn f ->
          path = Path.join(outcomes_dir, f)

          path
          |> File.stream!()
          |> Stream.map(fn line ->
            case Jason.decode(String.trim(line)) do
              {:ok, record} -> record
              {:error, _} -> nil
            end
          end)
          |> Stream.reject(&is_nil/1)
          |> Stream.filter(fn r ->
            ts = Map.get(r, "timestamp", "")
            ts >= cutoff_str
          end)
          |> Enum.to_list()
        end)

      {:error, _} -> []
    end
  end

  # --- Private: Anomaly Detection ---

  # Detect recipes that are failing across multiple repos simultaneously
  defp detect_recipe_anomalies(recent_outcomes) do
    recent_outcomes
    |> Enum.group_by(fn o ->
      Map.get(o, "recipe_id", Map.get(o, "pattern", "unknown"))
    end)
    |> Enum.flat_map(fn {recipe_id, outcomes} ->
      repos = outcomes |> Enum.map(&Map.get(&1, "repo")) |> Enum.uniq()
      failures = Enum.count(outcomes, &(Map.get(&1, "outcome") != "success"))
      total = length(outcomes)
      failure_rate = if total > 0, do: failures / total, else: 0.0

      if failure_rate >= @anomaly_failure_rate_threshold and
         length(repos) >= @anomaly_min_repos do
        [%{
          type: :recipe_degradation,
          recipe_id: recipe_id,
          failure_rate: Float.round(failure_rate, 3),
          affected_repos: length(repos),
          total_outcomes: total,
          repos: Enum.take(repos, 10),
          severity: if(failure_rate >= 0.5, do: :critical, else: :warning),
          detected_at: DateTime.utc_now() |> DateTime.to_iso8601()
        }]
      else
        []
      end
    end)
  end

  # Detect language families with elevated failure rates
  defp detect_language_anomalies(recent_outcomes) do
    recent_outcomes
    |> Enum.group_by(fn o ->
      repo = Map.get(o, "repo", "")
      lang = detect_repo_language(repo)
      Map.get(@language_families, lang, :unknown)
    end)
    |> Enum.reject(fn {family, _} -> family == :unknown end)
    |> Enum.flat_map(fn {family, outcomes} ->
      repos = outcomes |> Enum.map(&Map.get(&1, "repo")) |> Enum.uniq()
      failures = Enum.count(outcomes, &(Map.get(&1, "outcome") != "success"))
      total = length(outcomes)
      failure_rate = if total > 0, do: failures / total, else: 0.0

      if failure_rate >= @anomaly_failure_rate_threshold and
         length(repos) >= @anomaly_min_repos do
        [%{
          type: :language_family_anomaly,
          language_family: family,
          failure_rate: Float.round(failure_rate, 3),
          affected_repos: length(repos),
          total_outcomes: total,
          severity: if(failure_rate >= 0.5, do: :critical, else: :warning),
          detected_at: DateTime.utc_now() |> DateTime.to_iso8601()
        }]
      else
        []
      end
    end)
  end

  # --- Private: Language Detection ---

  # Detect primary language for a repo from scan data.
  # Falls back to "unknown" if scan data isn't available.
  defp detect_repo_language(repo) do
    scan_path = Path.join([Path.expand(@verisimdb_data_path), "scans", "#{repo}.json"])

    case File.read(scan_path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, data} ->
            # Scan data may contain language info directly or in weak_points
            cond do
              Map.has_key?(data, "primary_language") ->
                String.downcase(Map.get(data, "primary_language", "unknown"))

              Map.has_key?(data, "languages") ->
                data
                |> Map.get("languages", %{})
                |> Enum.max_by(fn {_lang, count} -> count end, fn -> {"unknown", 0} end)
                |> elem(0)
                |> String.downcase()

              true ->
                "unknown"
            end

          {:error, _} -> "unknown"
        end

      {:error, _} -> "unknown"
    end
  end
end
