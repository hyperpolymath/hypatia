# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Neural.TrainingPipeline do
  @moduledoc """
  Training data extraction and network training for ESN and RBF.

  Reads real outcome and pattern data from verisim-data,
  transforms it into training format, and trains the networks.

  Data sources:
  - ESN: outcomes/*.jsonl -> confidence time series per recipe
  - RBF: patterns/registry.json -> 8-D feature vectors
  """

  require Logger

  alias Hypatia.{OutcomeTracker, Paths}

  alias Hypatia.Neural.{
    EchoStateNetwork,
    RadialNeuralNetwork,
    Rebalancer
  }

  # --- ESN Training ---

  @doc """
  Build ESN training data from outcome JSONL files.

  Reads `.jsonl` files from the configured outcomes directory and retains
  objects qualified by `OutcomeTracker` with a non-blank string identity.
  It uses `recipe_id`, falling back to `pattern` when `recipe_id` is nil or
  false. A blank or invalid `recipe_id` does not fall back to `pattern`.

  Each recipe's observations are sorted by `timestamp`, falling back to
  `fixed_at` and then an empty string when the preceding value is nil or
  false. The series is the cumulative success rate in that order; qualified
  failures and false positives count as non-successes.

  Returns a list of maps sorted by series length (longest first), each
  containing :recipe_id, :series (list of floats 0.0-1.0), and :count.
  Only recipes with at least 10 qualified observations are included. Invalid
  JSON lines and unreadable files are ignored; if the directory cannot be
  listed or no recipe meets the threshold, returns an empty list.
  """
  def build_esn_training_data, do: build_esn_training_data(Paths.verisimdb_data())

  @doc false
  def build_esn_training_data(data_root) when is_binary(data_root) do
    outcomes_dir = Path.join(Path.expand(data_root), "outcomes")

    outcomes =
      case File.ls(outcomes_dir) do
        {:ok, files} ->
          files
          |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
          |> Enum.flat_map(fn file ->
            path = Path.join(outcomes_dir, file)

            case File.read(path) do
              {:ok, content} ->
                content
                |> String.split("\n", trim: true)
                |> Enum.map(fn line ->
                  case Jason.decode(line) do
                    {:ok, data} -> data
                    _ -> nil
                  end
                end)
                |> Enum.reject(&is_nil/1)

              _ ->
                []
            end
          end)

        _ ->
          []
      end

    build_esn_training_data(outcomes)
  end

  def build_esn_training_data(outcomes) when is_list(outcomes) do
    # Use the same evidence qualification as Bayesian and cross-repo learning.
    # Exit-zero-only, incomplete scans and contradictory successes remain audit
    # records, but must not become positive neural training observations.
    # Group by recipe_id, build running confidence series (cumulative success rate).
    # The canonical outcome schema keys on "pattern" + "fixed_at" (~96% of the
    # corpus); only the small fleet-import slice carries "recipe_id"/"timestamp".
    # Fall back to those the same way GraphOfTrust.build_edges/1 does, otherwise
    # 6682/6964 outcomes collapse into a single nil-keyed, unordered blob and the
    # ESN forecaster trains on one meaningless blended trajectory.
    outcomes
    |> Enum.filter(fn outcome ->
      is_map(outcome) and OutcomeTracker.qualified_outcome?(outcome) and
        not is_nil(recipe_identity(outcome))
    end)
    |> Enum.group_by(&recipe_identity/1)
    |> Enum.map(fn {recipe_id, recipe_outcomes} ->
      # Sort by timestamp for temporal ordering (fixed_at is the canonical field)
      sorted =
        Enum.sort_by(
          recipe_outcomes,
          &(Map.get(&1, "timestamp") || Map.get(&1, "fixed_at") || "")
        )

      # Build confidence time series (running average of successes)
      {series, _} =
        Enum.map_reduce(sorted, {0, 0}, fn outcome, {successes, total} ->
          new_total = total + 1

          new_successes =
            if Map.get(outcome, "outcome") == "success",
              do: successes + 1,
              else: successes

          confidence = new_successes / new_total
          {confidence, {new_successes, new_total}}
        end)

      %{recipe_id: recipe_id, series: series, count: length(series)}
    end)
    |> Enum.filter(fn entry -> entry.count >= 10 end)
    |> Enum.sort_by(& &1.count, :desc)
  end

  defp recipe_identity(outcome) do
    case Map.get(outcome, "recipe_id") || Map.get(outcome, "pattern") do
      id when is_binary(id) -> if String.trim(id) == "", do: nil, else: id
      _ -> nil
    end
  end

  @doc """
  Train ESN on accumulated confidence trajectories.

  Uses the longest available recipe confidence series (must be >= 60
  data points for the ESN reservoir to learn meaningful dynamics).
  Returns the trained ESN struct, or the original if insufficient data.

  Options:
    * `:rebalance` — inject synthetic regression events into the
      training series to counter the ~99% success skew
      (default `true`; set to `false` to reproduce the pre-rebalance
      behaviour). See `Hypatia.Neural.Rebalancer.augment_esn_series/2`.
    * `:rebalance_opts` — keyword passed through to the rebalancer
      (e.g. `[rate: 0.05, dip_depth: 0.4, seed: 42]`).
  """
  def train_esn(esn, opts \\ []) do
    data_root = Keyword.get(opts, :data_path, Paths.verisimdb_data())
    training_data = build_esn_training_data(data_root)

    if training_data == [] do
      Logger.warning("TrainingPipeline: no ESN training data available")
      esn
    else
      # Use the longest series for primary training
      primary = hd(training_data)

      if primary.count >= 60 do
        rebalance = Keyword.get(opts, :rebalance, true)
        rebalance_opts = Keyword.get(opts, :rebalance_opts, [])

        rebalance_strategy = Keyword.get(opts, :rebalance_strategy, :a)

        series =
          if rebalance do
            case rebalance_strategy do
              :b ->
                Logger.info(
                  "TrainingPipeline: rebalancing ESN series for #{primary.recipe_id} (adversarial patterns)"
                )

                Rebalancer.adversarial_esn_series(primary.series, rebalance_opts)

              :c ->
                Logger.info(
                  "TrainingPipeline: rebalancing ESN series for #{primary.recipe_id} (corpus dips)"
                )

                data_path = Keyword.get(rebalance_opts, :data_path, data_root)
                templates = Rebalancer.load_failure_templates(data_path)

                if templates == [] do
                  Logger.warning(
                    "TrainingPipeline: no failure templates found, falling back to Strategy A"
                  )

                  Rebalancer.augment_esn_series(primary.series, rebalance_opts)
                else
                  Logger.info("TrainingPipeline: #{length(templates)} failure templates loaded")
                  Rebalancer.corpus_esn_series(primary.series, templates, rebalance_opts)
                end

              _ ->
                Logger.info(
                  "TrainingPipeline: rebalancing ESN series for #{primary.recipe_id} (synthetic regressions)"
                )

                Rebalancer.augment_esn_series(primary.series, rebalance_opts)
            end
          else
            primary.series
          end

        Logger.info(
          "TrainingPipeline: training ESN on #{primary.recipe_id} (#{primary.count} points)"
        )

        trained_esn = EchoStateNetwork.train(esn, series)

        # Log availability of additional series for future cross-validation
        other_count = length(training_data) - 1

        Logger.info(
          "TrainingPipeline: ESN trained. #{other_count} additional series available for validation"
        )

        trained_esn
      else
        Logger.warning(
          "TrainingPipeline: longest ESN series only #{primary.count} points (need 60+)"
        )

        esn
      end
    end
  end

  # --- RBF Training ---

  @doc """
  Build RBF training data from pattern registry.

  Reads patterns/registry.json, converts each pattern into an 8-D
  feature vector via RadialNeuralNetwork.finding_to_vector/1, and
  maps the safety triangle tier to a numeric target:
  - eliminate -> 1.0
  - substitute -> 0.5
  - control -> 0.0

  Returns {vectors, targets} where vectors is a list of 8-element
  lists and targets is a list of floats.
  """
  def build_rbf_training_data(data_root \\ Paths.verisimdb_data()) do
    registry_path = Path.join([Path.expand(data_root), "patterns", "registry.json"])

    patterns =
      case File.read(registry_path) do
        {:ok, content} ->
          case Jason.decode(content) do
            # Direct array of pattern objects
            {:ok, data} when is_list(data) ->
              data

            # Object with "patterns" key containing an array
            {:ok, %{"patterns" => patterns}} when is_list(patterns) ->
              patterns

            # Object with "patterns" key containing a map keyed by pattern ID
            # (canonical verisim-data format: {"patterns": {"PA005-...": {...}, ...}})
            {:ok, %{"patterns" => patterns}} when is_map(patterns) ->
              Map.values(patterns)

            _ ->
              []
          end

        {:error, reason} ->
          Logger.warning("TrainingPipeline: cannot read #{registry_path}: #{inspect(reason)}")
          []
      end

    if patterns == [] do
      Logger.warning("TrainingPipeline: no patterns found in registry")
      {[], []}
    else
      # Convert patterns to 8-D feature vectors + triangle tier targets
      {vectors, targets} =
        patterns
        |> Enum.map(fn pattern -> {pattern_to_finding(pattern), tier_to_target(pattern)} end)
        |> Enum.map(fn {finding, target} ->
          {RadialNeuralNetwork.finding_to_vector(finding), target}
        end)
        |> Enum.unzip()

      Logger.info("TrainingPipeline: built #{length(vectors)} RBF training vectors (8-D)")
      {vectors, targets}
    end
  end

  @doc """
  Train RBF on pattern registry data.

  Requires at least 5 patterns to train (minimum for RBF center
  placement). Returns the trained RBF struct, or the original if
  insufficient data.

  Options:
    * `:rebalance` — append synthetic 8-D feature vectors covering
      under-represented regions of the feature space, with
      rule-based targets matching the safety-triangle heuristic
      (default `true`). See
      `Hypatia.Neural.Rebalancer.rebalance_rbf/2`.
    * `:rebalance_opts` — keyword passed through to the rebalancer
      (e.g. `[count: 30, seed: 42]`).
  """
  def train_rbf(rbf, opts \\ []) do
    data_root = Keyword.get(opts, :data_path, Paths.verisimdb_data())
    {real_vectors, real_targets} = build_rbf_training_data(data_root)

    {vectors, targets} =
      if Keyword.get(opts, :rebalance, true) and real_vectors != [] do
        Logger.info("TrainingPipeline: rebalancing RBF training set (synthetic coverage)")

        Rebalancer.rebalance_rbf(
          {real_vectors, real_targets},
          Keyword.get(opts, :rebalance_opts, [])
        )
      else
        {real_vectors, real_targets}
      end

    if vectors == [] or length(vectors) < 5 do
      Logger.warning(
        "TrainingPipeline: insufficient RBF training data (#{length(vectors)} vectors)"
      )

      rbf
    else
      Logger.info(
        "TrainingPipeline: training RBF on #{length(vectors)} vectors " <>
          "(#{length(real_vectors)} real + #{length(vectors) - length(real_vectors)} synthetic)"
      )

      RadialNeuralNetwork.train(rbf, vectors, targets)
    end
  end

  # --- Full Training ---

  @doc """
  Run full training pipeline for both ESN and RBF.

  Initializes fresh network instances, trains both on available data
  from verisim-data, and returns a map with `:esn` and `:rbf` keys.

  `:data_path` captures the source for training reads. An explicit
  `:rebalance_opts` data path remains a deliberate Strategy C corpus override.
  Options are forwarded to `train_esn/2` and `train_rbf/2` — see those
  functions for the `:rebalance` and `:rebalance_opts` knobs.
  """
  def run_full_training(opts \\ []) do
    Logger.info("TrainingPipeline: starting full training run")

    esn = EchoStateNetwork.init()
    rbf = RadialNeuralNetwork.init()

    trained_esn = train_esn(esn, opts)
    trained_rbf = train_rbf(rbf, opts)

    esn_status = if trained_esn.trained, do: "trained", else: "untrained"
    rbf_status = if trained_rbf.trained, do: "trained", else: "untrained"

    Logger.info("TrainingPipeline: complete. ESN=#{esn_status}, RBF=#{rbf_status}")

    %{esn: trained_esn, rbf: trained_rbf}
  end

  # --- Helpers ---

  # Convert a pattern registry entry into the finding map format expected
  # by RadialNeuralNetwork.finding_to_vector/1 (8 string-keyed fields).
  #
  # The canonical verisim-data registry format uses these fields:
  #   severity (title-case), category, occurrences, repos_affected,
  #   first_seen, last_seen, triangle_tier, pa_rule, recipe_id, trend
  #
  # We map these to the 8-D feature vector fields, using sensible
  # defaults where the registry does not carry a direct equivalent.
  defp pattern_to_finding(pattern) do
    %{
      "severity" => pattern |> Map.get("severity", "medium") |> String.downcase(),
      "confidence" => Map.get(pattern, "confidence", 0.5),
      "complexity" => Map.get(pattern, "complexity", 0.5),
      "frequency" =>
        Map.get(
          pattern,
          "frequency",
          Map.get(pattern, "occurrences", Map.get(pattern, "count", 1))
        )
        |> normalize_frequency(),
      "fix_rate" => Map.get(pattern, "fix_rate", 0.5),
      "category" => Map.get(pattern, "category", ""),
      "age_days" => Map.get(pattern, "age_days", compute_age_days(pattern)),
      "affected_files" =>
        Map.get(
          pattern,
          "affected_files",
          Map.get(pattern, "repos_affected", Map.get(pattern, "affected_repos", 1))
        )
    }
  end

  # Compute age in days from the "first_seen" ISO-8601 timestamp.
  # Returns a default of 30 days if the field is missing or unparseable.
  defp compute_age_days(pattern) do
    case Map.get(pattern, "first_seen") do
      nil ->
        30

      timestamp when is_binary(timestamp) ->
        case DateTime.from_iso8601(timestamp) do
          {:ok, dt, _offset} ->
            DateTime.diff(DateTime.utc_now(), dt, :day)

          _ ->
            # Try NaiveDateTime for timestamps without timezone
            case NaiveDateTime.from_iso8601(timestamp) do
              {:ok, ndt} ->
                NaiveDateTime.diff(NaiveDateTime.utc_now(), ndt, :day)

              _ ->
                30
            end
        end

      _ ->
        30
    end
  end

  # Normalize frequency values: if > 1.0 treat as raw count and scale to 0-1 range.
  defp normalize_frequency(freq) when is_number(freq) and freq > 1.0,
    do: min(freq / 100.0, 1.0)

  defp normalize_frequency(freq) when is_number(freq), do: freq
  defp normalize_frequency(_), do: 0.5

  # Map safety triangle tier names to numeric regression targets.
  defp tier_to_target(pattern) do
    case Map.get(pattern, "triangle_tier", Map.get(pattern, "tier", "control")) do
      "eliminate" -> 1.0
      "substitute" -> 0.5
      "control" -> 0.0
      _ -> 0.0
    end
  end
end
