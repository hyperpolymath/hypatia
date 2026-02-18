# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.Neural.TrainingPipeline do
  @moduledoc """
  Training data extraction and network training for ESN and RBF.

  Reads real outcome and pattern data from verisimdb-data,
  transforms it into training format, and trains the networks.

  Data sources:
  - ESN: outcomes/*.jsonl -> confidence time series per recipe
  - RBF: patterns/registry.json -> 8-D feature vectors
  """

  require Logger

  alias Hypatia.Neural.{
    EchoStateNetwork,
    RadialNeuralNetwork
  }

  @verisimdb_data_path "~/Documents/hyperpolymath-repos/verisimdb-data"

  # --- ESN Training ---

  @doc """
  Build ESN training data from outcome JSONL files.

  Reads all .jsonl files from the outcomes/ directory, parses each line
  as a JSON object, groups by recipe_id, and builds a running confidence
  time series (cumulative success rate) for each recipe.

  Returns a list of maps sorted by series length (longest first), each
  containing :recipe_id, :series (list of floats 0.0-1.0), and :count.
  Only recipes with >= 10 data points are included.
  """
  def build_esn_training_data do
    outcomes_dir = Path.join(Path.expand(@verisimdb_data_path), "outcomes")

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

    # Group by recipe_id, build running confidence series (cumulative success rate)
    outcomes
    |> Enum.group_by(&Map.get(&1, "recipe_id"))
    |> Enum.map(fn {recipe_id, recipe_outcomes} ->
      # Sort by timestamp for temporal ordering
      sorted = Enum.sort_by(recipe_outcomes, &Map.get(&1, "timestamp", ""))

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

  @doc """
  Train ESN on accumulated confidence trajectories.

  Uses the longest available recipe confidence series (must be >= 60
  data points for the ESN reservoir to learn meaningful dynamics).
  Returns the trained ESN struct, or the original if insufficient data.
  """
  def train_esn(esn) do
    training_data = build_esn_training_data()

    if training_data == [] do
      Logger.warning("TrainingPipeline: no ESN training data available")
      esn
    else
      # Use the longest series for primary training
      primary = hd(training_data)

      if primary.count >= 60 do
        Logger.info(
          "TrainingPipeline: training ESN on #{primary.recipe_id} (#{primary.count} points)"
        )

        trained_esn = EchoStateNetwork.train(esn, primary.series)

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
  def build_rbf_training_data do
    registry_path = Path.join(Path.expand(@verisimdb_data_path), "patterns/registry.json")

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
            # (canonical verisimdb-data format: {"patterns": {"PA005-...": {...}, ...}})
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
  """
  def train_rbf(rbf) do
    {vectors, targets} = build_rbf_training_data()

    if vectors == [] or length(vectors) < 5 do
      Logger.warning(
        "TrainingPipeline: insufficient RBF training data (#{length(vectors)} vectors)"
      )

      rbf
    else
      Logger.info("TrainingPipeline: training RBF on #{length(vectors)} patterns")
      RadialNeuralNetwork.train(rbf, vectors, targets)
    end
  end

  # --- Full Training ---

  @doc """
  Run full training pipeline for both ESN and RBF.

  Initializes fresh network instances, trains both on available data
  from verisimdb-data, and returns a map with :esn and :rbf keys.
  """
  def run_full_training do
    Logger.info("TrainingPipeline: starting full training run")

    esn = EchoStateNetwork.init()
    rbf = RadialNeuralNetwork.init()

    trained_esn = train_esn(esn)
    trained_rbf = train_rbf(rbf)

    esn_status = if trained_esn.trained, do: "trained", else: "untrained"
    rbf_status = if trained_rbf.trained, do: "trained", else: "untrained"

    Logger.info("TrainingPipeline: complete. ESN=#{esn_status}, RBF=#{rbf_status}")

    %{esn: trained_esn, rbf: trained_rbf}
  end

  # --- Helpers ---

  # Convert a pattern registry entry into the finding map format expected
  # by RadialNeuralNetwork.finding_to_vector/1 (8 string-keyed fields).
  #
  # The canonical verisimdb-data registry format uses these fields:
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
        Map.get(pattern, "frequency", Map.get(pattern, "occurrences", Map.get(pattern, "count", 1)))
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
