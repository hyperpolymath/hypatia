# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.OutcomeTracker do
  @moduledoc """
  Closes the feedback loop by recording fix outcomes and updating recipe confidence.

  Writes to:
  - verisimdb-data/outcomes/YYYY-MM.jsonl (append-only outcome log)
  - verisimdb-data/recipes/{id}.json (updated confidence scores)
  - gitbot-fleet/shared-context/learning/fix-outcomes.jsonl (fleet learning feed)
  """

  require Logger

  @verisimdb_data_path "~/Documents/hyperpolymath-repos/verisimdb-data"
  @fleet_path "~/Documents/hyperpolymath-repos/gitbot-fleet"

  # Confidence adjustment constants (from plan)
  @successful_fix_boost 0.02
  @failed_fix_penalty -0.05
  @false_positive_penalty -0.10
  @confidence_cap 0.99
  @confidence_floor 0.10

  @doc """
  Record an outcome from a fix attempt.

  Parameters:
  - recipe_id: which recipe was applied
  - repo: repository name
  - file: file that was fixed
  - outcome: :success | :failure | :false_positive
  """
  def record_outcome(recipe_id, repo, file, outcome) do
    now = DateTime.utc_now() |> DateTime.to_iso8601()
    outcome_str = Atom.to_string(outcome)

    record = %{
      "pattern_id" => nil,
      "recipe_id" => recipe_id,
      "repo" => repo,
      "file" => file,
      "outcome" => outcome_str,
      "timestamp" => now,
      "bot" => "hypatia"
    }

    # Write to verisimdb-data outcomes (append-only JSONL per month)
    write_outcome_log(record)

    # Write to fleet learning pipeline
    write_fleet_outcome(record)

    # Update recipe confidence
    update_recipe_confidence(recipe_id)

    Logger.info("Outcome recorded: #{recipe_id} in #{repo}/#{file} -> #{outcome_str}")
    {:ok, record}
  end

  @doc """
  Recalculate confidence for a recipe based on all recorded outcomes.
  Updates the recipe JSON file in verisimdb-data/recipes/.
  """
  def update_recipe_confidence(recipe_id) do
    outcomes = load_outcomes_for_recipe(recipe_id)

    if Enum.empty?(outcomes) do
      :no_outcomes
    else
      successes = Enum.count(outcomes, fn o -> Map.get(o, "outcome") == "success" end)
      failures = Enum.count(outcomes, fn o -> Map.get(o, "outcome") == "failure" end)
      false_positives = Enum.count(outcomes, fn o -> Map.get(o, "outcome") == "false_positive" end)

      # Calculate confidence adjustment from baseline
      adjustment =
        successes * @successful_fix_boost +
          failures * @failed_fix_penalty +
          false_positives * @false_positive_penalty

      # Load current recipe and apply adjustment
      recipe_path = find_recipe_file(recipe_id)

      if recipe_path do
        case File.read(recipe_path) do
          {:ok, content} ->
            case Jason.decode(content) do
              {:ok, recipe} ->
                base_confidence = Map.get(recipe, "confidence", 0.5)

                new_confidence =
                  (base_confidence + adjustment)
                  |> max(@confidence_floor)
                  |> min(@confidence_cap)

                updated_recipe =
                  recipe
                  |> Map.put("confidence", Float.round(new_confidence, 4))
                  |> Map.put("successful_fixes", successes)
                  |> Map.put("failed_fixes", failures)
                  |> Map.put("total_attempts", length(outcomes))

                case Jason.encode(updated_recipe, pretty: true) do
                  {:ok, json} ->
                    File.write!(recipe_path, json <> "\n")
                    Logger.info("Updated #{recipe_id} confidence: #{base_confidence} -> #{new_confidence}")
                    {:ok, new_confidence}

                  {:error, reason} ->
                    Logger.error("Failed to encode recipe: #{inspect(reason)}")
                    {:error, reason}
                end

              {:error, reason} ->
                Logger.error("Failed to decode recipe: #{inspect(reason)}")
                {:error, reason}
            end

          {:error, reason} ->
            Logger.error("Failed to read recipe file: #{inspect(reason)}")
            {:error, reason}
        end
      else
        Logger.warning("No recipe file found for #{recipe_id}")
        :recipe_not_found
      end
    end
  end

  @doc """
  Check if confidence for a recipe is trending up or down.
  Returns :improving, :declining, or :stable.
  """
  def confidence_trend(recipe_id) do
    outcomes = load_outcomes_for_recipe(recipe_id)

    if length(outcomes) < 3 do
      :insufficient_data
    else
      # Look at last 10 outcomes
      recent = Enum.take(outcomes, -10)
      success_rate = Enum.count(recent, fn o -> Map.get(o, "outcome") == "success" end) / length(recent)

      cond do
        success_rate >= 0.8 -> :improving
        success_rate <= 0.4 -> :declining
        true -> :stable
      end
    end
  end

  # --- Private ---

  defp write_outcome_log(record) do
    {{year, month, _}, _} = :calendar.universal_time()
    month_str = String.pad_leading("#{month}", 2, "0")
    filename = "#{year}-#{month_str}.jsonl"
    path = Path.join([Path.expand(@verisimdb_data_path), "outcomes", filename])

    case Jason.encode(record) do
      {:ok, json} ->
        File.write(path, json <> "\n", [:append, :utf8])

      {:error, reason} ->
        Logger.error("Failed to write outcome log: #{inspect(reason)}")
    end
  end

  defp write_fleet_outcome(record) do
    path =
      Path.join([
        Path.expand(@fleet_path),
        "shared-context/learning/fix-outcomes.jsonl"
      ])

    fleet_record = %{
      "pattern" => Map.get(record, "recipe_id", "unknown"),
      "file" => Map.get(record, "file", "unknown"),
      "repo" => Map.get(record, "repo", "unknown"),
      "outcome" => Map.get(record, "outcome"),
      "fixed_at" => Map.get(record, "timestamp"),
      "bot" => "hypatia"
    }

    case Jason.encode(fleet_record) do
      {:ok, json} ->
        File.write(path, json <> "\n", [:append, :utf8])

      {:error, reason} ->
        Logger.error("Failed to write fleet outcome: #{inspect(reason)}")
    end
  end

  defp load_outcomes_for_recipe(recipe_id) do
    outcomes_dir = Path.join(Path.expand(@verisimdb_data_path), "outcomes")

    case File.ls(outcomes_dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
        |> Enum.flat_map(fn f ->
          path = Path.join(outcomes_dir, f)

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
              |> Enum.filter(fn r -> Map.get(r, "recipe_id") == recipe_id end)

            {:error, _} ->
              []
          end
        end)

      {:error, _} ->
        []
    end
  end

  defp find_recipe_file(recipe_id) do
    recipes_dir = Path.join(Path.expand(@verisimdb_data_path), "recipes")

    case File.ls(recipes_dir) do
      {:ok, files} ->
        case Enum.find(files, fn f ->
               String.starts_with?(f, "recipe-") and String.ends_with?(f, ".json") and
                 match_recipe_id?(Path.join(recipes_dir, f), recipe_id)
             end) do
          nil -> nil
          file -> Path.join(recipes_dir, file)
        end

      {:error, _} ->
        nil
    end
  end

  defp match_recipe_id?(path, recipe_id) do
    case File.read(path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, data} -> Map.get(data, "id") == recipe_id
          {:error, _} -> false
        end

      {:error, _} ->
        false
    end
  end
end
