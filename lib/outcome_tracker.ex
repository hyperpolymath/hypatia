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

  @verisimdb_data_path Application.compile_env(:hypatia, :verisimdb_data_path, "data/verisimdb")
  @fleet_path Application.compile_env(:hypatia, :fleet_path, "~/Documents/hyperpolymath-repos/gitbot-fleet")

  # Bayesian confidence updating via Beta distribution
  # Prior strength controls how much weight initial confidence carries
  # relative to observed evidence. Higher = more conservative updates.
  @prior_strength 10
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
  Verify a fix by re-scanning the target repo and checking if the weak point is gone.

  Runs panic-attacker assail on the specific repo, then checks if the original
  pattern still appears in the results. Returns :verified (gone), :still_present,
  or :scan_failed.
  """
  def verify_fix(repo_path, pattern_id, category) do
    case System.cmd("panic-attack", ["assail", repo_path, "--output-format", "json", "--quiet"],
           stderr_to_stdout: true) do
      {output, 0} ->
        case Jason.decode(output) do
          {:ok, scan} ->
            weak_points = Map.get(scan, "weak_points", [])
            still_found = Enum.any?(weak_points, fn wp ->
              Map.get(wp, "category", "") == category
            end)

            if still_found do
              Logger.warning("Verification FAILED for #{pattern_id} in #{repo_path} — pattern still present")
              :still_present
            else
              Logger.info("Verification PASSED for #{pattern_id} in #{repo_path} — pattern removed")
              :verified
            end

          {:error, _} ->
            Logger.error("Failed to parse re-scan output for #{repo_path}")
            :scan_failed
        end

      {_output, _code} ->
        Logger.error("Re-scan failed for #{repo_path}")
        :scan_failed
    end
  end

  @doc """
  Record an outcome and optionally verify the fix by re-scanning.

  If verify: true is passed, runs panic-attacker against the repo after
  recording the outcome. If the pattern is still present, records a
  :false_positive to correct the confidence.
  """
  def record_and_verify(recipe_id, repo, file, outcome, opts \\ []) do
    {:ok, record} = record_outcome(recipe_id, repo, file, outcome)

    if Keyword.get(opts, :verify, false) and outcome == :success do
      repo_path = Keyword.get(opts, :repo_path, "/var/mnt/eclipse/repos/#{repo}")
      category = Keyword.get(opts, :category, "")
      pattern_id = Keyword.get(opts, :pattern_id, "")

      case verify_fix(repo_path, pattern_id, category) do
        :verified ->
          {:ok, record, :verified}

        :still_present ->
          Logger.warning("Fix claimed success but pattern still present — recording false_positive")
          record_outcome(recipe_id, repo, file, :false_positive)
          {:ok, record, :false_positive}

        :scan_failed ->
          {:ok, record, :scan_unavailable}
      end
    else
      {:ok, record, :not_verified}
    end
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

      recipe_path = find_recipe_file(recipe_id)

      if recipe_path do
        case File.read(recipe_path) do
          {:ok, content} ->
            case Jason.decode(content) do
              {:ok, recipe} ->
                base_confidence = Map.get(recipe, "confidence", 0.5)

                new_confidence = bayesian_update(
                  base_confidence, successes, failures + false_positives
                )

                updated_recipe =
                  recipe
                  |> Map.put("confidence", Float.round(new_confidence, 4))
                  |> Map.put("successful_fixes", successes)
                  |> Map.put("failed_fixes", failures)
                  |> Map.put("false_positives", false_positives)
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
  Bayesian confidence update using Beta distribution conjugate prior.

  The recipe's initial confidence is treated as the prior mean of a Beta(alpha, beta)
  distribution. Each success increments alpha, each failure increments beta.
  The posterior mean (alpha / (alpha + beta)) is the updated confidence.

  Properties:
  - With no observations, returns the prior (initial confidence)
  - Many successes push confidence toward 1.0, but asymptotically
  - A single failure among many successes has proportional (not outsized) impact
  - Small sample sizes preserve uncertainty (stay close to prior)
  - @prior_strength controls how conservative updates are
  """
  def bayesian_update(prior_confidence, successes, failures) do
    # Convert prior confidence to Beta distribution parameters
    # prior_confidence = alpha / (alpha + beta), with alpha + beta = @prior_strength
    alpha_prior = prior_confidence * @prior_strength
    beta_prior = (1.0 - prior_confidence) * @prior_strength

    # Posterior = prior + observations (conjugate update)
    alpha_posterior = alpha_prior + successes
    beta_posterior = beta_prior + failures

    # Posterior mean
    posterior = alpha_posterior / (alpha_posterior + beta_posterior)

    posterior
    |> max(@confidence_floor)
    |> min(@confidence_cap)
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
