# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.OutcomeTracker do
  @moduledoc """
  Closes the feedback loop by recording fix outcomes and updating recipe confidence.

  Writes to:
  - verisim-data/outcomes/YYYY-MM.jsonl (append-only outcome log)
  - verisim-data/recipes/{id}.json (updated confidence scores)
  - gitbot-fleet/shared-context/learning/fix-outcomes.jsonl (fleet learning feed)
  """

  require Logger

  alias Hypatia.ConfidenceAnnealing

  @verisimdb_data_path Application.compile_env(:hypatia, :verisimdb_data_path, "data/verisim")
  @fleet_path Application.compile_env(:hypatia, :fleet_path, "~/Documents/hyperpolymath-repos/gitbot-fleet")

  # Bayesian confidence updating via Beta distribution
  # Prior strength controls how much weight initial confidence carries
  # relative to observed evidence. Higher = more conservative updates.
  @prior_strength 10
  @confidence_cap 0.99
  @confidence_floor 0.10

  # Path for persisted annealing states (per-recipe temperature tracking)
  @annealing_state_path Application.compile_env(
    :hypatia, :annealing_state_path,
    "data/verisim/annealing-states"
  )

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

    # Write to verisim-data outcomes (append-only JSONL per month)
    write_outcome_log(record)

    # Write to fleet learning pipeline
    write_fleet_outcome(record)

    # Update annealing state for this recipe
    update_annealing_state(recipe_id, outcome)

    # Update recipe confidence (now annealing-aware)
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
              Logger.warning("Verification FAILED for #{pattern_id} in #{repo_path} -- pattern still present")
              :still_present
            else
              Logger.info("Verification PASSED for #{pattern_id} in #{repo_path} -- pattern removed")
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
      repos_dir = System.get_env("HYPATIA_REPOS_DIR", File.cwd!())
      repo_path = Keyword.get(opts, :repo_path, Path.join(repos_dir, repo))
      category = Keyword.get(opts, :category, "")
      pattern_id = Keyword.get(opts, :pattern_id, "")

      case verify_fix(repo_path, pattern_id, category) do
        :verified ->
          {:ok, record, :verified}

        :still_present ->
          Logger.warning("Fix claimed success but pattern still present -- recording false_positive")
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
  Updates the recipe JSON file in verisim-data/recipes/.
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

                raw_confidence = bayesian_update(
                  base_confidence, successes, failures + false_positives
                )

                # Apply temperature-based annealing to the raw confidence.
                # This flattens confidence toward 0.5 when the recipe has
                # few outcomes, and lets it sharpen as evidence accumulates.
                annealing_state = load_annealing_state(recipe_id)
                new_confidence = ConfidenceAnnealing.anneal(raw_confidence, annealing_state)
                annealing_summary = ConfidenceAnnealing.summary(annealing_state)

                updated_recipe =
                  recipe
                  |> Map.put("confidence", Float.round(new_confidence, 4))
                  |> Map.put("confidence_raw", Float.round(raw_confidence, 4))
                  |> Map.put("successful_fixes", successes)
                  |> Map.put("failed_fixes", failures)
                  |> Map.put("false_positives", false_positives)
                  |> Map.put("total_attempts", length(outcomes))
                  |> Map.put("annealing", annealing_summary)

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

    # H6 — outcome log monotonicity. Was previously proven in
    # verification/proofs/agda/OutcomeLog.agda; that proof was replaced
    # by this runtime assertion + an echidnabot audit over the JSONL.
    # A violation means NTP step, concurrent writer, or the caller
    # supplied a stale timestamp — all investigation-worthy.
    assert_h6_monotone(path, Map.get(record, "timestamp"))

    case Jason.encode(record) do
      {:ok, json} ->
        File.write(path, json <> "\n", [:append, :utf8])

      {:error, reason} ->
        Logger.error("Failed to write outcome log: #{inspect(reason)}")
    end
  end

  # H6 runtime check. ISO-8601 timestamps sort lexicographically, so
  # string comparison gives the right ordering without DateTime parsing
  # on the hot path.
  defp assert_h6_monotone(path, new_ts) when is_binary(new_ts) do
    case tail_timestamp(path) do
      {:ok, last_ts} when new_ts > last_ts -> :ok
      {:ok, last_ts} ->
        Logger.error(
          "H6 violation: outcome log non-monotone at #{path} " <>
            "(last=#{last_ts}, new=#{new_ts})"
        )
      :none -> :ok
    end
  end

  defp assert_h6_monotone(_path, _), do: :ok

  defp tail_timestamp(path) do
    with true <- File.exists?(path),
         [line] <- path |> File.stream!() |> Enum.take(-1),
         {:ok, %{"timestamp" => ts}} when is_binary(ts) <-
           line |> String.trim_trailing() |> Jason.decode() do
      {:ok, ts}
    else
      _ -> :none
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

          # Stream lines and pre-filter by string containment before
          # JSON decoding. This avoids parsing thousands of irrelevant
          # JSONL records and prevents timeout cascades on large datasets.
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
            Map.get(r, "recipe_id") == recipe_id or
              Map.get(r, "pattern") == recipe_id
          end)
          |> Enum.to_list()
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

  # --- Annealing State Management ---

  @doc """
  Get the annealing state for a recipe, computing the max dispatch tier.

  Returns {:ok, strategy} where strategy is the annealing-clamped
  dispatch tier for the given raw strategy.
  """
  def annealed_strategy(recipe_id, raw_strategy) do
    state = load_annealing_state(recipe_id)
    ConfidenceAnnealing.clamp_strategy(raw_strategy, state)
  end

  @doc """
  Get annealing-adjusted confidence for a recipe in a cross-repo context.

  When repo A's outcome data is used to inform dispatch in repo B,
  this applies a higher effective temperature (more conservative).
  """
  def cross_repo_confidence(recipe_id, raw_confidence) do
    state = load_annealing_state(recipe_id)
    ConfidenceAnnealing.anneal_cross_repo(raw_confidence, state)
  end

  @doc "Get the full annealing state for a recipe (for diagnostics)."
  def get_annealing_state(recipe_id) do
    load_annealing_state(recipe_id)
  end

  defp update_annealing_state(recipe_id, outcome) do
    state = load_annealing_state(recipe_id)

    outcome_atom = case outcome do
      :success -> :success
      :failure -> :failure
      :false_positive -> :false_positive
      _ -> :failure
    end

    new_state = ConfidenceAnnealing.record_outcome(state, outcome_atom)
    save_annealing_state(recipe_id, new_state)
    new_state
  end

  defp load_annealing_state(recipe_id) do
    path = annealing_state_path(recipe_id)

    case File.read(path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, data} -> deserialize_annealing_state(data)
          {:error, _} -> bootstrap_annealing_state(recipe_id)
        end

      {:error, _} ->
        bootstrap_annealing_state(recipe_id)
    end
  end

  # Bootstrap annealing state from existing outcome data for recipes
  # that predate the annealing system.
  defp bootstrap_annealing_state(recipe_id) do
    outcomes = load_outcomes_for_recipe(recipe_id)

    if Enum.empty?(outcomes) do
      ConfidenceAnnealing.new_state()
    else
      successes = Enum.count(outcomes, fn o -> Map.get(o, "outcome") == "success" end)
      failures = Enum.count(outcomes, fn o -> Map.get(o, "outcome") == "failure" end)
      false_positives = Enum.count(outcomes, fn o -> Map.get(o, "outcome") == "false_positive" end)

      state = ConfidenceAnnealing.from_existing(successes, failures, false_positives)
      save_annealing_state(recipe_id, state)
      state
    end
  end

  defp save_annealing_state(recipe_id, state) do
    path = annealing_state_path(recipe_id)
    dir = Path.dirname(path)
    File.mkdir_p!(dir)

    serialized = %{
      "temperature" => state.temperature,
      "outcome_count" => state.outcome_count,
      "stage" => Atom.to_string(state.stage),
      "recent_outcomes" => Enum.map(state.recent_outcomes, &Atom.to_string/1),
      "last_reheat" => if(state.last_reheat, do: DateTime.to_iso8601(state.last_reheat)),
      "created_at" => DateTime.to_iso8601(state.created_at),
      "updated_at" => DateTime.utc_now() |> DateTime.to_iso8601()
    }

    case Jason.encode(serialized, pretty: true) do
      {:ok, json} -> File.write!(path, json <> "\n")
      {:error, reason} -> Logger.error("Failed to save annealing state: #{inspect(reason)}")
    end
  end

  defp deserialize_annealing_state(data) do
    %{
      temperature: Map.get(data, "temperature", 1.0),
      outcome_count: Map.get(data, "outcome_count", 0),
      stage: String.to_existing_atom(Map.get(data, "stage", "nascent")),
      recent_outcomes:
        data
        |> Map.get("recent_outcomes", [])
        |> Enum.map(&String.to_existing_atom/1),
      last_reheat:
        case Map.get(data, "last_reheat") do
          nil -> nil
          ts -> case DateTime.from_iso8601(ts) do
            {:ok, dt, _} -> dt
            _ -> nil
          end
        end,
      created_at:
        case DateTime.from_iso8601(Map.get(data, "created_at", "")) do
          {:ok, dt, _} -> dt
          _ -> DateTime.utc_now()
        end
    }
  end

  defp annealing_state_path(recipe_id) do
    # Sanitise recipe_id for filesystem safety
    safe_id = String.replace(recipe_id, ~r/[^a-zA-Z0-9_\-]/, "_")
    Path.join(Path.expand(@annealing_state_path), "#{safe_id}.json")
  end
end
