# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.LearningScheduler do
  @moduledoc """
  GenServer that automatically closes the feedback loop.

  Periodically:
  1. Checks verisim-data/outcomes/ for new JSONL entries
  2. Calls OutcomeTracker.update_recipe_confidence/1 for affected recipes
  3. Checks gitbot-fleet fix-outcomes.jsonl for fleet-reported outcomes
  4. Ingests fleet outcomes back into hypatia's learning pipeline
  5. Reports confidence drift (recipes dropping or rising)

  This is the critical missing piece that makes the learning loop automatic.
  Without it, outcomes sit in JSONL files and never update recipe confidence.
  """

  use GenServer
  require Logger

  alias Hypatia.ConfidenceAnnealing

  # 5 minutes
  @poll_interval_ms 5 * 60 * 1_000
  @verisimdb_data_path Application.compile_env(:hypatia, :verisimdb_data_path, "data/verisim")
  @fleet_path Application.compile_env(
                :hypatia,
                :fleet_path,
                "~/Documents/hyperpolymath-repos/gitbot-fleet"
              )
  @annealing_state_path Application.compile_env(
                          :hypatia,
                          :annealing_state_path,
                          "data/verisim/annealing-states"
                        )

  # --- Client API ---

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Force an immediate learning cycle (for testing/manual trigger)"
  def run_now do
    GenServer.cast(__MODULE__, :run_cycle)
  end

  @doc "Get the current state (last run, outcomes processed, etc.)"
  def status do
    GenServer.call(__MODULE__, :status)
  end

  # --- GenServer Callbacks ---

  @impl true
  def init(_opts) do
    state = %{
      last_run: nil,
      # filename => byte offset of last read
      last_outcome_positions: %{},
      total_outcomes_processed: 0,
      total_confidence_updates: 0,
      total_annealing_reheats: 0,
      started_at: DateTime.utc_now(),
      errors: []
    }

    # Schedule first poll after a short delay (let app boot)
    Process.send_after(self(), :poll, 5_000)
    Logger.info("LearningScheduler started. Polling every #{div(@poll_interval_ms, 1000)}s.")
    {:ok, state}
  end

  @impl true
  def handle_info(:poll, state) do
    new_state = run_learning_cycle(state)
    Process.send_after(self(), :poll, @poll_interval_ms)
    {:noreply, new_state}
  end

  @impl true
  def handle_cast(:run_cycle, state) do
    new_state = run_learning_cycle(state)
    {:noreply, new_state}
  end

  @impl true
  def handle_call(:status, _from, state) do
    {:reply, state, state}
  end

  # --- Core Learning Cycle ---

  defp run_learning_cycle(state) do
    Logger.info("LearningScheduler: running feedback cycle...")

    {new_positions, outcomes_count, recipe_ids} =
      ingest_new_outcomes(state.last_outcome_positions)

    fleet_outcomes_count = ingest_fleet_outcomes()

    confidence_updates = update_affected_recipes(recipe_ids)

    report_confidence_drift()

    # Report annealing state across all tracked recipes
    annealing_report = report_annealing_state()

    # N4: Detect strategy-shift events and re-queue candidates.
    shift_events = detect_and_requeue_strategy_shifts()

    # N5: Retrain ProverRecommender from VeriSimDB proof_attempts.
    # Reads the growing proof_attempts table (written by echidnabot's
    # VeriSimWriter) and re-fits per-class RBF networks. This closes the
    # echidna → VeriSimDB → Hypatia learning loop: new attempts from
    # echidnabot immediately influence the next prover hint recommendation.
    proof_model_updated = retrain_prover_recommender()

    now = DateTime.utc_now()
    total = outcomes_count + fleet_outcomes_count

    if total > 0 do
      Logger.info(
        "LearningScheduler: processed #{total} new outcomes, " <>
          "updated #{confidence_updates} recipes, " <>
          "annealing: #{annealing_report.nascent} nascent, " <>
          "#{annealing_report.adolescent} adolescent, " <>
          "#{annealing_report.mature} mature, " <>
          "#{annealing_report.veteran} veteran, " <>
          "#{annealing_report.reheated} reheated"
      )
    end

    if length(shift_events) > 0 do
      Logger.info("LearningScheduler: detected #{length(shift_events)} strategy shifts")
    end

    if proof_model_updated do
      Logger.info("LearningScheduler: prover recommender retrained from VeriSimDB proof_attempts")
    end

    %{
      state
      | last_run: now,
        last_outcome_positions: new_positions,
        total_outcomes_processed: state.total_outcomes_processed + total,
        total_confidence_updates: state.total_confidence_updates + confidence_updates,
        total_annealing_reheats: state.total_annealing_reheats + annealing_report.reheated,
        total_strategy_shifts: Map.get(state, :total_strategy_shifts, 0) + length(shift_events),
        proof_model_retrain_count:
          Map.get(state, :proof_model_retrain_count, 0) + if(proof_model_updated, do: 1, else: 0)
    }
  end

  # --- N5: ProverRecommender retraining from VeriSimDB proof_attempts --------

  # Fetches proof_attempts from VeriSimDB and re-fits ProverRecommender's
  # per-class RBF networks. Returns true if training succeeded, false if
  # VeriSimDB was unreachable or returned too few samples.
  #
  # Minimum sample threshold (50) prevents overfitting on early sparse data.
  # The existing in-memory models continue serving recommendations until
  # a successful retrain replaces them.
  defp retrain_prover_recommender do
    base_url = System.get_env("HYPATIA_VERISIM_URL") || "http://localhost:8080"

    try do
      case Hypatia.Neural.ProverRecommender.train_from_verisim(base_url: base_url) do
        {:ok, models} ->
          sample_size = Map.get(models, :sample_size, 0)

          if sample_size >= 50 do
            # Store the retrained models in the ProverRecommender agent/GenServer.
            # ProverRecommender.update_models/1 swaps the live model atomically.
            :ok = Hypatia.Neural.ProverRecommender.update_models(models)
            true
          else
            Logger.debug(
              "LearningScheduler: prover_recommender skip (only #{sample_size} samples, need 50)"
            )

            false
          end

        {:error, reason} ->
          Logger.debug(
            "LearningScheduler: prover_recommender retrain skipped -- #{inspect(reason)}"
          )

          false
      end
    rescue
      e ->
        Logger.warning("LearningScheduler: prover_recommender retrain error: #{inspect(e)}")
        false
    end
  end

  # --- N4: Strategy-shift detection + re-queueing ----------------------

  # Runs StrategyDrift.check_all_shifts/1 on each tick. For each shift
  # event, enqueues the candidate failed-attempt IDs via echidnabot's
  # submitProofObligation mutation with the new top prover as hint.
  # Logs but does not block on re-queueing errors.
  defp detect_and_requeue_strategy_shifts do
    base_url = System.get_env("HYPATIA_VERISIM_URL") || "http://localhost:8080"

    try do
      events = Hypatia.Rules.StrategyDrift.check_all_shifts(base_url: base_url)

      Enum.each(events, fn {:shift, class, old_top, new_top, candidates} ->
        Logger.info(
          "strategy shift: class=#{class} #{old_top} → #{new_top} " <>
            "(#{length(candidates)} candidates to re-queue)"
        )

        requeue_candidates(class, new_top, candidates)
      end)

      events
    rescue
      e ->
        Logger.warning("StrategyDrift check failed: #{inspect(e)}")
        []
    end
  end

  # Re-queue each candidate attempt_id via echidnabot. We construct a
  # fresh submitProofObligation mutation with the class and new prover
  # hint. Failures logged but non-fatal -- the scheduler must keep
  # running even if echidnabot is unreachable.
  defp requeue_candidates(_class, _new_top, []), do: :ok

  defp requeue_candidates(class, new_top, candidates) do
    echidnabot_url = System.get_env("HYPATIA_ECHIDNABOT_URL") || "http://localhost:9001/graphql"

    success_count =
      candidates
      # rate-limit: max 20 re-queues per tick
      |> Enum.take(20)
      |> Enum.count(fn attempt_id ->
        submit_requeue(echidnabot_url, class, new_top, attempt_id)
      end)

    if success_count > 0 do
      Logger.info(
        "  re-queued #{success_count}/#{length(candidates)} attempts via echidnabot (capped at 20/tick)"
      )
    end

    :ok
  end

  defp submit_requeue(url, class, prover, attempt_id) do
    # Best-effort GraphQL mutation. The prover field is an enum in
    # echidnabot's schema; we send it unquoted as the enum literal.
    prover_upper = prover |> String.upcase() |> String.replace("-", "_")

    claim = "requeue-of-#{attempt_id}"
    context = "strategy-shift class=#{class}"

    mutation = """
    mutation {
      submitProofObligation(input: {
        repo: "hyperpolymath/requeue",
        claim: "#{claim}",
        context: "#{context}",
        prover: #{prover_upper},
        inline: true
      }) { success proofId }
    }
    """

    body = Jason.encode!(%{query: mutation})

    case http_post_json(url, body, 5_000) do
      {:ok, response_body} ->
        case Jason.decode(response_body) do
          {:ok, %{"data" => %{"submitProofObligation" => %{"success" => true}}}} -> true
          _ -> false
        end

      {:error, _} ->
        false
    end
  end

  defp http_post_json(url, body, timeout_ms) do
    Application.ensure_all_started(:inets)
    Application.ensure_all_started(:ssl)

    case :httpc.request(
           :post,
           {String.to_charlist(url), [{~c"accept", ~c"application/json"}], ~c"application/json",
            body},
           [{:timeout, timeout_ms}],
           []
         ) do
      {:ok, {{_, status, _}, _headers, resp_body}} when status in 200..299 ->
        {:ok, List.to_string(resp_body)}

      {:ok, {{_, status, _}, _, _}} ->
        {:error, {:http_status, status}}

      {:error, reason} ->
        {:error, {:transport, reason}}
    end
  end

  # --- Ingest new outcomes from verisim-data/outcomes/*.jsonl ---

  defp ingest_new_outcomes(last_positions) do
    outcomes_dir = Path.join(Path.expand(@verisimdb_data_path), "outcomes")

    case File.ls(outcomes_dir) do
      {:ok, files} ->
        jsonl_files = Enum.filter(files, &String.ends_with?(&1, ".jsonl"))

        Enum.reduce(jsonl_files, {last_positions, 0, MapSet.new()}, fn file,
                                                                       {positions, count, recipes} ->
          path = Path.join(outcomes_dir, file)
          last_offset = Map.get(positions, file, 0)

          case File.stat(path) do
            {:ok, %{size: size}} when size > last_offset ->
              case File.open(path, [:read, :binary]) do
                {:ok, fd} ->
                  :file.position(fd, last_offset)
                  new_data = IO.read(fd, :eof)
                  File.close(fd)

                  new_records =
                    new_data
                    |> to_string()
                    |> String.split("\n", trim: true)
                    |> Enum.map(fn line ->
                      case Jason.decode(line) do
                        {:ok, record} -> record
                        {:error, _} -> nil
                      end
                    end)
                    |> Enum.reject(&is_nil/1)

                  new_recipe_ids =
                    new_records
                    |> Enum.map(fn r -> Map.get(r, "recipe_id") end)
                    |> Enum.reject(&is_nil/1)
                    |> MapSet.new()

                  {Map.put(positions, file, size), count + length(new_records),
                   MapSet.union(recipes, new_recipe_ids)}

                {:error, _} ->
                  {positions, count, recipes}
              end

            _ ->
              {positions, count, recipes}
          end
        end)

      {:error, _} ->
        {last_positions, 0, MapSet.new()}
    end
  end

  # --- Ingest outcomes from fleet fix-outcomes.jsonl ---

  defp ingest_fleet_outcomes do
    fleet_file =
      Path.join([
        Path.expand(@fleet_path),
        "shared-context/learning/fix-outcomes.jsonl"
      ])

    case File.read(fleet_file) do
      {:ok, content} ->
        records =
          content
          |> String.split("\n", trim: true)
          |> Enum.map(fn line ->
            case Jason.decode(line) do
              {:ok, record} -> record
              {:error, _} -> nil
            end
          end)
          |> Enum.reject(&is_nil/1)

        # Process any fleet outcomes that don't have hypatia as the source
        fleet_only =
          Enum.filter(records, fn r ->
            Map.get(r, "bot") != "hypatia"
          end)

        # Record each fleet outcome into hypatia's tracker
        Enum.each(fleet_only, fn outcome ->
          recipe_id = Map.get(outcome, "pattern", "unknown")
          repo = Map.get(outcome, "repo", "unknown")
          file = Map.get(outcome, "file", "unknown")

          outcome_atom =
            case Map.get(outcome, "outcome", "failure") do
              "success" -> :success
              "failure" -> :failure
              "false_positive" -> :false_positive
              _ -> :failure
            end

          Hypatia.OutcomeTracker.record_outcome(recipe_id, repo, file, outcome_atom)
        end)

        length(fleet_only)

      {:error, _} ->
        0
    end
  end

  # --- Update confidence for all affected recipes ---

  defp update_affected_recipes(recipe_ids) do
    recipe_ids
    |> MapSet.to_list()
    |> Enum.map(fn recipe_id ->
      case Hypatia.OutcomeTracker.update_recipe_confidence(recipe_id) do
        {:ok, _confidence} -> 1
        _ -> 0
      end
    end)
    |> Enum.sum()
  end

  # --- Report any recipes with concerning confidence drift ---

  defp report_confidence_drift do
    recipes_dir = Path.join(Path.expand(@verisimdb_data_path), "recipes")

    case File.ls(recipes_dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".json"))
        |> Enum.each(fn file ->
          path = Path.join(recipes_dir, file)

          with {:ok, content} <- File.read(path),
               {:ok, recipe} <- Jason.decode(content) do
            confidence = Map.get(recipe, "confidence", 0.5)
            recipe_id = Map.get(recipe, "id", file)

            cond do
              confidence <= 0.3 ->
                Logger.warning(
                  "DRIFT ALERT: Recipe #{recipe_id} confidence dropped to #{confidence} -- " <>
                    "consider disabling auto-execute"
                )

              confidence >= 0.95 ->
                Logger.info(
                  "Recipe #{recipe_id} at high confidence #{confidence} -- auto-execute eligible"
                )

              true ->
                :ok
            end
          end
        end)

      {:error, _} ->
        :ok
    end
  end

  # --- Annealing State Reporting ---

  # Scans all persisted annealing state files and produces a summary
  # of stage distribution and drift/reheat counts.
  defp report_annealing_state do
    annealing_dir = Path.expand(@annealing_state_path)

    stage_counts = %{nascent: 0, adolescent: 0, mature: 0, veteran: 0}
    reheated = 0

    case File.ls(annealing_dir) do
      {:ok, files} ->
        Enum.reduce(
          Enum.filter(files, &String.ends_with?(&1, ".json")),
          %{stages: stage_counts, reheated: reheated},
          fn file, acc ->
            path = Path.join(annealing_dir, file)

            case File.read(path) do
              {:ok, content} ->
                case Jason.decode(content) do
                  {:ok, data} ->
                    stage = String.to_existing_atom(Map.get(data, "stage", "nascent"))

                    drift =
                      ConfidenceAnnealing.drift_detected?(
                        data
                        |> Map.get("recent_outcomes", [])
                        |> Enum.map(&String.to_existing_atom/1)
                      )

                    stages = Map.update!(acc.stages, stage, &(&1 + 1))
                    reheated = if drift, do: acc.reheated + 1, else: acc.reheated
                    %{acc | stages: stages, reheated: reheated}

                  {:error, _} ->
                    acc
                end

              {:error, _} ->
                acc
            end
          end
        )
        |> then(fn acc ->
          Map.merge(acc.stages, %{reheated: acc.reheated})
        end)

      {:error, _} ->
        Map.merge(stage_counts, %{reheated: 0})
    end
  end
end
