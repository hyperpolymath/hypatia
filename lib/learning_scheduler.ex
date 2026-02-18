# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.LearningScheduler do
  @moduledoc """
  GenServer that automatically closes the feedback loop.

  Periodically:
  1. Checks verisimdb-data/outcomes/ for new JSONL entries
  2. Calls OutcomeTracker.update_recipe_confidence/1 for affected recipes
  3. Checks gitbot-fleet fix-outcomes.jsonl for fleet-reported outcomes
  4. Ingests fleet outcomes back into hypatia's learning pipeline
  5. Reports confidence drift (recipes dropping or rising)

  This is the critical missing piece that makes the learning loop automatic.
  Without it, outcomes sit in JSONL files and never update recipe confidence.
  """

  use GenServer
  require Logger

  @poll_interval_ms 5 * 60 * 1_000  # 5 minutes
  @verisimdb_data_path "~/Documents/hyperpolymath-repos/verisimdb-data"
  @fleet_path "~/Documents/hyperpolymath-repos/gitbot-fleet"

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
      last_outcome_positions: %{},  # filename => byte offset of last read
      total_outcomes_processed: 0,
      total_confidence_updates: 0,
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

    now = DateTime.utc_now()
    total = outcomes_count + fleet_outcomes_count

    if total > 0 do
      Logger.info(
        "LearningScheduler: processed #{total} new outcomes, " <>
        "updated #{confidence_updates} recipes"
      )
    end

    %{state |
      last_run: now,
      last_outcome_positions: new_positions,
      total_outcomes_processed: state.total_outcomes_processed + total,
      total_confidence_updates: state.total_confidence_updates + confidence_updates
    }
  end

  # --- Ingest new outcomes from verisimdb-data/outcomes/*.jsonl ---

  defp ingest_new_outcomes(last_positions) do
    outcomes_dir = Path.join(Path.expand(@verisimdb_data_path), "outcomes")

    case File.ls(outcomes_dir) do
      {:ok, files} ->
        jsonl_files = Enum.filter(files, &String.ends_with?(&1, ".jsonl"))

        Enum.reduce(jsonl_files, {last_positions, 0, MapSet.new()}, fn file, {positions, count, recipes} ->
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

                  {Map.put(positions, file, size), count + length(new_records), MapSet.union(recipes, new_recipe_ids)}

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
    fleet_file = Path.join([
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
                  "DRIFT ALERT: Recipe #{recipe_id} confidence dropped to #{confidence} — " <>
                  "consider disabling auto-execute"
                )
              confidence >= 0.95 ->
                Logger.info("Recipe #{recipe_id} at high confidence #{confidence} — auto-execute eligible")
              true ->
                :ok
            end
          end
        end)

      {:error, _} ->
        :ok
    end
  end
end
