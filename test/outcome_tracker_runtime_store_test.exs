# SPDX-License-Identifier: MPL-2.0
defmodule Hypatia.OutcomeTrackerRuntimeStoreTest do
  use ExUnit.Case, async: false

  alias Hypatia.{LearningScheduler, OutcomeTracker}

  setup do
    old_root = Application.fetch_env!(:hypatia, :verisimdb_data_path) |> Path.expand()
    assert String.contains?(old_root, "/_build/test/verisim-data")
    keys = [:verisimdb_data_path, :annealing_state_path, :fleet_path]
    original = Map.new(keys, fn key -> {key, Application.fetch_env(:hypatia, key)} end)
    id = "recipe-runtime-boundary-#{System.unique_integer([:positive])}"
    root = Path.join(System.tmp_dir!(), id)
    for part <- ["recipes", "outcomes"], do: File.mkdir_p!(Path.join(root, part))
    Application.put_env(:hypatia, :verisimdb_data_path, root)
    Application.put_env(:hypatia, :annealing_state_path, Path.join(root, "annealing-states"))
    Application.put_env(:hypatia, :fleet_path, Path.join(root, "fleet"))

    on_exit(fn ->
      for {key, value} <- original do
        case value do
          {:ok, previous} -> Application.put_env(:hypatia, key, previous)
          :error -> Application.delete_env(:hypatia, key)
        end
      end

      File.rm_rf!(root)
    end)

    %{root: root, old_root: old_root, id: id}
  end

  defp write_row(root, name, row) do
    path = Path.join([root, "outcomes", name <> ".jsonl"])
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, Jason.encode!(row) <> "\n")
    path
  end

  test "scheduler-discovered outcomes update only the selected runtime recipe", ctx do
    recipe_name = ctx.id <> ".json"
    current_recipe = Path.join([ctx.root, "recipes", recipe_name])
    previous_recipe = Path.join([ctx.old_root, "recipes", recipe_name])
    previous_log = Path.join([ctx.old_root, "outcomes", ctx.id <> ".jsonl"])
    File.mkdir_p!(Path.dirname(previous_recipe))
    original_recipe = Jason.encode!(%{"id" => ctx.id, "confidence" => 0.2}) <> "\n"
    File.write!(previous_recipe, original_recipe)
    File.write!(current_recipe, Jason.encode!(%{"id" => ctx.id, "confidence" => 0.8}))

    on_exit(fn ->
      File.rm(previous_recipe)
      File.rm(previous_log)
    end)

    base = %{"recipe_id" => ctx.id, "repo" => "synthetic", "timestamp" => "2026-11-04T00:00:00Z"}

    write_row(
      ctx.old_root,
      ctx.id,
      Map.merge(base, %{"outcome" => "success", "verification" => "verified"})
    )

    write_row(
      ctx.root,
      ctx.id,
      Map.merge(base, %{"outcome" => "failure", "verification" => "failed"})
    )

    assert {_, 1, ids} = LearningScheduler.ingest_new_outcomes(%{})
    assert ids == MapSet.new([ctx.id])
    assert {:ok, _confidence} = OutcomeTracker.update_recipe_confidence(ctx.id)
    current = current_recipe |> File.read!() |> Jason.decode!()
    assert current["successful_fixes"] == 0
    assert current["failed_fixes"] == 1
    assert current["confidence_prior"] == 0.8
    assert File.read!(previous_recipe) == original_recipe
    assert File.regular?(Path.join([ctx.root, "annealing-states", ctx.id <> ".json"]))
  end

  defp fleet_row(recipe_id) do
    %{
      "recipe_id" => recipe_id,
      "repo" => "synthetic",
      "file" => ".",
      "bot" => "fixture-bot",
      "outcome" => "failure",
      "verdict" => "failed",
      "exit_code" => 1
    }
  end

  defp write_fleet(fleet_root, content) do
    path = Path.join([fleet_root, "shared-context", "learning", "fix-outcomes.jsonl"])
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, content)
    path
  end

  test "fleet ingestion follows runtime path changes without replay or source echo", ctx do
    first_root = Path.join(ctx.root, "fleet-first")
    second_root = Path.join(ctx.root, "fleet-second")
    first_line = Jason.encode!(fleet_row(ctx.id <> "-first"))
    second_line = Jason.encode!(fleet_row(ctx.id <> "-second"))
    first_file = write_fleet(first_root, first_line <> "\n")
    second_file = write_fleet(second_root, second_line <> "\n")

    Application.put_env(:hypatia, :fleet_path, first_root)
    assert {1, first_signature} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert is_binary(first_signature)
    first_id = LearningScheduler.fleet_source_id(first_line, 1)
    assert {:ok, ids} = OutcomeTracker.imported_fleet_ids()
    assert ids == MapSet.new([first_id])

    [outcome_file] = Path.wildcard(Path.join([ctx.root, "outcomes", "*.jsonl"]))
    before_replay = File.read!(outcome_file)
    assert {0, ^first_signature} = LearningScheduler.ingest_fleet_outcomes(first_signature)
    assert File.read!(outcome_file) == before_replay

    Application.put_env(:hypatia, :fleet_path, second_root)
    assert {1, second_signature} = LearningScheduler.ingest_fleet_outcomes(first_signature)
    assert second_signature != first_signature
    second_id = LearningScheduler.fleet_source_id(second_line, 1)
    assert {:ok, ids} = OutcomeTracker.imported_fleet_ids()
    assert ids == MapSet.new([first_id, second_id])
    assert File.read!(first_file) == first_line <> "\n"
    assert File.read!(second_file) == second_line <> "\n"
    assert {0, ^second_signature} = LearningScheduler.ingest_fleet_outcomes(second_signature)

    Application.put_env(:hypatia, :fleet_path, Path.join(ctx.root, "fleet-missing"))
    before_missing = File.read!(outcome_file)
    assert {0, ^second_signature} = LearningScheduler.ingest_fleet_outcomes(second_signature)
    assert File.read!(outcome_file) == before_missing
  end

  test "runtime fleet ingestion does not credit unverified success or acknowledge a partial tail", ctx do
    fleet_root = Path.join(ctx.root, "fleet-partial")
    Application.put_env(:hypatia, :fleet_path, fleet_root)
    unqualified = Jason.encode!(%{"recipe_id" => ctx.id, "repo" => "synthetic", "outcome" => "success"})
    partial = unqualified <> "\n" <> ~s({"recipe_id":)
    fleet_file = write_fleet(fleet_root, partial)

    assert {0, nil} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert {:ok, ids} = OutcomeTracker.imported_fleet_ids()
    assert ids == MapSet.new()
    assert File.read!(fleet_file) == partial

    qualified = Jason.encode!(fleet_row(ctx.id <> "-qualified"))
    complete = unqualified <> "\n" <> qualified <> "\n"
    File.write!(fleet_file, complete)
    assert {1, signature} = LearningScheduler.ingest_fleet_outcomes(nil)
    qualified_id = LearningScheduler.fleet_source_id(qualified, 2)
    assert {:ok, ids} = OutcomeTracker.imported_fleet_ids()
    assert ids == MapSet.new([qualified_id])
    assert File.read!(fleet_file) == complete
    assert {0, ^signature} = LearningScheduler.ingest_fleet_outcomes(signature)
  end

  test "fleet replay identity lookup follows the same runtime outcome store", ctx do
    write_row(ctx.root, ctx.id, %{"fleet_import_id" => ctx.id, "recipe_id" => ctx.id})
    assert {:ok, ids} = OutcomeTracker.imported_fleet_ids()
    assert ids == MapSet.new([ctx.id])
  end

  test "recording resolves outcome, fleet and annealing destinations at runtime", ctx do
    assert {:ok, _} =
             OutcomeTracker.record_outcome(ctx.id, "synthetic", ".", :failure, %{
               "verification" => "failed"
             })

    [outcome] = Path.wildcard(Path.join([ctx.root, "outcomes", "*.jsonl"]))
    assert File.read!(outcome) =~ ctx.id

    assert File.read!(
             Path.join([ctx.root, "fleet", "shared-context", "learning", "fix-outcomes.jsonl"])
           ) =~ ctx.id

    assert File.regular?(Path.join([ctx.root, "annealing-states", ctx.id <> ".json"]))
  end

  test "an absent annealing override follows the selected data store", ctx do
    Application.delete_env(:hypatia, :annealing_state_path)

    assert {:ok, _} =
             OutcomeTracker.record_fleet_outcome(
               ctx.id,
               "synthetic",
               ".",
               :failure,
               ctx.id,
               "failed"
             )

    assert File.regular?(Path.join([ctx.root, "annealing-states", ctx.id <> ".json"]))
  end
end
