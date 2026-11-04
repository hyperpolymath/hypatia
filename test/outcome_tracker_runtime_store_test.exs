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
