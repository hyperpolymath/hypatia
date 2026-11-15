# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# E0-27: BatchRollback.rollback/2 records the outcomes that it marks through the
# trusted entry OutcomeTracker.record_batch_rollback/4, because the public
# record_outcome/5 no longer accepts a rollback_requested row.
defmodule Hypatia.Safety.BatchRollbackTest do
  use ExUnit.Case, async: false

  alias Hypatia.OutcomeTracker
  alias Hypatia.Safety.BatchRollback

  setup do
    data = Application.fetch_env!(:hypatia, :verisimdb_data_path) |> Path.expand()
    outcomes = Path.join(data, "outcomes")
    File.mkdir_p!(outcomes)

    batch = "batch-e0-27-#{System.unique_integer([:positive])}"
    recipe = "recipe-e0-27-#{System.unique_integer([:positive])}"
    log = Path.join(outcomes, "#{batch}.jsonl")

    seed = %{
      "recipe_id" => recipe,
      "repo" => "owner/repo",
      "file" => "a.txt",
      "outcome" => "success",
      "verification" => "verified",
      "batch_id" => batch,
      "timestamp" => DateTime.utc_now() |> DateTime.to_iso8601(),
      "bot" => "hypatia"
    }

    File.write!(log, Jason.encode!(seed) <> "\n")
    on_exit(fn -> File.rm(log) end)

    # The rollback writes into the same store; read the rows that carry this batch.
    rows = fn ->
      outcomes
      |> Path.join("*.jsonl")
      |> Path.wildcard()
      |> Enum.flat_map(&(File.read!(&1) |> String.split("\n", trim: true)))
      |> Enum.map(&Jason.decode!/1)
      |> Enum.filter(&(&1["batch_id"] == batch))
    end

    %{batch: batch, recipe: recipe, rows: rows}
  end

  test "a rollback writes one false positive for each outcome of the batch, and the gate counts it",
       %{batch: batch, recipe: recipe, rows: rows} do
    assert length(rows.()) == 1

    BatchRollback.rollback(batch, "test")

    rollback_rows = Enum.filter(rows.(), &(&1["verification"] == "rollback_requested"))
    assert [row] = rollback_rows
    assert row["recipe_id"] == recipe
    assert row["outcome"] == "false_positive"
    assert row["caused_by"] == "batch_rollback"

    assert OutcomeTracker.qualified_outcome_counts([row]) ==
             %{successes: 0, failures: 0, false_positives: 1, ignored: 0}
  end
end
