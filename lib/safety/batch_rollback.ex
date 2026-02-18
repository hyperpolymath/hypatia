# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.Safety.BatchRollback do
  @moduledoc """
  Batch rollback capability for Hypatia dispatches.

  Every batch of dispatches gets a unique batch_id. If a batch
  causes problems (e.g., a recipe turns out to produce broken fixes),
  the entire batch can be rolled back by:

  1. Marking all outcomes in the batch as false_positive
  2. Reverting recipe confidence to pre-batch levels
  3. Generating rollback instructions for the fleet
  4. Recording the rollback in ArangoDB for learning

  This is a safety net for the auto_execute tier.
  """

  require Logger

  @verisimdb_data_path "~/Documents/hyperpolymath-repos/verisimdb-data"

  @doc "Create a new batch and return its ID"
  def create_batch(dispatch_count, strategy) do
    batch_id = "batch_#{System.system_time(:millisecond)}"
    ts = DateTime.utc_now() |> DateTime.to_iso8601()

    batch = %{
      "batch_id" => batch_id,
      "timestamp" => ts,
      "dispatch_count" => dispatch_count,
      "strategy" => to_string(strategy),
      "status" => "pending"
    }

    # Persist to ArangoDB if available
    if Process.whereis(Hypatia.Data.ArangoDB) do
      Hypatia.Data.ArangoDB.upsert("dispatch_batches", batch_id, batch)
    end

    # Also write to verisimdb-data dispatch directory
    batch_path = Path.join([Path.expand(@verisimdb_data_path), "dispatch", "batches.jsonl"])
    File.write(batch_path, Jason.encode!(batch) <> "\n", [:append, :utf8])

    {:ok, batch_id}
  end

  @doc "Mark a batch as completed"
  def complete_batch(batch_id) do
    update_batch_status(batch_id, "completed")
  end

  @doc "Roll back an entire batch"
  def rollback(batch_id, reason) do
    Logger.warning("ROLLBACK batch #{batch_id}: #{reason}")

    # 1. Find all outcomes for this batch
    outcomes = find_batch_outcomes(batch_id)

    # 2. Mark all as false_positive
    Enum.each(outcomes, fn outcome ->
      recipe_id = Map.get(outcome, "recipe_id")
      if recipe_id do
        # Revert confidence
        Hypatia.OutcomeTracker.record_outcome(
          recipe_id,
          Map.get(outcome, "repo", "unknown"),
          Map.get(outcome, "file", ""),
          :false_positive
        )
      end
    end)

    # 3. Generate rollback manifest
    rollback_manifest = %{
      "batch_id" => batch_id,
      "reason" => reason,
      "timestamp" => DateTime.utc_now() |> DateTime.to_iso8601(),
      "affected_outcomes" => length(outcomes),
      "action" => "revert",
      "instructions" => Enum.map(outcomes, fn o ->
        %{
          "repo" => Map.get(o, "repo"),
          "file" => Map.get(o, "file"),
          "action" => "git revert"
        }
      end)
    }

    rollback_path = Path.join([Path.expand(@verisimdb_data_path), "dispatch", "rollbacks.jsonl"])
    File.write(rollback_path, Jason.encode!(rollback_manifest) <> "\n", [:append, :utf8])

    # 4. Update batch status
    update_batch_status(batch_id, "rolled_back")

    # 5. Record anomaly
    if Process.whereis(Hypatia.Data.ArangoDB) do
      Hypatia.Data.ArangoDB.record_anomaly(
        %{"batch_id" => batch_id, "reason" => reason},
        0.0, :rollback, :critical)
    end

    Logger.warning("Rollback complete: #{length(outcomes)} outcomes reverted for batch #{batch_id}")
    {:ok, length(outcomes)}
  end

  @doc "List recent batches with their status"
  def list_batches(limit \\ 20) do
    if Process.whereis(Hypatia.Data.ArangoDB) do
      case Hypatia.Data.ArangoDB.query("""
        FOR b IN dispatch_batches
          SORT b.timestamp DESC
          LIMIT @limit
          RETURN b
      """, %{"limit" => limit}) do
        {:ok, batches} -> batches
        _ -> list_batches_from_file(limit)
      end
    else
      list_batches_from_file(limit)
    end
  end

  # --- Internal ---

  defp find_batch_outcomes(batch_id) do
    if Process.whereis(Hypatia.Data.ArangoDB) do
      case Hypatia.Data.ArangoDB.query("""
        FOR o IN outcomes
          FILTER o.batch_id == @batch_id
          RETURN o
      """, %{"batch_id" => batch_id}) do
        {:ok, outcomes} -> outcomes
        _ -> find_outcomes_from_file(batch_id)
      end
    else
      find_outcomes_from_file(batch_id)
    end
  end

  defp find_outcomes_from_file(batch_id) do
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
              |> Enum.map(&Jason.decode/1)
              |> Enum.filter(fn
                {:ok, o} -> Map.get(o, "batch_id") == batch_id
                _ -> false
              end)
              |> Enum.map(fn {:ok, o} -> o end)
            _ -> []
          end
        end)
      _ -> []
    end
  end

  defp update_batch_status(batch_id, status) do
    if Process.whereis(Hypatia.Data.ArangoDB) do
      Hypatia.Data.ArangoDB.upsert("dispatch_batches", batch_id, %{
        "batch_id" => batch_id,
        "status" => status,
        "updated" => DateTime.utc_now() |> DateTime.to_iso8601()
      })
    end
  end

  defp list_batches_from_file(limit) do
    batch_path = Path.join([Path.expand(@verisimdb_data_path), "dispatch", "batches.jsonl"])
    case File.read(batch_path) do
      {:ok, content} ->
        content
        |> String.split("\n", trim: true)
        |> Enum.map(&Jason.decode/1)
        |> Enum.filter(&match?({:ok, _}, &1))
        |> Enum.map(fn {:ok, b} -> b end)
        |> Enum.reverse()
        |> Enum.take(limit)
      _ -> []
    end
  end
end
