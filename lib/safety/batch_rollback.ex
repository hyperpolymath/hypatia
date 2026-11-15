# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Safety.BatchRollback do
  @moduledoc """
  Batch rollback capability for Hypatia dispatches.

  Every batch of dispatches gets a unique batch_id. If a batch
  causes problems (e.g., a recipe turns out to produce broken fixes),
  the entire batch can be rolled back by:

  1. Marking all outcomes in the batch as false_positive
  2. Reverting recipe confidence to pre-batch levels
  3. Generating rollback instructions for the fleet
  4. Recording the rollback for learning

  This is a safety net for the auto_execute tier.
  """

  require Logger

  @verisimdb_data_path Application.compile_env(:hypatia, :verisimdb_data_path, "data/verisim")

  @doc "Create a new batch and return its ID"
  def create_batch(dispatch_count, strategy) do
    batch_id = "batch_#{System.system_time(:millisecond)}_#{System.unique_integer([:positive])}"
    ts = DateTime.utc_now() |> DateTime.to_iso8601()

    batch = %{
      "batch_id" => batch_id,
      "timestamp" => ts,
      "dispatch_count" => dispatch_count,
      "strategy" => to_string(strategy),
      "status" => "pending"
    }

    batch_path = Path.join([Path.expand(@verisimdb_data_path), "dispatch", "batches.jsonl"])
    File.write(batch_path, Jason.encode!(batch) <> "\n", [:append, :utf8])

    {:ok, batch_id}
  end

  @doc "Mark a batch as completed"
  def complete_batch(batch_id) do
    update_batch_status(batch_id, "completed")
  end

  @doc """
  Roll back an entire batch.

  The manifest lists the revert instructions from the NEWEST outcome to the oldest, so
  that a change that depends on an earlier one is reverted first. The order is the order
  of the outcome log (files by name, one for each month, then the lines of each file),
  from its last position to its first. It is not sorted by `timestamp`, `repo` or `file`:
  none of them is an order, and a tie would revert dependent changes the wrong way round.

  A rollback is safe to repeat (E0-25):

    * a batch whose status is `rolled_back` returns `{:ok, 0}` and writes nothing;
    * the status is set only after every write succeeded. A failed write returns
      `{:error, reason}` and leaves the status as it was, so that a later call can
      finish the rollback;
    * a retry writes no second outcome row for an outcome that already has its rollback
      row. For each key (`recipe_id`, `repo`, `file`), with N original outcomes and M
      rollback rows already in the log, it writes N - M rows (none when M >= N). The
      manifest still lists every original outcome, once.
  """
  def rollback(batch_id, reason) do
    if batch_status(batch_id) == "rolled_back" do
      {:ok, 0}
    else
      do_rollback(batch_id, reason)
    end
  end

  defp do_rollback(batch_id, reason) do
    Logger.warning("ROLLBACK batch #{batch_id}: #{reason}")

    # 1. The rows of this batch, in the order of the log. A row that a rollback wrote
    #    is not an outcome of the batch: it is what a rollback did to one.
    {originals, written} =
      batch_id
      |> find_batch_outcomes()
      |> Enum.split_with(&(Map.get(&1, "caused_by") != "batch_rollback"))

    with :ok <- mark_false_positives(originals, written, batch_id),
         :ok <- write_manifest(batch_id, reason, originals),
         :ok <- set_status(batch_id, "rolled_back") do
      Logger.warning(
        "Rollback complete: #{length(originals)} outcomes reverted for batch #{batch_id}"
      )

      {:ok, length(originals)}
    else
      {:error, why} = error ->
        Logger.error("Rollback of batch #{batch_id} did not complete: #{inspect(why)}")
        error
    end
  end

  # Write one rollback row for each original outcome that has none yet. Outcomes have
  # no stable id, and 2 of them can share a batch, recipe, repo and file, so they are
  # counted by key: the first M originals of a key are the ones that already have a row.
  defp mark_false_positives(originals, written, batch_id) do
    already = Enum.frequencies_by(written, &outcome_key/1)

    originals
    |> Enum.reduce_while({:ok, already}, fn outcome, {:ok, skip} ->
      key = outcome_key(outcome)

      cond do
        is_nil(Map.get(outcome, "recipe_id")) ->
          {:cont, {:ok, skip}}

        Map.get(skip, key, 0) > 0 ->
          {:cont, {:ok, Map.update!(skip, key, &(&1 - 1))}}

        true ->
          case Hypatia.OutcomeTracker.record_batch_rollback(
                 Map.get(outcome, "recipe_id"),
                 Map.get(outcome, "repo", "unknown"),
                 Map.get(outcome, "file", ""),
                 batch_id
               ) do
            {:ok, _record} -> {:cont, {:ok, skip}}
            {:error, why} -> {:halt, {:error, {:outcome_write_failed, why}}}
            other -> {:halt, {:error, {:outcome_write_failed, other}}}
          end
      end
    end)
    |> case do
      {:ok, _skip} -> :ok
      error -> error
    end
  end

  defp outcome_key(outcome) do
    {Map.get(outcome, "recipe_id"), Map.get(outcome, "repo", "unknown"),
     Map.get(outcome, "file", "")}
  end

  # The manifest, newest outcome first. One is written for a batch, ever: a retry after
  # a failed status write must not add a second list of instructions.
  defp write_manifest(batch_id, reason, originals) do
    rollback_path = Path.join([Path.expand(@verisimdb_data_path), "dispatch", "rollbacks.jsonl"])

    if manifest_written?(rollback_path, batch_id) do
      :ok
    else
      manifest = %{
        "batch_id" => batch_id,
        "reason" => reason,
        "timestamp" => DateTime.utc_now() |> DateTime.to_iso8601(),
        "affected_outcomes" => length(originals),
        "action" => "revert",
        "instructions" =>
          originals
          |> Enum.reverse()
          |> Enum.map(fn o ->
            %{
              "repo" => Map.get(o, "repo"),
              "file" => Map.get(o, "file"),
              "action" => "git revert"
            }
          end)
      }

      case File.write(rollback_path, Jason.encode!(manifest) <> "\n", [:append, :utf8]) do
        :ok -> :ok
        {:error, why} -> {:error, {:manifest_write_failed, why}}
      end
    end
  end

  defp manifest_written?(path, batch_id) do
    case File.read(path) do
      {:ok, content} ->
        content
        |> String.split("\n", trim: true)
        |> Enum.any?(fn line ->
          match?({:ok, %{"batch_id" => ^batch_id}}, Jason.decode(line))
        end)

      _ ->
        false
    end
  end

  defp set_status(batch_id, status) do
    case update_batch_status(batch_id, status) do
      :ok -> :ok
      {:error, why} -> {:error, {:status_write_failed, why}}
    end
  end

  # The latest status that the batch log gives for this batch, or nil.
  defp batch_status(batch_id) do
    list_batches_from_file(:infinity)
    |> Enum.find(&(Map.get(&1, "batch_id") == batch_id and Map.has_key?(&1, "status")))
    |> case do
      nil -> nil
      entry -> Map.get(entry, "status")
    end
  end

  @doc "List recent batches with their status"
  def list_batches(limit \\ 20) do
    list_batches_from_file(limit)
  end

  # --- Internal ---

  defp find_batch_outcomes(batch_id) do
    outcomes_dir = Path.join(Path.expand(@verisimdb_data_path), "outcomes")

    case File.ls(outcomes_dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
        # File.ls/1 has no fixed order. One file for each month, so a name sort is a
        # time sort; the lines of a file keep their order.
        |> Enum.sort()
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

            _ ->
              []
          end
        end)

      _ ->
        []
    end
  end

  defp update_batch_status(batch_id, status) do
    batch_path = Path.join([Path.expand(@verisimdb_data_path), "dispatch", "batches.jsonl"])

    update =
      Jason.encode!(%{
        "batch_id" => batch_id,
        "status" => status,
        "updated" => DateTime.utc_now() |> DateTime.to_iso8601()
      })

    File.write(batch_path, update <> "\n", [:append, :utf8])
  end

  defp take_limit(batches, :infinity), do: batches
  defp take_limit(batches, limit), do: Enum.take(batches, limit)

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
        |> take_limit(limit)

      _ ->
        []
    end
  end
end
