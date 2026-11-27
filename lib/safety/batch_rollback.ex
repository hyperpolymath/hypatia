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

  Only one rollback of a batch runs at a time, across VMs and OS processes on the same
  store: it takes the exclusive lock file `<store>/dispatch/locks/<batch_id>.lock`. When
  another holder has it, the call returns `{:error, {:locked, holder}}` and writes
  nothing. `holder` has the holder's `os_pid`, `node`, `acquired_at`, `age_s`, `lock_path`
  and `stale` (older than 15 minutes). A stale lock is reported, never taken over.
  A batch id that is not a plain file name returns `{:error, {:invalid_batch_id, id}}`.

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
    # Two locks, for two scopes. The outer `:global.trans` queues the callers in this VM
    # (and in connected nodes), so that overlapping callers wait their turn and each sees
    # the status the one before it wrote. The inner lock file is the one that holds between
    # separate VMs and OS processes on the same store: an exclusive create (O_EXCL) that
    # the filesystem makes atomic. Without the outer lock, same-VM callers would fail
    # with `{:error, {:locked, _}}` instead of waiting. The status is read again inside both.
    # The lock is not held across hosts: a store that two hosts write through separate
    # checkouts is out of scope (git is what merges those).
    with :ok <- check_batch_id(batch_id) do
      :global.trans({{__MODULE__, batch_id}, self()}, fn ->
        with_lock_file(batch_id, fn -> locked_rollback(batch_id, reason) end)
      end)
    end
  end

  # The batch id is a file name here, so it must be a plain one.
  defp check_batch_id(batch_id) do
    if is_binary(batch_id) and batch_id =~ ~r/\A[A-Za-z0-9][A-Za-z0-9_.-]{0,199}\z/ and
         not String.contains?(batch_id, "..") do
      :ok
    else
      {:error, {:invalid_batch_id, batch_id}}
    end
  end

  # A lock older than this is reported as `"stale" => true`. It is still never taken over:
  # a rollback can be slow, and the pid of a holder on another VM cannot be checked, so the
  # age is evidence for the operator, not a licence to steal. To clear a stale lock, read
  # the holder in the error, confirm that the process is gone, and remove the file at
  # `"lock_path"`.
  @stale_after_s 15 * 60

  defp lock_path(batch_id),
    do: Path.join([Path.expand(@verisimdb_data_path), "dispatch", "locks", "#{batch_id}.lock"])

  # Run `fun` while this process holds the lock file of the batch. Fails closed: when the
  # lock cannot be taken, `fun` does not run and nothing is written. The lock is removed
  # in `after` (also when `fun` raises), and only if it still carries our token.
  defp with_lock_file(batch_id, fun) do
    path = lock_path(batch_id)

    case acquire_lock(path) do
      {:ok, token} ->
        try do
          fun.()
        after
          release_lock(path, token)
        end

      {:error, _} = error ->
        fail(batch_id, error)
    end
  end

  defp acquire_lock(path) do
    token = Base.encode16(:crypto.strong_rand_bytes(8), case: :lower)

    holder = %{
      "token" => token,
      "os_pid" => System.pid(),
      "node" => Atom.to_string(node()),
      "acquired_unix" => System.os_time(:second),
      "acquired_at" => DateTime.utc_now() |> DateTime.to_iso8601()
    }

    with :ok <- mkdir_for_lock(path) do
      case File.open(path, [:write, :exclusive]) do
        {:ok, io} -> write_holder(io, path, holder, token)
        {:error, :eexist} -> {:error, {:locked, read_holder(path)}}
        {:error, why} -> {:error, {:lock_failed, why}}
      end
    end
  end

  defp mkdir_for_lock(path) do
    case File.mkdir_p(Path.dirname(path)) do
      :ok -> :ok
      {:error, why} -> {:error, {:lock_failed, why}}
    end
  end

  # A lock that was created but could not be written is removed again: an empty lock
  # would block every later rollback of the batch.
  defp write_holder(io, path, holder, token) do
    result =
      try do
        IO.binwrite(io, Jason.encode!(holder))
      rescue
        e -> {:error, e}
      end

    File.close(io)

    case result do
      :ok ->
        {:ok, token}

      other ->
        File.rm(path)
        {:error, {:lock_failed, other}}
    end
  end

  defp read_holder(path) do
    info =
      case File.read(path) do
        {:ok, content} ->
          case Jason.decode(content) do
            {:ok, %{} = holder} -> holder
            _ -> %{"unreadable" => true}
          end

        {:error, why} ->
          %{"unreadable" => true, "read_error" => inspect(why)}
      end

    age = System.os_time(:second) - (info["acquired_unix"] || 0)

    info
    |> Map.put("lock_path", path)
    |> Map.put("age_s", age)
    |> Map.put("stale", age > @stale_after_s)
  end

  defp release_lock(path, token) do
    case File.read(path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, %{"token" => ^token}} -> File.rm(path)
          _ -> Logger.error("Lock #{path} is no longer ours; left in place")
        end

      _ ->
        Logger.error("Lock #{path} was gone at release")
    end

    :ok
  end

  defp locked_rollback(batch_id, reason) do
    case batch_status(batch_id) do
      {:ok, "rolled_back"} -> {:ok, 0}
      {:ok, _status} -> do_rollback(batch_id, reason)
      {:error, _} = error -> fail(batch_id, error)
    end
  end

  defp fail(batch_id, {:error, why} = error) do
    Logger.error("Rollback of batch #{batch_id} did not complete: #{inspect(why)}")
    error
  end

  defp do_rollback(batch_id, reason) do
    Logger.warning("ROLLBACK batch #{batch_id}: #{reason}")

    # 1. The rows of this batch, in the order of the log. A row that a rollback wrote
    #    is not an outcome of the batch: it is what a rollback did to one.
    with {:ok, outcomes} <- find_batch_outcomes(batch_id),
         {originals, written} =
           Enum.split_with(outcomes, &(Map.get(&1, "caused_by") != "batch_rollback")),
         :ok <- mark_false_positives(originals, written, batch_id),
         :ok <- write_manifest(batch_id, reason, originals),
         :ok <- set_status(batch_id, "rolled_back") do
      Logger.warning(
        "Rollback complete: #{length(originals)} outcomes reverted for batch #{batch_id}"
      )

      {:ok, length(originals)}
    else
      {:error, _} = error -> fail(batch_id, error)
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

    case manifest_written?(rollback_path, batch_id) do
      {:ok, true} -> :ok
      {:ok, false} -> append_manifest(rollback_path, batch_id, reason, originals)
      {:error, _} = error -> error
    end
  end

  defp append_manifest(rollback_path, batch_id, reason, originals) do
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

  defp manifest_written?(path, batch_id) do
    case File.read(path) do
      {:ok, content} ->
        {:ok,
         content
         |> String.split("\n", trim: true)
         |> Enum.any?(fn line ->
           match?({:ok, %{"batch_id" => ^batch_id}}, Jason.decode(line))
         end)}

      # Only a missing log means that no manifest was written. Any other failure may hide
      # one, and appending a second would break the idempotency of a retry.
      {:error, :enoent} ->
        {:ok, false}

      {:error, why} ->
        {:error, {:manifest_read_failed, why}}
    end
  end

  defp set_status(batch_id, status) do
    case update_batch_status(batch_id, status) do
      :ok -> :ok
      {:error, why} -> {:error, {:status_write_failed, why}}
    end
  end

  # The latest status that the batch log gives for this batch, or nil.
  # {:error, _} when the log cannot be read: that is not "no status", and a rolled_back
  # row may be in it. A missing log is no status.
  defp batch_status(batch_id) do
    batch_path = Path.join([Path.expand(@verisimdb_data_path), "dispatch", "batches.jsonl"])

    case File.read(batch_path) do
      {:ok, content} ->
        status =
          content
          |> String.split("\n", trim: true)
          |> Enum.map(&Jason.decode/1)
          |> Enum.flat_map(fn
            {:ok, %{"batch_id" => ^batch_id, "status" => status}} -> [status]
            _ -> []
          end)
          |> List.last()

        {:ok, status}

      {:error, :enoent} ->
        {:ok, nil}

      {:error, why} ->
        {:error, {:batch_log_read_failed, why}}
    end
  end

  @doc "List recent batches with their status"
  def list_batches(limit \\ 20) do
    list_batches_from_file(limit)
  end

  # --- Internal ---

  # {:ok, rows} in log order, or {:error, _} if the log directory or any monthly file
  # cannot be read: a partial list would give an incomplete manifest, and the status
  # that follows would make that final. A missing directory is an empty log.
  defp find_batch_outcomes(batch_id) do
    outcomes_dir = Path.join(Path.expand(@verisimdb_data_path), "outcomes")

    case File.ls(outcomes_dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
        # File.ls/1 has no fixed order. One file for each month, so a name sort is a
        # time sort; the lines of a file keep their order.
        |> Enum.sort()
        |> Enum.reduce_while({:ok, []}, fn f, {:ok, acc} ->
          path = Path.join(outcomes_dir, f)

          case File.read(path) do
            {:ok, content} ->
              rows =
                content
                |> String.split("\n", trim: true)
                |> Enum.map(&Jason.decode/1)
                |> Enum.flat_map(fn
                  {:ok, %{"batch_id" => ^batch_id} = o} -> [o]
                  _ -> []
                end)

              {:cont, {:ok, [rows | acc]}}

            {:error, why} ->
              {:halt, {:error, {:outcome_read_failed, f, why}}}
          end
        end)
        |> case do
          {:ok, chunks} -> {:ok, chunks |> Enum.reverse() |> Enum.concat()}
          error -> error
        end

      {:error, :enoent} ->
        {:ok, []}

      {:error, why} ->
        {:error, {:outcome_read_failed, outcomes_dir, why}}
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
