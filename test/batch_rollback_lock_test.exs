# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# E0-25 (owner ruling 18 Nov): BatchRollback.rollback/2 takes an exclusive lock file
# per batch, so that two VMs or two OS processes on one store cannot both run the
# rollback of a batch. The lock lives at <store>/dispatch/locks/<batch_id>.lock.
defmodule Hypatia.Safety.BatchRollbackLockTest do
  use ExUnit.Case, async: false

  alias Hypatia.Safety.BatchRollback

  setup do
    data = Application.fetch_env!(:hypatia, :verisimdb_data_path) |> Path.expand()
    outcomes = Path.join(data, "outcomes")
    dispatch = Path.join(data, "dispatch")
    File.mkdir_p!(outcomes)
    File.mkdir_p!(dispatch)

    uniq = System.unique_integer([:positive])
    {:ok, batch} = BatchRollback.create_batch(1, :auto_execute)
    log = Path.join(outcomes, "e0-25-lock-#{uniq}.jsonl")
    lock = Path.join([dispatch, "locks", "#{batch}.lock"])

    seed = %{
      "recipe_id" => "recipe-e0-25-lock",
      "repo" => "owner/repo",
      "file" => "a.txt",
      "outcome" => "success",
      "verification" => "verified",
      "batch_id" => batch,
      "timestamp" => DateTime.utc_now() |> DateTime.to_iso8601(),
      "bot" => "hypatia"
    }

    File.write!(log, Jason.encode!(seed) <> "\n")

    on_exit(fn ->
      File.rm(log)
      File.rm(lock)
    end)

    manifests = fn ->
      case File.read(Path.join(dispatch, "rollbacks.jsonl")) do
        {:ok, c} ->
          c
          |> String.split("\n", trim: true)
          |> Enum.map(&Jason.decode!/1)
          |> Enum.filter(&(&1["batch_id"] == batch))

        _ ->
          []
      end
    end

    rollback_rows = fn ->
      outcomes
      |> Path.join("*.jsonl")
      |> Path.wildcard()
      |> Enum.flat_map(&(File.read!(&1) |> String.split("\n", trim: true)))
      |> Enum.map(&Jason.decode!/1)
      |> Enum.filter(&(&1["batch_id"] == batch and &1["caused_by"] == "batch_rollback"))
    end

    %{
      batch: batch,
      lock: lock,
      dispatch: dispatch,
      uniq: uniq,
      manifests: manifests,
      rollback_rows: rollback_rows
    }
  end

  # An independent OS process (its own VM, not a Task) that takes the lock file the same
  # way (exclusive create), holds it until `release` exists, then removes it.
  defp start_foreign_holder(lock, release, now_offset_s \\ 0) do
    File.mkdir_p!(Path.dirname(lock))

    script = """
    [lock, release, off] = System.argv()
    {:ok, f} = File.open(lock, [:write, :exclusive])
    at = System.os_time(:second) - String.to_integer(off)
    IO.binwrite(f, ~s({"os_pid":"foreign","node":"foreign","token":"foreign-token","acquired_unix":\#{at}}))
    File.close(f)
    wait = fn wait -> if File.exists?(release), do: :ok, else: (Process.sleep(50); wait.(wait)) end
    wait.(wait)
    File.rm(lock)
    """

    exe = System.find_executable("elixir")

    port =
      Port.open({:spawn_executable, exe}, [
        :binary,
        :exit_status,
        args: ["-e", script, lock, release, Integer.to_string(now_offset_s)]
      ])

    wait_until(fn -> File.exists?(lock) and File.read!(lock) != "" end, 60_000)
    port
  end

  defp wait_until(fun, budget) do
    cond do
      fun.() -> :ok
      budget <= 0 -> flunk("condition not met in time")
      true -> Process.sleep(50) && wait_until(fun, budget - 50)
    end
  end

  test "a lock held by another OS process stops the rollback; nothing is written, then it proceeds",
       ctx do
    release = Path.join(ctx.dispatch, "release-#{ctx.uniq}")
    on_exit(fn -> File.write(release, "") end)
    port = start_foreign_holder(ctx.lock, release)

    assert {:error, {:locked, holder}} = BatchRollback.rollback(ctx.batch, "t")
    assert holder["os_pid"] == "foreign"
    assert ctx.rollback_rows.() == []
    assert ctx.manifests.() == []
    assert {:ok, [%{"status" => status} | _]} = {:ok, BatchRollback.list_batches()}
    refute status == "rolled_back"

    File.write!(release, "")
    assert_receive {^port, {:exit_status, 0}}, 30_000
    refute File.exists?(ctx.lock)

    assert {:ok, 1} = BatchRollback.rollback(ctx.batch, "t")
    assert length(ctx.rollback_rows.()) == 1
    assert length(ctx.manifests.()) == 1
    refute File.exists?(ctx.lock)
  end

  test "a stale lock (old, holder gone) is refused and reported, never stolen", ctx do
    release = Path.join(ctx.dispatch, "release-#{ctx.uniq}")
    on_exit(fn -> File.write(release, "") end)
    port = start_foreign_holder(ctx.lock, release, 7 * 24 * 3600)
    before = File.read!(ctx.lock)

    assert {:error, {:locked, holder}} = BatchRollback.rollback(ctx.batch, "t")
    assert holder["stale"] == true
    assert holder["age_s"] >= 7 * 24 * 3600
    assert holder["lock_path"] == ctx.lock
    assert File.read!(ctx.lock) == before
    assert ctx.rollback_rows.() == []
    assert ctx.manifests.() == []

    File.write!(release, "")
    assert_receive {^port, {:exit_status, 0}}, 30_000
  end

  test "a holder that raises releases the lock", ctx do
    # An invalid UTF-8 reason makes the manifest encode raise inside the critical section.
    assert_raise Jason.EncodeError, fn -> BatchRollback.rollback(ctx.batch, <<255, 254>>) end
    refute File.exists?(ctx.lock)
    assert {:ok, 1} = BatchRollback.rollback(ctx.batch, "after the crash")
    refute File.exists?(ctx.lock)
  end

  test "a lock that cannot be taken fails closed: error, no writes", ctx do
    # A regular file where the lock directory should be.
    locks = Path.dirname(ctx.lock)
    File.rm_rf!(locks)
    File.write!(locks, "not a directory")
    on_exit(fn -> File.rm(locks) end)

    assert {:error, {:lock_failed, _}} = BatchRollback.rollback(ctx.batch, "t")
    assert ctx.rollback_rows.() == []
    assert ctx.manifests.() == []
  end

  test "a batch id that is not a plain name is refused", ctx do
    assert {:error, {:invalid_batch_id, _}} = BatchRollback.rollback("../escape", "t")
    assert {:error, {:invalid_batch_id, _}} = BatchRollback.rollback("", "t")
    assert ctx.rollback_rows.() == []
  end
end
