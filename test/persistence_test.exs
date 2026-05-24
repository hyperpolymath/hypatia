# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Watcher.PersistenceTest do
  use ExUnit.Case, async: false

  alias Hypatia.Watcher.Persistence
  alias Hypatia.Telemetry, as: T

  setup do
    # Isolate from the on-disk verisim metrics by pointing the module
    # at a per-test tmp dir.
    tmp = Path.join(System.tmp_dir!(), "hypatia-persist-#{System.unique_integer([:positive])}")
    File.mkdir_p!(tmp)
    Application.put_env(:hypatia, :metrics_path, tmp)

    case Process.whereis(Hypatia.Watcher.PubSub) do
      nil ->
        {:ok, pid} = Registry.start_link(keys: :duplicate, name: Hypatia.Watcher.PubSub)
        on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid) end)

      _ ->
        :ok
    end

    case Process.whereis(Hypatia.Watcher) do
      nil ->
        {:ok, pid} = Hypatia.Watcher.start_link([])
        on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid) end)

      _ ->
        :ok
    end

    case Process.whereis(Persistence) do
      nil ->
        {:ok, pid} = Persistence.start_link([])
        on_exit(fn -> if Process.alive?(pid), do: GenServer.stop(pid) end)

      _ ->
        :ok
    end

    on_exit(fn ->
      Application.delete_env(:hypatia, :metrics_path)
      File.rm_rf!(tmp)
    end)

    {:ok, tmp_dir: tmp}
  end

  describe "snapshot_now/0" do
    test "writes a JSONL record to today's file", %{tmp_dir: tmp} do
      T.scan_complete(50, 3, path: "/tmp/x", severity_floor: "low")
      Process.sleep(50)

      {:ok, path, record} = Persistence.snapshot_now()

      assert String.starts_with?(path, tmp)
      assert String.ends_with?(path, ".jsonl")
      assert File.exists?(path)

      [line | _] = File.read!(path) |> String.split("\n", trim: true) |> Enum.reverse()
      decoded = Jason.decode!(line)

      assert decoded["at_ms"] == record.at_ms
      assert is_map(decoded["counts_m5"])
      assert is_map(decoded["counts_h1"])
      assert is_map(decoded["counts_d1"])
      assert is_map(decoded["queue_depths"])
      assert is_map(decoded["recipe_health_summary"])
      assert is_integer(decoded["alert_count"])
    end

    test "multiple snapshots produce multiple JSONL lines" do
      Persistence.snapshot_now()
      Persistence.snapshot_now()
      Persistence.snapshot_now()

      path = Persistence.current_file()
      lines = File.read!(path) |> String.split("\n", trim: true)

      assert length(lines) >= 3

      Enum.each(lines, fn line ->
        assert {:ok, _} = Jason.decode(line),
               "Each persistence line must be valid JSON: #{line}"
      end)
    end

    test "counts_m5 keys are dotted-string event names, not lists" do
      T.dispatch_decision(0.9, strategy: :review, tier: :eliminate, recipe_id: "r", repo: "x")
      Process.sleep(50)

      {:ok, _path, record} = Persistence.snapshot_now()

      keys = Map.keys(record.counts_m5)

      Enum.each(keys, fn k ->
        assert is_binary(k),
               "counts_m5 keys must be strings (got #{inspect(k)})"

        assert k =~ ".",
               "counts_m5 keys must be dotted (got #{inspect(k)})"
      end)
    end
  end

  describe "current_file/0" do
    test "today's path uses YYYY-MM-DD.jsonl" do
      path = Persistence.current_file()
      assert path =~ ~r/\d{4}-\d{2}-\d{2}\.jsonl$/
    end
  end
end
