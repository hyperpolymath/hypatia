# SPDX-License-Identifier: MPL-2.0
# NS-01: drive the real scheduler over isolated fleet and destination stores.
defmodule Hypatia.FleetRecoverySchedulerTest do
  use ExUnit.Case, async: false

  alias Hypatia.{EventId, LearningScheduler, OutcomeTracker, Receipt}
  alias Hypatia.Neural.Coordinator

  @moduletag timeout: 120_000
  @moduletag capture_log: true
  @scheduler_id :fleet_recovery_scheduler

  setup do
    original_root = Application.fetch_env!(:hypatia, :verisimdb_data_path)
    assert String.contains?(Path.expand(original_root), "/_build/test/verisim-data")

    keys = [:verisimdb_data_path, :annealing_state_path, :fleet_path, :cross_org_policies]
    original = Map.new(keys, &{&1, Application.fetch_env(:hypatia, &1)})
    urls = Map.new(~w(HYPATIA_VERISIM_URL HYPATIA_ECHIDNABOT_URL), &{&1, System.get_env(&1)})
    cwd = File.cwd!()

    root =
      Path.join(System.tmp_dir!(), "hypatia-fleet-recovery-#{System.unique_integer([:positive])}")

    data = Path.join(root, "data")
    fleet = Path.join(root, "fleet")
    annealing = Path.join(data, "annealing-states")

    # An application-owned scheduler or anomaly timer must not use this test's
    # temporary global roots. The neural coordinator is unavailable during the
    # test: a scheduler cycle must not train or block that unrelated cohort.
    paused =
      for module <- [LearningScheduler, Hypatia.Watcher.AnomalyDetector],
          pid = Process.whereis(module),
          is_pid(pid),
          do: pid

    coordinator_running? =
      is_pid(Process.whereis(Hypatia.Supervisor)) and is_pid(Process.whereis(Coordinator))

    on_exit(fn ->
      File.cd!(cwd)

      for {key, value} <- original do
        case value do
          {:ok, previous} -> Application.put_env(:hypatia, key, previous)
          :error -> Application.delete_env(:hypatia, key)
        end
      end

      for {key, value} <- urls do
        if value == nil, do: System.delete_env(key), else: System.put_env(key, value)
      end

      if coordinator_running? do
        case Supervisor.restart_child(Hypatia.Supervisor, Coordinator) do
          {:ok, _pid} -> :ok
          {:error, :running} -> :ok
          other -> raise "could not restore neural coordinator: #{inspect(other)}"
        end
      end

      for pid <- paused, Process.alive?(pid), do: :sys.resume(pid)
      File.rm_rf!(root)
    end)

    for pid <- paused, do: :ok = :sys.suspend(pid)

    if coordinator_running?,
      do: :ok = Supervisor.terminate_child(Hypatia.Supervisor, Coordinator)

    for part <- ["outcomes", "recipes"], do: File.mkdir_p!(Path.join(data, part))
    Application.put_env(:hypatia, :verisimdb_data_path, data)
    Application.put_env(:hypatia, :annealing_state_path, annealing)
    Application.put_env(:hypatia, :fleet_path, fleet)
    Application.put_env(:hypatia, :cross_org_policies, [])
    for key <- Map.keys(urls), do: System.delete_env(key)

    # The scheduler's daily SHA job uses this relative cache. A fresh file in
    # the isolated cwd prevents it from spawning a network-capable Mix task.
    File.write!(Path.join(data, "verified-action-shas.json"), "{}\n")
    File.cd!(root)

    %{root: root, data: data, fleet: fleet, annealing: annealing}
  end

  test "unchanged fleet bytes retain receipt work after the destination cursor advances", ctx do
    id = "recipe-fleet-receipt-retry"
    recipe_path = write_recipe(ctx, id)
    row = fleet_row(id, "verified")
    fleet_path = write_fleet(ctx, [row])
    fleet_bytes = File.read!(fleet_path)
    receipts_path = Path.join(ctx.data, "receipts")
    File.write!(receipts_path, "blocked")
    server = start_scheduler()

    first = cycle(server)
    assert first.total_outcomes_processed == 1
    assert [stored] = rows(ctx)
    assert stored["event_id"] == row["event_id"]
    assert stored["event_id_source"] == "producer"
    assert stored["origin"] == "gitbot-fleet"
    assert Receipt.all(ctx.data) == []
    assert read_json(recipe_path)["successful_fixes"] == 1
    initial_annealing = read_json(Path.join(ctx.annealing, id <> ".json"))

    # The source signature now skips importing; local replay must still select
    # the committed fleet row for repair without counting another outcome.
    blocked = cycle(server)
    assert blocked.last_fleet_signature == first.last_fleet_signature
    assert blocked.total_outcomes_processed == 1
    assert map_size(blocked.last_outcome_positions) == 1
    assert MapSet.member?(blocked.pending_confidence_recipes, id)
    assert blocked.total_confidence_updates == 0
    assert Receipt.all(ctx.data) == []
    assert read_json(recipe_path)["successful_fixes"] == 1

    File.rm!(receipts_path)
    recovered = cycle(server)
    assert recovered.last_outcome_positions == blocked.last_outcome_positions
    assert recovered.last_fleet_signature == first.last_fleet_signature
    assert recovered.total_outcomes_processed == 1
    assert recovered.total_confidence_updates == 1
    assert recovered.pending_confidence_recipes == MapSet.new()
    assert [%{"event_id" => event_id}] = Receipt.all(ctx.data)
    assert event_id == row["event_id"]
    assert read_json(Path.join(ctx.annealing, id <> ".json")) == initial_annealing

    before_replay = persisted_bytes(ctx)
    replayed = cycle(server)
    assert replayed.total_outcomes_processed == 1
    assert replayed.total_confidence_updates == 1
    assert persisted_bytes(ctx) == before_replay
    assert File.read!(fleet_path) == fleet_bytes
  end

  test "a fresh scheduler repairs receipt and derived writes from a fleet-only committed row",
       ctx do
    id = "recipe-fleet-cold-recovery"
    recipe_path = write_recipe(ctx, id)
    row = fleet_row(id, "verified")
    fleet_path = write_fleet(ctx, [row])
    fleet_bytes = File.read!(fleet_path)
    receipts_path = Path.join(ctx.data, "receipts")
    File.write!(receipts_path, "blocked")
    File.write!(ctx.annealing, "blocked")
    server = start_scheduler()

    first = cycle(server)
    checkpoint = cycle(server)
    assert first.total_outcomes_processed == 1
    assert checkpoint.total_outcomes_processed == 1
    assert map_size(checkpoint.last_outcome_positions) == 1
    assert [stored] = rows(ctx)
    assert stored["origin"] == "gitbot-fleet"
    assert stored["event_id"] == row["event_id"]
    assert {:ok, imported_ids} = OutcomeTracker.imported_fleet_ids()
    assert imported_ids == MapSet.new([row["event_id"]])
    assert Receipt.all(ctx.data) == []
    refute Map.has_key?(read_json(recipe_path), "successful_fixes")

    # No manual recipe selection and no unrelated local event can rescue this.
    # A new process loses all pending work and cursors; only the log survives.
    :ok = stop_supervised(@scheduler_id)
    restarted = start_scheduler()
    fresh = GenServer.call(restarted, :status)
    assert fresh.last_outcome_positions == %{}
    assert fresh.last_fleet_signature == nil
    assert fresh.pending_confidence_recipes == MapSet.new()
    File.rm!(receipts_path)
    File.rm!(ctx.annealing)

    repaired = cycle(restarted)
    assert repaired.total_outcomes_processed == 0
    assert repaired.last_fleet_signature == first.last_fleet_signature
    assert repaired.pending_confidence_recipes == MapSet.new()
    assert [%{"event_id" => event_id}] = Receipt.all(ctx.data)
    assert event_id == row["event_id"]
    assert MapSet.member?(checkpoint.pending_confidence_recipes, id)
    assert repaired.total_confidence_updates == 1
    recipe = read_json(recipe_path)
    assert recipe["successful_fixes"] == 1
    assert recipe["failed_fixes"] == 0
    assert recipe["total_attempts"] == 1
    assert recipe["unqualified_attempts"] == 0
    annealing = read_json(Path.join(ctx.annealing, id <> ".json"))
    assert annealing["outcome_count"] == 1
    assert annealing["last_reheat"] == nil

    before_replay = persisted_bytes(ctx)
    assert cycle(restarted).total_outcomes_processed == 0
    assert cycle(restarted).total_outcomes_processed == 0
    assert persisted_bytes(ctx) == before_replay
    assert rows(ctx) == [stored]
    assert File.read!(fleet_path) == fleet_bytes
  end

  test "repairing audit receipts gives unverified, no-op and start rows no learning credit",
       ctx do
    ids = ["recipe-fleet-unverified", "recipe-fleet-no-op"]
    recipe_paths = Enum.map(ids, &write_recipe(ctx, &1))
    audit_rows = Enum.zip_with(ids, ["unverified", "no_op"], &fleet_row/2)
    start = fleet_row("recipe-fleet-start", "verified") |> Map.put("kind", "start")
    write_fleet(ctx, audit_rows ++ [start])
    receipts_path = Path.join(ctx.data, "receipts")
    File.write!(receipts_path, "blocked")
    server = start_scheduler()

    assert cycle(server).total_outcomes_processed == 0
    blocked = cycle(server)
    assert blocked.total_outcomes_processed == 0
    File.rm!(receipts_path)
    recovered = cycle(server)
    assert recovered.total_outcomes_processed == 0
    assert recovered.pending_confidence_recipes == MapSet.new()
    assert length(Receipt.all(ctx.data)) == 2

    assert Enum.map(Receipt.all(ctx.data), & &1["event_id"]) |> Enum.sort() ==
             Enum.map(audit_rows, & &1["event_id"]) |> Enum.sort()

    stored = rows(ctx)
    assert length(stored) == 2

    assert OutcomeTracker.qualified_outcome_counts(stored) ==
             %{successes: 0, failures: 0, false_positives: 0, ignored: 2}

    for path <- recipe_paths do
      recipe = read_json(path)
      assert recipe["confidence"] == 0.6
      assert recipe["successful_fixes"] == 0
      assert recipe["failed_fixes"] == 0
      assert recipe["false_positives"] == 0
      assert recipe["unqualified_attempts"] == 1
      assert recipe["annealing"]["outcome_count"] == 0
    end

    refute File.exists?(ctx.annealing)
    before_replay = persisted_bytes(ctx)
    assert cycle(server).total_outcomes_processed == 0
    assert persisted_bytes(ctx) == before_replay
  end

  test "a terminal receipt refusal does not mask a retryable failure for the same recipe", ctx do
    id = "recipe-fleet-mixed-receipt-status"
    recipe_path = write_recipe(ctx, id)
    refused_id = EventId.generate()

    # A historical, inconsistent fleet row cannot satisfy the receipt contract:
    # it claims verified without any observations. Its unknown outcome remains
    # audit-only under the unchanged qualification gate.
    refused_row = %{
      "event_id" => refused_id,
      "event_id_source" => "producer",
      "fleet_import_id" => refused_id,
      "origin" => "gitbot-fleet",
      "source_kind" => "fleet_import",
      "recipe_id" => id,
      "repo" => "fixture/repo",
      "file" => "src/legacy.ex",
      "outcome" => "unknown",
      "verification" => "verified"
    }

    File.write!(
      Path.join([ctx.data, "outcomes", "legacy.jsonl"]),
      Jason.encode!(refused_row) <> "\n"
    )

    valid_row = fleet_row(id, "verified")
    write_fleet(ctx, [valid_row])
    receipts_path = Path.join(ctx.data, "receipts")
    File.write!(receipts_path, "blocked")
    server = start_scheduler()

    blocked_log =
      ExUnit.CaptureLog.capture_log(fn ->
        blocked = cycle(server)
        assert blocked.total_outcomes_processed == 1
        assert MapSet.member?(blocked.pending_confidence_recipes, id)
        assert blocked.total_confidence_updates == 0
      end)

    assert blocked_log =~ "receipt_refused event_id=#{refused_id}"
    assert blocked_log =~ "verified_without_evidence"
    assert blocked_log =~ "receipt_pending event_id=#{valid_row["event_id"]}"
    assert Receipt.all(ctx.data) == []

    File.rm!(receipts_path)

    recovered_log =
      ExUnit.CaptureLog.capture_log(fn ->
        recovered = cycle(server)
        assert recovered.pending_confidence_recipes == MapSet.new()
        assert recovered.total_confidence_updates == 1
        assert recovered.total_outcomes_processed == 1
      end)

    assert recovered_log =~ "receipt_refused event_id=#{refused_id}"
    assert [%{"event_id" => event_id}] = Receipt.all(ctx.data)
    assert event_id == valid_row["event_id"]
    assert refused_row in rows(ctx)
    assert read_json(recipe_path)["successful_fixes"] == 1
    assert read_json(recipe_path)["unqualified_attempts"] == 1

    before_replay = persisted_bytes(ctx)

    replay_log =
      ExUnit.CaptureLog.capture_log(fn ->
        replayed = cycle(server)
        assert replayed.pending_confidence_recipes == MapSet.new()
        assert replayed.total_confidence_updates == 1
      end)

    refute replay_log =~ "receipt_refused"
    assert persisted_bytes(ctx) == before_replay
  end

  defp start_scheduler do
    start_supervised!(%{
      id: @scheduler_id,
      start: {GenServer, :start_link, [LearningScheduler, [], []]}
    })
  end

  defp cycle(server) do
    GenServer.cast(server, :run_cycle)
    GenServer.call(server, :status, 30_000)
  end

  defp write_recipe(ctx, id) do
    path = Path.join([ctx.data, "recipes", id <> ".json"])
    File.write!(path, Jason.encode!(%{"id" => id, "confidence" => 0.6}) <> "\n")
    path
  end

  defp fleet_row(id, verdict) do
    %{
      "event_id" => EventId.generate(),
      "recipe_id" => id,
      "repo" => "fixture/repo",
      "file" => "src/fixture.ex",
      "bot" => "robot-repo-automaton",
      "outcome" => "success",
      "verdict" => verdict,
      "exit_code" => 0,
      "before_hits" => 2,
      "after_hits" => 0
    }
  end

  defp write_fleet(ctx, rows) do
    path = Path.join([ctx.fleet, "shared-context/learning/fix-outcomes.jsonl"])
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, Enum.map_join(rows, "", &(Jason.encode!(&1) <> "\n")))
    path
  end

  defp rows(ctx) do
    Path.wildcard(Path.join([ctx.data, "outcomes", "*.jsonl"]))
    |> Enum.flat_map(fn path ->
      path |> File.read!() |> String.split("\n", trim: true) |> Enum.map(&Jason.decode!/1)
    end)
  end

  defp read_json(path), do: path |> File.read!() |> Jason.decode!()

  defp persisted_bytes(ctx) do
    for path <- Path.wildcard(Path.join(ctx.data, "**/*")),
        File.regular?(path),
        into: %{},
        do: {Path.relative_to(path, ctx.data), File.read!(path)}
  end
end
