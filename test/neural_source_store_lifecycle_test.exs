# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
# Synthetic lifecycle controls: isolated real processes, no dispatch or actuator.
defmodule Hypatia.Neural.SourceStoreLifecycleTest do
  use ExUnit.Case, async: false

  alias Hypatia.{FleetDispatcher, LearningScheduler, OutcomeTracker}
  alias Hypatia.Diagnostics.Monitor
  alias Hypatia.Kin.Gate
  alias Hypatia.Watcher.AnomalyDetector

  alias Hypatia.Neural.{
    Blackboard,
    Coordinator,
    EchoStateNetwork,
    GraphOfTrust,
    LiquidStateMachine,
    Persistence,
    SourceStore,
    TrainingPipeline
  }

  @finding %{
    "id" => "recipe-source-fixture",
    "recipe_id" => "recipe-source-fixture",
    "category" => "CommandInjection",
    "severity" => "High",
    "language" => "shell"
  }

  setup do
    original = Application.fetch_env!(:hypatia, :verisimdb_data_path)
    assert String.contains?(Path.expand(original), "/_build/test/verisim-data")
    root = Path.join(System.tmp_dir!(), "hypatia-model-source-#{System.unique_integer([:positive])}")
    first = Path.join(root, "first")
    second = Path.join(root, "second")

    for store <- [first, second], part <- ["outcomes", "recipes", "patterns", "scans"] do
      File.mkdir_p!(Path.join(store, part))
    end

    Application.put_env(:hypatia, :verisimdb_data_path, first)

    on_exit(fn ->
      Application.put_env(:hypatia, :verisimdb_data_path, original)
      File.rm_rf!(root)
    end)

    %{root: root, first: first, second: second}
  end

  test "source identity is an expanded pathname and never a content attestation", ctx do
    Application.put_env(:hypatia, :verisimdb_data_path, Path.join(ctx.first, "."))
    assert SourceStore.current() == ctx.first
    assert SourceStore.check(SourceStore.fields(), ctx.first) == :ok
    assert {:error, :unbound_source_store} = SourceStore.check(%{}, ctx.first)
    refute SourceStore.snapshot_matches?(%{"source_store" => ctx.first}, ctx.first)
    assert {:error, :unbound_source_store} = Persistence.save_all(nil)
    refute File.exists?(Path.join(ctx.first, "neural-states"))
  end

  test "an in-flight root change refuses the updated model and latches the original source", ctx do
    state = Map.merge(SourceStore.fields(), %{model: :original})

    assert {:error, {:source_store_changed, first, second}, blocked} =
             SourceStore.transact(state, ctx.first, fn ->
               Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)
               {:ok, %{state | model: :uncommitted}}
             end)

    assert {first, second} == {ctx.first, ctx.second}
    assert blocked.model == :original
    assert blocked.source_store == ctx.first
    assert blocked.source_status == :blocked

    Application.put_env(:hypatia, :verisimdb_data_path, ctx.first)
    assert {:error, {:source_lifecycle_blocked, _}} = SourceStore.check(blocked, ctx.first)
    assert SourceStore.diagnostic(blocked).restart_required
  end

  test "an operation error is a refusal, not a successful transaction" do
    state = Map.merge(SourceStore.fields(), %{model: :original})

    assert {:error, :fixture_refusal, refused} =
             SourceStore.transact(state, state.source_store, fn ->
               {{:error, :fixture_refusal}, %{state | model: :uncommitted}}
             end)

    assert refused.model == :original
    assert refused.source_error == :fixture_refusal
  end

  test "an actual ready cohort can analyze, train and persist its own source", ctx do
    write_training_store(ctx.first)
    {server, _cohort} = coordinator()
    assert %{source_store: source, source_status: :ready, network_count: 8} =
             Coordinator.analyze(@finding, server)
    assert source == ctx.first
    assert :ok = Coordinator.force_cycle(server)
    assert {:ok, %{cycle_count: 1}} = barrier(server, ctx.first)

    state = :sys.get_state(server)
    assert state.esn.trained
    assert state.rbf.trained
    assert state.source_store == ctx.first

    snapshots = Persistence.load_all(ctx.first)
    assert map_size(snapshots) == 8

    for {_name, snapshot} <- snapshots do
      assert {:ok, data} = snapshot
      assert SourceStore.snapshot_matches?(data, ctx.first)
    end

    refute File.exists?(Path.join(ctx.second, "neural-states"))
  end

  test "running A cannot answer, learn or persist as empty B and cannot resume by switching back", ctx do
    write_training_store(ctx.first)
    {server, _cohort} = coordinator()
    assert :ok = Coordinator.force_cycle(server)
    assert {:ok, %{cycle_count: 1}} = barrier(server, ctx.first)
    before = :sys.get_state(server)
    old_files = snapshot_bytes(ctx.first)
    Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)

    assert {:error, _} = Coordinator.analyze(@finding, server)
    assert {:error, _} = Coordinator.dispatch_recommendation(@finding, server)
    assert {:error, _} = Coordinator.force_cycle(server)
    assert {:error, _} = Coordinator.record_outcome(@finding, "success", server)
    assert {:error, {:neural_source_unavailable, details}} = Coordinator.status(server)
    assert details.source_store == ctx.first
    assert details.current_store == ctx.second
    assert details.restart_required
    assert {:error, _} = Coordinator.health_report(server)
    assert {:error, _} = Persistence.save_all(:sys.get_state(server))

    after_refusal = :sys.get_state(server)
    assert after_refusal.rbf == before.rbf
    assert after_refusal.esn == before.esn
    assert after_refusal.moe == before.moe
    assert after_refusal.cycle_count == before.cycle_count
    assert snapshot_bytes(ctx.first) == old_files
    refute File.exists?(Path.join(ctx.second, "neural-states"))

    Application.put_env(:hypatia, :verisimdb_data_path, ctx.first)
    assert {:error, _} = Coordinator.force_cycle(server)
    assert :sys.get_state(server).source_status == :blocked
  end

  test "fresh B refuses A peers while a fresh B cohort remains cold without B inputs", ctx do
    write_training_store(ctx.first)
    {first_server, first_cohort} = coordinator()
    assert :ok = Coordinator.force_cycle(first_server)
    assert {:ok, %{cycle_count: 1}} = barrier(first_server, ctx.first)
    first_files = snapshot_bytes(ctx.first)

    Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)
    mismatched = start_coordinator(first_cohort)
    assert {:error, _} = Coordinator.force_cycle(mismatched)
    assert :sys.get_state(mismatched).source_status == :blocked
    refute File.exists?(Path.join(ctx.second, "neural-states"))

    {second_server, _cohort} = coordinator()
    assert %{source_status: :ready, rbf: %{trained: false}, esn: %{trained: false}} =
             Coordinator.health_report(second_server)
    assert :ok = Coordinator.force_cycle(second_server)
    assert {:ok, %{cycle_count: 1}} = barrier(second_server, ctx.second)
    second_state = :sys.get_state(second_server)
    refute second_state.rbf.trained
    refute second_state.esn.trained
    assert second_state.source_store == ctx.second
    assert snapshot_bytes(ctx.first) == first_files
    assert {:ok, persisted} = Persistence.load_all(ctx.second).rbf
    assert SourceStore.snapshot_matches?(persisted, ctx.second)
    refute persisted["trained"]
  end

  test "queued admission and execution use the same captured source", ctx do
    cohort = isolated_cohort()
    assert {:ok, state} = Coordinator.init(cohort)
    assert {:reply, :ok, admitted} =
             Coordinator.handle_call({:queue, ctx.first, :force_cycle}, {self(), make_ref()}, state)
    assert_receive {:source_bound, source, :force_cycle} = queued
    assert source == ctx.first

    Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)
    assert {:noreply, refused} = Coordinator.handle_info(queued, admitted)
    assert refused.cycle_count == 0
    assert refused.source_store == ctx.first
    assert refused.source_status == :blocked
    refute File.exists?(Path.join(ctx.first, "neural-states"))
    refute File.exists?(Path.join(ctx.second, "neural-states"))
  end

  test "a stale A request cannot be admitted by a fresh B lifecycle", ctx do
    Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)
    {server, _cohort} = coordinator()
    assert {:error, {:request_source_mismatch, first, second}} =
             GenServer.call(server, {:queue, ctx.first, :force_cycle})
    assert {first, second} == {ctx.first, ctx.second}
    assert {:ok, %{cycle_count: 0, source_status: :ready}} = Coordinator.status(server)
    refute File.exists?(Path.join(ctx.second, "neural-states"))
  end

  test "named models refuse unbound and foreign restoration and expose a blocked lifecycle", ctx do
    cohort = isolated_cohort()

    for {_name, server} <- Keyword.fetch!(cohort, :named_networks) do
      assert {:ok, own} = GenServer.call(server, {:snapshot, ctx.first})
      assert :ok = GenServer.call(server, {:restore, ctx.first, own})
      assert {:error, :unbound_or_foreign_snapshot} =
               GenServer.call(server, {:restore, ctx.first, Map.delete(own, "source_schema")})
      assert {:error, :unbound_or_foreign_snapshot} =
               GenServer.call(server, {:restore, ctx.first, Map.put(own, "source_store", ctx.second)})
    end

    Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)

    for {_name, server} <- Keyword.fetch!(cohort, :named_networks) do
      diagnostic = GenServer.call(server, :get_state)
      assert diagnostic["source_store"] == ctx.first
      assert diagnostic["source_status"] == "blocked"
    end

    Application.put_env(:hypatia, :verisimdb_data_path, ctx.first)

    for {_name, server} <- Keyword.fetch!(cohort, :named_networks) do
      assert {:error, {:source_lifecycle_blocked, _}} =
               GenServer.call(server, {:snapshot, ctx.first})
    end
  end

  test "all named snapshots must be available before the first persistence write", ctx do
    {server, cohort} = coordinator()
    state = :sys.get_state(server)
    sequence = Keyword.fetch!(cohort, :named_networks).sequence
    GenServer.stop(sequence)

    assert {:error, {:cohort_source_mismatch, :sequence, _}} = Persistence.save_all(state)
    refute File.exists?(Path.join(ctx.first, "neural-states"))
    assert {:error, _} = Coordinator.status(server)
    assert :sys.get_state(server).source_status == :blocked
  end

  test "encoding failure cannot leave a partially collected bundle reported as saved", ctx do
    {server, _cohort} = coordinator()
    state = :sys.get_state(server)
    bad = %{state | rbf: %{state.rbf | training_stats: %{unencodable: fn -> :fixture end}}}
    assert {:error, _} = Persistence.save_all(bad)
    refute File.exists?(Path.join(ctx.first, "neural-states"))
  end

  test "a matching saved RBF is restorable but legacy and foreign bytes remain diagnostic", ctx do
    write_training_store(ctx.first)
    {server, _cohort} = coordinator()
    assert :ok = Coordinator.force_cycle(server)
    assert {:ok, %{cycle_count: 1}} = barrier(server, ctx.first)
    assert {:ok, bound} = Persistence.load_all(ctx.first).rbf
    assert bound["trained"]

    {restored, _cohort} = coordinator()
    assert :sys.get_state(restored).rbf.trained
    assert :sys.get_state(restored).rbf.output_weights == bound["output_weights"]

    for diagnostic <- [
          Map.drop(bound, ["source_schema", "source_store", "source_status"]),
          Map.put(bound, "source_store", ctx.second),
          Map.put(bound, "source_schema", 999)
        ] do
      path = Path.join([ctx.first, "neural-states", "rbf.json"])
      write_json(path, diagnostic)
      bytes = File.read!(path)
      {cold, _cohort} = coordinator()
      refute :sys.get_state(cold).rbf.trained
      assert {:ok, ^diagnostic} = Persistence.load_all(ctx.first).rbf
      assert File.read!(path) == bytes
    end
  end

  test "an old A task cannot write into a new B blackboard", ctx do
    first_board = Keyword.fetch!(isolated_cohort(), :blackboard)
    assert {:ok, _} = Blackboard.start_round_source(ctx.first, first_board)
    assert :ok = Blackboard.write_source(ctx.first, :fixture, :value, :from_a, first_board)

    Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)
    second_board = Keyword.fetch!(isolated_cohort(), :blackboard)
    assert {:ok, _} = Blackboard.start_round_source(ctx.second, second_board)
    assert {:error, {:request_source_mismatch, _, _}} =
             Blackboard.write_source(ctx.first, :fixture, :value, :from_a, second_board)
    assert :not_found = Blackboard.read_source(ctx.second, :fixture, :value, second_board)
    assert {:ok, %{entry_count: 0}} = Blackboard.snapshot_source(ctx.second, second_board)
    assert {:error, {:source_store_changed, _, _}} =
             Blackboard.write_source(ctx.first, :fixture, :value, :late, first_board)
  end

  test "raw unbound blackboard entries cannot enter a source-qualified trace", ctx do
    board = Keyword.fetch!(isolated_cohort(), :blackboard)
    assert {:ok, round} = Blackboard.start_round_source(ctx.first, board)
    table = :sys.get_state(board).table
    legacy = %{network: :fixture, key: :legacy, value: :unbound, round: round}
    :ets.insert(table, {{:fixture, :legacy}, legacy})

    assert {:error, :foreign_blackboard_entry} =
             Blackboard.read_source(ctx.first, :fixture, :legacy, board)
    assert {:error, :foreign_blackboard_entry} = Blackboard.snapshot_source(ctx.first, board)
    assert :sys.get_state(board).source_error == :foreign_blackboard_entry

    assert {:ok, _} = Blackboard.start_round_source(ctx.first, board)
    assert :ok = Blackboard.write_source(ctx.first, :fixture, :bound, :value, board)
    assert {:ok, %{entry_count: 1}} = Blackboard.snapshot_source(ctx.first, board)
  end

  test "captured training, trust language context and strategy reads cannot switch to B", ctx do
    write_training_store(ctx.first)
    write_json(Path.join([ctx.first, "scans", "fixture-repo.json"]), %{"primary_language" => "rust"})
    write_json(Path.join([ctx.second, "scans", "fixture-repo.json"]), %{"primary_language" => "elixir"})
    Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)

    assert TrainingPipeline.build_esn_training_data() == []
    assert [%{count: 71}] = TrainingPipeline.build_esn_training_data(ctx.first)
    assert TrainingPipeline.build_rbf_training_data() == {[], []}
    assert {vectors, targets} = TrainingPipeline.build_rbf_training_data(ctx.first)
    assert length(vectors) == 5
    assert targets == List.duplicate(1.0, 5)
    assert GraphOfTrust.build().nodes == %{}
    pinned = GraphOfTrust.build_from_store(ctx.first)
    assert pinned.language_clusters[:systems] == ["fixture-repo"]
    assert GraphOfTrust.trust_score(pinned, "recipe-source-fixture") > 0.5
    assert OutcomeTracker.annealed_strategy_from_store("recipe-source-fixture", :auto_execute, ctx.first) ==
             :auto_execute
    assert OutcomeTracker.annealed_strategy_from_store("recipe-source-fixture", :auto_execute, ctx.second) ==
             :report_only
    refute File.exists?(Path.join(ctx.first, "annealing-states"))
    refute File.exists?(Path.join(ctx.second, "annealing-states"))
  end

  test "Kin requires a ready source assessment and preserves manual-review tiers", ctx do
    action = %{pattern_id: "recipe-source-fixture", dispatch_tier: :auto_execute, confidence: 0.99}
    ready = %{
      source_store: ctx.first,
      source_status: :ready,
      confidence: 0.99,
      strategy: :auto_execute,
      is_novel: false
    }
    assert :pass = Gate.neural_consensus(action, fn _ -> ready end)

    for result <- [
          {:error, :blocked},
          :unavailable,
          %{confidence: 0.99},
          %{ready | source_store: ctx.second},
          %{ready | source_status: :blocked},
          %{ready | confidence: 0.5},
          %{ready | confidence: -0.01},
          %{ready | confidence: 1.01},
          %{ready | confidence: "0.99"},
          %{ready | strategy: :review},
          %{ready | strategy: :report_only},
          %{ready | strategy: "auto_execute"},
          %{ready | strategy: nil},
          Map.delete(ready, :strategy),
          %{ready | is_novel: true},
          %{ready | is_novel: "false"},
          %{ready | is_novel: nil},
          Map.delete(ready, :is_novel)
        ] do
      assert {:hold, _} = Gate.neural_consensus(action, fn _ -> result end)
    end

    assert {:hold, _} = Gate.neural_consensus(action, fn _ -> raise "fixture failure" end)
    assert {:hold, _} = Gate.neural_consensus(action, fn _ -> exit(:fixture_unavailable) end)

    for tier <- [:review, :report_only] do
      assert :pass =
               Gate.neural_consensus(%{action | dispatch_tier: tier}, fn _ ->
                 flunk("manual tier must not invoke neural prediction")
               end)
    end
  end

  test "diagnostics and watcher refuse blocked, unbound and old-source reports", ctx do
    {server, _cohort} = coordinator()
    assert {:ok, status} = Coordinator.status(server)
    assert {:ok, {:neural, ^status}} = Monitor.classify_neural_status({:ok, status})
    report = Coordinator.health_report(server)
    signal = %{report | esn: %{trained: true, drift: :rising_drift}}
    assert {:rising_drift, true} = AnomalyDetector.source_esn_signal(signal)

    for result <- [
          {:error, :blocked},
          Map.delete(signal, :source_store),
          %{signal | source_status: :blocked},
          %{signal | source_store: ctx.second},
          %{signal | esn: %{trained: false, drift: :rising_drift}}
        ] do
      assert {:unavailable, false} = AnomalyDetector.source_esn_signal(result)
    end

    for result <- [
          nil,
          {:error, :blocked},
          {:ok, Map.delete(status, :source_store)},
          {:ok, %{status | source_status: :blocked}},
          {:ok, %{status | source_store: ctx.second}}
        ] do
      assert {:error, {:neural_source_unavailable, _}} = Monitor.classify_neural_status(result)
    end

    Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)
    assert {:error, _} = Coordinator.health_report(server)
    assert {:error, _} = Monitor.classify_neural_status(Coordinator.status(server))
    assert Process.alive?(server)
    assert :sys.get_state(server).source_status == :blocked
  end

  test "diagnostics retains actual refusal tuples even when notification fails" do
    reason = {:neural_source_unavailable, %{source_status: :blocked}}
    components = [{:error, reason}, {:ok, {:fixture, %{dropped: 0, dispatched: 1}}}]
    state = %{components: [], failures: [], last_check: nil, recovery_attempts: %{}}
    observed = Monitor.record_observations(state, components)
    assert observed.components == components
    assert observed.failures == [{:failure, reason}]
    assert %DateTime{} = observed.last_check
    assert Monitor.source_lifecycle_failure?(reason)
    refute Monitor.source_lifecycle_failure?(:pipeline_crashed)

    assert Monitor.detect_failures([
             {:error, :pipeline_crashed},
             {:ok, {:fixture, %{dropped: 2, dispatched: 2}}},
             {:ok, {:fixture, %{buffered: 10, max_buffer: 10}}}
           ]) == [
             {:failure, :pipeline_crashed},
             {:failure, :high_error_rate},
             {:failure, :buffer_overflow}
           ]

    for notifier <- [
          fn _ -> raise "fixture notification failure" end,
          fn _ -> exit(:fixture_notification_exit) end
        ] do
      assert Monitor.finish_observations(observed, notifier, fn _, _ ->
               flunk("notification failure must not trigger recovery")
             end) == observed
    end

    observer = self()
    notify = fn failures -> send(observer, {:fixture_notification, failures}) end

    recover = fn failures, snapshot ->
      send(observer, {:fixture_recovery_pass, failures})
      snapshot
    end

    assert Monitor.finish_observations(observed, notify, recover) == observed
    assert_receive {:fixture_notification, [{:failure, ^reason}]}
    refute_received {:fixture_notification, _}
    refute_received {:fixture_recovery_pass, _}
    assert observed.recovery_attempts == %{}

    mixed = Monitor.record_observations(state, components ++ [{:error, :pipeline_crashed}])
    assert Monitor.finish_observations(mixed, notify, recover) == mixed

    assert_receive {:fixture_notification,
                    [{:failure, ^reason}, {:failure, :pipeline_crashed}]}

    assert_receive {:fixture_recovery_pass, [{:failure, :pipeline_crashed}]}
    refute_received {:fixture_notification, _}
    refute_received {:fixture_recovery_pass, _}
  end

  test "a busy or unavailable actual coordinator is not reported as healthy", ctx do
    {server, _cohort} = coordinator()
    assert {:ok, {:neural, _}} = Monitor.check_neural(server)
    :ok = :sys.suspend(server)

    try do
      assert {:error, {:neural_source_unavailable, :busy_or_unverified}} =
               Monitor.check_neural(server, 1)
    after
      :sys.resume(server)
    end

    assert Process.alive?(server)
    Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)
    assert {:error, {:neural_source_unavailable, _}} = Monitor.check_neural(server)
    GenServer.stop(server)
    assert {:error, :neural_crashed} = Monitor.check_neural(server)
    assert {:error, :neural_crashed} = Monitor.check_neural({:invalid_server, :fixture})
  end

  test "Scheduler and dispatcher distinguish queue admission from refusal without actuation", ctx do
    {server, _cohort} = coordinator()
    assert LearningScheduler.trigger_neural_cycle(server)
    assert {:ok, %{cycle_count: 1}} = barrier(server, ctx.first)
    assert :queued = FleetDispatcher.record_neural_outcome(@finding, "failure", server)
    assert {:ok, _} = barrier(server, ctx.first)
    before = :sys.get_state(server)

    Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)
    refute LearningScheduler.trigger_neural_cycle(server)
    assert {:error, _} = FleetDispatcher.record_neural_outcome(@finding, "success", server)
    assert :sys.get_state(server).moe == before.moe
    assert :sys.get_state(server).cycle_count == before.cycle_count
    refute File.exists?(Path.join(ctx.second, "neural-states"))

    missing = spawn(fn -> :ok end)
    monitor = Process.monitor(missing)
    assert_receive {:DOWN, ^monitor, :process, ^missing, _}
    refute LearningScheduler.trigger_neural_cycle(missing)
    assert {:error, :neural_unavailable} =
             FleetDispatcher.record_neural_outcome(@finding, "failure", missing)
  end

  test "trust rejects missing, blank and nonstring subjects without partial positive edges" do
    valid = qualified_row()
    recipes = [%{"id" => "recipe-source-fixture"}]

    invalid =
      for field <- ["repo", "bot", "recipe_id"],
          value <- [nil, false, "", " \t\n", 42, [], %{}] do
        Map.put(valid, field, value)
      end

    invalid = invalid ++ Enum.map(["repo", "bot", "recipe_id"], &Map.delete(valid, &1))

    for row <- invalid do
      assert OutcomeTracker.qualified_outcome?(row)
      graph = GraphOfTrust.build([row], recipes)
      assert graph.edges == []
      assert graph.cross_repo_edges == []
      assert graph.nodes == %{"recipe-source-fixture" => :recipe}
      assert graph.trust_scores == %{"recipe-source-fixture" => 0.5}
    end

    graph = GraphOfTrust.build(invalid ++ [valid], recipes)
    assert length(graph.edges) == 3
    assert GraphOfTrust.trust_score(graph, "recipe-source-fixture") > 0.5
    refute Map.has_key?(graph.nodes, "unknown")
    assert valid == qualified_row()
  end

  test "canonical pattern is an explicit identity but cannot mask a malformed recipe_id" do
    canonical = qualified_row() |> Map.delete("recipe_id") |> Map.put("pattern", "recipe-source-fixture")
    recipes = [%{"id" => "recipe-source-fixture"}]
    assert length(GraphOfTrust.build([canonical], recipes).edges) == 3

    for bad <- ["", " ", 42, [], %{}] do
      graph = GraphOfTrust.build([Map.put(canonical, "recipe_id", bad)], recipes)
      assert graph.edges == []
      assert graph.trust_scores == %{"recipe-source-fixture" => 0.5}
    end
  end

  defp isolated_cohort do
    networks =
      Map.new(Persistence.default_networks(), fn {name, module} ->
        {name, isolated_process(module, [])}
      end)

    [named_networks: networks, blackboard: isolated_process(Blackboard, [isolated: true])]
  end

  defp coordinator do
    cohort = isolated_cohort()
    {start_coordinator(cohort), cohort}
  end

  defp start_coordinator(cohort) do
    server = isolated_process(Coordinator, cohort)

    # Small real reservoirs bound test cost; model training still executes.
    :sys.replace_state(server, fn state ->
      %{state |
        esn: EchoStateNetwork.init(reservoir_size: 4, seed: 17),
        lsm: LiquidStateMachine.init(reservoir_size: 4, seed: 17)}
    end)

    server
  end

  defp isolated_process(module, opts) do
    start_supervised!(%{id: make_ref(), start: {GenServer, :start_link, [module, opts, []]}})
  end

  defp barrier(server, source), do: GenServer.call(server, {:status, source}, 30_000)

  defp qualified_row do
    %{
      "recipe_id" => "recipe-source-fixture",
      "repo" => "fixture-repo",
      "bot" => "fixture-bot",
      "outcome" => "success",
      "verification" => "verified",
      "timestamp" => "2026-11-04T00:00:00Z"
    }
  end

  defp write_training_store(root) do
    write_json(Path.join([root, "recipes", "fixture.json"]), %{"id" => "recipe-source-fixture"})
    rows = List.duplicate(qualified_row(), 71)
    File.write!(Path.join([root, "outcomes", "fixture.jsonl"]), Enum.map_join(rows, "", &(Jason.encode!(&1) <> "\n")))
    patterns =
      for index <- 1..5 do
        %{"id" => "pattern-#{index}", "severity" => "high", "category" => "CommandInjection",
          "triangle_tier" => "eliminate", "frequency" => index / 5, "age_days" => index}
      end
    write_json(Path.join([root, "patterns", "registry.json"]), %{"patterns" => patterns})
  end

  defp write_json(path, data) do
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, Jason.encode!(data) <> "\n")
  end

  defp snapshot_bytes(root) do
    Path.join([root, "neural-states", "*.json"])
    |> Path.wildcard()
    |> Map.new(fn path -> {Path.basename(path), File.read!(path)} end)
  end
end
