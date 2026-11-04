# SPDX-License-Identifier: MPL-2.0
# Synthetic store-boundary controls. No estate corpus or actuator is used.
defmodule Hypatia.LearningRuntimeStoreTest do
  use ExUnit.Case, async: false

  alias Hypatia.{LearningScheduler, OutcomeTracker, Paths, PatternRegistry, VerisimConnector}

  alias Hypatia.Neural.{
    EchoStateNetwork,
    GraphOfTrust,
    LiquidStateMachine,
    MixtureOfExperts,
    Persistence,
    RadialNeuralNetwork,
    Rebalancer,
    TrainingPipeline,
    SourceStore
  }

  alias Hypatia.VCL.{Client, FileExecutor}

  setup do
    old_root = Application.fetch_env!(:hypatia, :verisimdb_data_path) |> Path.expand()
    assert String.contains?(old_root, "/_build/test/verisim-data")
    keys = [:verisimdb_data_path, :annealing_state_path, :fleet_path]
    original = Map.new(keys, fn key -> {key, Application.fetch_env(:hypatia, key)} end)
    root = Path.join(System.tmp_dir!(), "hypatia-store-cycle-#{System.unique_integer([:positive])}")
    first = Path.join(root, "first")
    second = Path.join(root, "second")
    fleet = Path.join(root, "fleet")

    for store <- [first, second], part <- ["outcomes", "recipes", "patterns", "scans"] do
      File.mkdir_p!(Path.join(store, part))
    end

    for module <- [LearningScheduler, Hypatia.Watcher.AnomalyDetector] do
      pid = Process.whereis(module)
      assert is_pid(pid)
      :ok = :sys.suspend(pid)

      on_exit(fn ->
        # Restore every overridden path before a queued timer may run.
        for {key, value} <- original do
          case value do
            {:ok, previous} -> Application.put_env(:hypatia, key, previous)
            :error -> Application.delete_env(:hypatia, key)
          end
        end

        if Process.alive?(pid), do: :sys.resume(pid)
      end)
    end

    Application.put_env(:hypatia, :verisimdb_data_path, first)
    Application.delete_env(:hypatia, :annealing_state_path)
    Application.put_env(:hypatia, :fleet_path, fleet)

    on_exit(fn ->
      for {key, value} <- original do
        case value do
          {:ok, previous} -> Application.put_env(:hypatia, key, previous)
          :error -> Application.delete_env(:hypatia, key)
        end
      end

      File.rm_rf!(root)
    end)

    %{root: root, first: first, second: second, fleet: fleet}
  end

  test "unchanged fleet bytes are imported once into each runtime outcome store", ctx do
    line =
      Jason.encode!(%{
        "recipe_id" => "recipe-store-cache",
        "repo" => "synthetic",
        "file" => ".",
        "bot" => "fixture",
        "outcome" => "failure",
        "verdict" => "failed",
        "exit_code" => 1
      })

    fleet_file = write_fleet(ctx.fleet, line <> "\n")
    source_id = LearningScheduler.fleet_source_id(line, 1)

    assert {1, first_signature} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert {:ok, ids} = OutcomeTracker.imported_fleet_ids()
    assert ids == MapSet.new([source_id])
    first_bytes = directory_bytes(Path.join(ctx.first, "outcomes"))
    assert map_size(first_bytes) == 1

    Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)
    assert {:ok, empty} = OutcomeTracker.imported_fleet_ids()
    assert empty == MapSet.new()
    assert {1, second_signature} = LearningScheduler.ingest_fleet_outcomes(first_signature)
    refute second_signature == first_signature
    assert {:ok, ids} = OutcomeTracker.imported_fleet_ids()
    assert ids == MapSet.new([source_id])
    second_bytes = directory_bytes(Path.join(ctx.second, "outcomes"))
    assert map_size(second_bytes) == 1

    assert {0, ^second_signature} = LearningScheduler.ingest_fleet_outcomes(second_signature)
    assert directory_bytes(Path.join(ctx.second, "outcomes")) == second_bytes
    assert directory_bytes(Path.join(ctx.first, "outcomes")) == first_bytes

    Application.put_env(:hypatia, :verisimdb_data_path, ctx.first)
    assert {0, ^first_signature} = LearningScheduler.ingest_fleet_outcomes(second_signature)
    assert directory_bytes(Path.join(ctx.first, "outcomes")) == first_bytes
    assert File.read!(fleet_file) == line <> "\n"
  end

  test "a legacy content-only cache signature cannot acknowledge an empty destination", ctx do
    line =
      Jason.encode!(%{
        "recipe_id" => "recipe-legacy-cache",
        "repo" => "synthetic",
        "file" => ".",
        "bot" => "fixture",
        "outcome" => "failure",
        "verdict" => "failed",
        "exit_code" => 1
      })

    content = line <> "\n"
    fleet_file = write_fleet(ctx.fleet, content)
    legacy_signature = :crypto.hash(:sha256, content)

    assert {1, signature} = LearningScheduler.ingest_fleet_outcomes(legacy_signature)
    refute signature == legacy_signature
    assert {0, ^signature} = LearningScheduler.ingest_fleet_outcomes(signature)
    assert File.read!(fleet_file) == content
  end

  test "annealing writer and report follow the same runtime default directory", ctx do
    record_failures("recipe-first-stage", 1)
    first_path = Path.join([ctx.first, "annealing-states", "recipe-first-stage.json"])
    first_bytes = File.read!(first_path)
    assert Jason.decode!(first_bytes)["qualification_version"] == 1
    assert LearningScheduler.report_annealing_state() == stage_report(1, 0, 0)

    Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)
    assert LearningScheduler.report_annealing_state() == stage_report(0, 0, 0)
    record_failures("recipe-second-stage", 5)
    second_path = Path.join([ctx.second, "annealing-states", "recipe-second-stage.json"])
    assert Jason.decode!(File.read!(second_path))["outcome_count"] == 5
    assert LearningScheduler.report_annealing_state() == stage_report(0, 1, 1)
    assert File.read!(first_path) == first_bytes

    Application.put_env(:hypatia, :verisimdb_data_path, ctx.first)
    assert LearningScheduler.report_annealing_state() == stage_report(1, 0, 0)
  end

  test "an explicit runtime annealing override is shared by writer and report", ctx do
    first = Path.join(ctx.root, "annealing-first")
    second = Path.join(ctx.root, "annealing-second")
    Application.put_env(:hypatia, :annealing_state_path, first)
    assert Paths.annealing_states() == first
    record_failures("recipe-override-first", 1)
    first_bytes = directory_bytes(first)
    assert LearningScheduler.report_annealing_state() == stage_report(1, 0, 0)

    Application.put_env(:hypatia, :annealing_state_path, second)
    assert Paths.annealing_states() == second
    assert LearningScheduler.report_annealing_state() == stage_report(0, 0, 0)
    record_failures("recipe-override-second", 5)
    assert LearningScheduler.report_annealing_state() == stage_report(0, 1, 1)
    assert directory_bytes(first) == first_bytes
    refute File.exists?(Path.join(ctx.first, "annealing-states"))

    Application.put_env(:hypatia, :annealing_state_path, first)
    assert LearningScheduler.report_annealing_state() == stage_report(1, 0, 0)
  end

  test "confidence persists one qualified evidence snapshot without touching the other store", ctx do
    recipe = "recipe-confidence-stable"
    paths = confidence_fixture(ctx, recipe)
    second_before = store_tree_bytes(ctx.second)
    outcomes_before = directory_bytes(Path.join(ctx.first, "outcomes"))

    assert {:ok, confidence} = OutcomeTracker.update_recipe_confidence(recipe)
    assert is_float(confidence)

    updated = Jason.decode!(File.read!(paths.recipe))
    assert updated["confidence_prior"] == 0.8
    assert updated["successful_fixes"] == 0
    assert updated["failed_fixes"] == 1
    assert updated["total_attempts"] == 2
    assert updated["unqualified_attempts"] == 1
    assert updated["confidence"] == Float.round(confidence, 4)
    assert Jason.decode!(File.read!(paths.annealing))["outcome_count"] == 1
    assert store_tree_bytes(ctx.second) == second_before
    assert directory_bytes(Path.join(ctx.first, "outcomes")) == outcomes_before
  end

  test "confidence refuses an A-to-B switch after evidence and before either write", ctx do
    recipe = "recipe-confidence-switch"
    confidence_fixture(ctx, recipe)
    before = store_tree_bytes(ctx.root)

    assert {:error, {:outcome_store_changed, captured, current}} =
             OutcomeTracker.update_recipe_confidence(recipe, fn ->
               send(self(), :confidence_evidence_collected)
               Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)
             end)

    assert_received :confidence_evidence_collected
    assert captured.data_root == ctx.first
    assert current.data_root == ctx.second
    assert captured.annealing_dir == Path.join(ctx.first, "annealing-states")
    assert current.annealing_dir == Path.join(ctx.second, "annealing-states")
    assert store_tree_bytes(ctx.root) == before
  end

  test "both explicit transactions refuse an annealing override change without redirecting writes",
       ctx do
    recipe = "recipe-confidence-override"
    confidence_fixture(ctx, recipe)
    first_override = Path.join(ctx.root, "override-first")
    second_override = Path.join(ctx.root, "override-second")

    for transaction <- [:confidence, :annealing] do
      Application.put_env(:hypatia, :annealing_state_path, first_override)
      before = store_tree_bytes(ctx.root)
      barrier = fn -> Application.put_env(:hypatia, :annealing_state_path, second_override) end

      result =
        case transaction do
          :confidence -> OutcomeTracker.update_recipe_confidence(recipe, barrier)
          :annealing -> OutcomeTracker.update_annealing_state(recipe, :failure, barrier)
        end

      assert {:error, {:outcome_store_changed, captured, current}} = result
      assert captured.data_root == ctx.first
      assert current.data_root == ctx.first
      assert captured.annealing_dir == first_override
      assert current.annealing_dir == second_override
      assert store_tree_bytes(ctx.root) == before
      refute File.exists?(first_override)
      refute File.exists?(second_override)
    end
  end

  test "incremental annealing advances A but refuses a later switch before persistence", ctx do
    recipe = "recipe-incremental-switch"
    paths = confidence_fixture(ctx, recipe)
    second_before = store_tree_bytes(ctx.second)

    assert {:ok, first} = OutcomeTracker.update_annealing_state(recipe, :failure, fn -> :ok end)
    assert first.outcome_count == 1

    write_rows(ctx.first, List.duplicate(outcome(recipe, "failure", "failed"), 2))
    assert {:ok, second} = OutcomeTracker.update_annealing_state(recipe, :failure, fn -> :ok end)
    assert second.outcome_count == 2
    assert :failure in second.recent_outcomes
    assert Jason.decode!(File.read!(paths.annealing))["outcome_count"] == 2
    assert store_tree_bytes(ctx.second) == second_before

    write_rows(ctx.first, List.duplicate(outcome(recipe, "failure", "failed"), 3))
    before = store_tree_bytes(ctx.root)

    assert {:error, {:outcome_store_changed, captured, current}} =
             OutcomeTracker.update_annealing_state(recipe, :failure, fn ->
               send(self(), :incremental_evidence_collected)
               Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)
             end)

    assert_received :incremental_evidence_collected
    assert captured.data_root == ctx.first
    assert current.data_root == ctx.second
    assert store_tree_bytes(ctx.root) == before
  end

  test "a stable explicit override receives the state and leaves the default absent", ctx do
    recipe = "recipe-confidence-stable-override"
    paths = confidence_fixture(ctx, recipe)
    override = Path.join(ctx.root, "override-stable")
    Application.put_env(:hypatia, :annealing_state_path, override)
    second_before = store_tree_bytes(ctx.second)

    assert {:ok, _confidence} = OutcomeTracker.update_recipe_confidence(recipe)
    state_path = Path.join(override, recipe <> ".json")
    assert Jason.decode!(File.read!(state_path))["outcome_count"] == 1
    refute File.exists?(paths.annealing)
    assert store_tree_bytes(ctx.second) == second_before
  end

  test "annealing I/O refusal is returned before the recipe summary is written", ctx do
    recipe = "recipe-confidence-io"
    confidence_fixture(ctx, recipe)
    blocker = Path.join(ctx.root, "not-a-directory")
    File.write!(blocker, "synthetic I/O refusal")
    override = Path.join(blocker, "states")
    Application.put_env(:hypatia, :annealing_state_path, override)
    before = store_tree_bytes(ctx.root)

    assert {:error, {:annealing_state_write, path, reason}} =
             OutcomeTracker.update_recipe_confidence(recipe)

    assert path == Path.join(override, recipe <> ".json")
    assert reason in [:enotdir, :eexist]
    assert store_tree_bytes(ctx.root) == before

    assert {:error, {:annealing_state_write, ^path, second_reason}} =
             OutcomeTracker.update_annealing_state(recipe, :failure, fn -> :ok end)

    assert second_reason in [:enotdir, :eexist]
    assert store_tree_bytes(ctx.root) == before
  end

  test "diagnostic and numeric annealing reads reconcile without a hidden disk write", ctx do
    recipe = "recipe-read-only-reconcile"
    paths = confidence_fixture(ctx, recipe)

    write_json(paths.annealing, %{
      "qualification_version" => 1,
      "temperature" => 0.1,
      "outcome_count" => 1000,
      "stage" => "veteran",
      "recent_outcomes" => ["success"],
      "last_reheat" => nil,
      "created_at" => "2026-11-04T00:00:00Z",
      "updated_at" => "2026-11-04T00:00:00Z"
    })

    before = store_tree_bytes(ctx.root)
    state = OutcomeTracker.get_annealing_state(recipe)
    assert state.outcome_count == 1
    assert state.stage == :nascent
    assert OutcomeTracker.annealed_strategy(recipe, :auto_execute) == :report_only
    assert is_float(OutcomeTracker.cross_repo_confidence(recipe, 0.9))
    assert OutcomeTracker.get_annealing_state("recipe-absent").outcome_count == 0
    assert store_tree_bytes(ctx.root) == before
    refute File.exists?(Path.join([ctx.first, "annealing-states", "recipe-absent.json"]))
  end

  test "a durable raw outcome remains committed when derived persistence is refused", ctx do
    recipe = "recipe-durable-derived-refusal"
    paths = confidence_fixture(ctx, recipe)
    recipe_before = File.read!(paths.recipe)
    second_before = store_tree_bytes(ctx.second)
    blocker = Path.join(ctx.root, "derived-write-blocker")
    File.write!(blocker, "synthetic I/O refusal")
    Application.put_env(:hypatia, :annealing_state_path, Path.join(blocker, "states"))

    assert {:ok, record} =
             OutcomeTracker.record_fleet_outcome(
               recipe,
               "synthetic",
               ".",
               :failure,
               "durable-derived-refusal",
               "failed"
             )

    assert record["fleet_import_id"] == "durable-derived-refusal"
    assert {:ok, ids} = OutcomeTracker.imported_fleet_ids()
    assert MapSet.member?(ids, "durable-derived-refusal")
    assert File.read!(paths.recipe) == recipe_before
    assert store_tree_bytes(ctx.second) == second_before
    refute File.exists?(paths.annealing)
  end

  test "ESN input switches outcomes without promoting historical successes", ctx do
    first = outcome("recipe-first-series", "failure", "failed")
    second = outcome("recipe-second-series", "success", "verified")
    legacy = outcome("recipe-legacy-series", "success", "unverified")
    write_rows(ctx.first, List.duplicate(first, 10) ++ List.duplicate(legacy, 20))
    write_rows(ctx.second, List.duplicate(second, 10) ++ List.duplicate(legacy, 20))

    expected_first = [
      %{recipe_id: "recipe-first-series", count: 10, series: List.duplicate(0.0, 10)}
    ]

    expected_second = [
      %{recipe_id: "recipe-second-series", count: 10, series: List.duplicate(1.0, 10)}
    ]
    assert TrainingPipeline.build_esn_training_data() == expected_first

    Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)
    assert TrainingPipeline.build_esn_training_data() == expected_second
    Application.put_env(:hypatia, :verisimdb_data_path, ctx.first)
    assert TrainingPipeline.build_esn_training_data() == expected_first
  end

  test "an empty replacement cannot fall back to the preceding training store", ctx do
    write_rows(ctx.first, List.duplicate(outcome("recipe-present", "failure", "failed"), 10))
    write_substitution(ctx.first, "eliminate")
    assert {:ok, _} = PatternRegistry.sync_from_scans([scan("first", "High")])
    assert [%{count: 10}] = TrainingPipeline.build_esn_training_data()
    assert {[_], [1.0]} = TrainingPipeline.build_rbf_training_data()
    assert length(Rebalancer.load_failure_templates()) == 10

    Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)
    assert TrainingPipeline.build_esn_training_data() == []
    assert TrainingPipeline.build_rbf_training_data() == {[], []}
    assert Rebalancer.load_failure_templates() == []
    assert PatternRegistry.all_patterns() == []
  end

  test "registry writer, substitutions and RBF reader share the selected store", ctx do
    write_substitution(ctx.first, "control")
    write_substitution(ctx.second, "eliminate")
    first_scan = scan("first", "Low")
    second_scan = scan("second", "High")

    assert {:ok, first_registry} = PatternRegistry.sync_from_scans([first_scan])
    [first_pattern] = Map.values(first_registry["patterns"])
    assert first_pattern["triangle_tier"] == "control"
    assert first_pattern["repos_affected_list"] == ["first"]
    assert {[first_vector], first_targets} = TrainingPipeline.build_rbf_training_data()
    assert first_targets == [0.0]
    assert length(first_vector) == 8
    first_bytes = directory_bytes(Path.join(ctx.first, "patterns"))

    Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)
    assert PatternRegistry.all_patterns() == []
    assert {:ok, second_registry} = PatternRegistry.sync_from_scans([second_scan])
    [second_pattern] = Map.values(second_registry["patterns"])
    assert second_pattern["triangle_tier"] == "eliminate"
    assert second_pattern["repos_affected_list"] == ["second"]
    assert {[second_vector], [1.0]} = TrainingPipeline.build_rbf_training_data()
    assert length(second_vector) == 8
    refute second_vector == first_vector
    assert directory_bytes(Path.join(ctx.first, "patterns")) == first_bytes
    assert PatternRegistry.get_pattern(first_pattern["id"]) == nil
    assert {:ok, ^second_registry} = VerisimConnector.fetch_pattern_registry_fallback()

    Application.put_env(:hypatia, :verisimdb_data_path, ctx.first)
    assert PatternRegistry.get_pattern(second_pattern["id"]) == nil
    assert PatternRegistry.all_patterns() == [first_pattern]
    assert {[^first_vector], restored_targets} = TrainingPipeline.build_rbf_training_data()
    assert restored_targets == [0.0]
  end

  test "registry synchronization refuses an observed root change without overwriting either store", ctx do
    write_substitution(ctx.first, "control")
    write_substitution(ctx.second, "eliminate")
    assert {:ok, _} = PatternRegistry.sync_from_scans([scan("first-original", "Low")])
    Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)
    assert {:ok, _} = PatternRegistry.sync_from_scans([scan("second-original", "High")])
    first_before = directory_bytes(Path.join(ctx.first, "patterns"))
    second_before = directory_bytes(Path.join(ctx.second, "patterns"))
    Application.put_env(:hypatia, :verisimdb_data_path, ctx.first)

    scans =
      Stream.map([scan("late-first", "High")], fn entry ->
        Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)
        entry
      end)

    assert {:error, {:pattern_store_changed, first, second}} =
             PatternRegistry.sync_from_scans(scans)
    assert {first, second} == {ctx.first, ctx.second}
    assert directory_bytes(Path.join(ctx.first, "patterns")) == first_before
    assert directory_bytes(Path.join(ctx.second, "patterns")) == second_before
  end

  test "registry encoding and filesystem failures cannot return synchronized success", ctx do
    assert {:ok, _} = PatternRegistry.sync_from_scans([scan("original", "Low")])
    path = Path.join([ctx.first, "patterns", "registry.json"])
    before = File.read!(path)
    malformed =
      put_in(scan("unencodable", "High"), [:scan, "weak_points", Access.at(0), "description"],
        fn -> :not_json end)

    assert {:error, {:registry_encoding_failed, _}} =
             PatternRegistry.sync_from_scans([malformed])
    assert File.read!(path) == before

    File.rm!(path)
    File.mkdir!(path)
    assert {:error, {:registry_write_failed, _}} =
             PatternRegistry.sync_from_scans([scan("unwritable", "High")])
    assert File.dir?(path)
    assert File.ls!(path) == []
    refute File.exists?(Path.join([ctx.second, "patterns", "registry.json"]))
  end

  test "corpus templates follow runtime defaults and retain an explicit store override", ctx do
    first =
      Map.put(outcome("recipe-first-corpus", "failure", "failed"), "failure_reason", "timeout")

    second =
      Map.put(
        outcome("recipe-second-corpus", "failure", "failed"),
        "failure_reason",
        "merge_conflict"
      )
    legacy = outcome("recipe-legacy-corpus", "failure", "unverified")
    write_rows(ctx.first, [first, legacy])
    write_rows(ctx.second, [second, second, legacy])

    assert [%{duration: 2}] = Rebalancer.load_failure_templates()
    Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)
    assert [%{duration: 4}, %{duration: 4}] = Rebalancer.load_failure_templates()
    assert [%{duration: 2}] = Rebalancer.load_failure_templates(ctx.first)
    assert Application.fetch_env!(:hypatia, :verisimdb_data_path) == ctx.second
  end

  test "strategy C follows the runtime corpus and honors an explicit override", ctx do
    success = outcome("recipe-corpus-training", "success", "verified")
    failure = outcome("recipe-corpus-training", "failure", "failed")
    write_rows(ctx.first, List.duplicate(success, 70) ++ [failure])
    write_rows(ctx.second, [failure, failure])

    # A small actual ESN keeps this a real training call without an expensive
    # default-size reservoir. No coordinator or dispatch callback is invoked.
    esn = EchoStateNetwork.init(reservoir_size: 4, seed: 17)

    default_log =
      ExUnit.CaptureLog.capture_log(fn ->
        trained =
          TrainingPipeline.train_esn(esn, rebalance_strategy: :c, rebalance_opts: [seed: 17])
        assert trained.trained
      end)

    assert default_log =~ "1 failure templates loaded"
    refute default_log =~ "falling back to Strategy A"

    override_log =
      ExUnit.CaptureLog.capture_log(fn ->
        trained =
          TrainingPipeline.train_esn(esn,
            rebalance_strategy: :c,
            rebalance_opts: [seed: 17, data_path: ctx.second]
          )

        assert trained.trained
      end)

    assert override_log =~ "2 failure templates loaded"
    refute override_log =~ "falling back to Strategy A"
    assert Application.fetch_env!(:hypatia, :verisimdb_data_path) == ctx.first
  end

  test "VCL and direct file backend read each selected root", ctx do
    for {store, marker} <- [{ctx.first, "first"}, {ctx.second, "second"}] do
      write_backend_fixture(store, marker)
    end

    case Process.whereis(Client) do
      nil -> start_supervised!(Client)
      _pid -> :ok
    end

    for {store, marker} <- [{ctx.first, "first"}, {ctx.second, "second"}, {ctx.first, "first"}] do
      Application.put_env(:hypatia, :verisimdb_data_path, store)
      assert [%{repo: "fixture", scan: %{"marker" => ^marker}}] = VerisimConnector.fetch_all_scans()
      assert VerisimConnector.fetch_all_scans_fallback() == VerisimConnector.fetch_all_scans()
      assert {:ok, registry} = VerisimConnector.fetch_pattern_registry()
      assert registry["patterns"]["fixture-pattern"]["marker"] == marker
      assert {:ok, ^registry} = VerisimConnector.fetch_pattern_registry_fallback()
      assert [%{"id" => "recipe-fixture", "marker" => ^marker}] =
               VerisimConnector.fetch_all_recipes()
      assert VerisimConnector.fetch_all_recipes_fallback() == VerisimConnector.fetch_all_recipes()
      assert [%{"marker" => ^marker}] = VerisimConnector.fetch_substitutions()
      assert VerisimConnector.fetch_substitutions_fallback() ==
               VerisimConnector.fetch_substitutions()
      assert {:ok, %{"marker" => ^marker}} = VerisimConnector.fetch_index()
      assert VerisimConnector.fetch_index_fallback() == VerisimConnector.fetch_index()
      assert [%{"recipe_id" => "recipe-fixture", "marker" => ^marker}] =
               VerisimConnector.fetch_all_outcomes()
      assert VerisimConnector.fetch_all_outcomes_fallback() == VerisimConnector.fetch_all_outcomes()
    end
  end

  test "VCL live cache cannot answer a new root with the prior store's results", ctx do
    write_backend_fixture(ctx.first, "first")
    write_backend_fixture(ctx.second, "second")
    {:ok, state} = Client.init([])
    query = "SELECT DOCUMENT FROM STORE scans"
    from = {self(), make_ref()}
    assert {:reply, {:ok, [first]}, state} = Client.handle_call({:query, query, []}, from, state)
    assert first["marker"] == "first"
    assert state.query_count == 1
    assert {:reply, {:ok, [^first]}, state} = Client.handle_call({:query, query, []}, from, state)
    assert state.query_count == 1

    Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)
    assert {:reply, {:ok, [second]}, state} = Client.handle_call({:query, query, []}, from, state)
    assert second["marker"] == "second"
    assert state.query_count == 2

    assert {:reply, {:ok, [^first]}, state} =
             Client.handle_call({:query, query, [data_path_override: ctx.first]}, from, state)

    assert state.query_count == 2
    assert map_size(state.cache) == 2
    Application.put_env(:hypatia, :verisimdb_data_path, ctx.first)
    assert {:reply, {:ok, [^first]}, _state} = Client.handle_call({:query, query, []}, from, state)
  end

  test "explicit file roots govern store, federation and all hexad lookup paths", ctx do
    write_backend_fixture(ctx.first, "first")
    write_backend_fixture(ctx.second, "second")
    Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)

    sources = [
      {:store, "scans"},
      {:federation, "/scans/*", nil},
      {:hexad, "fixture"},
      {:hexad, "recipe-fixture"},
      {:hexad, "fixture-pattern"}
    ]

    for source <- sources do
      assert {:ok, [default]} = FileExecutor.execute(ast(source))
      assert default["marker"] == "second"
      assert {:ok, [overridden]} = FileExecutor.execute(ast(source), data_path_override: ctx.first)
      assert overridden["marker"] == "first"
    end

    assert Application.fetch_env!(:hypatia, :verisimdb_data_path) == ctx.second
  end

  test "language-aware federation resolves scan context within the selected root", ctx do
    for store <- [ctx.first, ctx.second] do
      write_rows(store, [%{"recipe_id" => "recipe-language", "repo" => "fixture"}])
    end

    write_json(Path.join([ctx.first, "scans", "fixture.json"]), %{"primary_language" => "rust"})
    write_json(Path.join([ctx.second, "scans", "fixture.json"]), %{"primary_language" => "elixir"})
    query = ast({:federation, "/outcomes/*", "language_aware"})
    assert {:ok, [first]} = FileExecutor.execute(query)
    assert first["_language_family"] == "systems"

    Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)
    assert {:ok, [second]} = FileExecutor.execute(query)
    assert second["_language_family"] == "beam"
    assert {:ok, [overridden]} = FileExecutor.execute(query, data_path_override: ctx.first)
    assert overridden["_language_family"] == "systems"
    assert Application.fetch_env!(:hypatia, :verisimdb_data_path) == ctx.second
  end

  test "neural persistence follows runtime stores while trust is rebuilt from source", ctx do
    write_json(Path.join([ctx.first, "recipes", "recipe-first.json"]), %{"id" => "recipe-first"})
    write_json(Path.join([ctx.second, "recipes", "recipe-second.json"]), %{"id" => "recipe-second"})

    first_state = neural_state("first")
    assert :ok = Persistence.save_all(first_state)
    assert %{trust: {:ok, first_trust}, rbf: {:ok, first_rbf}} = Persistence.load_all()
    assert first_trust["restore_policy"] == "recompute_from_current_qualified_outcomes"
    assert first_trust["trust_scores"] == %{"recipe-first" => 0.5}
    assert first_rbf["training_stats"] == %{"fixture_store" => "first"}
    first_bytes = directory_bytes(Path.join(ctx.first, "neural-states"))

    Application.put_env(:hypatia, :verisimdb_data_path, ctx.second)
    assert %{trust: :not_found, rbf: :not_found} = Persistence.load_all()
    second_state = neural_state("second")
    assert :ok = Persistence.save_all(second_state)
    assert %{trust: {:ok, second_trust}, rbf: {:ok, second_rbf}} = Persistence.load_all()
    assert second_trust["trust_scores"] == %{"recipe-second" => 0.5}
    assert second_rbf["training_stats"] == %{"fixture_store" => "second"}
    assert directory_bytes(Path.join(ctx.first, "neural-states")) == first_bytes

    Application.put_env(:hypatia, :verisimdb_data_path, ctx.first)
    assert %{trust: {:ok, ^first_trust}, rbf: {:ok, ^first_rbf}} = Persistence.load_all()
    assert GraphOfTrust.build().trust_scores == %{"recipe-first" => 0.5}
  end

  defp confidence_fixture(ctx, recipe) do
    for {root, confidence} <- [{ctx.first, 0.8}, {ctx.second, 0.2}] do
      write_json(Path.join([root, "recipes", recipe <> ".json"]), %{
        "id" => recipe,
        "confidence" => confidence
      })
    end

    write_rows(ctx.first, [
      outcome(recipe, "failure", "failed"),
      outcome(recipe, "success", "unverified")
    ])

    write_rows(ctx.second, List.duplicate(outcome(recipe, "success", "verified"), 5))

    %{
      recipe: Path.join([ctx.first, "recipes", recipe <> ".json"]),
      annealing: Path.join([ctx.first, "annealing-states", recipe <> ".json"])
    }
  end

  defp store_tree_bytes(root) do
    root
    |> Path.join("**/*")
    |> Path.wildcard(match_dot: true)
    |> Enum.filter(&File.regular?/1)
    |> Map.new(fn path -> {Path.relative_to(path, root), File.read!(path)} end)
  end

  defp write_json(path, value) do
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, Jason.encode!(value) <> "\n")
  end

  defp write_rows(root, rows) do
    File.write!(
      Path.join([root, "outcomes", "fixture.jsonl"]),
      Enum.map_join(rows, "", &(Jason.encode!(&1) <> "\n"))
    )
  end

  defp write_fleet(root, content) do
    path = Path.join([root, "shared-context", "learning", "fix-outcomes.jsonl"])
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, content)
    path
  end

  defp directory_bytes(directory) do
    directory
    |> Path.join("*")
    |> Path.wildcard()
    |> Map.new(fn path -> {Path.basename(path), File.read!(path)} end)
  end

  defp record_failures(recipe, count) do
    for index <- 1..count do
      assert {:ok, _} =
               OutcomeTracker.record_fleet_outcome(
                 recipe,
                 "synthetic",
                 ".",
                 :failure,
                 "#{recipe}-#{index}",
                 "failed"
               )
    end
  end

  defp stage_report(nascent, adolescent, reheated) do
    %{nascent: nascent, adolescent: adolescent, mature: 0, veteran: 0, reheated: reheated}
  end

  defp outcome(recipe, result, verification) do
    %{
      "recipe_id" => recipe,
      "repo" => "synthetic",
      "bot" => "fixture",
      "timestamp" => "2026-11-04T00:00:00Z",
      "outcome" => result,
      "verification" => verification
    }
  end

  defp write_substitution(root, tier) do
    write_json(Path.join([root, "recipes", "proven-substitutions.json"]), %{
      "substitutions" => [%{"category" => "CommandInjection", "triangle_tier" => tier}]
    })
  end

  defp scan(repo, severity) do
    %{
      repo: repo,
      scan: %{
        "program_path" => "synthetic/#{repo}",
        "weak_points" => [
          %{
            "category" => "CommandInjection",
            "description" => "#{repo} pattern",
            "severity" => severity
          },
          %{
            "category" => "CommandInjection",
            "description" => "suppressed",
            "severity" => "Critical",
            "suppressed" => true
          }
        ]
      }
    }
  end

  defp write_backend_fixture(root, marker) do
    write_json(Path.join([root, "scans", "fixture.json"]), %{"marker" => marker})
    write_json(Path.join([root, "recipes", "recipe-fixture.json"]), %{
      "id" => "recipe-fixture",
      "marker" => marker
    })

    write_json(Path.join([root, "patterns", "registry.json"]), %{
      "patterns" => %{"fixture-pattern" => %{"marker" => marker}}
    })

    write_json(Path.join([root, "recipes", "proven-substitutions.json"]), %{
      "substitutions" => [%{"marker" => marker}]
    })

    write_json(Path.join(root, "index.json"), %{"marker" => marker})
    write_rows(root, [%{"recipe_id" => "recipe-fixture", "marker" => marker}])
  end

  defp ast(source) do
    %{source: source, modalities: [:document], where: nil, proof: nil, limit: nil, offset: nil}
  end

  defp neural_state(marker) do
    networks =
      Map.new(Persistence.default_networks(), fn {name, module} ->
        pid = start_supervised!(%{id: make_ref(), start: {GenServer, :start_link, [module, [], []]}})
        {name, pid}
      end)

    %{
      trust_graph: GraphOfTrust.build(),
      moe: MixtureOfExperts.init(),
      lsm: LiquidStateMachine.init(reservoir_size: 4, seed: 17),
      esn: EchoStateNetwork.init(reservoir_size: 4, seed: 17),
      rbf: %{RadialNeuralNetwork.init() | training_stats: %{"fixture_store" => marker}},
      cycle_count: 0,
      named_networks: networks
    }
    |> Map.merge(SourceStore.fields())
  end
end
