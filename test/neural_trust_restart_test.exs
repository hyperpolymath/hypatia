# SPDX-License-Identifier: MPL-2.0
# Synthetic source and snapshot controls; no production corpus is modified.
defmodule Hypatia.Neural.TrustRestartTest do
  use ExUnit.Case, async: false

  alias Hypatia.Neural.{Coordinator, GraphOfTrust, Persistence}

  setup do
    root = Application.fetch_env!(:hypatia, :verisimdb_data_path) |> Path.expand()
    # The standard test configuration is explicitly a disposable build store.
    assert String.contains?(root, "/_build/test/verisim-data")
    recipe = Path.join([root, "recipes", "restart-qualification-fixture.json"])
    snapshot = Path.join([root, "neural-states", "trust.json"])
    originals = for path <- [recipe, snapshot], into: %{}, do: {path, File.read(path)}
    File.mkdir_p!(Path.dirname(recipe))
    File.mkdir_p!(Path.dirname(snapshot))
    File.write!(recipe, Jason.encode!(%{"id" => "restart-qualification-fixture"}))

    on_exit(fn ->
      Application.put_env(:hypatia, :verisimdb_data_path, root)

      for {path, original} <- originals do
        case original do
          {:ok, bytes} -> File.write!(path, bytes)
          {:error, :enoent} -> File.rm(path)
        end
      end
    end)

    %{root: root, snapshot: snapshot}
  end

  test "coordinator startup rejects legacy, current-labelled and future trust-score caches", %{
    snapshot: path
  } do
    for version <- [nil, 2, 999] do
      snapshot = %{
        "trust_scores" => %{
          "restart-qualification-fixture" => 0.999,
          "legacy-only-poisoned-entry" => 1.0
        },
        "score_semantics" => "qualified_positive_support",
        "restorable" => true
      }

      snapshot = if version, do: Map.put(snapshot, "schema_version", version), else: snapshot
      File.write!(path, Jason.encode!(snapshot))

      log =
        ExUnit.CaptureLog.capture_log(fn ->
          assert {:ok, state} = Coordinator.init(isolated_cohort())
          assert state.trust_graph.nodes["restart-qualification-fixture"] == :recipe

          assert GraphOfTrust.trust_score(state.trust_graph, "restart-qualification-fixture") ==
                   0.5

          refute Map.has_key?(state.trust_graph.trust_scores, "legacy-only-poisoned-entry")
        end)

      assert log =~ "persisted trust scores ignored"
      refute log =~ "trust scores restored"
    end
  end

  test "runtime store replacement rebuilds only that source and writes diagnostic versioned snapshots",
       %{
         root: root,
         snapshot: old_snapshot
       } do
    File.write!(old_snapshot, Jason.encode!(%{"trust_scores" => %{"old-store-poison" => 1.0}}))
    replacement = Path.join(root, "replacement-store")
    File.mkdir_p!(Path.join(replacement, "recipes"))

    File.write!(
      Path.join([replacement, "recipes", "fresh.json"]),
      Jason.encode!(%{"id" => "replacement-only-recipe"})
    )

    on_exit(fn -> File.rm_rf!(replacement) end)
    Application.put_env(:hypatia, :verisimdb_data_path, replacement)
    assert {:ok, state} = Coordinator.init(isolated_cohort())
    assert state.trust_graph.nodes == %{"replacement-only-recipe" => :recipe}
    assert state.trust_graph.trust_scores == %{"replacement-only-recipe" => 0.5}
    assert state.source_status == :ready
    assert :ok = Persistence.save_all(state)
    new_snapshot = Path.join([replacement, "neural-states", "trust.json"])
    decoded = new_snapshot |> File.read!() |> Jason.decode!()
    assert decoded["schema_version"] == 2
    assert decoded["restore_policy"] == "recompute_from_current_qualified_outcomes"
    assert decoded["trust_scores"] == %{"replacement-only-recipe" => 0.5}

    assert File.read!(old_snapshot) |> Jason.decode!() == %{
             "trust_scores" => %{"old-store-poison" => 1.0}
           }

    # Even an apparently current snapshot can be altered or outlive its
    # source. The writer's version is metadata, not restore authority.
    File.write!(new_snapshot, Jason.encode!(Map.put(decoded, "trust_scores", %{"poison" => 1.0})))
    assert {:ok, restarted} = Coordinator.init(isolated_cohort())
    assert restarted.trust_graph.trust_scores == state.trust_graph.trust_scores

    # A fresh qualified success must still contribute after restart. Keeping
    # every entity permanently neutral is not a valid way to reject caches.
    File.mkdir_p!(Path.join(replacement, "outcomes"))

    File.write!(
      Path.join([replacement, "outcomes", "synthetic-restart.jsonl"]),
      Jason.encode!(%{
        "recipe_id" => "replacement-only-recipe",
        "repo" => "replacement-repo",
        "bot" => "replacement-bot",
        "outcome" => "success",
        "verification" => "verified"
      }) <> "\n"
    )

    assert {:ok, supported} = Coordinator.init(isolated_cohort())
    assert GraphOfTrust.trust_score(supported.trust_graph, "replacement-only-recipe") > 0.5
    refute Map.has_key?(supported.trust_graph.trust_scores, "poison")
  end

  defp isolated_cohort do
    networks =
      Map.new(Persistence.default_networks(), fn {name, module} ->
        pid = start_supervised!(%{id: make_ref(), start: {GenServer, :start_link, [module, [], []]}})
        {name, pid}
      end)

    board =
      start_supervised!(%{
        id: make_ref(),
        start: {GenServer, :start_link, [Hypatia.Neural.Blackboard, [isolated: true], []]}
      })

    [named_networks: networks, blackboard: board]
  end
end
