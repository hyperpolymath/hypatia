# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Tests for the Hypatia Neural Blackboard — ETS-backed shared state
# for the blackboard architecture (8 networks, 6 phases).

defmodule Hypatia.BlackboardTest do
  use ExUnit.Case, async: false

  alias Hypatia.Neural.Blackboard

  # ===================================================================
  # ETS Operations
  # ===================================================================

  describe "write/read (scoped)" do
    test "write/3 and read/2 round-trip a value" do
      Blackboard.clear()
      :ok = Blackboard.write(:rbf, :novelty, %{score: 0.8, status: :known})
      assert {:ok, %{score: 0.8, status: :known}} = Blackboard.read(:rbf, :novelty)
    end

    test "read/2 returns :not_found for missing key" do
      Blackboard.clear()
      assert :not_found = Blackboard.read(:rbf, :nonexistent)
    end

    test "write/3 overwrites existing key" do
      Blackboard.clear()
      Blackboard.write(:moe, :confidence, 0.7)
      Blackboard.write(:moe, :confidence, 0.9)
      assert {:ok, 0.9} = Blackboard.read(:moe, :confidence)
    end

    test "different networks can use the same key name without collision" do
      Blackboard.clear()
      Blackboard.write(:rbf, :result, "rbf_value")
      Blackboard.write(:moe, :result, "moe_value")
      assert {:ok, "rbf_value"} = Blackboard.read(:rbf, :result)
      assert {:ok, "moe_value"} = Blackboard.read(:moe, :result)
    end
  end

  describe "write/read (unscoped)" do
    test "write/2 and read/1 use :global namespace" do
      Blackboard.clear()
      :ok = Blackboard.write(:finding, %{id: "PA001"})
      assert {:ok, %{id: "PA001"}} = Blackboard.read(:finding)
    end

    test "read/1 returns :not_found for missing global key" do
      Blackboard.clear()
      assert :not_found = Blackboard.read(:nonexistent_global)
    end
  end

  describe "read_network/1" do
    test "returns all entries for a given network" do
      Blackboard.clear()
      Blackboard.write(:rbf, :novelty, 0.8)
      Blackboard.write(:rbf, :similarity, 0.6)
      Blackboard.write(:moe, :confidence, 0.9)

      rbf_entries = Blackboard.read_network(:rbf)
      assert Map.has_key?(rbf_entries, :novelty)
      assert Map.has_key?(rbf_entries, :similarity)
      refute Map.has_key?(rbf_entries, :confidence)
    end

    test "returns empty map for network with no entries" do
      Blackboard.clear()
      assert %{} = Blackboard.read_network(:nonexistent_network)
    end
  end

  describe "snapshot/0" do
    test "returns grouped entries by network" do
      Blackboard.clear()
      Blackboard.write(:rbf, :novelty, 0.8)
      Blackboard.write(:moe, :confidence, 0.9)

      snap = Blackboard.snapshot()
      assert is_map(snap)
      assert Map.has_key?(snap, :networks)
      assert Map.has_key?(snap, :round)
      assert Map.has_key?(snap, :entry_count)
      assert snap.entry_count == 2
    end

    test "snapshot on empty blackboard returns empty networks" do
      Blackboard.clear()
      snap = Blackboard.snapshot()
      assert snap.networks == %{}
      assert snap.entry_count == 0
    end
  end

  # ===================================================================
  # Round Lifecycle
  # ===================================================================

  describe "start_round/0" do
    test "clears previous entries and increments round counter" do
      Blackboard.write(:rbf, :old_data, "should_be_cleared")
      {:ok, round} = Blackboard.start_round()
      assert is_integer(round)
      assert round > 0
      assert :not_found = Blackboard.read(:rbf, :old_data)
    end

    test "successive rounds increment the counter" do
      {:ok, r1} = Blackboard.start_round()
      {:ok, r2} = Blackboard.start_round()
      assert r2 == r1 + 1
    end
  end

  describe "end_round/0" do
    test "returns the reasoning trace with all entries from the round" do
      {:ok, _} = Blackboard.start_round()
      Blackboard.write(:rbf, :novelty, 0.8)
      Blackboard.write(:moe, :confidence, 0.9)

      {:ok, trace} = Blackboard.end_round()
      assert is_map(trace)
      assert trace.entry_count == 2
      assert Map.has_key?(trace, :networks)
      assert Map.has_key?(trace, :round)
    end

    test "round data includes entries written during the round" do
      {:ok, _} = Blackboard.start_round()
      Blackboard.write(:esn, :drift, :no_drift)
      {:ok, trace} = Blackboard.end_round()

      esn_entries = Map.get(trace.networks, :esn, [])
      assert length(esn_entries) == 1
      assert hd(esn_entries).value == :no_drift
    end
  end

  describe "clear/0" do
    test "removes all entries without affecting round counter" do
      {:ok, round_before} = Blackboard.start_round()
      Blackboard.write(:test, :data, "value")
      Blackboard.clear()
      assert :not_found = Blackboard.read(:test, :data)

      # Round counter should not have changed (clear != start_round)
      stats = Blackboard.stats()
      assert stats.round == round_before
    end
  end

  # ===================================================================
  # Concurrent Reads
  # ===================================================================

  describe "concurrent access" do
    test "multiple processes can read from the blackboard concurrently" do
      Blackboard.clear()
      Blackboard.write(:shared, :value, 42)

      tasks =
        for _i <- 1..10 do
          Task.async(fn ->
            Blackboard.read(:shared, :value)
          end)
        end

      results = Task.await_many(tasks, 5_000)
      assert Enum.all?(results, fn r -> r == {:ok, 42} end)
    end

    test "concurrent writes from different networks do not interfere" do
      {:ok, _} = Blackboard.start_round()

      tasks =
        for net <- [:rbf, :moe, :lsm, :esn, :gnn, :vae, :sequence, :pagerank] do
          Task.async(fn ->
            Blackboard.write(net, :result, Atom.to_string(net))
          end)
        end

      Task.await_many(tasks, 5_000)

      for net <- [:rbf, :moe, :lsm, :esn, :gnn, :vae, :sequence, :pagerank] do
        assert {:ok, Atom.to_string(net)} == Blackboard.read(net, :result)
      end
    end

    test "snapshot is consistent (reads all entries atomically from ETS)" do
      {:ok, _} = Blackboard.start_round()

      for i <- 1..20 do
        Blackboard.write(:test, :"key_#{i}", i)
      end

      snap = Blackboard.snapshot()
      assert snap.entry_count == 20
    end
  end

  # ===================================================================
  # Statistics
  # ===================================================================

  describe "stats/0" do
    test "returns round counter and entry count" do
      {:ok, _} = Blackboard.start_round()
      Blackboard.write(:test, :a, 1)
      Blackboard.write(:test, :b, 2)

      stats = Blackboard.stats()
      assert is_integer(stats.round)
      assert stats.entries == 2
      assert Map.has_key?(stats, :round_started_at)
    end
  end

  # ===================================================================
  # alive?/0
  # ===================================================================

  describe "alive?/0" do
    test "returns true when blackboard is running" do
      assert Blackboard.alive?() == true
    end
  end
end
