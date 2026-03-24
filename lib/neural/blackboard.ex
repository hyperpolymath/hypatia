# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
#
# Hypatia Neural Blackboard — shared state for hub-and-spoke→blackboard migration
#
# Replaces the fixed-weight aggregation in Coordinator with a shared ETS table.
# Each network reads the blackboard before computing, writes after.
# No direct network-to-network communication.
#
# Harvard Architecture parallel: blackboard = Data (shared, immutable per cycle),
# network computations = Control (effectful, independent).

defmodule Hypatia.Neural.Blackboard do
  @moduledoc """
  Shared blackboard for neural network coordination.

  Each analysis round:
  1. Input finding written to blackboard
  2. Networks execute in phased order, reading/writing the blackboard
  3. Final state is the complete reasoning trace

  Phase order:
    1. RBF (novelty) — everything benefits from knowing novel vs known
    2. PageRank + GNN (parallel) — graph structure
    3. MoE (domain routing) — reads novelty + trust
    4. ESN + LSM (parallel) — temporal, reads domain + novelty
    5. VAE (interpretation clustering) — reads everything
    6. Sequence model (choreography prediction) — reads everything
  """

  use GenServer

  @table_name :hypatia_blackboard

  # ── Public API ──────────────────────────────────────────

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Clear the blackboard for a new analysis round."
  def clear do
    GenServer.call(__MODULE__, :clear)
  end

  @doc "Write a network's output to the blackboard."
  def write(network, key, value) do
    GenServer.call(__MODULE__, {:write, network, key, value})
  end

  @doc "Read a value from the blackboard."
  def read(network, key) do
    GenServer.call(__MODULE__, {:read, network, key})
  end

  @doc "Read all entries from a specific network."
  def read_network(network) do
    GenServer.call(__MODULE__, {:read_network, network})
  end

  @doc "Get the complete blackboard state (full reasoning trace)."
  def snapshot do
    GenServer.call(__MODULE__, :snapshot)
  end

  @doc "Get blackboard statistics."
  def stats do
    GenServer.call(__MODULE__, :stats)
  end

  # ── GenServer Callbacks ────────────────────────────────

  @impl true
  def init(_opts) do
    table = :ets.new(@table_name, [:named_table, :public, :set])
    {:ok, %{table: table, round: 0, entries: 0}}
  end

  @impl true
  def handle_call(:clear, _from, state) do
    :ets.delete_all_objects(@table_name)
    new_state = %{state | round: state.round + 1, entries: 0}
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call({:write, network, key, value}, _from, state) do
    full_key = {network, key}
    entry = %{
      network: network,
      key: key,
      value: value,
      round: state.round,
      timestamp: System.os_time(:millisecond)
    }
    :ets.insert(@table_name, {full_key, entry})
    {:reply, :ok, %{state | entries: state.entries + 1}}
  end

  @impl true
  def handle_call({:read, network, key}, _from, state) do
    full_key = {network, key}
    result = case :ets.lookup(@table_name, full_key) do
      [{_, entry}] -> {:ok, entry.value}
      [] -> :not_found
    end
    {:reply, result, state}
  end

  @impl true
  def handle_call({:read_network, network}, _from, state) do
    entries = :ets.tab2list(@table_name)
    |> Enum.filter(fn {{net, _key}, _entry} -> net == network end)
    |> Enum.map(fn {{_net, key}, entry} -> {key, entry.value} end)
    |> Map.new()
    {:reply, entries, state}
  end

  @impl true
  def handle_call(:snapshot, _from, state) do
    entries = :ets.tab2list(@table_name)
    |> Enum.map(fn {_key, entry} -> entry end)
    |> Enum.group_by(& &1.network)
    snapshot = %{
      round: state.round,
      networks: entries,
      entry_count: state.entries,
      timestamp: System.os_time(:millisecond)
    }
    {:reply, snapshot, state}
  end

  @impl true
  def handle_call(:stats, _from, state) do
    {:reply, %{
      round: state.round,
      entries: state.entries,
      table_size: :ets.info(@table_name, :size)
    }, state}
  end
end
