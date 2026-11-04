# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Hypatia Neural Blackboard -- shared ETS state for blackboard architecture
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

  Uses ETS for lock-free concurrent reads across network GenServers.
  The GenServer process owns the ETS table and manages round lifecycle;
  raw diagnostic reads and writes go directly to ETS. The neural execution
  path uses source-qualified calls through the owner so an old task cannot
  insert an A result into a newly initialized B board.

  Each analysis round:
  1. `start_round_source/2` -- clears this board and increments its round counter
  2. Networks execute in phased order, reading/writing the blackboard
  3. `end_round_source/2` -- returns the source-checked reasoning trace

  Phase order (6 phases, 8 networks):
    1. RBF (novelty) -- everything benefits from knowing novel vs known
    2. PageRank + GNN (parallel) -- graph structure
    3. MoE (domain routing) -- reads novelty + trust
    4. ESN + LSM (parallel) -- temporal, reads domain + novelty
    5. VAE (interpretation clustering) -- reads everything
    6. Sequence model (choreography prediction) -- reads everything
  """

  use GenServer
  require Logger

  alias Hypatia.Neural.SourceStore

  @table_name :hypatia_blackboard
  @meta_table :hypatia_blackboard_meta

  # ── Public API ──────────────────────────────────────────

  @doc "Start the blackboard GenServer (owns the ETS tables)."
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Initialise the blackboard.

  Called automatically by GenServer start. Creates the ETS tables.
  Can also be called manually in test scenarios.
  Returns `:ok`.
  """
  @spec init() :: :ok
  def init do
    # Convenience wrapper -- the real init happens in GenServer init/1.
    # This is a no-op if the GenServer is already running.
    case GenServer.whereis(__MODULE__) do
      nil -> {:error, :not_started}
      _pid -> :ok
    end
  end

  @doc """
  Clear the blackboard, removing all entries.

  Does not affect round counter. Use `start_round/0` for round lifecycle.
  """
  @spec clear() :: :ok
  def clear do
    GenServer.call(__MODULE__, :clear)
  end

  @doc """
  Write a key-value pair to the blackboard (unscoped).

  For network-scoped writes, use `write/3`.
  """
  @spec write(atom(), term()) :: :ok
  def write(key, value) do
    write(:global, key, value)
  end

  @doc """
  Write a network's output to the blackboard.

  The entry is stored in ETS keyed by `{network, key}` with metadata
  including round number and timestamp.
  """
  @spec write(atom(), atom(), term()) :: :ok
  def write(network, key, value) do
    # Write directly to ETS for performance (table is :public).
    # GenServer tracks entry count via periodic sync.
    entry = %{
      network: network,
      key: key,
      value: value,
      round: current_round(),
      timestamp: System.os_time(:millisecond),
      source_store: nil
    }

    :ets.insert(@table_name, {{network, key}, entry})
    :ok
  end

  @doc """
  Read a value from the blackboard by key (unscoped, checks :global namespace).

  Returns `{:ok, value}` or `:not_found`.
  """
  @spec read(atom()) :: {:ok, term()} | :not_found
  def read(key) do
    read(:global, key)
  end

  @doc """
  Read a value from the blackboard scoped to a specific network.

  Returns `{:ok, value}` or `:not_found`.
  Reads directly from ETS (no GenServer bottleneck).
  """
  @spec read(atom(), atom()) :: {:ok, term()} | :not_found
  def read(network, key) do
    case :ets.lookup(@table_name, {network, key}) do
      [{_, entry}] -> {:ok, entry.value}
      [] -> :not_found
    end
  end

  @doc """
  Read all entries written by a specific network.

  Returns a map of `%{key => value}`.
  """
  @spec read_network(atom()) :: map()
  def read_network(network) do
    :ets.tab2list(@table_name)
    |> Enum.filter(fn {{net, _key}, _entry} -> net == network end)
    |> Enum.map(fn {{_net, key}, entry} -> {key, entry.value} end)
    |> Map.new()
  end

  @doc """
  Get the complete blackboard state as a snapshot (full reasoning trace).

  Returns a map with `:round`, `:networks` (grouped by network name),
  `:entry_count`, and `:timestamp`.
  """
  @spec snapshot() :: map()
  def snapshot do
    entries =
      :ets.tab2list(@table_name)
      |> Enum.map(fn {_key, entry} -> entry end)
      |> Enum.group_by(& &1.network)

    %{
      round: current_round(),
      networks: entries,
      entry_count: :ets.info(@table_name, :size),
      timestamp: System.os_time(:millisecond)
    }
  end

  @doc """
  Start a new analysis round.

  Clears all entries from the previous round, increments the round counter,
  and records the start timestamp. Returns the new round number.
  """
  @spec start_round() :: {:ok, non_neg_integer()}
  def start_round do
    GenServer.call(__MODULE__, :start_round)
  end

  @doc """
  End the current analysis round.

  Freezes the blackboard state and returns the complete reasoning trace
  as a snapshot. The snapshot includes all network contributions from
  the completed round.
  """
  @spec end_round() :: {:ok, map()}
  def end_round do
    GenServer.call(__MODULE__, :end_round)
  end

  @doc "Get blackboard statistics (round, entry count, table size)."
  @spec stats() :: map()
  def stats do
    GenServer.call(__MODULE__, :stats)
  end

  @doc "Check if the blackboard ETS table exists and is accessible."
  @spec alive?() :: boolean()
  def alive? do
    case :ets.whereis(@table_name) do
      :undefined -> false
      _ref -> true
    end
  end

  @doc "Start a round only in the board's captured source lifecycle."
  def start_round_source(source, server \\ __MODULE__),
    do: GenServer.call(server, {:source, source, :start_round})

  def end_round_source(source, server \\ __MODULE__),
    do: GenServer.call(server, {:source, source, :end_round})

  def write_source(source, network, key, value, server \\ __MODULE__),
    do: GenServer.call(server, {:source, source, {:write, network, key, value}})

  def read_source(source, network, key, server \\ __MODULE__),
    do: GenServer.call(server, {:source, source, {:read, network, key}})

  def snapshot_source(source, server \\ __MODULE__),
    do: GenServer.call(server, {:source, source, :snapshot})

  def identity_source(source, server \\ __MODULE__),
    do: GenServer.call(server, {:source, source, :identity})

  # ── GenServer Callbacks ────────────────────────────────

  @impl true
  def init(opts) do
    # Isolated actual boards use unnamed tables for lifecycle tests. The
    # production instance retains its existing named diagnostic tables.
    naming = if Keyword.get(opts, :isolated, false), do: [], else: [:named_table]

    table =
      :ets.new(
        @table_name,
        naming ++ [:public, :set, read_concurrency: true, write_concurrency: true]
      )

    meta = :ets.new(@meta_table, naming ++ [:public, :set, read_concurrency: true])
    :ets.insert(meta, {:round, 0})
    :ets.insert(meta, {:round_started_at, nil})
    :ets.insert(meta, {:round_ended_at, nil})

    Logger.info("Blackboard initialised (ETS tables: #{inspect(table)}, #{inspect(meta)})")
    {:ok, Map.merge(%{table: table, meta: meta}, SourceStore.fields())}
  end

  @impl true
  def handle_call(:clear, _from, state) do
    :ets.delete_all_objects(@table_name)
    {:reply, :ok, state}
  end

  @impl true
  def handle_call(:start_round, _from, state) do
    # Clear previous round data
    :ets.delete_all_objects(@table_name)

    # Increment round counter
    new_round = current_round() + 1
    :ets.insert(@meta_table, {:round, new_round})
    :ets.insert(@meta_table, {:round_started_at, System.os_time(:millisecond)})
    :ets.insert(@meta_table, {:round_ended_at, nil})

    Logger.debug("Blackboard round #{new_round} started")
    {:reply, {:ok, new_round}, state}
  end

  @impl true
  def handle_call(:end_round, _from, state) do
    now = System.os_time(:millisecond)
    :ets.insert(@meta_table, {:round_ended_at, now})

    trace = snapshot()
    round = current_round()
    Logger.debug("Blackboard round #{round} ended (#{trace.entry_count} entries)")
    {:reply, {:ok, trace}, state}
  end

  @impl true
  def handle_call(:stats, _from, state) do
    stats = %{
      round: current_round(),
      entries: :ets.info(@table_name, :size),
      table_size: :ets.info(@table_name, :size),
      round_started_at: meta_value(:round_started_at),
      round_ended_at: meta_value(:round_ended_at)
    }

    {:reply, stats, state}
  end

  def handle_call({:source, source, operation}, _from, state) do
    SourceStore.reply(state, source, fn ->
      {source_operation(state, source, operation), state}
    end)
  end

  defp source_operation(state, _source, :identity),
    do: {:ok, SourceStore.tag(%{}, state)}

  defp source_operation(state, _source, :start_round) do
    :ets.delete_all_objects(state.table)
    round = (table_meta(state.meta, :round) || 0) + 1
    :ets.insert(state.meta, {:round, round})
    :ets.insert(state.meta, {:round_started_at, System.os_time(:millisecond)})
    :ets.insert(state.meta, {:round_ended_at, nil})
    {:ok, round}
  end

  defp source_operation(state, source, :end_round) do
    result = source_snapshot(state, source)
    :ets.insert(state.meta, {:round_ended_at, System.os_time(:millisecond)})
    result
  end

  defp source_operation(state, source, :snapshot), do: source_snapshot(state, source)

  defp source_operation(state, source, {:write, network, key, value}) do
    entry = %{
      network: network,
      key: key,
      value: value,
      source_store: source,
      round: table_meta(state.meta, :round),
      timestamp: System.os_time(:millisecond)
    }

    :ets.insert(state.table, {{network, key}, entry})
    :ok
  end

  defp source_operation(state, source, {:read, network, key}) do
    case :ets.lookup(state.table, {network, key}) do
      [{_, entry}] ->
        if Map.get(entry, :source_store) == source and
             entry.round == table_meta(state.meta, :round) do
          {:ok, entry.value}
        else
          {:error, :foreign_blackboard_entry}
        end

      [] ->
        :not_found
    end
  end

  defp source_snapshot(state, source) do
    entries = :ets.tab2list(state.table) |> Enum.map(&elem(&1, 1))
    round = table_meta(state.meta, :round)

    if Enum.all?(entries, &(Map.get(&1, :source_store) == source and &1.round == round)) do
      {:ok,
       %{
         source_store: source,
         round: round,
         networks: Enum.group_by(entries, & &1.network),
         entry_count: length(entries),
         timestamp: System.os_time(:millisecond)
       }}
    else
      {:error, :foreign_blackboard_entry}
    end
  end

  defp table_meta(table, key) do
    case :ets.lookup(table, key) do
      [{^key, value}] -> value
      [] -> nil
    end
  end

  # ── Private Helpers ─────────────────────────────────────

  @doc false
  def current_round do
    case :ets.lookup(@meta_table, :round) do
      [{:round, n}] -> n
      [] -> 0
    end
  rescue
    ArgumentError -> 0
  end

  defp meta_value(key) do
    case :ets.lookup(@meta_table, key) do
      [{^key, v}] -> v
      [] -> nil
    end
  rescue
    ArgumentError -> nil
  end
end
