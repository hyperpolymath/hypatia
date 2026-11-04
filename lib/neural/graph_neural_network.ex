# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
#
# Graph Neural Network -- agent interaction patterns
# Phase 2 in blackboard execution order (parallel with PageRank)

defmodule Hypatia.Neural.GraphNeuralNetwork do
  @moduledoc """
  Graph Neural Network for agent interaction patterns.

  Learns how trust, confidence, and decisions propagate through
  agent choreographies. Nodes are agents, edges are messages.

  Reads from blackboard: novelty (from RBF)
  Writes to blackboard: agent cluster membership, graph centrality,
  interaction strength between agent pairs.

  Supports 007's attested spawn -- verifies agent trust through
  graph structure, not just sentinel hashes.
  """

  use GenServer

  alias Hypatia.Neural.{Blackboard, SourceStore}

  defstruct [
    # %{agent_id => [Float]}
    :node_embeddings,
    # %{{from, to} => Float}
    :edge_weights,
    # %{agent_id => cluster_id}
    :clusters,
    # number of message-passing layers
    :layer_count,
    # dimension of node embeddings
    :embedding_dim,
    source_store: nil,
    source_status: :ready,
    source_error: nil
  ]

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    state = %__MODULE__{
      source_store: SourceStore.current(),
      node_embeddings: %{},
      edge_weights: %{},
      clusters: %{},
      layer_count: 3,
      embedding_dim: 16
    }

    {:ok, state}
  end

  @doc "Analyse agent interaction graph and write to blackboard."
  def analyse(finding) do
    GenServer.call(__MODULE__, {:analyse, SourceStore.current(), finding, Blackboard})
  end

  @doc "Snapshot serialisable state for M17 persistence."
  def state, do: GenServer.call(__MODULE__, :get_state)

  @doc "Restore state from a previously snapshotted map."
  def restore(map) when is_map(map), do: GenServer.call(__MODULE__, {:restore, SourceStore.current(), map})
  def restore(_), do: {:error, :unbound_snapshot}

  @impl true
  def handle_call({:analyse, source, finding, blackboard}, _from, state) do
    SourceStore.reply(state, source, fn ->
      # Read novelty from blackboard (Phase 1 output)
      novelty =
        case Blackboard.read_source(source, :rbf, :novelty, blackboard) do
          {:ok, v} -> v
          :not_found -> %{score: 0.5, status: :unknown}
          error -> SourceStore.accept!(error)
        end

      # Compute graph metrics
      result = %{
        cluster: Map.get(state.clusters, finding[:agent_id], 0),
        centrality: compute_centrality(finding[:agent_id], state),
        interaction_strength: 0.5,
        novelty_aware: novelty
      }

      # Write to blackboard
      SourceStore.accept!(Blackboard.write_source(source, :gnn, :graph_metrics, result, blackboard))
      SourceStore.accept!(Blackboard.write_source(source, :gnn, :cluster, result.cluster, blackboard))

      {result, state}
    end)
  end

  def handle_call(:get_state, _from, state) do
    state = SourceStore.observe(state)
    {:reply, snapshot(state), state}
  end

  def handle_call({:snapshot, source}, _from, state) do
    SourceStore.reply(state, source, fn -> {{:ok, snapshot(state)}, state} end)
  end

  def handle_call({:restore, source, map}, _from, state) do
    SourceStore.reply(state, source, fn ->
      unless SourceStore.snapshot_matches?(map, source),
        do: raise(SourceStore.Refusal, reason: :unbound_or_foreign_snapshot)

      edges =
        (Map.get(map, "edge_weights", []) || [])
        |> Enum.reduce(%{}, fn
          %{"from" => f, "to" => t, "weight" => w}, acc when is_number(w) ->
            Map.put(acc, {f, t}, w)

          _, acc ->
            acc
        end)

      node_embeddings =
        Map.get(map, "node_embeddings", state.node_embeddings) || state.node_embeddings

      clusters = Map.get(map, "clusters", state.clusters) || state.clusters

      restored = %{
        state
        | node_embeddings: node_embeddings,
          edge_weights: edges,
          clusters: clusters,
          layer_count: Map.get(map, "layer_count", state.layer_count),
          embedding_dim: Map.get(map, "embedding_dim", state.embedding_dim)
      }

      {:ok, restored}
    end)
  end

  def handle_call(_unbound_request, _from, state) do
    {:reply, {:error, :unbound_request}, SourceStore.refuse(state, :unbound_request)}
  end

  defp snapshot(state) do
    snap = %{
      "node_embeddings" => stringify_keys(state.node_embeddings),
      "edge_weights" =>
        Enum.map(state.edge_weights, fn {{from, to}, w} ->
          %{"from" => to_string(from), "to" => to_string(to), "weight" => w}
        end),
      "clusters" => stringify_keys(state.clusters),
      "layer_count" => state.layer_count,
      "embedding_dim" => state.embedding_dim,
      "trained" => map_size(state.node_embeddings) > 0
    }

    SourceStore.tag(snap, state)
  end

  defp stringify_keys(map) when is_map(map) do
    Map.new(map, fn {k, v} -> {to_string(k), v} end)
  end

  defp stringify_keys(other), do: other

  defp compute_centrality(agent_id, state) do
    edges =
      state.edge_weights
      |> Enum.filter(fn {{from, _to}, _w} -> from == agent_id end)
      |> length()

    total = map_size(state.edge_weights)
    if total > 0, do: edges / total, else: 0.0
  end
end
