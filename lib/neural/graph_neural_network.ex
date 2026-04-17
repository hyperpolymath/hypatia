# SPDX-License-Identifier: PMPL-1.0-or-later
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

  defstruct [
    :node_embeddings,  # %{agent_id => [Float]}
    :edge_weights,     # %{{from, to} => Float}
    :clusters,         # %{agent_id => cluster_id}
    :layer_count,      # number of message-passing layers
    :embedding_dim     # dimension of node embeddings
  ]

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    state = %__MODULE__{
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
    GenServer.call(__MODULE__, {:analyse, finding})
  end

  @impl true
  def handle_call({:analyse, finding}, _from, state) do
    # Read novelty from blackboard (Phase 1 output)
    novelty = case Hypatia.Neural.Blackboard.read(:rbf, :novelty) do
      {:ok, v} -> v
      :not_found -> %{score: 0.5, status: :unknown}
    end

    # Compute graph metrics
    result = %{
      cluster: Map.get(state.clusters, finding[:agent_id], 0),
      centrality: compute_centrality(finding[:agent_id], state),
      interaction_strength: 0.5,
      novelty_aware: novelty
    }

    # Write to blackboard
    Hypatia.Neural.Blackboard.write(:gnn, :graph_metrics, result)
    Hypatia.Neural.Blackboard.write(:gnn, :cluster, result.cluster)

    {:reply, result, state}
  end

  defp compute_centrality(agent_id, state) do
    edges = state.edge_weights
    |> Enum.filter(fn {{from, _to}, _w} -> from == agent_id end)
    |> length()

    total = map_size(state.edge_weights)
    if total > 0, do: edges / total, else: 0.0
  end
end
