# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
#
# Variational Autoencoder -- hermeneutic clustering for Layer 4
# Phase 5 in blackboard execution order

defmodule Hypatia.Neural.VariationalAutoencoder do
  @moduledoc """
  Variational Autoencoder for hermeneutic clustering.

  Given traces from different agents making the same decision,
  learns a latent space of "interpretation styles." The latent
  dimensions may reveal what Layer 4 looks like.

  Reads from blackboard: everything (needs full context to cluster)
  Writes to blackboard: latent coordinates, cluster ID,
  interpretation style label.

  This IS Layer 4 analysis -- the VAE's latent space is the
  telescope's first image of agent interpretive patterns.
  """

  use GenServer

  alias Hypatia.Neural.{Blackboard, SourceStore}

  defstruct [
    # input → latent (mu, sigma)
    :encoder_weights,
    # latent → reconstruction
    :decoder_weights,
    # dimension of latent space
    :latent_dim,
    # learned cluster centers
    :clusters,
    # human-readable cluster names
    :cluster_labels,
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
      encoder_weights: nil,
      decoder_weights: nil,
      latent_dim: 8,
      clusters: [],
      cluster_labels: %{}
    }

    {:ok, state}
  end

  @doc "Cluster an interpretation based on full blackboard context."
  def cluster(finding) do
    GenServer.call(__MODULE__, {:cluster, SourceStore.current(), finding, Blackboard})
  end

  @doc "Snapshot serialisable state for M17 persistence."
  def state, do: GenServer.call(__MODULE__, :get_state)

  @doc "Restore state from a previously snapshotted map."
  def restore(map) when is_map(map), do: GenServer.call(__MODULE__, {:restore, SourceStore.current(), map})
  def restore(_), do: {:error, :unbound_snapshot}

  @impl true
  def handle_call({:cluster, source, _finding, blackboard}, _from, state) do
    SourceStore.reply(state, source, fn ->
      # Read full blackboard context
      context = SourceStore.accept!(Blackboard.snapshot_source(source, blackboard))

      # Encode to latent space (placeholder -- needs training)
      latent = List.duplicate(0.0, state.latent_dim)

      # Find nearest cluster
      cluster_id = 0
      label = Map.get(state.cluster_labels, cluster_id, "unclustered")

      result = %{
        latent: latent,
        cluster_id: cluster_id,
        label: label,
        context_size: map_size(context.networks)
      }

      SourceStore.accept!(Blackboard.write_source(source, :vae, :interpretation, result, blackboard))
      SourceStore.accept!(Blackboard.write_source(source, :vae, :latent, latent, blackboard))

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

      restored = %{
        state
        | encoder_weights: Map.get(map, "encoder_weights", state.encoder_weights),
          decoder_weights: Map.get(map, "decoder_weights", state.decoder_weights),
          latent_dim: Map.get(map, "latent_dim", state.latent_dim),
          clusters: Map.get(map, "clusters", state.clusters),
          cluster_labels:
            map
            |> Map.get("cluster_labels", %{})
            |> Map.new(fn {k, v} ->
              {case Integer.parse(to_string(k)) do
                 {n, ""} -> n
                 _ -> k
               end, v}
            end)
      }

      {:ok, restored}
    end)
  end

  def handle_call(_unbound_request, _from, state) do
    {:reply, {:error, :unbound_request}, SourceStore.refuse(state, :unbound_request)}
  end

  defp snapshot(state) do
    snap = %{
      "encoder_weights" => state.encoder_weights,
      "decoder_weights" => state.decoder_weights,
      "latent_dim" => state.latent_dim,
      "clusters" => state.clusters,
      "cluster_labels" => Map.new(state.cluster_labels, fn {k, v} -> {to_string(k), v} end),
      "trained" => state.encoder_weights != nil
    }

    SourceStore.tag(snap, state)
  end

end
