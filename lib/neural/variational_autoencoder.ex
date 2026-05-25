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

  defstruct [
    :encoder_weights,   # input → latent (mu, sigma)
    :decoder_weights,   # latent → reconstruction
    :latent_dim,        # dimension of latent space
    :clusters,          # learned cluster centers
    :cluster_labels     # human-readable cluster names
  ]

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    state = %__MODULE__{
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
    GenServer.call(__MODULE__, {:cluster, finding})
  end

  @doc "Snapshot serialisable state for M17 persistence."
  def state, do: GenServer.call(__MODULE__, :get_state)

  @doc "Restore state from a previously snapshotted map."
  def restore(map) when is_map(map), do: GenServer.call(__MODULE__, {:restore, map})
  def restore(_), do: :ok

  @impl true
  def handle_call({:cluster, _finding}, _from, state) do
    # Read full blackboard context
    blackboard = Hypatia.Neural.Blackboard.snapshot()

    # Encode to latent space (placeholder -- needs training)
    latent = List.duplicate(0.0, state.latent_dim)

    # Find nearest cluster
    cluster_id = 0
    label = Map.get(state.cluster_labels, cluster_id, "unclustered")

    result = %{
      latent: latent,
      cluster_id: cluster_id,
      label: label,
      context_size: map_size(blackboard.networks)
    }

    Hypatia.Neural.Blackboard.write(:vae, :interpretation, result)
    Hypatia.Neural.Blackboard.write(:vae, :latent, latent)

    {:reply, result, state}
  end

  def handle_call(:get_state, _from, state) do
    snap = %{
      "encoder_weights" => state.encoder_weights,
      "decoder_weights" => state.decoder_weights,
      "latent_dim" => state.latent_dim,
      "clusters" => state.clusters,
      "cluster_labels" => Map.new(state.cluster_labels, fn {k, v} -> {to_string(k), v} end),
      "trained" => state.encoder_weights != nil
    }

    {:reply, snap, state}
  end

  def handle_call({:restore, map}, _from, state) do
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

    {:reply, :ok, restored}
  end
end
