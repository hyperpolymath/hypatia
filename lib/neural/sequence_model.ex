# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
#
# Sequence Model -- choreography trace prediction
# Phase 6 in blackboard execution order (last)

defmodule Hypatia.Neural.SequenceModel do
  @moduledoc """
  Lightweight sequence model for choreography trace prediction.

  Given a sequence of prior decisions (from the trace cache),
  predicts the next likely decision. Feeds @neural dispatch.

  Reads from blackboard: everything (needs maximum context)
  Writes to blackboard: predicted next step, confidence,
  suggested branch arm.

  Replaces ESN's single-value prediction with multi-step
  choreography-aware prediction.
  """

  use GenServer

  alias Hypatia.Neural.{Blackboard, SourceStore}

  defstruct [
    # current hidden state
    :hidden_state,
    # hidden dimension
    :hidden_dim,
    # recent decisions for context
    :sequence_buffer,
    # maximum sequence length
    :max_sequence,
    # trained weights (nil until trained)
    :weights,
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
      hidden_state: nil,
      hidden_dim: 32,
      sequence_buffer: [],
      max_sequence: 50,
      weights: nil
    }

    {:ok, state}
  end

  @doc "Predict the next decision based on trace history."
  def predict(finding) do
    GenServer.call(__MODULE__, {:predict, SourceStore.current(), finding, Blackboard})
  end

  @doc "Append a decision to the sequence buffer (for training)."
  def observe(decision) do
    GenServer.call(__MODULE__, {:observe, SourceStore.current(), decision})
  end

  @doc """
  Snapshot the network's serialisable state for persistence
  (M17 — neural restart continuity). Returns a plain map; pass
  to `restore/1` on a fresh process to rehydrate.
  """
  def state do
    GenServer.call(__MODULE__, :get_state)
  end

  @doc """
  Restore network state from a previously snapshotted map. Fields
  not present in the map keep their fresh-init defaults. Unbound or
  foreign-source maps are refused and remain available through raw diagnostics.
  """
  def restore(map) when is_map(map) do
    GenServer.call(__MODULE__, {:restore, SourceStore.current(), map})
  end

  def restore(_), do: {:error, :unbound_snapshot}

  @impl true
  def handle_call({:predict, source, _finding, blackboard}, _from, state) do
    SourceStore.reply(state, source, fn ->
      # Read full blackboard context
      _context = SourceStore.accept!(Blackboard.snapshot_source(source, blackboard))

      # Predict from sequence buffer (placeholder -- needs training)
      prediction =
        case state.sequence_buffer do
          [] -> %{next_step: "unknown", confidence: 0.0}
          [last | _] -> %{next_step: last, confidence: 0.3}
        end

      result = %{
        predicted_next: prediction.next_step,
        confidence: prediction.confidence,
        sequence_length: length(state.sequence_buffer),
        model_trained: state.weights != nil
      }

      SourceStore.accept!(Blackboard.write_source(source, :sequence, :prediction, result, blackboard))

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
        | hidden_dim: Map.get(map, "hidden_dim", state.hidden_dim),
          max_sequence: Map.get(map, "max_sequence", state.max_sequence),
          sequence_buffer: Map.get(map, "sequence_buffer", state.sequence_buffer),
          weights: Map.get(map, "weights", state.weights)
      }

      {:ok, restored}
    end)
  end

  def handle_call({:observe, source, decision}, _from, state) do
    SourceStore.reply(state, source, fn ->
      buffer = [decision | state.sequence_buffer] |> Enum.take(state.max_sequence)
      {:ok, %{state | sequence_buffer: buffer}}
    end)
  end

  def handle_call(_unbound_request, _from, state) do
    {:reply, {:error, :unbound_request}, SourceStore.refuse(state, :unbound_request)}
  end

  @impl true
  def handle_cast(_unbound_request, state) do
    {:noreply, SourceStore.refuse(state, :unbound_request)}
  end

  defp snapshot(state) do
    snap = %{
      "hidden_dim" => state.hidden_dim,
      "max_sequence" => state.max_sequence,
      "sequence_buffer" => state.sequence_buffer,
      "weights" => state.weights,
      "trained" => state.weights != nil
    }

    SourceStore.tag(snap, state)
  end

end
