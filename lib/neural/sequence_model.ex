# SPDX-License-Identifier: PMPL-1.0-or-later
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

  defstruct [
    :hidden_state,     # current hidden state
    :hidden_dim,       # hidden dimension
    :sequence_buffer,  # recent decisions for context
    :max_sequence,     # maximum sequence length
    :weights           # trained weights (nil until trained)
  ]

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    state = %__MODULE__{
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
    GenServer.call(__MODULE__, {:predict, finding})
  end

  @doc "Append a decision to the sequence buffer (for training)."
  def observe(decision) do
    GenServer.cast(__MODULE__, {:observe, decision})
  end

  @impl true
  def handle_call({:predict, _finding}, _from, state) do
    # Read full blackboard context
    _blackboard = Hypatia.Neural.Blackboard.snapshot()

    # Predict from sequence buffer (placeholder -- needs training)
    prediction = case state.sequence_buffer do
      [] -> %{next_step: "unknown", confidence: 0.0}
      [last | _] -> %{next_step: last, confidence: 0.3}
    end

    result = %{
      predicted_next: prediction.next_step,
      confidence: prediction.confidence,
      sequence_length: length(state.sequence_buffer),
      model_trained: state.weights != nil
    }

    Hypatia.Neural.Blackboard.write(:sequence, :prediction, result)

    {:reply, result, state}
  end

  @impl true
  def handle_cast({:observe, decision}, state) do
    buffer = [decision | state.sequence_buffer]
    |> Enum.take(state.max_sequence)
    {:noreply, %{state | sequence_buffer: buffer}}
  end
end
