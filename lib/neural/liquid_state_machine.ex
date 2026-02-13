# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.Neural.LiquidStateMachine do
  @moduledoc """
  Liquid State Machine (LSM) for Hypatia.

  A reservoir computing approach for processing temporal sequences of
  repository events (commits, scans, dispatches, outcomes). The reservoir
  provides a high-dimensional, fading-memory representation of event history.

  Architecture:
  - Input layer: encodes events as spike trains (binary temporal patterns)
  - Reservoir: randomly-connected recurrent network with leaky integrate-and-fire neurons
  - Readout: trained linear layer mapping reservoir state to predictions

  Key properties:
  - Separation property: different inputs produce different reservoir states
  - Fading memory: recent events weighted more heavily (exponential decay)
  - No backpropagation through reservoir (only readout is trained)

  This enables:
  - Temporal pattern detection (e.g., "3 failures in 24h → likely regression")
  - Anomaly detection in event streams
  - Trend prediction for confidence trajectories
  - Sequence-aware dispatch decisions
  """

  require Logger

  @reservoir_size 100
  @spectral_radius 0.9  # Edge of chaos for maximum computation
  @leak_rate 0.3  # Neuron leakiness (fading memory speed)
  @input_scaling 0.5
  @sparsity 0.1  # 10% connectivity in reservoir

  defstruct [
    reservoir_weights: nil,
    input_weights: nil,
    readout_weights: nil,
    state: nil,
    history: [],
    max_history: 1000,
    trained: false
  ]

  # --- Public API ---

  @doc "Initialize a new LSM with random reservoir"
  def init(opts \\ []) do
    size = Keyword.get(opts, :reservoir_size, @reservoir_size)
    seed = Keyword.get(opts, :seed, :erlang.system_time(:microsecond))

    :rand.seed(:exsss, {seed, seed + 1, seed + 2})

    reservoir_weights = generate_reservoir(size)
    input_weights = generate_input_weights(size)
    readout_weights = List.duplicate(0.0, size)
    initial_state = List.duplicate(0.0, size)

    %__MODULE__{
      reservoir_weights: reservoir_weights,
      input_weights: input_weights,
      readout_weights: readout_weights,
      state: initial_state
    }
  end

  @doc "Process an event and update reservoir state"
  def process_event(%__MODULE__{} = lsm, event) do
    input = encode_event(event)
    new_state = update_reservoir(lsm, input)

    history_entry = %{
      event: event,
      state_norm: vector_norm(new_state),
      timestamp: DateTime.utc_now()
    }

    updated_history = [history_entry | lsm.history] |> Enum.take(lsm.max_history)

    %{lsm | state: new_state, history: updated_history}
  end

  @doc "Get prediction from current reservoir state"
  def predict(%__MODULE__{state: state, readout_weights: weights, trained: false}) do
    # Untrained: return reservoir activation as anomaly score
    activation = dot_product(state, List.duplicate(1.0 / length(state), length(state)))
    {sigmoid(activation), :untrained}
  end

  def predict(%__MODULE__{state: state, readout_weights: weights}) do
    raw = dot_product(state, weights)
    {sigmoid(raw), :trained}
  end

  @doc "Train readout weights on collected history with labels"
  def train(%__MODULE__{} = lsm, labeled_events) do
    # Collect reservoir states for each event sequence
    {states, targets} = Enum.reduce(labeled_events, {[], []}, fn {events, target}, {s_acc, t_acc} ->
      # Run event sequence through reservoir
      final_lsm = Enum.reduce(events, %{lsm | state: List.duplicate(0.0, length(lsm.state))},
        fn event, acc -> process_event(acc, event) end)

      {[final_lsm.state | s_acc], [target | t_acc]}
    end)

    # Ridge regression for readout weights
    readout = ridge_regression(Enum.reverse(states), Enum.reverse(targets))

    Logger.info("LSM trained on #{length(labeled_events)} sequences")
    %{lsm | readout_weights: readout, trained: true}
  end

  @doc "Detect anomalous patterns in recent history"
  def detect_anomalies(%__MODULE__{history: history}) when length(history) < 10 do
    []
  end

  def detect_anomalies(%__MODULE__{history: history}) do
    norms = Enum.map(history, fn h -> h.state_norm end)
    mean = Enum.sum(norms) / length(norms)
    variance = Enum.reduce(norms, 0.0, fn n, acc -> acc + (n - mean) * (n - mean) end) / length(norms)
    std_dev = :math.sqrt(max(variance, 0.0001))

    # Flag events with state norm > 2 standard deviations from mean
    history
    |> Enum.filter(fn h -> abs(h.state_norm - mean) > 2 * std_dev end)
    |> Enum.map(fn h -> %{event: h.event, deviation: abs(h.state_norm - mean) / std_dev, timestamp: h.timestamp} end)
  end

  @doc "Get temporal summary of reservoir dynamics"
  def temporal_summary(%__MODULE__{history: history, state: state}) do
    %{
      current_activation: vector_norm(state),
      history_length: length(history),
      recent_trend: compute_trend(Enum.take(history, 20)),
      anomaly_count: length(detect_anomalies(%__MODULE__{history: history}))
    }
  end

  # --- Reservoir Internals ---

  defp generate_reservoir(size) do
    # Sparse random matrix scaled to spectral radius
    matrix = for _i <- 1..size do
      for _j <- 1..size do
        if :rand.uniform() < @sparsity do
          (:rand.uniform() * 2.0 - 1.0)
        else
          0.0
        end
      end
    end

    # Approximate spectral radius scaling
    max_abs = matrix
    |> List.flatten()
    |> Enum.map(&abs/1)
    |> Enum.max(fn -> 1.0 end)

    scale = @spectral_radius / max(max_abs, 0.001)
    Enum.map(matrix, fn row -> Enum.map(row, fn v -> v * scale end) end)
  end

  defp generate_input_weights(size) do
    for _i <- 1..size do
      (:rand.uniform() * 2.0 - 1.0) * @input_scaling
    end
  end

  defp update_reservoir(%__MODULE__{reservoir_weights: w, input_weights: iw, state: state}, input) do
    size = length(state)

    # Leaky integrate-and-fire update
    for i <- 0..(size - 1) do
      # Recurrent input
      recurrent = Enum.reduce(0..(size - 1), 0.0, fn j, acc ->
        w_ij = w |> Enum.at(i) |> Enum.at(j)
        s_j = Enum.at(state, j)
        acc + w_ij * s_j
      end)

      # External input
      external = Enum.at(iw, i) * input

      # Leaky integration
      old = Enum.at(state, i)
      new_val = (1.0 - @leak_rate) * old + @leak_rate * :math.tanh(recurrent + external)
      new_val
    end
  end

  defp encode_event(event) when is_map(event) do
    # Encode event features into a single scalar input
    severity = severity_score(Map.get(event, "severity", "medium"))
    outcome = outcome_score(Map.get(event, "outcome", "unknown"))
    type = type_score(Map.get(event, "type", "scan"))

    (severity * 0.4 + outcome * 0.4 + type * 0.2) * 2.0 - 1.0  # Scale to [-1, 1]
  end

  defp encode_event(_), do: 0.0

  defp severity_score("critical"), do: 1.0
  defp severity_score("high"), do: 0.8
  defp severity_score("medium"), do: 0.5
  defp severity_score("low"), do: 0.3
  defp severity_score("info"), do: 0.1
  defp severity_score(_), do: 0.5

  defp outcome_score("success"), do: 1.0
  defp outcome_score("failure"), do: 0.0
  defp outcome_score("false_positive"), do: -0.5
  defp outcome_score(_), do: 0.5

  defp type_score("scan"), do: 0.3
  defp type_score("dispatch"), do: 0.6
  defp type_score("outcome"), do: 0.9
  defp type_score(_), do: 0.5

  # --- Linear Algebra Helpers ---

  defp dot_product(a, b) do
    Enum.zip(a, b)
    |> Enum.reduce(0.0, fn {ai, bi}, acc -> acc + ai * bi end)
  end

  defp vector_norm(v) do
    :math.sqrt(Enum.reduce(v, 0.0, fn x, acc -> acc + x * x end))
  end

  defp sigmoid(x), do: 1.0 / (1.0 + :math.exp(-max(min(x, 500), -500)))

  defp compute_trend(history) when length(history) < 2, do: :stable

  defp compute_trend(history) do
    norms = Enum.map(history, fn h -> h.state_norm end)
    first_half = Enum.take(norms, div(length(norms), 2))
    second_half = Enum.drop(norms, div(length(norms), 2))

    avg_first = Enum.sum(first_half) / max(length(first_half), 1)
    avg_second = Enum.sum(second_half) / max(length(second_half), 1)

    diff = avg_first - avg_second  # first_half is more recent
    cond do
      diff > 0.1 -> :increasing
      diff < -0.1 -> :decreasing
      true -> :stable
    end
  end

  defp ridge_regression(states, targets, lambda \\ 0.01) do
    # Simplified ridge regression: w = (X^T X + λI)^-1 X^T y
    # For efficiency, use gradient descent approximation
    size = length(List.first(states, []))
    if size == 0, do: [], else: gradient_descent_readout(states, targets, size, lambda)
  end

  defp gradient_descent_readout(states, targets, size, _lambda) do
    weights = List.duplicate(0.0, size)
    lr = 0.001
    epochs = 100

    Enum.reduce(1..epochs, weights, fn _epoch, w ->
      Enum.zip(states, targets)
      |> Enum.reduce(w, fn {state, target}, w_acc ->
        pred = dot_product(state, w_acc)
        error = target - pred
        Enum.zip(w_acc, state)
        |> Enum.map(fn {wi, si} -> wi + lr * error * si end)
      end)
    end)
  end
end
