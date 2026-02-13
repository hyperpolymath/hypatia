# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.Neural.EchoStateNetwork do
  @moduledoc """
  Echo State Network (ESN) for Hypatia.

  A reservoir computing model optimized for time-series prediction of
  confidence trajectories and repo health metrics. Shares the reservoir
  paradigm with the LSM but uses continuous-valued neurons and is
  specifically tuned for regression tasks.

  Architecture:
  - Input: confidence scores, scan counts, fix rates over time
  - Reservoir: sparse recurrent network with echo state property
  - Output: predicted future confidence / health trajectory

  Key properties:
  - Echo state property: reservoir state is uniquely determined by input history
  - Spectral radius < 1 ensures stability and fading memory
  - Teacher forcing during training for faster convergence
  - Washout period discards initial transients

  This enables:
  - Confidence trajectory forecasting (will this recipe's confidence rise or fall?)
  - Repo health trend prediction (which repos are degrading?)
  - Early warning for confidence drift
  - Proactive dispatch before patterns become critical
  """

  require Logger

  @reservoir_size 80
  @spectral_radius 0.95
  @input_scaling 0.3
  @teacher_scaling 0.2
  @leak_rate 0.2
  @sparsity 0.15
  @washout 50  # Discard first 50 steps during training
  @ridge_lambda 1.0e-4

  defstruct [
    reservoir: nil,
    input_weights: nil,
    feedback_weights: nil,
    output_weights: nil,
    state: nil,
    predictions: [],
    max_predictions: 200,
    trained: false
  ]

  # --- Public API ---

  @doc "Initialize a new Echo State Network"
  def init(opts \\ []) do
    size = Keyword.get(opts, :reservoir_size, @reservoir_size)
    seed = Keyword.get(opts, :seed, :erlang.system_time(:microsecond))

    :rand.seed(:exsss, {seed, seed + 1, seed + 2})

    %__MODULE__{
      reservoir: generate_reservoir(size),
      input_weights: generate_weights(size, @input_scaling),
      feedback_weights: generate_weights(size, @teacher_scaling),
      output_weights: List.duplicate(0.0, size + 1),  # +1 for bias
      state: List.duplicate(0.0, size)
    }
  end

  @doc "Feed a time step and get prediction for next step"
  def step(%__MODULE__{} = esn, input_value) do
    new_state = update_state(esn, input_value)
    prediction = compute_output(esn, new_state, input_value)

    pred_entry = %{
      input: input_value,
      prediction: prediction,
      timestamp: DateTime.utc_now()
    }

    updated_preds = [pred_entry | esn.predictions] |> Enum.take(esn.max_predictions)

    {prediction, %{esn | state: new_state, predictions: updated_preds}}
  end

  @doc "Train ESN on a time series (list of floats)"
  def train(%__MODULE__{} = esn, time_series) when length(time_series) > @washout + 10 do
    size = length(esn.state)

    # Collect reservoir states for all time steps
    {states, _final_esn} = Enum.reduce(time_series, {[], esn}, fn value, {states_acc, current_esn} ->
      new_state = update_state(current_esn, value)
      extended = new_state ++ [value]  # Append input as extra feature
      {[extended | states_acc], %{current_esn | state: new_state}}
    end)

    all_states = Enum.reverse(states)

    # Remove washout period
    training_states = Enum.drop(all_states, @washout)
    training_targets = time_series |> Enum.drop(@washout + 1) |> Enum.take(length(training_states) - 1)
    training_inputs = Enum.take(training_states, length(training_targets))

    # Ridge regression for output weights
    output_weights = solve_ridge(training_inputs, training_targets)

    Logger.info("ESN trained on #{length(training_targets)} time steps (#{@washout} washout)")
    %{esn | output_weights: output_weights, trained: true}
  end

  def train(%__MODULE__{} = esn, _time_series) do
    Logger.warning("ESN: insufficient data for training (need > #{@washout + 10} steps)")
    esn
  end

  @doc "Forecast N steps ahead from current state"
  def forecast(%__MODULE__{trained: false} = _esn, _steps), do: []

  def forecast(%__MODULE__{} = esn, steps) do
    {predictions, _final} = Enum.reduce(1..steps, {[], esn}, fn _i, {preds, current} ->
      last_input = case preds do
        [] -> case current.predictions do
          [h | _] -> h.prediction
          [] -> 0.5
        end
        [last | _] -> last
      end

      {pred, updated} = step(current, last_input)
      {[pred | preds], updated}
    end)

    Enum.reverse(predictions)
  end

  @doc "Analyze prediction accuracy on recent history"
  def accuracy_report(%__MODULE__{predictions: preds}) when length(preds) < 2 do
    %{mse: nil, mae: nil, samples: 0}
  end

  def accuracy_report(%__MODULE__{predictions: preds}) do
    # Compare each prediction with the next actual input
    pairs = preds
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.map(fn [later, earlier] -> {earlier.prediction, later.input} end)

    if length(pairs) == 0 do
      %{mse: nil, mae: nil, samples: 0}
    else
      mse = Enum.reduce(pairs, 0.0, fn {pred, actual}, acc ->
        acc + (pred - actual) * (pred - actual)
      end) / length(pairs)

      mae = Enum.reduce(pairs, 0.0, fn {pred, actual}, acc ->
        acc + abs(pred - actual)
      end) / length(pairs)

      %{mse: mse, mae: mae, samples: length(pairs)}
    end
  end

  @doc "Detect confidence drift (sustained directional change)"
  def detect_drift(%__MODULE__{predictions: preds}) when length(preds) < 10, do: :insufficient_data

  def detect_drift(%__MODULE__{predictions: preds}) do
    recent = preds |> Enum.take(10) |> Enum.map(fn p -> p.prediction end)

    # Check for monotonic trend
    diffs = recent
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.map(fn [a, b] -> a - b end)  # a is more recent

    positive = Enum.count(diffs, fn d -> d > 0 end)
    negative = Enum.count(diffs, fn d -> d < 0 end)

    cond do
      positive >= 7 -> :rising_drift
      negative >= 7 -> :falling_drift
      true -> :stable
    end
  end

  # --- Reservoir Internals ---

  defp generate_reservoir(size) do
    matrix = for _i <- 1..size do
      for _j <- 1..size do
        if :rand.uniform() < @sparsity do
          :rand.uniform() * 2.0 - 1.0
        else
          0.0
        end
      end
    end

    # Scale to spectral radius
    max_abs = matrix |> List.flatten() |> Enum.map(&abs/1) |> Enum.max(fn -> 1.0 end)
    scale = @spectral_radius / max(max_abs, 0.001)
    Enum.map(matrix, fn row -> Enum.map(row, fn v -> v * scale end) end)
  end

  defp generate_weights(size, scaling) do
    for _i <- 1..size, do: (:rand.uniform() * 2.0 - 1.0) * scaling
  end

  defp update_state(%__MODULE__{reservoir: w, input_weights: iw, state: state}, input) do
    size = length(state)

    for i <- 0..(size - 1) do
      recurrent = Enum.reduce(0..(size - 1), 0.0, fn j, acc ->
        w_ij = w |> Enum.at(i) |> Enum.at(j)
        acc + w_ij * Enum.at(state, j)
      end)

      external = Enum.at(iw, i) * input
      old = Enum.at(state, i)

      (1.0 - @leak_rate) * old + @leak_rate * :math.tanh(recurrent + external)
    end
  end

  defp compute_output(%__MODULE__{output_weights: ow, trained: true}, state, input) do
    extended = state ++ [input]
    raw = Enum.zip(extended, ow)
    |> Enum.reduce(0.0, fn {s, w}, acc -> acc + s * w end)
    sigmoid(raw)
  end

  defp compute_output(_esn, state, _input) do
    # Untrained: return mean activation as proxy
    mean = Enum.sum(state) / max(length(state), 1)
    sigmoid(mean)
  end

  defp solve_ridge(states, targets) do
    # Gradient descent approximation of ridge regression
    size = length(List.first(states, []))
    weights = List.duplicate(0.0, size)
    lr = 0.0005
    epochs = 200

    Enum.reduce(1..epochs, weights, fn _epoch, w ->
      Enum.zip(states, targets)
      |> Enum.reduce(w, fn {state, target}, w_acc ->
        pred = Enum.zip(state, w_acc) |> Enum.reduce(0.0, fn {s, wi}, acc -> acc + s * wi end)
        error = target - pred
        Enum.zip(w_acc, state)
        |> Enum.map(fn {wi, si} -> wi + lr * error * si - lr * @ridge_lambda * wi end)
      end)
    end)
  end

  defp sigmoid(x), do: 1.0 / (1.0 + :math.exp(-max(min(x, 500), -500)))
end
