# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

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
  - The reservoir is scaled so its induced infinity-norm (largest absolute row sum)
    is 0.95. That makes the leaky-tanh update a contraction in the sup norm
    (modulus `1 - leak + leak * 0.95`), which gives the echo state property and
    fading memory, and it bounds the spectral radius by the same value
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
  # Induced infinity-norm of the reservoir (largest absolute row sum). Must stay < 1:
  # it bounds the spectral radius and makes the state update a contraction.
  @reservoir_norm 0.95
  @input_scaling 0.3
  @teacher_scaling 0.2
  @leak_rate 0.2
  @sparsity 0.15
  # Discard first 50 steps during training
  @washout 50
  @ridge_lambda 1.0e-4

  defstruct reservoir: nil,
            input_weights: nil,
            feedback_weights: nil,
            output_weights: nil,
            state: nil,
            predictions: [],
            max_predictions: 200,
            trained: false

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
      # Readout over [state..., input, bias]
      output_weights: List.duplicate(0.0, size + 2),
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
    _size = length(esn.state)

    # Collect reservoir states for all time steps
    {states, _final_esn} =
      Enum.reduce(time_series, {[], esn}, fn value, {states_acc, current_esn} ->
        new_state = update_state(current_esn, value)
        {[features(new_state, value) | states_acc], %{current_esn | state: new_state}}
      end)

    all_states = Enum.reverse(states)

    # Remove washout period
    training_states = Enum.drop(all_states, @washout)

    training_targets =
      time_series |> Enum.drop(@washout + 1) |> Enum.take(length(training_states) - 1)

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
    {predictions, _final} =
      Enum.reduce(1..steps, {[], esn}, fn _i, {preds, current} ->
        last_input =
          case preds do
            [] ->
              case current.predictions do
                [h | _] -> h.prediction
                [] -> 0.5
              end

            [last | _] ->
              last
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
    pairs =
      preds
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.map(fn [later, earlier] -> {earlier.prediction, later.input} end)

    if length(pairs) == 0 do
      %{mse: nil, mae: nil, samples: 0}
    else
      mse =
        Enum.reduce(pairs, 0.0, fn {pred, actual}, acc ->
          acc + (pred - actual) * (pred - actual)
        end) / length(pairs)

      mae =
        Enum.reduce(pairs, 0.0, fn {pred, actual}, acc ->
          acc + abs(pred - actual)
        end) / length(pairs)

      %{mse: mse, mae: mae, samples: length(pairs)}
    end
  end

  @doc "Detect confidence drift (sustained directional change)"
  def detect_drift(%__MODULE__{predictions: preds}) when length(preds) < 10,
    do: :insufficient_data

  def detect_drift(%__MODULE__{predictions: preds}) do
    recent = preds |> Enum.take(10) |> Enum.map(fn p -> p.prediction end)

    # Check for monotonic trend
    diffs =
      recent
      |> Enum.chunk_every(2, 1, :discard)
      # a is more recent
      |> Enum.map(fn [a, b] -> a - b end)

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
    matrix =
      for _i <- 1..size do
        for _j <- 1..size do
          if :rand.uniform() < @sparsity do
            :rand.uniform() * 2.0 - 1.0
          else
            0.0
          end
        end
      end

    # Scale the induced infinity-norm (largest absolute row sum) to @reservoir_norm.
    # Scaling the largest single entry instead, as this used to, left the row
    # sums at roughly 10-12 for the default 80-node reservoir: the spectral
    # radius came out near 2 and the echo state property failed.
    max_row_sum =
      matrix
      |> Enum.map(fn row -> row |> Enum.map(&abs/1) |> Enum.sum() end)
      |> Enum.max(fn -> 1.0 end)

    scale = @reservoir_norm / max(max_row_sum, 0.001)
    Enum.map(matrix, fn row -> Enum.map(row, fn v -> v * scale end) end)
  end

  defp generate_weights(size, scaling) do
    for _i <- 1..size, do: (:rand.uniform() * 2.0 - 1.0) * scaling
  end

  defp update_state(%__MODULE__{reservoir: w, input_weights: iw, state: state}, input) do
    size = length(state)

    for i <- 0..(size - 1) do
      recurrent =
        Enum.reduce(0..(size - 1), 0.0, fn j, acc ->
          w_ij = w |> Enum.at(i) |> Enum.at(j)
          acc + w_ij * Enum.at(state, j)
        end)

      external = Enum.at(iw, i) * input
      old = Enum.at(state, i)

      (1.0 - @leak_rate) * old + @leak_rate * :math.tanh(recurrent + external)
    end
  end

  # The readout is fitted as a linear map (solve_ridge/2), so it is applied as
  # one. It used to be passed through a sigmoid here, which the fit never saw:
  # a correct linear estimate of 0.5 came out as 0.62, and every forecast was
  # squeezed towards the middle. Clamp instead, since the series are
  # confidences in [0, 1].
  defp compute_output(%__MODULE__{output_weights: ow, trained: true}, state, input) do
    raw =
      Enum.zip(features(state, input), ow)
      |> Enum.reduce(0.0, fn {s, w}, acc -> acc + s * w end)

    raw |> max(0.0) |> min(1.0)
  end

  defp compute_output(_esn, state, _input) do
    # Untrained: return mean activation as proxy
    mean = Enum.sum(state) / max(length(state), 1)
    sigmoid(mean)
  end

  # Readout features: reservoir state, the current input, and a constant 1.0
  # for the bias. The input lets the readout fall back to persistence
  # ("next ≈ current"), which is a strong baseline for smooth confidence series.
  defp features(state, input), do: state ++ [input, 1.0]

  # Closed-form ridge regression: solve (XᵀX + λI') w = Xᵀy, where I' leaves the
  # bias unpenalised. This replaces 200 epochs of SGD at lr 5e-4, which stopped
  # far short of the minimum (the trained ESN forecast no better than the series
  # mean). The system is (reservoir size + 2) square, so Gaussian elimination is
  # cheap next to collecting the states.
  defp solve_ridge(states, targets) do
    dim = length(List.first(states, []))
    bias = dim - 1

    gram =
      Enum.reduce(states, List.duplicate(List.duplicate(0.0, dim), dim), fn x, acc ->
        Enum.zip_with(acc, x, fn row, xi ->
          Enum.zip_with(row, x, fn a, xj -> a + xi * xj end)
        end)
      end)

    rhs =
      Enum.zip(states, targets)
      |> Enum.reduce(List.duplicate(0.0, dim), fn {x, y}, acc ->
        Enum.zip_with(acc, x, fn a, xi -> a + xi * y end)
      end)

    gram
    |> Enum.with_index()
    |> Enum.map(fn {row, i} ->
      if i == bias, do: row, else: List.update_at(row, i, &(&1 + @ridge_lambda))
    end)
    |> gaussian_solve(rhs)
  end

  # Gaussian elimination with partial pivoting. A pivot below 1e-12 (only
  # possible with no training rows) contributes a zero weight.
  defp gaussian_solve(a, b) do
    rows = Enum.zip_with(a, b, fn row, bi -> row ++ [bi] end)
    n = length(rows)

    upper =
      Enum.reduce(0..(n - 1), rows, fn col, m ->
        {done, rest} = Enum.split(m, col)
        pivot_row = Enum.max_by(rest, fn r -> abs(Enum.at(r, col)) end)
        rest = List.delete(rest, pivot_row)
        p = Enum.at(pivot_row, col)

        if abs(p) < 1.0e-12 do
          done ++ [pivot_row | rest]
        else
          reduced =
            Enum.map(rest, fn r ->
              f = Enum.at(r, col) / p
              Enum.zip_with(r, pivot_row, fn x, y -> x - f * y end)
            end)

          done ++ [pivot_row | reduced]
        end
      end)

    upper
    |> Enum.with_index()
    |> Enum.reverse()
    |> Enum.reduce(%{}, fn {row, i}, solved ->
      p = Enum.at(row, i)

      if abs(p) < 1.0e-12 do
        Map.put(solved, i, 0.0)
      else
        known =
          Enum.reduce((i + 1)..(n - 1)//1, 0.0, fn j, acc -> acc + Enum.at(row, j) * solved[j] end)

        Map.put(solved, i, (Enum.at(row, n) - known) / p)
      end
    end)
    |> then(fn solved -> Enum.map(0..(n - 1), &solved[&1]) end)
  end

  defp sigmoid(x), do: 1.0 / (1.0 + :math.exp(-max(min(x, 500), -500)))
end
