# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Confidence Annealing -- temperature-scheduled confidence calibration
#
# Inspired by simulated annealing: early in a recipe's lifecycle (high
# temperature), confidence spreads are flattened toward 0.5 (conservative).
# As outcomes accumulate (temperature cools), confidence distinctions
# sharpen, allowing recipes to graduate to higher dispatch tiers.
#
# Reheat mechanism: if drift is detected (sudden failure spike), temperature
# rises again, forcing the system back to conservative dispatch until the
# new evidence stabilises.

defmodule Hypatia.ConfidenceAnnealing do
  @moduledoc """
  Temperature-scheduled confidence calibration for Hypatia's dispatch system.

  ## How it works

  Each recipe tracks an annealing state:
  - `temperature` -- controls how much confidence is flattened toward 0.5
  - `outcome_count` -- total observations (successes + failures + FPs)
  - `stage` -- dispatch tier lock based on outcome count thresholds
  - `last_reheat` -- timestamp of most recent drift-triggered reheat

  Temperature decays exponentially with each outcome:

      T(n) = T_initial * exp(-decay_rate * n)

  where `n` is the outcome count. At high T, raw confidence is pulled
  toward 0.5. At T ≈ 0, raw confidence passes through unchanged.

  ## Staged unlock

  Recipes must accumulate enough outcomes before graduating to higher
  dispatch tiers, regardless of raw confidence:

  | Stage        | Min outcomes | Max dispatch tier |
  |--------------|-------------|-------------------|
  | :nascent     | 0           | :report_only      |
  | :adolescent  | 5           | :review           |
  | :mature      | 15          | :auto_execute     |
  | :veteran     | 50          | :auto_execute     |

  ## Reheat

  When drift is detected (3+ failures in last 10 outcomes), temperature
  is partially reheated to force conservative dispatch until the new
  evidence pattern stabilises.

  ## Cross-repo temperature coupling

  When confidence transfers from repo A to repo B, the receiving repo
  applies a cross-repo discount factor to the temperature, making
  transferred confidence more conservative than locally-earned confidence.
  """

  require Logger

  # --- Configuration ---

  # Starting temperature -- high means "flatten confidence toward 0.5"
  @initial_temperature 1.0

  # Exponential decay rate -- higher = faster cooling
  # At 0.05, T drops to ~0.37 after 20 outcomes, ~0.08 after 50
  @decay_rate 0.05

  # Minimum temperature -- prevents complete crystallisation, allows
  # small corrections even for veteran recipes
  @min_temperature 0.01

  # Reheat multiplier -- how much temperature rises on drift detection
  @reheat_multiplier 3.0

  # Maximum temperature after reheat (never exceeds initial)
  @reheat_cap @initial_temperature

  # Drift detection: if >= this many failures in the last N outcomes,
  # trigger reheat
  @drift_failure_threshold 3
  @drift_window_size 10

  # Staged unlock thresholds
  @stage_thresholds [
    {:nascent, 0},
    {:adolescent, 5},
    {:mature, 15},
    {:veteran, 50}
  ]

  # Cross-repo discount -- transferred confidence uses higher effective
  # temperature (more conservative)
  @cross_repo_temperature_multiplier 2.0

  # --- Types ---

  @type stage :: :nascent | :adolescent | :mature | :veteran

  @type annealing_state :: %{
    temperature: float(),
    outcome_count: non_neg_integer(),
    stage: stage(),
    recent_outcomes: [atom()],
    last_reheat: DateTime.t() | nil,
    created_at: DateTime.t()
  }

  # --- Public API ---

  @doc """
  Create a fresh annealing state for a new or unknown recipe.

  Starts at maximum temperature (most conservative). The recipe must
  accumulate outcomes before it can graduate past :report_only.
  """
  @spec new_state() :: annealing_state()
  def new_state do
    %{
      temperature: @initial_temperature,
      outcome_count: 0,
      stage: :nascent,
      recent_outcomes: [],
      last_reheat: nil,
      created_at: DateTime.utc_now()
    }
  end

  @doc """
  Create an annealing state from existing recipe data.

  Used when bootstrapping annealing for recipes that already have
  outcome history. Sets temperature based on existing outcome count.
  """
  @spec from_existing(non_neg_integer(), non_neg_integer(), non_neg_integer()) :: annealing_state()
  def from_existing(successes, failures, false_positives) do
    total = successes + failures + false_positives
    temperature = compute_temperature(total)
    stage = compute_stage(total)

    # Reconstruct approximate recent outcomes for drift detection
    recent = build_approximate_recent(successes, failures, false_positives)

    %{
      temperature: temperature,
      outcome_count: total,
      stage: stage,
      recent_outcomes: recent,
      last_reheat: nil,
      created_at: DateTime.utc_now()
    }
  end

  @doc """
  Record a new outcome and update the annealing state.

  This is the core state transition. Each outcome:
  1. Increments outcome_count
  2. Appends to recent_outcomes (capped at drift window size)
  3. Recalculates temperature (exponential decay)
  4. Checks for drift → reheat if needed
  5. Recalculates stage based on new count

  Returns the updated annealing state.
  """
  @spec record_outcome(annealing_state(), :success | :failure | :false_positive) :: annealing_state()
  def record_outcome(state, outcome) do
    new_count = state.outcome_count + 1

    # Update recent outcomes window (FIFO, capped at drift window)
    recent = Enum.take([outcome | state.recent_outcomes], @drift_window_size)

    # Base temperature from decay curve
    base_temp = compute_temperature(new_count)

    # Check for drift -- if detected, reheat
    {temperature, last_reheat} =
      if drift_detected?(recent) do
        reheated = min(base_temp * @reheat_multiplier, @reheat_cap)
        Logger.warning(
          "Confidence annealing: drift detected at outcome ##{new_count}, " <>
          "reheating #{Float.round(base_temp, 4)} -> #{Float.round(reheated, 4)}"
        )
        {reheated, DateTime.utc_now()}
      else
        {base_temp, state.last_reheat}
      end

    %{state |
      temperature: temperature,
      outcome_count: new_count,
      stage: compute_stage(new_count),
      recent_outcomes: recent,
      last_reheat: last_reheat
    }
  end

  @doc """
  Apply temperature-based annealing to a raw confidence value.

  At high temperature, confidence is pulled toward 0.5 (maximum entropy).
  At low temperature, raw confidence passes through nearly unchanged.

  The transformation uses logit/sigmoid:

      annealed = sigmoid(logit(raw) / (1 + T))

  This preserves ordering (higher raw → higher annealed) while
  compressing the range at high temperature.
  """
  @spec anneal(float(), annealing_state()) :: float()
  def anneal(raw_confidence, state) do
    anneal_with_temperature(raw_confidence, state.temperature)
  end

  @doc """
  Apply annealing for cross-repo transferred confidence.

  Uses a higher effective temperature than the local state, making
  transferred confidence more conservative.
  """
  @spec anneal_cross_repo(float(), annealing_state()) :: float()
  def anneal_cross_repo(raw_confidence, state) do
    effective_temp = state.temperature * @cross_repo_temperature_multiplier
    anneal_with_temperature(raw_confidence, effective_temp)
  end

  @doc """
  Get the maximum dispatch tier allowed by the recipe's current stage.

  Regardless of confidence, a recipe cannot be dispatched at a tier
  higher than its stage allows.
  """
  @spec max_dispatch_tier(annealing_state()) :: :auto_execute | :review | :report_only
  def max_dispatch_tier(%{stage: :nascent}), do: :report_only
  def max_dispatch_tier(%{stage: :adolescent}), do: :review
  def max_dispatch_tier(%{stage: :mature}), do: :auto_execute
  def max_dispatch_tier(%{stage: :veteran}), do: :auto_execute

  @doc """
  Clamp a dispatch strategy to the maximum allowed by annealing stage.

  If the raw confidence would suggest :auto_execute but the recipe is
  only :adolescent, this returns :review instead.
  """
  @spec clamp_strategy(atom(), annealing_state()) :: atom()
  def clamp_strategy(strategy, state) do
    max_tier = max_dispatch_tier(state)

    tier_rank = %{report_only: 0, review: 1, auto_execute: 2}
    strategy_rank = Map.get(tier_rank, strategy, 0)
    max_rank = Map.get(tier_rank, max_tier, 0)

    if strategy_rank > max_rank do
      Logger.info(
        "Annealing clamped #{strategy} -> #{max_tier} " <>
        "(stage=#{state.stage}, outcomes=#{state.outcome_count})"
      )
      max_tier
    else
      strategy
    end
  end

  @doc """
  Check whether drift has been detected in the recent outcome window.
  """
  @spec drift_detected?(annealing_state() | [atom()]) :: boolean()
  def drift_detected?(%{recent_outcomes: recent}), do: drift_detected?(recent)

  def drift_detected?(recent) when is_list(recent) do
    failure_count =
      Enum.count(recent, fn o -> o in [:failure, :false_positive] end)

    failure_count >= @drift_failure_threshold
  end

  @doc """
  Get a summary of the annealing state for diagnostics/logging.
  """
  @spec summary(annealing_state()) :: map()
  def summary(state) do
    %{
      temperature: Float.round(state.temperature, 4),
      outcome_count: state.outcome_count,
      stage: state.stage,
      max_tier: max_dispatch_tier(state),
      drift_detected: drift_detected?(state),
      recent_success_rate: recent_success_rate(state),
      last_reheat: state.last_reheat
    }
  end

  @doc """
  Get the current temperature for a given outcome count (pure function).

  Useful for projecting temperature at future outcome counts.
  """
  @spec compute_temperature(non_neg_integer()) :: float()
  def compute_temperature(outcome_count) do
    raw = @initial_temperature * :math.exp(-@decay_rate * outcome_count)
    max(raw, @min_temperature)
  end

  @doc "Get configuration constants (for testing and diagnostics)."
  @spec config() :: map()
  def config do
    %{
      initial_temperature: @initial_temperature,
      decay_rate: @decay_rate,
      min_temperature: @min_temperature,
      reheat_multiplier: @reheat_multiplier,
      reheat_cap: @reheat_cap,
      drift_failure_threshold: @drift_failure_threshold,
      drift_window_size: @drift_window_size,
      stage_thresholds: @stage_thresholds,
      cross_repo_temperature_multiplier: @cross_repo_temperature_multiplier
    }
  end

  # --- Private ---

  # Sigmoid/logit-based temperature transformation.
  # At T=0, returns raw_confidence unchanged.
  # At T→∞, returns 0.5 regardless of input.
  defp anneal_with_temperature(raw, temperature) do
    # Clamp input to avoid log(0) / log(∞)
    clamped = raw |> max(0.001) |> min(0.999)

    # logit transform: log(p / (1-p))
    logit = :math.log(clamped / (1.0 - clamped))

    # Scale by inverse temperature: divide by (1 + T)
    # Higher T → smaller logit → closer to 0.5
    scaled = logit / (1.0 + temperature)

    # Sigmoid back: 1 / (1 + exp(-x))
    1.0 / (1.0 + :math.exp(-scaled))
  end

  # Determine stage from outcome count
  defp compute_stage(count) do
    @stage_thresholds
    |> Enum.reverse()
    |> Enum.find(fn {_stage, threshold} -> count >= threshold end)
    |> case do
      {stage, _} -> stage
      nil -> :nascent
    end
  end

  # Build approximate recent outcomes from aggregate counts.
  # Used when bootstrapping from existing data. Distributes outcomes
  # proportionally across the drift window.
  defp build_approximate_recent(successes, failures, false_positives) do
    total = successes + failures + false_positives

    if total == 0 do
      []
    else
      window = min(total, @drift_window_size)
      ratio_s = successes / total
      ratio_f = failures / total
      # Remaining slots go to false_positives

      n_success = round(window * ratio_s)
      n_failure = round(window * ratio_f)
      n_fp = window - n_success - n_failure

      List.duplicate(:success, max(n_success, 0)) ++
        List.duplicate(:failure, max(n_failure, 0)) ++
        List.duplicate(:false_positive, max(n_fp, 0))
    end
  end

  # Success rate in the recent outcome window
  defp recent_success_rate(%{recent_outcomes: []}), do: nil

  defp recent_success_rate(%{recent_outcomes: recent}) do
    successes = Enum.count(recent, &(&1 == :success))
    Float.round(successes / length(recent), 3)
  end
end
