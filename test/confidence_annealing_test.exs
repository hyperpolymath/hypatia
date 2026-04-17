# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.ConfidenceAnnealingTest do
  use ExUnit.Case, async: true

  alias Hypatia.ConfidenceAnnealing

  describe "new_state/0" do
    test "creates state at maximum temperature" do
      state = ConfidenceAnnealing.new_state()

      assert state.temperature == 1.0
      assert state.outcome_count == 0
      assert state.stage == :nascent
      assert state.recent_outcomes == []
      assert state.last_reheat == nil
    end
  end

  describe "from_existing/3" do
    test "bootstraps from existing outcome counts" do
      state = ConfidenceAnnealing.from_existing(45, 5, 2)

      assert state.outcome_count == 52
      assert state.stage == :veteran
      assert state.temperature < 0.1  # Should be well cooled by 52 outcomes
    end

    test "zero outcomes produces nascent state" do
      state = ConfidenceAnnealing.from_existing(0, 0, 0)

      assert state.outcome_count == 0
      assert state.stage == :nascent
      assert state.temperature == 1.0
    end

    test "few outcomes stays adolescent" do
      state = ConfidenceAnnealing.from_existing(5, 1, 0)

      assert state.outcome_count == 6
      assert state.stage == :adolescent
    end
  end

  describe "record_outcome/2" do
    test "increments outcome count and cools temperature" do
      state = ConfidenceAnnealing.new_state()

      state = ConfidenceAnnealing.record_outcome(state, :success)
      assert state.outcome_count == 1
      assert state.temperature < 1.0

      state = ConfidenceAnnealing.record_outcome(state, :success)
      assert state.outcome_count == 2
    end

    test "tracks recent outcomes for drift detection" do
      state = ConfidenceAnnealing.new_state()

      state = ConfidenceAnnealing.record_outcome(state, :success)
      state = ConfidenceAnnealing.record_outcome(state, :failure)

      assert :success in state.recent_outcomes
      assert :failure in state.recent_outcomes
    end

    test "transitions through stages with enough outcomes" do
      state = ConfidenceAnnealing.new_state()
      assert state.stage == :nascent

      # Get to adolescent (5 outcomes)
      state = Enum.reduce(1..5, state, fn _, s ->
        ConfidenceAnnealing.record_outcome(s, :success)
      end)
      assert state.stage == :adolescent

      # Get to mature (15 outcomes)
      state = Enum.reduce(1..10, state, fn _, s ->
        ConfidenceAnnealing.record_outcome(s, :success)
      end)
      assert state.stage == :mature

      # Get to veteran (50 outcomes)
      state = Enum.reduce(1..35, state, fn _, s ->
        ConfidenceAnnealing.record_outcome(s, :success)
      end)
      assert state.stage == :veteran
    end

    test "reheats on drift detection" do
      # Build up a cool state
      state = ConfidenceAnnealing.from_existing(20, 0, 0)
      pre_temp = state.temperature

      # Now slam in 3+ failures to trigger drift
      state = ConfidenceAnnealing.record_outcome(state, :failure)
      state = ConfidenceAnnealing.record_outcome(state, :failure)
      state = ConfidenceAnnealing.record_outcome(state, :failure)

      assert state.temperature > pre_temp
      assert state.last_reheat != nil
    end
  end

  describe "anneal/2" do
    test "high temperature pulls confidence toward 0.5" do
      hot_state = ConfidenceAnnealing.new_state()  # T=1.0

      # A high raw confidence should be pulled down toward 0.5
      annealed = ConfidenceAnnealing.anneal(0.95, hot_state)
      assert annealed < 0.95
      assert annealed > 0.5

      # A low raw confidence should be pulled up toward 0.5
      annealed_low = ConfidenceAnnealing.anneal(0.1, hot_state)
      assert annealed_low > 0.1
      assert annealed_low < 0.5
    end

    test "low temperature preserves raw confidence" do
      # Build a veteran state with very low temperature
      cold_state = ConfidenceAnnealing.from_existing(100, 2, 0)

      annealed = ConfidenceAnnealing.anneal(0.95, cold_state)
      # Should be very close to 0.95
      assert_in_delta annealed, 0.95, 0.03
    end

    test "0.5 confidence is unchanged at any temperature" do
      hot = ConfidenceAnnealing.new_state()
      cold = ConfidenceAnnealing.from_existing(100, 0, 0)

      assert_in_delta ConfidenceAnnealing.anneal(0.5, hot), 0.5, 0.01
      assert_in_delta ConfidenceAnnealing.anneal(0.5, cold), 0.5, 0.01
    end

    test "preserves ordering -- higher raw always produces higher annealed" do
      state = ConfidenceAnnealing.from_existing(10, 1, 0)

      low = ConfidenceAnnealing.anneal(0.3, state)
      mid = ConfidenceAnnealing.anneal(0.5, state)
      high = ConfidenceAnnealing.anneal(0.8, state)

      assert low < mid
      assert mid < high
    end
  end

  describe "anneal_cross_repo/2" do
    test "produces more conservative result than local anneal" do
      state = ConfidenceAnnealing.from_existing(20, 1, 0)

      local = ConfidenceAnnealing.anneal(0.9, state)
      cross = ConfidenceAnnealing.anneal_cross_repo(0.9, state)

      # Cross-repo should be closer to 0.5 (more conservative)
      assert abs(cross - 0.5) < abs(local - 0.5)
    end
  end

  describe "max_dispatch_tier/1" do
    test "nascent is locked to report_only" do
      state = ConfidenceAnnealing.new_state()
      assert ConfidenceAnnealing.max_dispatch_tier(state) == :report_only
    end

    test "adolescent can reach review" do
      state = ConfidenceAnnealing.from_existing(5, 0, 0)
      assert ConfidenceAnnealing.max_dispatch_tier(state) == :review
    end

    test "mature can reach auto_execute" do
      state = ConfidenceAnnealing.from_existing(15, 0, 0)
      assert ConfidenceAnnealing.max_dispatch_tier(state) == :auto_execute
    end

    test "veteran can reach auto_execute" do
      state = ConfidenceAnnealing.from_existing(50, 0, 0)
      assert ConfidenceAnnealing.max_dispatch_tier(state) == :auto_execute
    end
  end

  describe "clamp_strategy/2" do
    test "nascent clamps auto_execute to report_only" do
      state = ConfidenceAnnealing.new_state()
      assert ConfidenceAnnealing.clamp_strategy(:auto_execute, state) == :report_only
    end

    test "adolescent clamps auto_execute to review" do
      state = ConfidenceAnnealing.from_existing(5, 0, 0)
      assert ConfidenceAnnealing.clamp_strategy(:auto_execute, state) == :review
    end

    test "mature passes auto_execute through" do
      state = ConfidenceAnnealing.from_existing(15, 0, 0)
      assert ConfidenceAnnealing.clamp_strategy(:auto_execute, state) == :auto_execute
    end

    test "report_only passes through regardless of stage" do
      state = ConfidenceAnnealing.from_existing(50, 0, 0)
      assert ConfidenceAnnealing.clamp_strategy(:report_only, state) == :report_only
    end
  end

  describe "drift_detected?/1" do
    test "no drift with all successes" do
      state = %{recent_outcomes: [:success, :success, :success, :success, :success]}
      refute ConfidenceAnnealing.drift_detected?(state)
    end

    test "drift detected with 3+ failures" do
      state = %{recent_outcomes: [:failure, :failure, :failure, :success, :success]}
      assert ConfidenceAnnealing.drift_detected?(state)
    end

    test "false_positive counts toward drift" do
      state = %{recent_outcomes: [:false_positive, :failure, :false_positive, :success]}
      assert ConfidenceAnnealing.drift_detected?(state)
    end
  end

  describe "compute_temperature/1" do
    test "starts at initial temperature with 0 outcomes" do
      assert ConfidenceAnnealing.compute_temperature(0) == 1.0
    end

    test "decreases monotonically with outcome count" do
      temps = Enum.map(0..50, &ConfidenceAnnealing.compute_temperature/1)

      for [a, b] <- Enum.chunk_every(temps, 2, 1, :discard) do
        assert a >= b
      end
    end

    test "never drops below minimum temperature" do
      temp = ConfidenceAnnealing.compute_temperature(10_000)
      config = ConfidenceAnnealing.config()
      assert temp >= config.min_temperature
    end
  end

  describe "summary/1" do
    test "returns complete diagnostic map" do
      state = ConfidenceAnnealing.from_existing(10, 1, 0)
      summary = ConfidenceAnnealing.summary(state)

      assert is_float(summary.temperature)
      assert summary.outcome_count == 11
      assert summary.stage == :adolescent
      assert summary.max_tier == :review
      assert is_boolean(summary.drift_detected)
    end
  end
end
