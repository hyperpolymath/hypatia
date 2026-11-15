# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.NoveltyFailClosedTest do
  @moduledoc """
  E0-24: until the novelty network is trained, each finding counts as novel and goes to
  `report_only` (design of 2026-11-14 on merge honesty, decision 5: fail closed). The
  board status stays `:unknown`, so a reader can tell "untrained" from "novel by
  distance". The end-to-end cases are in `neural_source_store_lifecycle_test.exs`,
  which owns the isolated coordinator harness.
  """
  use ExUnit.Case, async: true

  alias Hypatia.Neural.{Coordinator, DispatchGuard, RadialNeuralNetwork}

  defp recipe,
    do: %{"id" => "recipe-novelty", "confidence" => 0.99, "triangle_tier" => "eliminate"}

  defp pattern, do: %{"id" => "PA700-novelty", "category" => "CommandInjection"}
  defp action, do: {:eliminate, recipe(), pattern()}

  defp recommendation(overrides) do
    Map.merge(
      %{
        strategy: :auto_execute,
        raw_strategy: :auto_execute,
        confidence: 0.99,
        is_novel: false,
        architecture: :blackboard,
        networks_consulted: 8,
        annealing_clamped: false
      },
      overrides
    )
  end

  describe "what the coordinator makes of the answer of the novelty network" do
    test "an untrained network counts as novel" do
      assert Coordinator.novelty_flag(:untrained) == true
    end

    test "a network that is trained says novel by distance or known" do
      assert Coordinator.novelty_flag({:novel, 3.2}) == true
      assert Coordinator.novelty_flag({:known, 0.1}) == false
    end

    test "the board status of an untrained network stays unknown, not novel" do
      assert Coordinator.novelty_status(:untrained) == :unknown
      assert Coordinator.novelty_status({:novel, 3.2}) == :novel
      assert Coordinator.novelty_status({:known, 0.1}) == :known
    end

    test "a real untrained RBF answers :untrained, so the flag is true for it" do
      rbf = RadialNeuralNetwork.init(num_inputs: 4, num_centers: 5)
      vector = [0.1, 0.2, 0.3, 0.4]

      assert RadialNeuralNetwork.detect_novelty(rbf, vector) == :untrained
      assert Coordinator.novelty_flag(RadialNeuralNetwork.detect_novelty(rbf, vector))
    end
  end

  describe "the strategy" do
    test "untrained network, confidence 0.99: report_only" do
      assert Coordinator.raw_strategy(Coordinator.novelty_flag(:untrained), 0.99) == :report_only
    end

    test "trained network and a known finding, confidence 0.99: not lowered" do
      assert Coordinator.raw_strategy(Coordinator.novelty_flag({:known, 0.1}), 0.99) ==
               :auto_execute
    end

    test "confidence alone still sets the other tiers when the finding is known" do
      assert Coordinator.raw_strategy(false, 0.90) == :review
      assert Coordinator.raw_strategy(false, 0.50) == :report_only
    end
  end

  describe "the dispatch guard" do
    test "an untrained network with confidence 0.99 lowers the action to control" do
      untrained = recommendation(%{is_novel: Coordinator.novelty_flag(:untrained)})

      assert {:control, lowered} = DispatchGuard.enforce(action(), untrained)
      assert lowered["neural_dispatch_guard"] == "novel"
    end

    test "a trained network and a known finding leave the action unchanged" do
      known = recommendation(%{is_novel: Coordinator.novelty_flag({:known, 0.1})})
      assert DispatchGuard.enforce(action(), known) == action()
    end
  end
end
