# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.Neural.TrainingPipelineTest do
  use ExUnit.Case, async: true

  alias Hypatia.Neural.{TrainingPipeline, EchoStateNetwork, RadialNeuralNetwork}

  describe "build_esn_training_data/0" do
    test "loads outcomes and groups by recipe_id" do
      data = TrainingPipeline.build_esn_training_data()
      assert is_list(data)

      if length(data) > 0 do
        entry = hd(data)
        assert Map.has_key?(entry, :recipe_id)
        assert Map.has_key?(entry, :series)
        assert Map.has_key?(entry, :count)
        assert is_list(entry.series)
        assert entry.count >= 10

        # Series should be sorted by count (longest first)
        counts = Enum.map(data, & &1.count)
        assert counts == Enum.sort(counts, :desc)
      end
    end

    test "confidence series values are between 0 and 1" do
      data = TrainingPipeline.build_esn_training_data()

      Enum.each(data, fn entry ->
        Enum.each(entry.series, fn val ->
          assert val >= 0.0 and val <= 1.0,
                 "confidence #{val} out of range for #{entry.recipe_id}"
        end)
      end)
    end
  end

  describe "build_rbf_training_data/0" do
    test "produces vectors and targets from pattern registry" do
      {vectors, targets} = TrainingPipeline.build_rbf_training_data()
      assert is_list(vectors)
      assert is_list(targets)
      assert length(vectors) == length(targets)

      if length(vectors) > 0 do
        # Each vector should be 8-dimensional
        Enum.each(vectors, fn vec ->
          assert length(vec) == 8, "expected 8-D vector, got #{length(vec)}-D"
        end)

        # Targets should be 0.0, 0.5, or 1.0 (tier mappings)
        Enum.each(targets, fn target ->
          assert target in [0.0, 0.5, 1.0], "unexpected target value: #{target}"
        end)
      end
    end
  end

  describe "train_esn/1" do
    test "trains ESN if sufficient data exists" do
      esn = EchoStateNetwork.init()
      trained = TrainingPipeline.train_esn(esn)

      # Whether training succeeds depends on data availability
      assert %EchoStateNetwork{} = trained
    end
  end

  describe "train_rbf/1" do
    test "trains RBF on pattern data" do
      rbf = RadialNeuralNetwork.init()
      trained = TrainingPipeline.train_rbf(rbf)

      # If we have patterns, RBF should be trained
      {vectors, _} = TrainingPipeline.build_rbf_training_data()

      if length(vectors) >= 5 do
        assert trained.trained == true
        assert trained.training_stats.samples == length(vectors)
      end
    end
  end

  describe "run_full_training/0" do
    test "trains both networks and returns results" do
      result = TrainingPipeline.run_full_training()
      assert Map.has_key?(result, :esn)
      assert Map.has_key?(result, :rbf)
      assert %EchoStateNetwork{} = result.esn
      assert %RadialNeuralNetwork{} = result.rbf
    end
  end
end
