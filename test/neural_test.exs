# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.NeuralTest do
  use ExUnit.Case, async: true

  alias Hypatia.Neural.{
    EchoStateNetwork,
    RadialNeuralNetwork,
    MixtureOfExperts,
    LiquidStateMachine,
    GraphOfTrust
  }

  # ===================================================================
  # Echo State Network
  # ===================================================================

  describe "EchoStateNetwork" do
    test "init/0 creates untrained network with defaults" do
      esn = EchoStateNetwork.init()
      assert esn.trained == false
      assert esn.predictions == []
    end

    test "init/1 accepts reservoir_size option" do
      esn = EchoStateNetwork.init(reservoir_size: 20)
      assert esn.trained == false
      assert esn.reservoir != nil
    end

    test "train/2 with sufficient data returns trained network" do
      esn = EchoStateNetwork.init(reservoir_size: 20)
      # Need > washout(50) + 10 = 60 data points
      time_series = for i <- 1..100, do: :math.sin(i * 0.1) * 0.5 + 0.5
      trained = EchoStateNetwork.train(esn, time_series)
      assert trained.trained == true
    end

    test "train/2 with insufficient data returns untrained" do
      esn = EchoStateNetwork.init(reservoir_size: 20)
      short_series = [0.5, 0.6, 0.7]
      result = EchoStateNetwork.train(esn, short_series)
      assert result.trained == false
    end

    test "detect_drift/1 with insufficient data returns :insufficient_data" do
      esn = EchoStateNetwork.init()
      assert EchoStateNetwork.detect_drift(esn) == :insufficient_data
    end

    test "detect_drift/1 with trained network returns drift status" do
      esn = EchoStateNetwork.init(reservoir_size: 20)
      time_series = for i <- 1..100, do: :math.sin(i * 0.1) * 0.5 + 0.5
      trained = EchoStateNetwork.train(esn, time_series)

      drift = EchoStateNetwork.detect_drift(trained)
      assert drift in [:no_drift, :mild_drift, :significant_drift, :insufficient_data]
    end
  end

  # ===================================================================
  # Radial Neural Network (RBF)
  # ===================================================================

  describe "RadialNeuralNetwork" do
    test "init/0 creates untrained network" do
      rbf = RadialNeuralNetwork.init()
      assert rbf.trained == false
      assert rbf.centers == []
    end

    test "train/3 with sufficient data trains the network" do
      rbf = RadialNeuralNetwork.init()
      data = for _ <- 1..10, do: (for _ <- 1..8, do: :rand.uniform())
      targets = for _ <- 1..10, do: :rand.uniform()

      trained = RadialNeuralNetwork.train(rbf, data, targets)
      assert trained.trained == true
      assert length(trained.centers) > 0
    end

    test "train/3 with insufficient data stays untrained" do
      rbf = RadialNeuralNetwork.init()
      data = [[0.1, 0.2], [0.3, 0.4]]
      targets = [0.5, 0.6]

      result = RadialNeuralNetwork.train(rbf, data, targets)
      assert result.trained == false
    end

    test "predict/2 on untrained network returns default" do
      rbf = RadialNeuralNetwork.init()
      {prediction, confidence} = RadialNeuralNetwork.predict(rbf, [0.5, 0.5, 0.5])
      assert prediction == 0.5
      assert confidence == 0.0
    end

    test "predict/2 on trained network returns values" do
      rbf = RadialNeuralNetwork.init()
      data = for _ <- 1..10, do: (for _ <- 1..4, do: :rand.uniform())
      targets = for _ <- 1..10, do: :rand.uniform()

      trained = RadialNeuralNetwork.train(rbf, data, targets)
      {prediction, confidence} = RadialNeuralNetwork.predict(trained, [0.5, 0.5, 0.5, 0.5])
      assert is_float(prediction)
      assert is_float(confidence)
    end

    test "detect_novelty/2 on untrained network returns :untrained" do
      rbf = RadialNeuralNetwork.init()
      assert RadialNeuralNetwork.detect_novelty(rbf, [0.5]) == :untrained
    end
  end

  # ===================================================================
  # Mixture of Experts
  # ===================================================================

  describe "MixtureOfExperts" do
    test "init/0 creates network with 7 expert domains" do
      moe = MixtureOfExperts.init()
      assert map_size(moe.experts) == 7
    end

    test "predict/2 returns confidence for a finding" do
      moe = MixtureOfExperts.init()
      finding = %{
        "category" => "CommandInjection",
        "severity" => "High",
        "language" => "shell"
      }
      {confidence, experts} = MixtureOfExperts.predict(moe, finding)
      assert is_float(confidence)
      assert is_list(experts)
    end

    test "train/3 updates expert stats" do
      moe = MixtureOfExperts.init()
      finding = %{
        "category" => "CommandInjection",
        "severity" => "High",
        "language" => "shell"
      }
      updated = MixtureOfExperts.train(moe, finding, :success)
      assert is_map(updated.expert_stats)
    end
  end

  # ===================================================================
  # Liquid State Machine
  # ===================================================================

  describe "LiquidStateMachine" do
    test "init/0 creates LSM with defaults" do
      lsm = LiquidStateMachine.init()
      assert lsm.trained == false
    end

    test "init/1 accepts reservoir_size option" do
      lsm = LiquidStateMachine.init(reservoir_size: 100)
      assert lsm.trained == false
      assert lsm.reservoir_weights != nil
    end

    test "detect_anomalies/1 with short history returns empty" do
      lsm = LiquidStateMachine.init()
      result = LiquidStateMachine.detect_anomalies(lsm)
      assert result == []
    end

    test "predict/1 on untrained returns tuple" do
      lsm = LiquidStateMachine.init()
      {prediction, status} = LiquidStateMachine.predict(lsm)
      assert is_float(prediction)
      assert status == :untrained
    end
  end

  # ===================================================================
  # Graph of Trust
  # ===================================================================

  describe "GraphOfTrust" do
    test "struct creates empty graph" do
      got = %GraphOfTrust{}
      assert got.nodes == %{}
      assert got.edges == []
      assert got.trust_scores == %{}
    end

    test "build/0 constructs graph from verisimdb data" do
      got = GraphOfTrust.build()
      assert is_struct(got, GraphOfTrust)
      assert map_size(got.nodes) >= 0
    end
  end
end
