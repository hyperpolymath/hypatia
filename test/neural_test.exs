# SPDX-License-Identifier: MPL-2.0

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

    test "reservoir's largest absolute row sum is 0.95 for any size and seed" do
      for size <- [10, 20, 80], seed <- 1..5 do
        esn = EchoStateNetwork.init(reservoir_size: size, seed: seed)

        max_row_sum =
          esn.reservoir
          |> Enum.map(fn row -> row |> Enum.map(&abs/1) |> Enum.sum() end)
          |> Enum.max()

        assert_in_delta max_row_sum, 0.95, 1.0e-9, "size=#{size} seed=#{seed}"
      end
    end

    # Echo state property: with an infinity-norm of 0.95 and leak 0.2, each step
    # shrinks the gap between two states by at least 1 - 0.2 + 0.2 * 0.95 = 0.99.
    # Under the old max-entry scaling the gap stayed above 0.5 after 300 steps.
    test "two different initial states converge under the same inputs" do
      for seed <- 1..3 do
        esn = EchoStateNetwork.init(seed: seed)
        a = %{esn | state: Enum.map(esn.state, fn _ -> 0.9 end)}
        b = %{esn | state: Enum.map(esn.state, fn _ -> -0.9 end)}
        steps = 300
        inputs = for t <- 1..steps, do: 0.5 + 0.3 * :math.sin(t / 7)

        {a, b} =
          Enum.reduce(inputs, {a, b}, fn u, {a, b} ->
            {elem(EchoStateNetwork.step(a, u), 1), elem(EchoStateNetwork.step(b, u), 1)}
          end)

        gap =
          Enum.zip(a.state, b.state)
          |> Enum.map(fn {x, y} -> abs(x - y) end)
          |> Enum.max()

        assert gap <= 1.8 * :math.pow(0.99, steps), "seed=#{seed} gap=#{gap}"
      end
    end

    test "detect_drift/1 with insufficient data returns :insufficient_data" do
      esn = EchoStateNetwork.init()
      assert EchoStateNetwork.detect_drift(esn) == :insufficient_data
    end

    # A constant series must be forecast as itself. The old readout had no bias
    # and squashed its linear fit through a sigmoid, so it forecast ~0.6 here.
    test "trained ESN forecasts a constant series as that constant" do
      esn = EchoStateNetwork.init(reservoir_size: 20, seed: 1)
      trained = EchoStateNetwork.train(esn, List.duplicate(0.3, 100))

      {_, ran} =
        Enum.reduce(1..20, {nil, trained}, fn _, {_, e} -> EchoStateNetwork.step(e, 0.3) end)

      {prediction, _} = EchoStateNetwork.step(ran, 0.3)
      assert_in_delta prediction, 0.3, 1.0e-3
    end

    # One-step forecasts on held-out data must beat "next = current", the
    # baseline any forecaster has to clear. The old SGD readout lost to it by
    # about 40x on this series and did no better than the series mean.
    test "trained ESN beats the persistence baseline on held-out data" do
      series = for t <- 1..300, do: 0.5 + 0.25 * :math.sin(t / 9) + 0.1 * :math.sin(t / 3.1)
      {train, _} = Enum.split(series, 200)

      trained =
        EchoStateNetwork.init(reservoir_size: 20, seed: 1) |> EchoStateNetwork.train(train)

      {_, ran} =
        Enum.reduce(series, {nil, trained}, fn u, {_, e} -> EchoStateNetwork.step(e, u) end)

      %{mse: esn_mse, samples: n} = EchoStateNetwork.accuracy_report(ran)
      held_out = Enum.take(series, -(n + 1))

      persistence_mse =
        held_out
        |> Enum.chunk_every(2, 1, :discard)
        |> Enum.map(fn [a, b] -> (a - b) * (a - b) end)
        |> Enum.sum()
        |> Kernel./(n)

      assert esn_mse < persistence_mse / 2,
             "esn_mse=#{esn_mse} persistence_mse=#{persistence_mse}"
    end

    test "trained ESN forecasts stay inside [0, 1]" do
      series = for t <- 1..120, do: if(rem(t, 10) < 5, do: 0.0, else: 1.0)

      trained =
        EchoStateNetwork.init(reservoir_size: 20, seed: 2) |> EchoStateNetwork.train(series)

      {_, ran} =
        Enum.reduce(series, {nil, trained}, fn u, {_, e} -> EchoStateNetwork.step(e, u) end)

      assert Enum.all?(ran.predictions, fn p -> p.prediction >= 0.0 and p.prediction <= 1.0 end)
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
      data = for _ <- 1..10, do: for(_ <- 1..8, do: :rand.uniform())
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
      data = for _ <- 1..10, do: for(_ <- 1..4, do: :rand.uniform())
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

    test "reservoir's largest absolute row sum is 0.9 for any size and seed" do
      for size <- [10, 20, 100], seed <- 1..5 do
        lsm = LiquidStateMachine.init(reservoir_size: size, seed: seed)

        max_row_sum =
          lsm.reservoir_weights
          |> Enum.map(fn row -> row |> Enum.map(&abs/1) |> Enum.sum() end)
          |> Enum.max()

        assert_in_delta max_row_sum, 0.9, 1.0e-9, "size=#{size} seed=#{seed}"
      end
    end

    # Fading memory: with an infinity-norm of 0.9 and leak 0.3, each event
    # shrinks the gap between two states by at least 1 - 0.3 + 0.3 * 0.9 = 0.97.
    # Under the old max-entry scaling the gap was still 0.06-2.0 after 300 events.
    test "two different initial states converge under the same events" do
      events =
        for i <- 1..300 do
          %{
            "severity" => Enum.at(~w(low medium high critical), rem(i, 4)),
            "outcome" => Enum.at(~w(success failure false_positive), rem(i, 3)),
            "type" => Enum.at(~w(scan dispatch outcome), rem(i * 7, 3))
          }
        end

      for seed <- 1..3 do
        lsm = LiquidStateMachine.init(seed: seed)
        a = %{lsm | state: Enum.map(lsm.state, fn _ -> 0.9 end)}
        b = %{lsm | state: Enum.map(lsm.state, fn _ -> -0.9 end)}

        {a, b} =
          Enum.reduce(events, {a, b}, fn e, {a, b} ->
            {LiquidStateMachine.process_event(a, e), LiquidStateMachine.process_event(b, e)}
          end)

        gap =
          Enum.zip(a.state, b.state)
          |> Enum.map(fn {x, y} -> abs(x - y) end)
          |> Enum.max()

        assert gap <= 1.8 * :math.pow(0.97, length(events)), "seed=#{seed} gap=#{gap}"
      end
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

    test "build/0 constructs graph from verisim data" do
      got = GraphOfTrust.build()
      assert is_struct(got, GraphOfTrust)
      assert map_size(got.nodes) >= 0
    end
  end
end
