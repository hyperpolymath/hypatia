# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Neural.ReadinessTest do
  use ExUnit.Case, async: true

  alias Hypatia.Neural.{
    Coordinator,
    EchoStateNetwork,
    LiquidStateMachine,
    MixtureOfExperts,
    RadialNeuralNetwork
  }

  alias Hypatia.{PatternAnalyzer, TriangleRouter}

  @pattern %{"id" => "PA-readiness", "routed_repo" => "fixture/repo"}
  @selected [:security, :quality]

  defp entry(key, value), do: %{key: key, value: value}

  defp trace(overrides \\ %{}) do
    %{
      networks:
        Map.merge(
          %{
            moe: [entry(:domain_confidence, %{confidence: 0.80, experts: @selected})],
            rbf: [entry(:novelty_result, %{similarity: 0.70})],
            lsm: [entry(:temporal_result, %{score: 0.60})],
            esn: [entry(:trajectory_result, %{predicted_next: 0.65})]
          },
          overrides
        )
    }
  end

  # Synthetic local bookkeeping exercises the readiness policy. The real
  # training/restore path is checked by SourceStoreLifecycleTest, not attested
  # by these controlled flags or the supplied trace scores.
  defp state(trained? \\ true) do
    moe = MixtureOfExperts.init()

    moe =
      if trained? do
        stats = Map.new(moe.expert_stats, fn {domain, stat} -> {domain, %{stat | total: 1}} end)
        %{moe | expert_stats: stats, last_trained: ~U[2026-11-23 00:00:00Z]}
      else
        moe
      end

    %Coordinator{
      moe: moe,
      rbf: %RadialNeuralNetwork{trained: trained?},
      lsm: %LiquidStateMachine{trained: trained?},
      esn: %EchoStateNetwork{trained: trained?}
    }
  end

  defp action(confidence \\ 0.99),
    do: {:eliminate, %{"id" => "recipe-readiness", "confidence" => confidence}, @pattern}

  defp recommendation(overrides \\ %{}) do
    Map.merge(
      %{
        strategy: :auto_execute,
        confidence: 0.98,
        confidence_readiness: Coordinator.confidence_assessment(trace(), state()).readiness,
        is_novel: false,
        architecture: :blackboard,
        networks_consulted: 8,
        annealing_clamped: false
      },
      overrides
    )
  end

  test "only state-ready outputs enter both parts of the weighted mean" do
    assessment = Coordinator.confidence_assessment(trace(), state())
    expected = (0.80 * 0.35 + 0.70 * 0.20 + 0.60 * 0.15 + 0.65 * 0.15) / 0.85

    assert_in_delta assessment.confidence, expected, 1.0e-12
    assert assessment.readiness.scope == :local_training_state
    assert assessment.readiness.contributing_models == [:moe, :rbf, :lsm, :esn]
    assert Coordinator.confidence_ready?(assessment.readiness)
    assert assessment.readiness.components.gnn == :placeholder
    assert assessment.readiness.components.vae == :placeholder
    assert assessment.readiness.components.sequence == :heuristic
    assert assessment.readiness.components.pagerank == :trust_routing
  end

  test "placeholder names, scores and labels change neither a ready aggregate nor its decision" do
    baseline = Coordinator.confidence_assessment(trace(), state())

    for namespace <- [:gnn, :vae, :sequence, :unrecognised, "gnn"],
        score <- [nil, -1.0, 0.0, 0.99, 1.0, 9.0],
        placeholder <- [true, false, nil] do
      # Supplying every familiar field does not turn these namespaces into a
      # locally trained contributor, even when a caller changes their labels.
      value = %{
        confidence: score,
        interaction_strength: score,
        cluster_id: score,
        placeholder: placeholder,
        trained: true,
        model_trained: true
      }

      extra = %{
        namespace => Enum.map([:graph_metrics, :interpretation, :prediction], &entry(&1, value))
      }

      assessment = Coordinator.confidence_assessment(trace(extra), state())
      assert assessment == baseline

      assert Coordinator.raw_strategy(false, assessment.confidence) ==
               Coordinator.raw_strategy(false, baseline.confidence)
    end
  end

  test "trace claims and out-of-band scores cannot qualify untrained or missing model state" do
    high = %{
      networks: %{
        moe: [entry(:domain_confidence, %{confidence: 0.99, experts: @selected, trained: true})],
        rbf: [entry(:novelty_result, %{similarity: 0.99, trained: true})],
        lsm: [entry(:temporal_result, %{score: 0.99, status: :trained})],
        esn: [entry(:trajectory_result, %{predicted_next: 0.99, trained: true})]
      }
    }

    for model_state <- [state(false), nil, %{rbf: %{trained: true}}] do
      assessment = Coordinator.confidence_assessment(high, model_state)
      assert assessment.confidence == 0.5
      refute assessment.readiness.ready
      assert assessment.readiness.contributing_models == []
      refute Coordinator.confidence_ready?(assessment.readiness)
    end
  end

  test "training another MoE expert does not qualify the selected experts" do
    model_state = state()
    partial = put_in(model_state.moe.expert_stats.quality.total, 0)
    only_moe = %{networks: %{moe: trace().networks.moe}}

    assessment = Coordinator.confidence_assessment(only_moe, partial)
    refute assessment.readiness.ready
    assert assessment.readiness.components.moe == :untrained_selected_experts
    assert assessment.confidence == 0.5

    assert Coordinator.confidence_assessment(only_moe, model_state).readiness.ready

    no_selection = %{networks: %{moe: [entry(:domain_confidence, %{confidence: 0.99})]}}
    refute Coordinator.confidence_assessment(no_selection, model_state).readiness.ready

    no_training_time = %{model_state | moe: %{model_state.moe | last_trained: nil}}
    refute Coordinator.confidence_assessment(only_moe, no_training_time).readiness.ready
  end

  test "missing or malformed scores have no numerator or denominator even with trained state" do
    for score <- [nil, "0.99", -0.1, 1.1] do
      input = %{
        networks: %{
          moe: [entry(:domain_confidence, %{confidence: score, experts: @selected})],
          rbf: [entry(:novelty_result, %{similarity: 0.8})],
          lsm: :malformed,
          esn: [nil, %{}, entry(:trajectory_result, %{})]
        }
      }

      assessment = Coordinator.confidence_assessment(input, state())
      assert_in_delta assessment.confidence, 0.8, 1.0e-12
      assert assessment.readiness.contributing_models == [:rbf]
      assert assessment.readiness.components.moe == :invalid_output
      assert assessment.readiness.components.lsm == :missing_output
      assert assessment.readiness.components.esn == :invalid_output
    end
  end

  test "empty and placeholder-only traces remain unready even when unrelated models are trained" do
    for networks <- [
          %{},
          %{gnn: [entry(:graph_metrics, %{interaction_strength: 1.0, trained: true})]},
          %{sequence: [entry(:prediction, %{confidence: 1.0, model_trained: true})]},
          %{vae: [entry(:interpretation, %{cluster_id: 0, placeholder: false})]}
        ] do
      assessment = Coordinator.confidence_assessment(%{networks: networks}, state())
      assert assessment.confidence == 0.5
      refute assessment.readiness.ready
      assert Coordinator.raw_strategy(false, assessment.confidence) == :report_only
    end
  end

  test "renormalization can cross the raw auto boundary without granting automatic eligibility" do
    input = %{
      networks: %{
        rbf: [entry(:novelty_result, %{similarity: 0.99, trained: true, is_novel: false})],
        gnn: [entry(:graph_metrics, %{interaction_strength: 0.5, placeholder: false})],
        sequence: [entry(:prediction, %{confidence: 0.3, model_trained: true})]
      }
    }

    # The former placeholder weights would depress this below the boundary.
    assert (0.99 * 0.20 + 0.5 * 0.05 + 0.3 * 0.05) / 0.30 < 0.95
    ready = Coordinator.confidence_assessment(input, state())
    assert Coordinator.confidence_ready?(ready.readiness)
    assert_in_delta ready.confidence, 0.99, 1.0e-12
    assert Coordinator.raw_strategy(false, ready.confidence) == :auto_execute

    unready = Coordinator.confidence_assessment(input, state(false))
    assert unready.confidence == 0.5
    refute unready.readiness.ready
    assert Coordinator.raw_strategy(false, unready.confidence) == :report_only

    # Novelty and the advisory ceiling still take precedence. Do not supply a
    # pre-clamped review reply: exercise the actual composition on an auto-shaped
    # reply whose local metadata is complete and whose annealing is not clamped.
    assert Coordinator.raw_strategy(true, ready.confidence) == :report_only

    rec =
      recommendation(%{confidence: ready.confidence, confidence_readiness: ready.readiness})

    assert {:eliminate, recipe, _} = PatternAnalyzer.apply_neural_recommendation(action(), rec)
    assert recipe["confidence"] == 0.94
    assert recipe["neural_prior_confidence"] == 0.99
    assert recipe["neural_confidence_ready"]
  end

  test "a ready score inside the former magic band is used for demotion" do
    input = %{networks: %{rbf: [entry(:novelty_result, %{similarity: 0.73})]}}
    assessment = Coordinator.confidence_assessment(input, state())
    assert Coordinator.confidence_ready?(assessment.readiness)

    rec =
      recommendation(%{
        strategy: :review,
        confidence: assessment.confidence,
        confidence_readiness: assessment.readiness
      })

    assert {:eliminate, recipe, _} = PatternAnalyzer.apply_neural_recommendation(action(), rec)
    assert_in_delta recipe["confidence"], 0.73, 1.0e-12
    assert recipe["neural_prior_confidence"] == 0.99
    assert recipe["neural_confidence_ready"]
  end

  test "unready scores outside the former band do not imply learning or allow auto execution" do
    unready = Coordinator.confidence_assessment(trace(), state(false)).readiness

    for score <- [0.2, 0.99] do
      rec = recommendation(%{confidence: score, confidence_readiness: unready})
      assert {:eliminate, recipe, _} = PatternAnalyzer.apply_neural_recommendation(action(), rec)
      assert recipe["confidence"] == 0.94
      assert recipe["neural_prior_confidence"] == 0.99
      refute recipe["neural_confidence_ready"]
      assert TriangleRouter.dispatch_strategy(recipe["confidence"]) == :review
    end
  end

  test "missing, legacy or malformed metadata cannot preserve an auto-shaped recommendation" do
    metadata = recommendation().confidence_readiness

    invalid = [
      nil,
      true,
      %{trained: true},
      %{ready: true},
      Map.delete(metadata, :scope),
      Map.put(metadata, :scope, :operational_qualification),
      Map.put(metadata, :ready, "true"),
      Map.put(metadata, :contributing_models, []),
      Map.put(metadata, :contributing_models, [:gnn]),
      Map.put(metadata, :contributing_models, [:moe, :moe]),
      Map.put(metadata, :components, %{rbf: :contributing}),
      put_in(metadata.components.gnn, :contributing)
    ]

    recs =
      [Map.delete(recommendation(), :confidence_readiness)] ++
        Enum.map(invalid, &recommendation(%{confidence_readiness: &1}))

    for rec <- recs do
      assert {:eliminate, recipe, _} = PatternAnalyzer.apply_neural_recommendation(action(), rec)
      assert recipe["confidence"] == 0.94
      assert recipe["neural_prior_confidence"] == 0.99
      refute recipe["neural_confidence_ready"]
    end
  end

  test "readiness never promotes a recipe and retains its prior confidence" do
    for prior <- [0.70, 0.99] do
      assert {:eliminate, recipe, _} =
               PatternAnalyzer.apply_neural_recommendation(action(prior), recommendation())

      assert recipe["confidence"] == min(prior, 0.94)
      assert recipe["neural_prior_confidence"] == prior
      assert recipe["neural_confidence_ready"]
    end
  end

  test "novel and report-only refusals survive absent readiness metadata" do
    for overrides <- [%{is_novel: true}, %{strategy: :report_only}] do
      rec = recommendation(overrides) |> Map.delete(:confidence_readiness)
      assert {:control, pattern} = PatternAnalyzer.apply_neural_recommendation(action(), rec)
      refute pattern["neural_confidence_ready"]
    end
  end

  test "unavailable coordinator keeps the dispatch guard's ceiling" do
    assert {:eliminate, recipe, _} =
             PatternAnalyzer.apply_neural_recommendation(action(), :unavailable)

    assert recipe["confidence"] == 0.94
    assert recipe["neural_prior_confidence"] == 0.99
    assert recipe["neural_dispatch_guard"] == "unavailable"
  end
end
