# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.Neural.Coordinator do
  @moduledoc """
  Neural Network Coordinator for Hypatia.

  Orchestrates all five neural network subsystems into a unified
  intelligence layer for the safety triangle pipeline.

  Network roles:
  - Graph of Trust: trust-weighted dispatch routing (who to send findings to)
  - Mixture of Experts: domain-specific confidence estimation (how confident)
  - Liquid State Machine: temporal anomaly detection (event stream monitoring)
  - Echo State Network: confidence trajectory forecasting (where is confidence heading)
  - Radial Neural Network: finding similarity and novelty detection (is this new?)

  The coordinator:
  1. Maintains shared state across all networks
  2. Routes findings through appropriate networks
  3. Aggregates predictions for dispatch decisions
  4. Feeds outcomes back for continuous learning
  """

  use GenServer
  require Logger

  alias Hypatia.Neural.{
    GraphOfTrust,
    MixtureOfExperts,
    LiquidStateMachine,
    EchoStateNetwork,
    RadialNeuralNetwork
  }

  defstruct [
    trust_graph: nil,
    moe: nil,
    lsm: nil,
    esn: nil,
    rbf: nil,
    cycle_count: 0,
    last_cycle: nil
  ]

  # --- GenServer API ---

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(_opts) do
    state = %__MODULE__{
      trust_graph: GraphOfTrust.build(),
      moe: MixtureOfExperts.init(),
      lsm: LiquidStateMachine.init(),
      esn: EchoStateNetwork.init(),
      rbf: RadialNeuralNetwork.init()
    }

    Logger.info("Neural Coordinator initialized with 5 networks")
    {:ok, state}
  end

  # --- Public API ---

  @doc "Analyze a finding through all relevant networks"
  def analyze(finding) do
    GenServer.call(__MODULE__, {:analyze, finding})
  end

  @doc "Record an outcome and update all networks"
  def record_outcome(finding, outcome) do
    GenServer.cast(__MODULE__, {:outcome, finding, outcome})
  end

  @doc "Get dispatch recommendation (combines trust + MoE + novelty)"
  def dispatch_recommendation(finding) do
    GenServer.call(__MODULE__, {:dispatch_rec, finding})
  end

  @doc "Get system-wide neural health report"
  def health_report do
    GenServer.call(__MODULE__, :health_report)
  end

  @doc "Force a learning cycle across all networks"
  def force_cycle do
    GenServer.cast(__MODULE__, :force_cycle)
  end

  # --- GenServer Callbacks ---

  def handle_call({:analyze, finding}, _from, state) do
    # MoE: domain-specific confidence
    {moe_confidence, experts} = MixtureOfExperts.predict(state.moe, finding)

    # RBF: similarity and novelty check
    feature_vec = RadialNeuralNetwork.finding_to_vector(finding)
    {rbf_output, rbf_confidence} = RadialNeuralNetwork.predict(state.rbf, feature_vec)
    novelty = RadialNeuralNetwork.detect_novelty(state.rbf, feature_vec)

    # LSM: process as temporal event
    updated_lsm = LiquidStateMachine.process_event(state.lsm, finding)
    {lsm_score, lsm_status} = LiquidStateMachine.predict(updated_lsm)
    anomalies = LiquidStateMachine.detect_anomalies(updated_lsm)

    # ESN: predict confidence trajectory
    {esn_pred, updated_esn} = EchoStateNetwork.step(state.esn, moe_confidence)
    drift = EchoStateNetwork.detect_drift(updated_esn)

    analysis = %{
      moe: %{confidence: moe_confidence, experts: experts},
      rbf: %{output: rbf_output, similarity: rbf_confidence, novelty: novelty},
      lsm: %{score: lsm_score, status: lsm_status, anomalies: length(anomalies)},
      esn: %{predicted_next: esn_pred, drift: drift},
      aggregated_confidence: aggregate_confidence(moe_confidence, rbf_confidence, lsm_score),
      is_novel: match?({:novel, _}, novelty)
    }

    {:reply, analysis, %{state | lsm: updated_lsm, esn: updated_esn}}
  end

  def handle_call({:dispatch_rec, finding}, _from, state) do
    # Trust graph: find best bot
    trusted_bots = GraphOfTrust.trusted_bots(state.trust_graph)
    trusted_recipes = GraphOfTrust.trusted_recipes(state.trust_graph)

    # MoE: confidence for this finding
    {confidence, experts} = MixtureOfExperts.predict(state.moe, finding)

    # RBF: novelty check
    feature_vec = RadialNeuralNetwork.finding_to_vector(finding)
    novelty = RadialNeuralNetwork.detect_novelty(state.rbf, feature_vec)

    strategy = cond do
      match?({:novel, _}, novelty) -> :report_only  # Novel findings need human review
      confidence >= 0.95 -> :auto_execute
      confidence >= 0.85 -> :review
      true -> :report_only
    end

    recommendation = %{
      strategy: strategy,
      confidence: confidence,
      experts: experts,
      preferred_bots: Enum.take(trusted_bots, 3),
      preferred_recipes: Enum.take(trusted_recipes, 5),
      is_novel: match?({:novel, _}, novelty),
      reason: dispatch_reason(strategy, confidence, novelty)
    }

    {:reply, recommendation, state}
  end

  def handle_call(:health_report, _from, state) do
    report = %{
      trust_graph: %{
        nodes: map_size(state.trust_graph.nodes),
        edges: length(state.trust_graph.edges),
        repos_needing_attention: length(GraphOfTrust.repos_needing_attention(state.trust_graph))
      },
      moe: %{
        utilization: MixtureOfExperts.expert_utilization(state.moe),
        last_trained: state.moe.last_trained
      },
      lsm: LiquidStateMachine.temporal_summary(state.lsm),
      esn: %{
        accuracy: EchoStateNetwork.accuracy_report(state.esn),
        drift: EchoStateNetwork.detect_drift(state.esn),
        trained: state.esn.trained
      },
      rbf: %{
        trained: state.rbf.trained,
        training_stats: state.rbf.training_stats
      },
      cycle_count: state.cycle_count,
      last_cycle: state.last_cycle
    }

    {:reply, report, state}
  end

  def handle_cast({:outcome, finding, outcome}, state) do
    # Update MoE
    atom_outcome = case outcome do
      "success" -> :success
      "failure" -> :failure
      "false_positive" -> :failure
      _ -> :failure
    end

    updated_moe = MixtureOfExperts.train(state.moe, finding, atom_outcome)

    # Process through LSM as event
    event = Map.put(finding, "outcome", outcome)
    updated_lsm = LiquidStateMachine.process_event(state.lsm, event)

    # Feed confidence to ESN
    {confidence, _} = MixtureOfExperts.predict(updated_moe, finding)
    {_pred, updated_esn} = EchoStateNetwork.step(state.esn, confidence)

    {:noreply, %{state | moe: updated_moe, lsm: updated_lsm, esn: updated_esn}}
  end

  def handle_cast(:force_cycle, state) do
    Logger.info("Neural Coordinator: forcing learning cycle ##{state.cycle_count + 1}")

    # Rebuild trust graph from latest data
    updated_graph = GraphOfTrust.build()

    # Train ESN on accumulated confidence trajectories
    trained_esn = Hypatia.Neural.TrainingPipeline.train_esn(state.esn)

    # Train RBF on pattern registry data
    trained_rbf = Hypatia.Neural.TrainingPipeline.train_rbf(state.rbf)

    updated_state = %{state |
      trust_graph: updated_graph,
      esn: trained_esn,
      rbf: trained_rbf,
      cycle_count: state.cycle_count + 1,
      last_cycle: DateTime.utc_now()
    }

    # Persist all neural states (ArangoDB + flat file backup)
    Hypatia.Neural.Persistence.save_all(updated_state)

    {:noreply, updated_state}
  end

  # --- Aggregation ---

  defp aggregate_confidence(moe_conf, rbf_conf, lsm_score) do
    # Weighted average: MoE is primary, RBF for similarity, LSM for anomaly
    weighted = moe_conf * 0.6 + rbf_conf * 0.25 + lsm_score * 0.15
    min(max(weighted, 0.0), 1.0)
  end

  defp dispatch_reason(strategy, confidence, novelty) do
    base = case strategy do
      :auto_execute -> "High confidence (#{Float.round(confidence, 3)})"
      :review -> "Moderate confidence (#{Float.round(confidence, 3)}), needs human review"
      :report_only -> "Low confidence (#{Float.round(confidence, 3)})"
    end

    case novelty do
      {:novel, distance} -> base <> "; NOVEL finding (distance #{Float.round(distance, 2)} from known patterns)"
      _ -> base
    end
  end
end
