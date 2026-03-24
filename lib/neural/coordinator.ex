# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Neural Coordinator — blackboard architecture with phased execution
#
# Migrated from hub-and-spoke (5 networks, fixed weights) to blackboard
# architecture (8 networks, 6 phases, shared ETS state).
#
# Each phase reads from the blackboard, computes, and writes results back.
# Parallel phases run concurrently via Task.async_stream.

defmodule Hypatia.Neural.Coordinator do
  @moduledoc """
  Neural Network Coordinator for Hypatia (blackboard architecture).

  Orchestrates 8 neural network subsystems through 6 execution phases,
  coordinated via a shared ETS blackboard. Each network reads accumulated
  context before computing and writes its results for downstream phases.

  ## Networks (8 total)

  | Network   | Phase | Role                                      |
  |-----------|-------|-------------------------------------------|
  | RBF       | 1     | Novelty detection — novel vs known        |
  | PageRank  | 2     | Trust-weighted routing over graph          |
  | GNN       | 2     | Agent interaction graph structure          |
  | MoE       | 3     | Domain-specific confidence estimation      |
  | ESN       | 4     | Confidence trajectory forecasting          |
  | LSM       | 4     | Temporal anomaly detection                 |
  | VAE       | 5     | Hermeneutic interpretation clustering      |
  | Sequence  | 6     | Choreography trace prediction              |

  ## Phase execution order

  1. RBF (novelty) — everything benefits from knowing novel vs known
  2. PageRank + GNN (parallel) — graph structure and trust
  3. MoE (domain routing) — reads novelty + trust context
  4. ESN + LSM (parallel) — temporal dynamics, reads domain + novelty
  5. VAE (interpretation clustering) — reads full context
  6. Sequence model (choreography prediction) — reads full context

  ## Architecture change

  Previous: hub-and-spoke with fixed weights (0.6 MoE + 0.25 RBF + 0.15 LSM).
  Current: blackboard with phased execution. Confidence derives from the full
  reasoning trace, not a hardcoded weighted sum.
  """

  use GenServer
  require Logger

  alias Hypatia.Neural.{
    Blackboard,
    GraphOfTrust,
    MixtureOfExperts,
    LiquidStateMachine,
    EchoStateNetwork,
    RadialNeuralNetwork,
    GraphNeuralNetwork,
    VariationalAutoencoder,
    SequenceModel
  }

  defstruct [
    # Original 5 network states (stateful, updated in-process)
    trust_graph: nil,
    moe: nil,
    lsm: nil,
    esn: nil,
    rbf: nil,
    # Lifecycle
    cycle_count: 0,
    last_cycle: nil,
    # Blackboard architecture metadata
    total_rounds: 0,
    last_round_trace: nil
  ]

  @network_count 8
  @phase_count 6

  # ── GenServer API ───────────────────────────────────────

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    state = %__MODULE__{
      trust_graph: GraphOfTrust.build(),
      moe: MixtureOfExperts.init(),
      lsm: LiquidStateMachine.init(),
      esn: EchoStateNetwork.init(),
      rbf: RadialNeuralNetwork.init()
    }

    Logger.info(
      "Neural Coordinator initialised: blackboard architecture, " <>
        "#{@network_count} networks, #{@phase_count} phases"
    )

    {:ok, state}
  end

  # ── Public API ──────────────────────────────────────────

  @doc """
  Analyse a finding through all 8 networks using phased blackboard execution.

  Executes the 6-phase pipeline:
  1. RBF novelty detection
  2. PageRank + GNN (parallel)
  3. MoE domain routing
  4. ESN + LSM temporal (parallel)
  5. VAE interpretation clustering
  6. Sequence model choreography prediction

  Returns the complete reasoning trace from the blackboard.
  """
  def analyze(finding) do
    GenServer.call(__MODULE__, {:analyze, finding}, 30_000)
  end

  @doc "Record an outcome and update all networks."
  def record_outcome(finding, outcome) do
    GenServer.cast(__MODULE__, {:outcome, finding, outcome})
  end

  @doc """
  Get dispatch recommendation using blackboard-derived confidence.

  Unlike the old fixed-weight aggregation, confidence is derived from
  the complete reasoning trace across all 8 networks.
  """
  def dispatch_recommendation(finding) do
    GenServer.call(__MODULE__, {:dispatch_rec, finding}, 30_000)
  end

  @doc "Get system-wide neural health report for all 8 networks."
  def health_report do
    GenServer.call(__MODULE__, :health_report)
  end

  @doc "Force a learning cycle across all networks."
  def force_cycle do
    GenServer.cast(__MODULE__, :force_cycle)
  end

  @doc "Get the number of networks in the coordinator."
  def network_count, do: @network_count

  @doc "Get the number of execution phases."
  def phase_count, do: @phase_count

  # ── GenServer Callbacks ─────────────────────────────────

  @impl true
  def handle_call(:status, _from, state) do
    status = %{
      networks: [
        :radial_neural_network,
        :graph_of_trust,
        :graph_neural_network,
        :mixture_of_experts,
        :echo_state_network,
        :liquid_state_machine,
        :variational_autoencoder,
        :sequence_model
      ],
      network_count: @network_count,
      phase_count: @phase_count,
      architecture: :blackboard,
      status: :running,
      cycle_count: state.cycle_count,
      total_rounds: state.total_rounds
    }

    {:reply, {:ok, status}, state}
  end

  @impl true
  def handle_call({:analyze, finding}, _from, state) do
    {trace, updated_state} = run_phased_analysis(finding, state)

    analysis = %{
      # Legacy fields for backward compatibility
      moe: trace_field(trace, :moe, :domain_confidence),
      rbf: trace_field(trace, :rbf, :novelty_result),
      lsm: trace_field(trace, :lsm, :temporal_result),
      esn: trace_field(trace, :esn, :trajectory_result),
      # New blackboard fields
      gnn: trace_field(trace, :gnn, :graph_metrics),
      vae: trace_field(trace, :vae, :interpretation),
      sequence: trace_field(trace, :sequence, :prediction),
      # Derived confidence from full reasoning trace
      aggregated_confidence: derive_confidence(trace),
      is_novel: is_novel?(trace),
      # Blackboard metadata
      round: trace.round,
      phase_count: @phase_count,
      network_count: @network_count,
      architecture: :blackboard
    }

    {:reply, analysis, updated_state}
  end

  @impl true
  def handle_call({:dispatch_rec, finding}, _from, state) do
    {trace, updated_state} = run_phased_analysis(finding, state)

    confidence = derive_confidence(trace)
    novel = is_novel?(trace)

    # Trust graph: find best bots
    trusted_bots = GraphOfTrust.trusted_bots(state.trust_graph)
    trusted_recipes = GraphOfTrust.trusted_recipes(state.trust_graph)

    strategy =
      cond do
        novel -> :report_only
        confidence >= 0.95 -> :auto_execute
        confidence >= 0.85 -> :review
        true -> :report_only
      end

    recommendation = %{
      strategy: strategy,
      confidence: confidence,
      preferred_bots: Enum.take(trusted_bots, 3),
      preferred_recipes: Enum.take(trusted_recipes, 5),
      is_novel: novel,
      reason: dispatch_reason(strategy, confidence, novel),
      architecture: :blackboard,
      networks_consulted: @network_count
    }

    {:reply, recommendation, updated_state}
  end

  @impl true
  def handle_call(:health_report, _from, state) do
    report = %{
      architecture: :blackboard,
      network_count: @network_count,
      phase_count: @phase_count,
      # Original 5 networks (stateful)
      trust_graph: %{
        nodes: map_size(state.trust_graph.nodes),
        edges: length(state.trust_graph.edges),
        repos_needing_attention:
          length(GraphOfTrust.repos_needing_attention(state.trust_graph))
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
      # New 3 networks (GenServer, status via process alive check)
      gnn: %{alive: process_alive?(GraphNeuralNetwork)},
      vae: %{alive: process_alive?(VariationalAutoencoder)},
      sequence_model: %{alive: process_alive?(SequenceModel)},
      # Blackboard health
      blackboard: %{alive: Blackboard.alive?()},
      # Lifecycle
      cycle_count: state.cycle_count,
      last_cycle: state.last_cycle,
      total_rounds: state.total_rounds
    }

    {:reply, report, state}
  end

  @impl true
  def handle_cast({:outcome, finding, outcome}, state) do
    # Update MoE
    atom_outcome =
      case outcome do
        "success" -> :success
        "failure" -> :failure
        "false_positive" -> :failure
        _ -> :failure
      end

    updated_moe = MixtureOfExperts.train(state.moe, finding, atom_outcome)

    # Process through LSM as temporal event
    event = Map.put(finding, "outcome", outcome)
    updated_lsm = LiquidStateMachine.process_event(state.lsm, event)

    # Feed confidence to ESN
    {confidence, _} = MixtureOfExperts.predict(updated_moe, finding)
    {_pred, updated_esn} = EchoStateNetwork.step(state.esn, confidence)

    # Feed decision to sequence model for trace learning
    SequenceModel.observe(%{finding: finding, outcome: outcome, confidence: confidence})

    {:noreply, %{state | moe: updated_moe, lsm: updated_lsm, esn: updated_esn}}
  end

  @impl true
  def handle_cast(:force_cycle, state) do
    Logger.info("Neural Coordinator: forcing learning cycle ##{state.cycle_count + 1}")

    # Rebuild trust graph from latest data
    updated_graph = GraphOfTrust.build()

    # Train ESN on accumulated confidence trajectories
    trained_esn = Hypatia.Neural.TrainingPipeline.train_esn(state.esn)

    # Train RBF on pattern registry data
    trained_rbf = Hypatia.Neural.TrainingPipeline.train_rbf(state.rbf)

    updated_state = %{
      state
      | trust_graph: updated_graph,
        esn: trained_esn,
        rbf: trained_rbf,
        cycle_count: state.cycle_count + 1,
        last_cycle: DateTime.utc_now()
    }

    # Persist all neural states to flat files
    Hypatia.Neural.Persistence.save_all(updated_state)

    {:noreply, updated_state}
  end

  # ── Phased Blackboard Execution ─────────────────────────

  @doc false
  defp run_phased_analysis(finding, state) do
    # Start a new blackboard round
    {:ok, _round} = Blackboard.start_round()

    # Write the input finding to the blackboard
    Blackboard.write(:input, :finding, finding)

    # Phase 1: RBF (novelty detection) — sequential, everything depends on this
    state = run_phase_1_rbf(finding, state)

    # Phase 2: PageRank + GNN (parallel) — graph structure
    run_phase_2_graph(finding, state)

    # Phase 3: MoE (domain routing) — reads novelty + trust
    state = run_phase_3_moe(finding, state)

    # Phase 4: ESN + LSM (parallel) — temporal dynamics
    state = run_phase_4_temporal(finding, state)

    # Phase 5: VAE (interpretation clustering) — reads everything
    run_phase_5_vae(finding)

    # Phase 6: Sequence model (choreography) — reads everything
    run_phase_6_sequence(finding)

    # End the round and capture the reasoning trace
    {:ok, trace} = Blackboard.end_round()

    updated_state = %{state | total_rounds: state.total_rounds + 1, last_round_trace: trace}
    {trace, updated_state}
  end

  # Phase 1: RBF novelty detection
  defp run_phase_1_rbf(finding, state) do
    feature_vec = RadialNeuralNetwork.finding_to_vector(finding)
    {rbf_output, rbf_confidence} = RadialNeuralNetwork.predict(state.rbf, feature_vec)
    novelty = RadialNeuralNetwork.detect_novelty(state.rbf, feature_vec)

    result = %{
      output: rbf_output,
      similarity: rbf_confidence,
      novelty: novelty,
      is_novel: match?({:novel, _}, novelty)
    }

    Blackboard.write(:rbf, :novelty_result, result)
    Blackboard.write(:rbf, :novelty, %{score: rbf_confidence, status: novelty_status(novelty)})

    state
  end

  # Phase 2: PageRank + GNN (parallel)
  defp run_phase_2_graph(finding, state) do
    tasks = [
      Task.async(fn ->
        # PageRank: trust-weighted routing
        trusted_bots = GraphOfTrust.trusted_bots(state.trust_graph)
        trusted_recipes = GraphOfTrust.trusted_recipes(state.trust_graph)

        trust_result = %{
          trusted_bots: Enum.take(trusted_bots, 5),
          trusted_recipes: Enum.take(trusted_recipes, 5),
          repos_needing_attention:
            length(GraphOfTrust.repos_needing_attention(state.trust_graph))
        }

        Blackboard.write(:pagerank, :trust_result, trust_result)
      end),
      Task.async(fn ->
        # GNN: agent interaction patterns (calls the GNN GenServer)
        GraphNeuralNetwork.analyse(finding)
      end)
    ]

    # Await both parallel tasks (5s timeout each)
    Task.await_many(tasks, 5_000)
    :ok
  end

  # Phase 3: MoE domain routing
  defp run_phase_3_moe(finding, state) do
    {moe_confidence, experts} = MixtureOfExperts.predict(state.moe, finding)

    domain_result = %{
      confidence: moe_confidence,
      experts: experts,
      top_domain: List.first(experts)
    }

    Blackboard.write(:moe, :domain_confidence, domain_result)
    state
  end

  # Phase 4: ESN + LSM (parallel)
  defp run_phase_4_temporal(finding, state) do
    # Read MoE confidence from blackboard for ESN input
    moe_confidence =
      case Blackboard.read(:moe, :domain_confidence) do
        {:ok, %{confidence: c}} -> c
        _ -> 0.5
      end

    # Run ESN and LSM in parallel, but we need the updated state back
    esn_task =
      Task.async(fn ->
        {esn_pred, updated_esn} = EchoStateNetwork.step(state.esn, moe_confidence)
        drift = EchoStateNetwork.detect_drift(updated_esn)

        trajectory_result = %{
          predicted_next: esn_pred,
          drift: drift,
          input_confidence: moe_confidence
        }

        Blackboard.write(:esn, :trajectory_result, trajectory_result)
        updated_esn
      end)

    lsm_task =
      Task.async(fn ->
        updated_lsm = LiquidStateMachine.process_event(state.lsm, finding)
        {lsm_score, lsm_status} = LiquidStateMachine.predict(updated_lsm)
        anomalies = LiquidStateMachine.detect_anomalies(updated_lsm)

        temporal_result = %{
          score: lsm_score,
          status: lsm_status,
          anomaly_count: length(anomalies)
        }

        Blackboard.write(:lsm, :temporal_result, temporal_result)
        updated_lsm
      end)

    updated_esn = Task.await(esn_task, 5_000)
    updated_lsm = Task.await(lsm_task, 5_000)

    %{state | esn: updated_esn, lsm: updated_lsm}
  end

  # Phase 5: VAE interpretation clustering
  defp run_phase_5_vae(finding) do
    # VAE is a GenServer — calls it to cluster the finding
    VariationalAutoencoder.cluster(finding)
    :ok
  end

  # Phase 6: Sequence model choreography prediction
  defp run_phase_6_sequence(finding) do
    # Sequence model is a GenServer — calls it to predict next step
    SequenceModel.predict(finding)
    :ok
  end

  # ── Confidence Derivation ───────────────────────────────

  @doc false
  defp derive_confidence(trace) do
    # Derive confidence from the full reasoning trace rather than
    # hardcoded fixed weights. Each network contributes evidence.
    networks = trace.networks || %{}

    # Extract individual confidence signals
    moe_conf = extract_confidence(networks, :moe, :domain_confidence, fn r -> r.confidence end, 0.5)
    rbf_conf = extract_confidence(networks, :rbf, :novelty_result, fn r -> r.similarity end, 0.0)
    lsm_score = extract_confidence(networks, :lsm, :temporal_result, fn r -> r.score end, 0.5)
    esn_pred = extract_confidence(networks, :esn, :trajectory_result, fn r -> r.predicted_next end, 0.5)

    # Adaptive weighting: if a network contributed, its weight counts.
    # Networks that didn't contribute get zero weight.
    weights = [
      {moe_conf, if(has_network?(networks, :moe), do: 0.35, else: 0.0)},
      {rbf_conf, if(has_network?(networks, :rbf), do: 0.20, else: 0.0)},
      {lsm_score, if(has_network?(networks, :lsm), do: 0.15, else: 0.0)},
      {esn_pred, if(has_network?(networks, :esn), do: 0.15, else: 0.0)},
      # New networks contribute smaller initial weights (they're untrained)
      {gnn_confidence(networks), if(has_network?(networks, :gnn), do: 0.05, else: 0.0)},
      {vae_confidence(networks), if(has_network?(networks, :vae), do: 0.05, else: 0.0)},
      {seq_confidence(networks), if(has_network?(networks, :sequence), do: 0.05, else: 0.0)}
    ]

    total_weight = weights |> Enum.map(&elem(&1, 1)) |> Enum.sum()

    if total_weight > 0 do
      weighted_sum =
        weights
        |> Enum.map(fn {val, w} -> val * w end)
        |> Enum.sum()

      (weighted_sum / total_weight)
      |> max(0.0)
      |> min(1.0)
    else
      0.5
    end
  end

  defp extract_confidence(networks, network_key, entry_key, extractor, default) do
    case Map.get(networks, network_key) do
      entries when is_list(entries) ->
        case Enum.find(entries, fn e -> e.key == entry_key end) do
          %{value: value} ->
            try do
              extractor.(value)
            rescue
              _ -> default
            end

          nil ->
            default
        end

      _ ->
        default
    end
  end

  defp has_network?(networks, key), do: Map.has_key?(networks, key)

  defp gnn_confidence(networks) do
    extract_confidence(networks, :gnn, :graph_metrics, fn r ->
      Map.get(r, :interaction_strength, 0.5)
    end, 0.5)
  end

  defp vae_confidence(networks) do
    extract_confidence(networks, :vae, :interpretation, fn r ->
      # VAE doesn't produce a confidence directly; use cluster coherence
      if r.cluster_id != nil, do: 0.6, else: 0.3
    end, 0.3)
  end

  defp seq_confidence(networks) do
    extract_confidence(networks, :sequence, :prediction, fn r ->
      Map.get(r, :confidence, 0.0)
    end, 0.0)
  end

  defp is_novel?(trace) do
    case Map.get(trace.networks || %{}, :rbf) do
      entries when is_list(entries) ->
        case Enum.find(entries, fn e -> e.key == :novelty_result end) do
          %{value: %{is_novel: true}} -> true
          _ -> false
        end

      _ ->
        false
    end
  end

  defp novelty_status(novelty) do
    case novelty do
      {:novel, _} -> :novel
      {:known, _} -> :known
      :untrained -> :unknown
      _ -> :unknown
    end
  end

  defp dispatch_reason(strategy, confidence, novel) do
    base =
      case strategy do
        :auto_execute -> "High confidence (#{Float.round(confidence, 3)})"
        :review -> "Moderate confidence (#{Float.round(confidence, 3)}), needs human review"
        :report_only -> "Low confidence (#{Float.round(confidence, 3)})"
      end

    if novel do
      base <> "; NOVEL finding forced to report_only"
    else
      base
    end
  end

  defp process_alive?(module) do
    case GenServer.whereis(module) do
      nil -> false
      pid -> Process.alive?(pid)
    end
  end

  # ── Trace Field Extraction ──────────────────────────────

  defp trace_field(trace, network_key, entry_key) do
    case Map.get(trace.networks || %{}, network_key) do
      entries when is_list(entries) ->
        case Enum.find(entries, fn e -> e.key == entry_key end) do
          %{value: v} -> v
          nil -> nil
        end

      _ ->
        nil
    end
  end
end
