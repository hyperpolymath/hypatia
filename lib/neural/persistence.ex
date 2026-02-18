# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.Neural.Persistence do
  @moduledoc """
  Persistence layer for neural network state.

  Handles saving/loading neural network weights and state to/from
  ArangoDB (when available) with fallback to flat files in verisimdb-data.

  On each learning cycle:
  1. Save trust graph edges to ArangoDB trusts collection
  2. Save MoE expert weights and gating weights
  3. Save confidence history entries (for ESN training)
  4. Save detected anomalies (from LSM)
  5. Save RBF centers and output weights

  On startup:
  1. Load saved state from ArangoDB (or flat files)
  2. Initialize networks with saved weights (or defaults)
  """

  require Logger

  @state_dir "~/Documents/hyperpolymath-repos/verisimdb-data/neural-states"

  @doc "Save all neural network states"
  def save_all(coordinator_state) do
    ensure_state_dir()

    save_trust_graph(coordinator_state.trust_graph)
    save_moe(coordinator_state.moe)
    save_lsm_summary(coordinator_state.lsm)
    save_esn_summary(coordinator_state.esn)
    save_rbf(coordinator_state.rbf)

    Logger.info("Neural states persisted (cycle #{coordinator_state.cycle_count})")
  end

  @doc "Load all neural network states (returns map of loaded data or :not_found)"
  def load_all do
    %{
      trust: load_state("trust"),
      moe: load_state("moe"),
      lsm: load_state("lsm"),
      esn: load_state("esn"),
      rbf: load_state("rbf")
    }
  end

  # --- Trust Graph ---

  defp save_trust_graph(graph) do
    data = %{
      "nodes" => Enum.map(graph.nodes, fn {id, type} -> %{"id" => id, "type" => to_string(type)} end),
      "edges" => Enum.map(graph.edges, fn {src, tgt, weight} -> %{"src" => src, "tgt" => tgt, "weight" => weight} end),
      "trust_scores" => graph.trust_scores,
      "last_computed" => if(graph.last_computed, do: DateTime.to_iso8601(graph.last_computed), else: nil)
    }

    save_state("trust", data)

    # Also persist trust edges to ArangoDB for graph queries
    if Process.whereis(Hypatia.Data.ArangoDB) do
      Enum.each(graph.edges, fn {src, tgt, weight} ->
        Hypatia.Data.ArangoDB.upsert_trust("repos", src, "repos", tgt, weight, 1)
      end)
    end
  end

  # --- Mixture of Experts ---

  defp save_moe(moe) do
    data = %{
      "gating_weights" => Enum.map(moe.gating_weights, fn {k, v} -> %{"domain" => to_string(k), "weight" => v} end),
      "expert_stats" => Enum.map(moe.expert_stats, fn {k, v} ->
        %{"domain" => to_string(k), "activations" => v.activations, "correct" => v.correct, "total" => v.total}
      end),
      "expert_weights" => Enum.map(moe.experts, fn {domain, expert} ->
        %{"domain" => to_string(domain), "weights" => expert.weights, "bias" => expert.bias}
      end),
      "last_trained" => if(moe.last_trained, do: DateTime.to_iso8601(moe.last_trained), else: nil)
    }

    save_state("moe", data)
  end

  # --- LSM (save summary, not full reservoir) ---

  defp save_lsm_summary(lsm) do
    summary = Hypatia.Neural.LiquidStateMachine.temporal_summary(lsm)
    anomalies = Hypatia.Neural.LiquidStateMachine.detect_anomalies(lsm)

    data = %{
      "summary" => summary,
      "anomaly_count" => length(anomalies),
      "history_length" => length(lsm.history),
      "trained" => lsm.trained
    }

    save_state("lsm", data)

    # Persist anomalies to ArangoDB
    if Process.whereis(Hypatia.Data.ArangoDB) do
      Enum.each(anomalies, fn anomaly ->
        Hypatia.Data.ArangoDB.record_anomaly(
          anomaly.event, anomaly.deviation, :lsm,
          if(anomaly.deviation > 3.0, do: :critical, else: :warning))
      end)
    end
  end

  # --- ESN (save accuracy and drift) ---

  defp save_esn_summary(esn) do
    accuracy = Hypatia.Neural.EchoStateNetwork.accuracy_report(esn)
    drift = Hypatia.Neural.EchoStateNetwork.detect_drift(esn)

    data = %{
      "accuracy" => accuracy,
      "drift" => to_string(drift),
      "trained" => esn.trained,
      "predictions_count" => length(esn.predictions)
    }

    save_state("esn", data)
  end

  # --- RBF ---

  defp save_rbf(rbf) do
    data = %{
      "centers" => rbf.centers,
      "widths" => rbf.widths,
      "output_weights" => rbf.output_weights,
      "num_inputs" => rbf.num_inputs,
      "num_centers" => rbf.num_centers,
      "trained" => rbf.trained,
      "training_stats" => rbf.training_stats
    }

    save_state("rbf", data)
  end

  # --- Generic Save/Load ---

  defp save_state(network, data) do
    # Save to ArangoDB
    if Process.whereis(Hypatia.Data.ArangoDB) do
      Hypatia.Data.ArangoDB.save_neural_state(
        network, data, 0, %{})
    end

    # Also save to flat file as backup
    path = Path.join(Path.expand(@state_dir), "#{network}.json")
    case Jason.encode(data, pretty: true) do
      {:ok, json} -> File.write(path, json)
      _ -> :error
    end
  end

  defp load_state(network) do
    # Try ArangoDB first
    result = if Process.whereis(Hypatia.Data.ArangoDB) do
      case Hypatia.Data.ArangoDB.load_neural_state(network) do
        {:ok, data} when is_map(data) -> {:ok, Map.get(data, "state_data", data)}
        _ -> :not_found
      end
    else
      :not_found
    end

    # Fall back to flat file
    case result do
      {:ok, data} -> {:ok, data}
      :not_found ->
        path = Path.join(Path.expand(@state_dir), "#{network}.json")
        case File.read(path) do
          {:ok, content} ->
            case Jason.decode(content) do
              {:ok, data} -> {:ok, data}
              _ -> :not_found
            end
          _ -> :not_found
        end
    end
  end

  defp ensure_state_dir do
    dir = Path.expand(@state_dir)
    File.mkdir_p(dir)
  end
end
