# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Neural.Persistence do
  @moduledoc """
  Persistence layer for neural network state.

  Handles saving/loading neural network weights and state to/from
  flat files in verisim-data/neural-states/.

  On each learning cycle:
  1. Save trust graph edges
  2. Save MoE expert weights and gating weights
  3. Save confidence history entries (for ESN training)
  4. Save detected anomalies (from LSM)
  5. Save RBF centers and output weights

  On startup:
  1. Load saved state from flat files
  2. Initialize learned networks only with matching source-labelled weights
     (or defaults); unbound and foreign snapshots remain diagnostic
  3. Rebuild trust from current qualified outcomes; its snapshots are diagnostic
     records and are never restored as authoritative scores
  """

  require Logger

  alias Hypatia.Neural.SourceStore

  def default_networks do
    %{
      gnn: Hypatia.Neural.GraphNeuralNetwork,
      vae: Hypatia.Neural.VariationalAutoencoder,
      sequence: Hypatia.Neural.SequenceModel
    }
  end

  @doc """
  Save a source-bound coordinator and all three matching named networks.

  Every named snapshot is collected and checked, and every document encoded,
  before any file is written. A refusal or missing process returns {:error,
  reason}. Writes use the captured source directory, never a freshly resolved
  runtime destination. Filesystem writes can still fail partway through a
  bundle; such an error is returned, not logged as successful persistence.
  """
  def save_all(coordinator_state) when is_map(coordinator_state) do
    source = Map.get(coordinator_state, :source_store)
    networks = Map.get(coordinator_state, :named_networks, default_networks())

    with :ok <- SourceStore.check(coordinator_state, source),
         {:ok, named} <- collect_named(source, networks),
         {:ok, encoded} <- encode_states(coordinator_state, named),
         :ok <- SourceStore.check(coordinator_state, source),
         :ok <- write_states(source, encoded) do
      Logger.info("Neural states persisted (cycle #{coordinator_state.cycle_count})")
      :ok
    else
      {:error, reason} = error ->
        Logger.warning("Neural persistence refused: #{inspect(reason)}")
        error
    end
  end

  def save_all(_), do: {:error, :unbound_source_store}

  @doc """
  Read raw diagnostic snapshots from one captured directory.

  This does not qualify a model for hydration. Legacy and foreign-labelled
  snapshots are retained and returned verbatim for diagnosis.
  """
  def load_all(source \\ SourceStore.current()) do
    for name <- [:trust, :moe, :lsm, :esn, :rbf, :gnn, :vae, :sequence], into: %{} do
      {name, load_state(source, Atom.to_string(name))}
    end
  end

  @doc false
  def collect_named(source, networks) when is_map(networks) do
    Enum.reduce_while([:gnn, :vae, :sequence], {:ok, %{}}, fn name, {:ok, snapshots} ->
      result =
        try do
          case Map.fetch(networks, name) do
            {:ok, server} -> GenServer.call(server, {:snapshot, source})
            :error -> {:error, :missing_network}
          end
        catch
          :exit, reason -> {:error, {:process_unavailable, reason}}
        end

      case result do
        {:ok, snapshot} when is_map(snapshot) ->
          if SourceStore.snapshot_matches?(snapshot, source) do
            {:cont, {:ok, Map.put(snapshots, Atom.to_string(name), snapshot)}}
          else
            {:halt, {:error, {:cohort_source_mismatch, name, :unqualified_snapshot}}}
          end

        {:error, reason} ->
          {:halt, {:error, {:cohort_source_mismatch, name, reason}}}

        other ->
          {:halt, {:error, {:cohort_source_mismatch, name, {:unexpected_reply, other}}}}
      end
    end)
  end

  def collect_named(_source, _networks), do: {:error, {:cohort_source_mismatch, :all, :invalid_cohort}}

  defp encode_states(state, named) do
    snapshots = %{
      "trust" => snapshot_trust_graph(state.trust_graph),
      "moe" => snapshot_moe(state.moe),
      "lsm" => snapshot_lsm_summary(state.lsm),
      "esn" => snapshot_esn_summary(state.esn),
      "rbf" => snapshot_rbf(state.rbf)
    }

    snapshots
    |> Map.new(fn {name, data} -> {name, SourceStore.tag(data, state)} end)
    |> Map.merge(named)
    |> Enum.sort_by(&elem(&1, 0))
    |> Enum.reduce_while({:ok, []}, fn {name, data}, {:ok, encoded} ->
      case Jason.encode(data, pretty: true) do
        {:ok, json} -> {:cont, {:ok, [{name, json} | encoded]}}
        {:error, reason} -> {:halt, {:error, {:snapshot_encoding, name, reason}}}
      end
    end)
  rescue
    error -> {:error, {:snapshot_encoding, Exception.message(error)}}
  end

  defp write_states(source, encoded) do
    directory = Path.join(source, "neural-states")

    with :ok <- File.mkdir_p(directory) do
      Enum.reduce_while(encoded, :ok, fn {name, json}, :ok ->
        path = Path.join(directory, "#{name}.json")

        case File.write(path, json) do
          :ok -> {:cont, :ok}
          {:error, reason} -> {:halt, {:error, {:snapshot_write, name, reason}}}
        end
      end)
    else
      {:error, reason} -> {:error, {:snapshot_directory, reason}}
    end
  end

  # --- Trust Graph ---

  defp snapshot_trust_graph(graph) do
    data = %{
      "schema_version" => 2,
      "score_basis" => "qualified_positive_support_subgraph",
      "restore_policy" => "recompute_from_current_qualified_outcomes",
      "nodes" =>
        Enum.map(graph.nodes, fn {id, type} -> %{"id" => id, "type" => to_string(type)} end),
      "edges" =>
        Enum.map(graph.edges, fn {src, tgt, weight} ->
          %{"src" => src, "tgt" => tgt, "weight" => weight}
        end),
      "trust_scores" => graph.trust_scores,
      "last_computed" =>
        if(graph.last_computed, do: DateTime.to_iso8601(graph.last_computed), else: nil)
    }

    data
  end

  # --- Mixture of Experts ---

  defp snapshot_moe(moe) do
    data = %{
      "gating_weights" =>
        Enum.map(moe.gating_weights, fn {k, v} -> %{"domain" => to_string(k), "weight" => v} end),
      "expert_stats" =>
        Enum.map(moe.expert_stats, fn {k, v} ->
          %{
            "domain" => to_string(k),
            "activations" => v.activations,
            "correct" => v.correct,
            "total" => v.total
          }
        end),
      "expert_weights" =>
        Enum.map(moe.experts, fn {domain, expert} ->
          %{"domain" => to_string(domain), "weights" => expert.weights, "bias" => expert.bias}
        end),
      "last_trained" => if(moe.last_trained, do: DateTime.to_iso8601(moe.last_trained), else: nil)
    }

    data
  end

  # --- LSM (save summary, not full reservoir) ---

  defp snapshot_lsm_summary(lsm) do
    summary = Hypatia.Neural.LiquidStateMachine.temporal_summary(lsm)
    anomalies = Hypatia.Neural.LiquidStateMachine.detect_anomalies(lsm)

    data = %{
      "summary" => summary,
      "anomaly_count" => length(anomalies),
      "history_length" => length(lsm.history),
      "trained" => lsm.trained
    }

    data
  end

  # --- ESN (save accuracy and drift) ---

  defp snapshot_esn_summary(esn) do
    accuracy = Hypatia.Neural.EchoStateNetwork.accuracy_report(esn)
    drift = Hypatia.Neural.EchoStateNetwork.detect_drift(esn)

    data = %{
      "accuracy" => accuracy,
      "drift" => to_string(drift),
      "trained" => esn.trained,
      "predictions_count" => length(esn.predictions)
    }

    data
  end

  # --- RBF ---

  defp snapshot_rbf(rbf) do
    data = %{
      "centers" => rbf.centers,
      "widths" => rbf.widths,
      "output_weights" => rbf.output_weights,
      "num_inputs" => rbf.num_inputs,
      "num_centers" => rbf.num_centers,
      "trained" => rbf.trained,
      "training_stats" => rbf.training_stats
    }

    data
  end

  # --- Raw Diagnostic Read ---

  defp load_state(source, network) do
    path = Path.join([source, "neural-states", "#{network}.json"])

    case File.read(path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, data} -> {:ok, data}
          _ -> :not_found
        end

      _ ->
        :not_found
    end
  end
end
