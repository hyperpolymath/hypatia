# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.PlaceholderNetworkTest do
  @moduledoc """
  NS-03 extends E0-23: current GNN/VAE placeholders and the sequence
  last-observation heuristic have no confidence weight. Trace labels alone
  cannot establish that a model has learned. Real cohort checks also live in
  neural_source_store_lifecycle_test.exs.
  """
  use ExUnit.Case, async: true

  alias Hypatia.Neural.Coordinator

  defp entry(key, value), do: %{key: key, value: value}

  defp trace(extra) do
    base = %{
      moe: [entry(:domain_confidence, %{confidence: 0.99, trained: true})],
      rbf: [entry(:novelty_result, %{similarity: 0.99, trained: true})],
      lsm: [entry(:temporal_result, %{score: 0.99, status: :trained})],
      esn: [entry(:trajectory_result, %{predicted_next: 0.99, trained: true})]
    }

    %{networks: Map.merge(base, extra)}
  end

  test "a legacy trace with no actual component state is unqualified at every score" do
    assert Coordinator.derive_confidence(trace(%{})) == 0.5
    assert Coordinator.derive_confidence(%{networks: %{}}) == 0.5
  end

  test "placeholder and heuristic labels cannot alter an unqualified aggregate" do
    for placeholder <- [true, false, nil], score <- [0.0, 0.3, 0.99, 1.0] do
      extra = %{
        gnn: [entry(:graph_metrics, %{interaction_strength: score, placeholder: placeholder})],
        vae: [entry(:interpretation, %{cluster_id: 7, placeholder: placeholder})],
        sequence: [entry(:prediction, %{confidence: score, model_trained: true})]
      }

      assert Coordinator.derive_confidence(trace(extra)) == 0.5
    end
  end

  test "a placeholder-only or heuristic-only trace cannot supply learned confidence" do
    traces = [
      %{gnn: [entry(:graph_metrics, %{interaction_strength: 1.0, placeholder: false})]},
      %{vae: [entry(:interpretation, %{cluster_id: 0, placeholder: false})]},
      %{sequence: [entry(:prediction, %{confidence: 1.0, model_trained: true})]},
      %{other_network: [entry(:domain_confidence, %{confidence: 1.0, trained: true})]}
    ]

    for networks <- traces do
      confidence = Coordinator.derive_confidence(%{networks: networks})
      assert confidence == 0.5
      assert Coordinator.raw_strategy(false, confidence) == :report_only
    end
  end
end
