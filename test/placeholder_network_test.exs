# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.PlaceholderNetworkTest do
  @moduledoc """
  E0-23: a network that returns a fixed placeholder adds nothing to the derived
  confidence, and its result says that it is a placeholder (design of 2026-11-14 on
  merge honesty, decision 4). The autoencoder encodes nothing yet: its latent vector is
  all zeros and its cluster is always 0, and the coordinator used to weigh that at 0.05.
  The mark on the result is `placeholder: true`; the end-to-end check is in
  `neural_source_store_lifecycle_test.exs`.
  """
  use ExUnit.Case, async: true

  alias Hypatia.Neural.Coordinator

  # A reasoning trace, in the shape the coordinator reads: networks -> entries.
  defp entry(key, value), do: %{key: key, value: value}

  defp trace(extra) do
    base = %{
      moe: [entry(:domain_confidence, %{confidence: 0.80})],
      rbf: [entry(:novelty_result, %{similarity: 0.70})],
      lsm: [entry(:temporal_result, %{score: 0.60})],
      esn: [entry(:trajectory_result, %{predicted_next: 0.65})],
      gnn: [entry(:graph_metrics, %{interaction_strength: 0.5})],
      sequence: [entry(:prediction, %{confidence: 0.3})]
    }

    %{networks: Map.merge(base, extra)}
  end

  defp vae(fields),
    do: %{vae: [entry(:interpretation, Map.merge(%{cluster_id: 0, latent: [0.0]}, fields))]}

  test "a placeholder autoencoder changes nothing in the derived confidence of 1 finding" do
    without = Coordinator.derive_confidence(trace(%{}))
    placeholder = Coordinator.derive_confidence(trace(vae(%{placeholder: true})))

    assert placeholder == without
  end

  test "the same holds whatever the placeholder's cluster would have been worth" do
    without = Coordinator.derive_confidence(trace(%{}))

    for cluster_id <- [0, 1, 7, nil] do
      assert Coordinator.derive_confidence(
               trace(vae(%{placeholder: true, cluster_id: cluster_id}))
             ) == without
    end
  end

  test "an autoencoder that is not marked as a placeholder still counts, at its weight" do
    without = Coordinator.derive_confidence(trace(%{}))
    counted = Coordinator.derive_confidence(trace(vae(%{placeholder: false})))

    assert counted != without

    # cluster 0 is worth 0.6 at weight 0.05; the others weigh 0.35 + 0.20 + 0.15 + 0.15 + 0.05 + 0.05.
    expected =
      (0.80 * 0.35 + 0.70 * 0.20 + 0.60 * 0.15 + 0.65 * 0.15 + 0.5 * 0.05 + 0.3 * 0.05 +
         0.6 * 0.05) /
        1.0

    assert_in_delta counted, expected, 1.0e-9
  end

  test "an autoencoder result with no mark is not mistaken for a placeholder" do
    assert Coordinator.derive_confidence(trace(vae(%{}))) !=
             Coordinator.derive_confidence(trace(%{}))
  end
end
