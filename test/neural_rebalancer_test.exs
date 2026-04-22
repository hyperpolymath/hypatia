# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.Neural.RebalancerTest do
  use ExUnit.Case, async: true

  alias Hypatia.Neural.Rebalancer

  describe "augment_esn_series/2" do
    test "preserves length" do
      series = for _ <- 1..100, do: 0.99

      assert length(Rebalancer.augment_esn_series(series)) == 100
    end

    test "values stay in [0, 1]" do
      series = for _ <- 1..200, do: 0.95

      augmented = Rebalancer.augment_esn_series(series, rate: 0.5, dip_depth: 1.0, seed: 7)

      Enum.each(augmented, fn v ->
        assert v >= 0.0 and v <= 1.0,
               "augmented value #{v} outside [0, 1]"
      end)
    end

    test "injects variance — variance strictly greater than flat input" do
      flat = for _ <- 1..500, do: 0.99

      augmented = Rebalancer.augment_esn_series(flat, rate: 0.10, dip_depth: 0.5, seed: 13)

      flat_variance = variance(flat)
      aug_variance = variance(augmented)

      assert aug_variance > flat_variance,
             "rebalanced variance (#{aug_variance}) should exceed flat (#{flat_variance})"
    end

    test "deterministic with fixed seed" do
      series = for _ <- 1..50, do: 0.99

      a = Rebalancer.augment_esn_series(series, seed: 99)
      b = Rebalancer.augment_esn_series(series, seed: 99)

      assert a == b
    end

    test "different seeds produce different outputs" do
      series = for _ <- 1..50, do: 0.99

      a = Rebalancer.augment_esn_series(series, rate: 0.5, seed: 1)
      b = Rebalancer.augment_esn_series(series, rate: 0.5, seed: 999)

      # Over 50 points at rate 0.5, the two runs will differ with high probability.
      assert a != b
    end

    test "rate 0.0 leaves the series untouched" do
      series = for _ <- 1..50, do: 0.9

      assert Rebalancer.augment_esn_series(series, rate: 0.0) == series
    end
  end

  describe "synthetic_rbf_examples/1" do
    test "produces the requested number of examples" do
      {vectors, targets} = Rebalancer.synthetic_rbf_examples(count: 20, seed: 3)

      assert length(vectors) == 20
      assert length(targets) == 20
    end

    test "vectors are 8-dimensional with values in [0, 1]" do
      {vectors, _} = Rebalancer.synthetic_rbf_examples(count: 25, seed: 5)

      Enum.each(vectors, fn v ->
        assert length(v) == 8

        Enum.each(v, fn x ->
          assert x >= 0.0 and x <= 1.0
        end)
      end)
    end

    test "targets are in the canonical set {0.0, 0.5, 1.0}" do
      {_, targets} = Rebalancer.synthetic_rbf_examples(count: 40, seed: 11)

      Enum.each(targets, fn t ->
        assert t in [0.0, 0.5, 1.0]
      end)
    end

    test "deterministic with fixed seed" do
      a = Rebalancer.synthetic_rbf_examples(count: 10, seed: 7)
      b = Rebalancer.synthetic_rbf_examples(count: 10, seed: 7)
      assert a == b
    end
  end

  describe "rebalance_rbf/2" do
    test "appends synthetic to existing training set" do
      real_vectors = [
        [0.8, 0.6, 0.4, 0.3, 0.2, 0.5, 0.5, 0.5],
        [0.4, 0.4, 0.5, 0.6, 0.7, 0.5, 0.5, 0.5]
      ]

      real_targets = [1.0, 0.5]

      {vectors, targets} =
        Rebalancer.rebalance_rbf({real_vectors, real_targets}, count: 5, seed: 2)

      assert length(vectors) == 2 + 5
      assert length(targets) == 2 + 5

      # Originals preserved at the head.
      assert Enum.take(vectors, 2) == real_vectors
      assert Enum.take(targets, 2) == real_targets
    end

    test "empty real set still returns the synthetic examples" do
      {vectors, targets} = Rebalancer.rebalance_rbf({[], []}, count: 4, seed: 17)

      assert length(vectors) == 4
      assert length(targets) == 4
    end
  end

  # ── helpers ──────────────────────────────────────────────────────────────

  defp variance(xs) do
    n = length(xs)
    mean = Enum.sum(xs) / n
    Enum.reduce(xs, 0.0, fn x, acc -> acc + (x - mean) * (x - mean) end) / n
  end
end
