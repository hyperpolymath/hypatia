# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.Neural.GraphOfTrustConvergenceTest do
  @moduledoc """
  Regression test for the PageRank stochastic-matrix fix
  (`PROOFS-GAP.md` — "graph_of_trust normalisation").

  Before the fix:
    * `build_edges/1` emitted negative weights for failure
      (`-0.5`) and false-positive (`-1.0`) outcomes.
    * `iterate_trust/4` divided by out-degree (count), not by
      out-weight-sum, so columns were not column-stochastic.
    * Together these violated the non-negative-weight / ρ ≤ 1
      premise PageRank needs to converge to a unique fixed
      point.

  After the fix:
    * Failure and false-positive outcomes emit no edges.
    * Each source's score is split proportionally to its outgoing
      positive-weight sum.
    * All trust scores therefore stay in `[0, 1]` and the
      iteration always terminates (either via convergence or via
      the `@max_iterations` fallthrough, which also normalises).

  This test asserts those invariants against whatever `build/0`
  produces for the real `data/verisim/outcomes/` — no fixture
  injection, just black-box contract enforcement.
  """

  use ExUnit.Case, async: true

  @moduletag timeout: 120_000

  alias Hypatia.Neural.GraphOfTrust

  describe "build/0 — invariants preserved by the stochastic fix" do
    test "all trust scores are finite and within [0, 1]" do
      %GraphOfTrust{trust_scores: scores} = GraphOfTrust.build()

      Enum.each(scores, fn {id, score} ->
        assert is_number(score),
               "trust score for #{inspect(id)} is not numeric: #{inspect(score)}"

        assert score >= 0.0 and score <= 1.0,
               "trust score for #{inspect(id)} = #{score} escaped [0, 1]"

        # PageRank cannot produce NaN/∞ on a well-formed non-negative
        # transition matrix; this test pins that guarantee.
        assert score == score, "trust score for #{inspect(id)} is NaN"
      end)
    end

    test "iteration terminates (last_computed is populated)" do
      graph = GraphOfTrust.build()
      assert %DateTime{} = graph.last_computed
    end

    test "every edge has a non-negative numeric weight" do
      %GraphOfTrust{edges: edges} = GraphOfTrust.build()

      Enum.each(edges, fn {src, tgt, weight} ->
        assert is_number(weight),
               "edge #{inspect(src)} -> #{inspect(tgt)} weight not numeric: #{inspect(weight)}"

        assert weight >= 0.0,
               "edge #{inspect(src)} -> #{inspect(tgt)} weight = #{weight} is negative; PageRank requires ρ(W) ≤ 1 which bars negative edges"
      end)
    end
  end
end
