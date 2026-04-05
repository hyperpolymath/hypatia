# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.Rules.ProofStrategySelectionTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.ProofStrategySelection, as: PS

  describe "classify_obligation/1" do
    test "linearity-family keywords" do
      assert PS.classify_obligation("Substitution preserves linearity") == "linearity"
      assert PS.classify_obligation("affine type stays unique") == "linearity"
      assert PS.classify_obligation("Ownership chain is intact") == "linearity"
      assert PS.classify_obligation("Borrow does not escape scope") == "linearity"
    end

    test "termination-family keywords" do
      assert PS.classify_obligation("Function terminates") == "termination"
      assert PS.classify_obligation("halting on all inputs") == "termination"
      assert PS.classify_obligation("Totality check passes") == "termination"
      assert PS.classify_obligation("structural recursion is well-founded") == "termination"
    end

    test "equiv-family keywords" do
      assert PS.classify_obligation("The rewrite is equivalent") == "equiv"
      assert PS.classify_obligation("bisimulation holds") == "equiv"
      assert PS.classify_obligation("Substitution preserves behavior") == "equiv"
      assert PS.classify_obligation("Substitution preserves behaviour") == "equiv"
    end

    test "safety-family keywords" do
      assert PS.classify_obligation("invariant I holds at all states") == "safety"
      assert PS.classify_obligation("Memory-safe under concurrent access") == "safety"
      assert PS.classify_obligation("non-interference across channels") == "safety"
    end

    test "unknown falls through" do
      assert PS.classify_obligation("prove this please") == "unknown"
      assert PS.classify_obligation("") == "unknown"
    end
  end

  describe "explain_strategy/1" do
    test "empty returns no-data message" do
      assert PS.explain_strategy([]) == "no historical data available"
    end

    test "single recommendation" do
      recs = [
        %{"prover" => "coq", "success_rate" => 0.95, "total_attempts" => 20}
      ]

      result = PS.explain_strategy(recs)
      assert result =~ "recommend coq"
      assert result =~ "95.0%"
      assert result =~ "20 attempts"
      refute result =~ "fallbacks"
    end

    test "multiple recommendations show top + fallbacks" do
      recs = [
        %{"prover" => "coq", "success_rate" => 0.95, "total_attempts" => 20},
        %{"prover" => "lean", "success_rate" => 0.78, "total_attempts" => 15},
        %{"prover" => "z3", "success_rate" => 0.42, "total_attempts" => 8}
      ]

      result = PS.explain_strategy(recs)
      assert result =~ "recommend coq"
      assert result =~ "fallbacks: lean:78.0%, z3:42.0%"
    end

    test "fourth recommendation is truncated" do
      recs = [
        %{"prover" => "coq", "success_rate" => 0.9, "total_attempts" => 10},
        %{"prover" => "lean", "success_rate" => 0.8, "total_attempts" => 10},
        %{"prover" => "z3", "success_rate" => 0.7, "total_attempts" => 10},
        %{"prover" => "agda", "success_rate" => 0.6, "total_attempts" => 10}
      ]

      result = PS.explain_strategy(recs)
      refute result =~ "agda"
    end
  end

  describe "recommend/2" do
    test "returns transport error when VeriSimDB is unreachable" do
      # Point at a port nothing is listening on
      result = PS.recommend("linearity", base_url: "http://127.0.0.1:1", timeout: 500)
      assert {:error, {:transport, _reason}} = result
    end
  end
end
