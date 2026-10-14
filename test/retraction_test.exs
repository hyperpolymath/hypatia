# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.RetractionTest do
  @moduledoc """
  HYP-01: quarantine is retraction.

  The specification is residual-evidence-types' Revision.agda
  (`survives-retraction`, `survives-dependent`), transcribed in
  `src/Hypatia/ABI/Retraction.idr`; the Idris positive control
  `tests/accept/IndependentTokenSurvives.idr` and expected rejection
  `tests/reject/RetractedSupportSurvives.idr` are the same cases run here
  through the runtime mirror.
  """

  # Quarantine is a named GenServer shared by the whole suite.
  use ExUnit.Case, async: false

  alias Hypatia.Safety.Quarantine
  alias Hypatia.Verdict
  alias Hypatia.Verdict.Retraction

  defp finding(rules, opts \\ []) do
    %{
      rule_module: "code_safety",
      type: "x",
      file: "src/a.rs",
      line: 3,
      severity: "high",
      reason: "r",
      action: "flag",
      evidence:
        Enum.map(rules, fn rule ->
          %{
            rule: rule,
            rule_version: Verdict.rule_version(),
            span: %{file: "src/a.rs", start_line: 3, end_line: 3},
            observations: 1
          }
          |> then(&if(bot = opts[:bot], do: Map.put(&1, :bot, bot), else: &1))
        end)
    }
    |> Verdict.annotate()
  end

  describe "Retraction.retract/2 (pure)" do
    test "a finding resting only on the retracted rule is retracted (RetractedSupportSurvives)" do
      f = finding(["code_safety/rule_a"])

      assert {[], [gone]} = Retraction.retract([f], [{:rule, "code_safety/rule_a"}])
      assert gone.retracted_by == ["rule:code_safety/rule_a"]
      # The original evidence is kept for audit, not erased.
      assert gone.evidence == f.evidence
    end

    test "a finding corroborated by another rule survives through it (IndependentTokenSurvives)" do
      f = finding(["code_safety/rule_a", "code_safety/rule_b"])

      assert {[kept], []} = Retraction.retract([f], [{:rule, "code_safety/rule_a"}])
      assert [%{rule: "code_safety/rule_b"}] = kept.evidence
      assert [%{rule: "code_safety/rule_a"}] = kept.retracted_evidence
      assert kept.level == "epi"
    end

    test "negative control: a finding that does not rest on the retracted rule is untouched" do
      f = finding(["code_safety/rule_b"])
      assert {[^f], []} = Retraction.retract([f], [{:rule, "code_safety/rule_a"}])
    end

    test "a token rests on its bot as well as its rule" do
      f = finding(["code_safety/rule_a"], bot: "panicbot")

      assert {[], [gone]} = Retraction.retract([f], [{:bot, "panicbot"}])
      assert gone.retracted_by == ["bot:panicbot"]

      assert {[^f], []} = Retraction.retract([f], [{:bot, "rhodibot"}])
    end

    test "a confirming re-check survives the quarantine of the rule that flagged it" do
      sound =
        finding(["code_safety/sorry"])
        |> Map.put(:support, %{
          kind: "mechanical",
          prover: "lean4",
          prover_version: "4.30.0",
          target: "t.lean",
          target_sha256: String.duplicate("a", 64),
          artefact_sha256: String.duplicate("b", 64),
          result: "confirms"
        })
        |> Verdict.annotate()

      assert sound.level == "sound"

      assert {[kept], []} = Retraction.retract([sound], [{:rule, "code_safety/sorry"}])
      assert kept.evidence == []
      assert kept.level == "sound"

      # ...but not the quarantine of its prover: then nothing supports it.
      assert {[], [gone]} =
               Retraction.retract([sound], [{:rule, "code_safety/sorry"}, {:prover, "lean4"}])

      assert Enum.sort(gone.retracted_by) == ["prover:lean4", "rule:code_safety/sorry"]

      # A retracted prover with a surviving rule leaves a pattern match, at epi.
      assert {[epi], []} = Retraction.retract([sound], [{:prover, "lean4"}])
      assert epi.support == %{kind: "pattern"}
      assert epi.level == "epi"
      assert epi.retracted_support.kind == "mechanical"
    end

    test "retracted_by is JSON-encodable" do
      assert {[], [gone]} =
               Retraction.retract([finding(["code_safety/rule_a"])], [
                 {:rule, "code_safety/rule_a"}
               ])

      assert {:ok, _} = Jason.encode(gone)
    end

    test "quarantine keys parse to the assumptions they retract" do
      assert Retraction.parse("rule:code_safety/sorry") == {:rule, "code_safety/sorry"}
      assert Retraction.parse("prover:lean4") == {:prover, "lean4"}
      assert Retraction.parse("rhodibot") == {:bot, "rhodibot"}
      assert Retraction.name({:rule, "code_safety/sorry"}) == "rule:code_safety/sorry"
    end
  end

  describe "Quarantine.retract_dependents/1" do
    setup do
      key = "rule:code_safety/retraction_test_#{System.unique_integer([:positive])}"
      on_exit(fn -> Quarantine.release(key) end)
      {:ok, key: key, rule: String.replace_prefix(key, "rule:", "")}
    end

    test "quarantining a rule retracts the findings that rested only on it", %{
      key: key,
      rule: rule
    } do
      only = finding([rule])
      both = finding([rule, "code_safety/other"])
      other = finding(["code_safety/other"])

      # Before the quarantine, nothing is retracted.
      assert {kept, []} = Quarantine.retract_dependents([only, both, other])
      assert length(kept) == 3

      Quarantine.quarantine(key, :hard, "retraction test")
      # quarantine/3 is a cast; a call orders after it.
      assert Map.has_key?(Quarantine.list_quarantined(), key)

      assert {kept, [gone]} = Quarantine.retract_dependents([only, both, other])
      assert gone.retracted_by == [key]
      assert length(kept) == 2
      assert Enum.all?(kept, &(&1.level == "epi"))

      # Release restores the support: nothing is retracted any more.
      Quarantine.release(key)
      refute Map.has_key?(Quarantine.list_quarantined(), key)
      assert {_, []} = Quarantine.retract_dependents([only, both, other])
    end

    test "a soft quarantine retracts too", %{key: key, rule: rule} do
      Quarantine.quarantine(key, :soft, "high false-positive rate")
      assert Map.has_key?(Quarantine.list_quarantined(), key)
      assert {[], [_]} = Quarantine.retract_dependents([finding([rule])])
    end
  end
end
