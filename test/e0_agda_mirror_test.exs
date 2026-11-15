# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Mirror test for the Agda model of the E0 qualification gate
# (verification/proofs/agda/E0Model.agda).
#
# The Agda files PROVE properties of a model. This test ties the model to the
# code: it runs the real Elixir functions over a finite table of inputs,
# renders what they answered as Agda (E0ModelTables.agda), and fails when the
# committed file differs. The Agda checker then checks that the model gives
# the same answers on every row. So a change of the Elixir gate breaks this
# test; regenerating the file then breaks the Agda check unless the model was
# changed with it. Agreement on a finite table is TESTED, not proved: the
# theorems are about the model.
#
# Regenerate after an intended change:
#
#     E0_MIRROR_WRITE=1 mix test test/e0_agda_mirror_test.exs
#
defmodule Hypatia.E0AgdaMirrorTest do
  use ExUnit.Case, async: true

  alias Hypatia.ConfidenceAnnealing
  alias Hypatia.LearningScheduler
  alias Hypatia.OutcomeTracker

  @table_path "verification/proofs/agda/E0ModelTables.agda"

  # ── The gate: OutcomeTracker.qualified_outcome_counts/1 ────────────────

  @outcomes [
    {"success", "success"},
    {"failure", "failure"},
    {"false_positive", "falsePositive"},
    {"weird", "otherOutcome"}
  ]

  @verifications [
    {"verified", "verified"},
    {"failed", "failed"},
    {"incomplete", "incomplete"},
    {"still_present", "stillPresent"},
    {"rollback_requested", "rollbackRequested"},
    {"unverified", "unverified"},
    {"no_op", "noOp"},
    {"unqualified", "unqualified"},
    {"scan_failed", "scanFailed"},
    {"scan_skipped", "scanSkipped"},
    {"weird", "otherVerification"},
    {nil, "missingVerification"}
  ]

  # The batch-rollback clause needs caused_by == "batch_rollback" and a
  # non-empty string batch_id. All 9 combinations only where it matters.
  @batch_all for c <- ["batch_rollback", "other", nil], b <- ["b-1", "", nil], do: {c, b}
  @batch_two [{"batch_rollback", "b-1"}, {nil, nil}]

  defp gate_rows do
    for {out, out_a} <- @outcomes,
        {ver, ver_a} <- @verifications,
        {caused, batch} <- if(ver == "rollback_requested", do: @batch_all, else: @batch_two) do
      row =
        %{"outcome" => out}
        |> put_unless_nil("verification", ver)
        |> put_unless_nil("caused_by", caused)
        |> put_unless_nil("batch_id", batch)

      batch_ok = caused == "batch_rollback" and is_binary(batch) and batch != ""
      counts = OutcomeTracker.qualified_outcome_counts([row])

      "(row #{out_a} #{ver_a} #{batch_ok} , #{bucket(counts)})"
    end
  end

  defp bucket(%{successes: 1, failures: 0, false_positives: 0, ignored: 0}), do: "creditSuccess"
  defp bucket(%{successes: 0, failures: 1, false_positives: 0, ignored: 0}), do: "creditFailure"

  defp bucket(%{successes: 0, failures: 0, false_positives: 1, ignored: 0}),
    do: "creditFalsePositive"

  defp bucket(%{successes: 0, failures: 0, false_positives: 0, ignored: 1}), do: "ignored"

  # ── The fleet classification: LearningScheduler.classify_fleet_record/1 ─

  @verdicts [
    {"verified", "vVerified"},
    {"failed", "vFailed"},
    {"incomplete", "vIncomplete"},
    {"no_op", "vNoOp"},
    {"unverified", "vUnverified"},
    {nil, "vNone"},
    {"weird", "vOther"}
  ]

  @fleet_outcomes [{"success", "fSuccess"}, {"failure", "fFailure"}, {"weird", "fOther"}]

  # {Agda constructor, JSON value or :absent}, for each integer field.
  @exit_codes [{"posInt", 1}, {"zeroInt", 0}, {"absent", :absent}]
  @befores [{"posInt", 2}, {"negInt", -1}, {"zeroInt", 0}, {"absent", :absent}]
  @afters [{"posInt", 3}, {"zeroInt", 0}, {"zeroFloat", 0.0}, {"absent", :absent}]

  defp fleet_record(bot, recipe, repo, verdict, outcome, exit_code, before_hits, after_hits) do
    %{"recipe_id" => recipe, "repo" => repo, "file" => "a.txt", "outcome" => outcome}
    |> put_unless_nil("bot", bot)
    |> put_unless_nil("verdict", verdict)
    |> put_hit("exit_code", exit_code)
    |> put_hit("before_hits", before_hits)
    |> put_hit("after_hits", after_hits)
  end

  defp put_hit(map, _key, :absent), do: map
  defp put_hit(map, key, value), do: Map.put(map, key, value)

  defp fleet_result(:skip), do: "skipRow"

  defp fleet_result({:ok, _recipe, _repo, _file, outcome, verification}),
    do: "qualifiedRow #{outcome_a(outcome)} #{verification_a(verification)}"

  defp fleet_result({:audit, _recipe, _repo, _file, outcome, verification}),
    do: "auditRow #{outcome_a(outcome)} #{verification_a(verification)}"

  defp outcome_a(:success), do: "success"
  defp outcome_a(:failure), do: "failure"
  defp outcome_a(:unknown), do: "otherOutcome"

  defp verification_a(v) do
    {_, a} = List.keyfind(@verifications, v, 0)
    a
  end

  defp fleet_rows do
    ok =
      for {vd, vd_a} <- @verdicts,
          {fo, fo_a} <- @fleet_outcomes,
          {e_a, e} <- @exit_codes,
          {b_a, b} <- @befores,
          {a_a, a} <- @afters do
        record = fleet_record("fix-bot", "recipe-a", "owner/repo", vd, fo, e, b, a)
        entry("cOk", vd_a, fo_a, e_a, b_a, a_a, LearningScheduler.classify_fleet_record(record))
      end

    # A row that Hypatia wrote itself is skipped, whatever it says.
    hypatia =
      for {vd, vd_a} <- @verdicts, {fo, fo_a} <- @fleet_outcomes do
        record = fleet_record("hypatia", "recipe-a", "owner/repo", vd, fo, 1, 2, 0)

        entry(
          "cHypatia",
          vd_a,
          fo_a,
          "posInt",
          "posInt",
          "zeroInt",
          LearningScheduler.classify_fleet_record(record)
        )
      end

    # No recipe, or no repo (missing or empty): skipped.
    invalid =
      for {recipe, repo} <- [
            {nil, "owner/repo"},
            {"", "owner/repo"},
            {"recipe-a", nil},
            {"recipe-a", ""}
          ] do
        record = fleet_record("fix-bot", recipe, repo, "verified", "success", 1, 2, 0)

        entry(
          "cInvalid",
          "vVerified",
          "fSuccess",
          "posInt",
          "posInt",
          "zeroInt",
          LearningScheduler.classify_fleet_record(record)
        )
      end

    ok ++ hypatia ++ invalid
  end

  defp entry(ctx, vd, fo, e, b, a, result) do
    "((#{ctx} , #{vd} , #{fo} , #{e} , #{b} , #{a}) , #{fleet_result(result)})"
  end

  # ── Stage and tier: ConfidenceAnnealing ─────────────────────────────────

  defp stage_rows do
    for n <- 0..120 do
      state = ConfidenceAnnealing.from_existing(n, 0, 0)
      "(#{n} , #{stage_a(state.stage)} , #{tier_a(ConfidenceAnnealing.max_dispatch_tier(state))})"
    end
  end

  defp stage_a(:nascent), do: "nascent"
  defp stage_a(:adolescent), do: "adolescent"
  defp stage_a(:mature), do: "mature"
  defp stage_a(:veteran), do: "veteran"

  defp tier_a(:report_only), do: "reportOnly"
  defp tier_a(:review), do: "review"
  defp tier_a(:auto_execute), do: "autoExecute"

  # ── Rendering ───────────────────────────────────────────────────────────

  defp put_unless_nil(map, _key, nil), do: map
  defp put_unless_nil(map, key, value), do: Map.put(map, key, value)

  defp list(name, type, [first | rest]) do
    body = Enum.map_join(rest, "", &"\n  ∷ #{&1}")
    "#{name} : #{type}\n#{name} =\n    #{first}#{body}\n  ∷ []\n"
  end

  @doc false
  def render do
    gate = gate_rows()
    fleet = fleet_rows()
    stage = stage_rows()

    """
    {-# OPTIONS --safe --without-K --cubical-compatible --double-check #-}
    -- SPDX-License-Identifier: MPL-2.0
    -- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
    --
    -- GENERATED by test/e0_agda_mirror_test.exs. Do not edit by hand.
    -- Regenerate: E0_MIRROR_WRITE=1 mix test test/e0_agda_mirror_test.exs
    --
    -- What the running Elixir answered for a finite table of inputs, and the
    -- proof that the model of E0Model.agda gives the same answer on every row:
    --   #{length(gate)} rows of OutcomeTracker.qualified_outcome_counts/1,
    --   #{length(fleet)} rows of LearningScheduler.classify_fleet_record/1,
    --   #{length(stage)} rows of ConfidenceAnnealing stage and dispatch tier (0..120).
    -- This is agreement on a finite table (tested), not a refinement proof.
    module E0ModelTables where

    open import Agda.Builtin.Bool
    open import Agda.Builtin.Nat
    open import Agda.Builtin.List
    open import Agda.Builtin.Equality
    open import E0Model
    open import E0Agreement

    #{list("gateTable", "GateTable", gate)}
    gateAgrees : GateAgrees gateTable
    gateAgrees = checkGate-sound gateTable refl

    #{list("fleetTable", "FleetTable", fleet)}
    fleetAgrees : FleetAgrees fleetTable
    fleetAgrees = checkFleet-sound fleetTable refl

    #{list("stageTable", "StageTable", stage)}
    stageAgrees : StageAgrees stageTable
    stageAgrees = checkStage-sound stageTable refl
    """
  end

  test "the committed Agda tables are what the running Elixir answers" do
    generated = render()

    if System.get_env("E0_MIRROR_WRITE") == "1" do
      File.write!(@table_path, generated)
    end

    assert File.read!(@table_path) == generated,
           "#{@table_path} is stale: the Elixir gate answers differently from the committed " <>
             "table. Regenerate with E0_MIRROR_WRITE=1 mix test test/e0_agda_mirror_test.exs, " <>
             "then check the model with agda (see verification/README.adoc)."
  end

  test "the table covers every bucket, every fleet result and every stage" do
    generated = render()

    for bucket <- ~w(creditSuccess creditFailure creditFalsePositive ignored) do
      assert generated =~ ", #{bucket})", "no gate row answers #{bucket}"
    end

    for result <- [
          "qualifiedRow success verified",
          "qualifiedRow failure failed",
          "qualifiedRow failure incomplete",
          "auditRow",
          "skipRow"
        ] do
      assert generated =~ result, "no fleet row answers #{result}"
    end

    for stage <- ~w(nascent adolescent mature veteran) do
      assert generated =~ ", #{stage} ,", "no stage row answers #{stage}"
    end
  end
end
