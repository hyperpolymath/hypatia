# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.KinCompetenceTest do
  use ExUnit.Case, async: true
  alias Hypatia.MergeOrchestration.{KinCompetence, KinCouncil, Strategist}

  # a minimal struct exposing trust_score/2, exactly the shape trust_from_got reads
  defmodule FakeGoT do
    defstruct scores: %{}
    def trust_score(%__MODULE__{scores: s}, bot), do: Map.get(s, bot, 0.0)
  end

  # ── the weight function ──────────────────────────────────────────────────────

  test "the specialist counts in full; a generalist is discounted; an unknown bot is neutral-trust" do
    w =
      KinCompetence.weight_fn(
        trust: %{"echidnabot" => 0.9, "rhodibot" => 0.8},
        authority: "echidnabot"
      )

    assert w.(%{bot: "echidnabot"}) == 0.9 * 1.0
    assert w.(%{bot: "rhodibot"}) == 0.8 * 0.5
    assert w.(%{bot: "ci"}) == 1.0 * 0.5
  end

  test "a zero-trust bot gets weight 0.0 (recuse)" do
    w = KinCompetence.weight_fn(trust: %{"spammy" => 0.0}, authority: "echidnabot")
    assert w.(%{bot: "spammy"}) == 0.0
  end

  # ── feeding KinCouncil (the point of the exercise) ───────────────────────────

  test "the council confidence is the competence-weighted mean of approvals" do
    w =
      KinCompetence.weight_fn(trust: %{"echidnabot" => 0.9, "ci" => 1.0}, authority: "echidnabot")

    council =
      KinCouncil.aggregate(
        [
          %{bot: "echidnabot", verdict: :approve, confidence: 0.6},
          %{bot: "ci", verdict: :approve, confidence: 1.0}
        ],
        weight: w
      )

    # (0.9*1.0*0.6 + 1.0*0.5*1.0) / (0.9 + 0.5) = 1.04 / 1.4
    assert_in_delta council.confidence, 1.04 / 1.4, 1.0e-9
  end

  # ── end-to-end through the Strategist ────────────────────────────────────────

  test "attach_weight demotes an approval that comes only from a zero-trust bot to :flag" do
    ctx = %{
      pr: %{repo: "hyperpolymath/panll", number: 7},
      change_class: :proof,
      change_level: :object,
      pool: :p2,
      attestations: [%{bot: "spammy", verdict: :approve, confidence: 0.99}]
    }

    # uniform council would arm on the 0.99
    assert Strategist.decide(ctx).confidence == 0.99

    # competence-aware: spammy has zero trust → recuses → no approvals → flag
    weighted = KinCompetence.attach_weight(ctx, %{"spammy" => 0.0})
    decision = Strategist.decide(weighted)
    assert decision.confidence == 0.0
    assert decision.safety == :flag
  end

  test "attach_weight derives the specialist from the change-class via route_authority" do
    ctx = %{
      pr: %{repo: "r", number: 1},
      change_class: :security,
      change_level: :object,
      pool: :p2,
      attestations: [%{bot: "panicbot", verdict: :approve, confidence: 0.99}]
    }

    weighted = KinCompetence.attach_weight(ctx, %{"panicbot" => 0.95})
    # panicbot is the security authority ⇒ full competence ⇒ confidence == the lone approval
    assert_in_delta Strategist.decide(weighted).confidence, 0.99, 1.0e-9
  end

  # ── the GoT snapshot glue ────────────────────────────────────────────────────

  test "trust_from_got snapshots scores per bot from a trust_score/2 struct" do
    got = %FakeGoT{scores: %{"echidnabot" => 0.9, "rhodibot" => 0.7}}

    assert KinCompetence.trust_from_got(got, ["echidnabot", "rhodibot", "unseen"]) ==
             %{"echidnabot" => 0.9, "rhodibot" => 0.7, "unseen" => 0.0}
  end
end
