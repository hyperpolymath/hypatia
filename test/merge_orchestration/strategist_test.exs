# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.StrategistTest do
  use ExUnit.Case, async: true
  alias Hypatia.MergeOrchestration.Strategist

  defp ctx(overrides) do
    Map.merge(
      %{
        pr: %{repo: "hyperpolymath/x", number: 1},
        change_class: :chore,
        change_level: :object,
        pool: :p2,
        attestations: [%{bot: "ci", verdict: :approve, confidence: 0.99}]
      },
      overrides
    )
  end

  test "green chore in P2 -> squash + arm_auto, unclamped" do
    d = Strategist.decide(ctx(%{}))
    assert d.method == :squash
    assert d.safety == :arm_auto
    assert d.clamped_by == nil
  end

  test "monotone veto: a veto attestation -> flag, even in aggressive P3" do
    d =
      Strategist.decide(
        ctx(%{
          pool: :p3,
          attestations: [
            %{bot: "ci", verdict: :approve, confidence: 0.99},
            %{bot: "panicbot", verdict: :veto, rationale: "reachable unmitigable CVE"}
          ]
        })
      )

    assert d.safety == :flag
    assert d.clamped_by =~ "veto:"
  end

  test "object/meta reflexivity guard: a meta change -> flag" do
    d = Strategist.decide(ctx(%{change_level: :meta, pool: :p3}))
    assert d.safety == :flag
    assert d.clamped_by == "meta"
  end

  test "pool clamp is monotone: P1 caps a green change at review" do
    d = Strategist.decide(ctx(%{pool: :p1}))
    assert d.safety == :review
    assert d.clamped_by == "pool-cap:p1"
  end

  test "pool clamp: P0 caps everything at flag" do
    assert Strategist.decide(ctx(%{pool: :p0})).safety == :flag
  end

  test "method is set by class, not confidence: a low-confidence chore still squashes" do
    d = Strategist.decide(ctx(%{attestations: [%{bot: "ci", verdict: :approve, confidence: 0.10}]}))
    assert d.method == :squash
    assert d.safety in [:review, :flag]
  end

  test "competence weighting flows through: a zero-weight dissenter doesn't sink a trusted approval" do
    d =
      Strategist.decide(
        ctx(%{
          attestations: [
            %{bot: "rhodibot", verdict: :approve, confidence: 1.0},
            %{bot: "noise", verdict: :approve, confidence: 0.0}
          ],
          weight: fn
            %{bot: "noise"} -> 0.0
            _ -> 1.0
          end
        })
      )

    assert d.confidence == 1.0
    assert d.safety == :arm_auto
  end

  test "route: bump -> patch-bridge; proof -> echidnabot + merge_commit" do
    assert Strategist.decide(ctx(%{change_class: :bump})).route.authority_bot == "patch-bridge"
    proof = Strategist.decide(ctx(%{change_class: :proof}))
    assert proof.route.authority_bot == "echidnabot"
    assert proof.method == :merge_commit
  end

  test "license touch -> no auto-authority and a hard (symbolic) flag" do
    d = Strategist.decide(ctx(%{license_touch: true}))
    assert d.route.authority_bot == nil
    assert d.safety == :flag
    assert d.clamped_by =~ "veto:"
  end
end
