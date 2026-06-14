# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.KinCouncilTest do
  use ExUnit.Case, async: true
  alias Hypatia.MergeOrchestration.KinCouncil

  test "uniform weights: confidence is the plain mean of approvals" do
    atts = [%{bot: "a", verdict: :approve, confidence: 1.0}, %{bot: "b", verdict: :approve, confidence: 0.0}]
    assert KinCouncil.aggregate(atts).confidence == 0.5
  end

  test "competence weighting: the more-competent voice dominates" do
    atts = [%{bot: "a", verdict: :approve, confidence: 1.0}, %{bot: "b", verdict: :approve, confidence: 0.0}]
    w = fn %{bot: "a"} -> 3.0; _ -> 1.0 end
    assert KinCouncil.aggregate(atts, weight: w).confidence == 0.75
  end

  test "monotone veto: any veto is surfaced regardless of approvals" do
    atts = [
      %{bot: "ci", verdict: :approve, confidence: 0.99},
      %{bot: "echidnabot", verdict: :veto, rationale: "proof drift"}
    ]

    r = KinCouncil.aggregate(atts)
    assert r.has_veto
    assert [%{bot: "echidnabot", reason: "proof drift"}] = r.vetoes
  end

  test "incompetent experts recuse (weight 0), not out-voted" do
    atts = [%{bot: "expert", verdict: :approve, confidence: 0.9}, %{bot: "noise", verdict: :approve, confidence: 0.1}]
    w = fn %{bot: "noise"} -> 0.0; _ -> 1.0 end
    assert KinCouncil.aggregate(atts, weight: w).confidence == 0.9
  end

  test "no approvals -> zero confidence (holds and empties don't count)" do
    assert KinCouncil.aggregate([]).confidence == 0.0
    assert KinCouncil.aggregate([%{bot: "x", verdict: :hold, confidence: 0.5}]).confidence == 0.0
  end

  test "competence_weight composes trust x competence" do
    assert KinCouncil.competence_weight(0.8, 0.5) == 0.4
  end
end
