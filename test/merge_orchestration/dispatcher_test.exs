# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.DispatcherTest do
  use ExUnit.Case, async: true
  alias Hypatia.MergeOrchestration.Dispatcher

  defp ctx(overrides) do
    Map.merge(
      %{
        pr: %{repo: "hyperpolymath/panll", number: 412},
        change_class: :chore,
        change_level: :object,
        pool: :p2,
        attestations: [%{bot: "ci", verdict: :approve, confidence: 0.99}]
      },
      overrides
    )
  end

  test "arm_auto maps onto the existing dispatch vocabulary: auto_execute / eliminate" do
    {_d, m, _l} = Dispatcher.dispatch(ctx(%{}))
    assert m["strategy"] == "auto_execute"
    assert m["tier"] == "eliminate"
    assert m["merge"]["method"] == "squash"
    assert m["repo"] == "hyperpolymath/panll"
  end

  test "a veto -> report_only / control, and the veto rides along in the manifest" do
    {_d, m, _l} = Dispatcher.dispatch(ctx(%{attestations: [%{bot: "panicbot", verdict: :veto, rationale: "license/SPDX"}]}))
    assert m["strategy"] == "report_only"
    assert m["tier"] == "control"
    assert [%{bot: "panicbot"}] = m["merge"]["vetoes"]
  end

  test "P1 clamp -> review / substitute" do
    {_d, m, _l} = Dispatcher.dispatch(ctx(%{pool: :p1}))
    assert m["strategy"] == "review"
    assert m["tier"] == "substitute"
  end

  test "a lease is minted for the PR territory, with a TTL (LE1)" do
    {_d, _m, l} = Dispatcher.dispatch(ctx(%{branch: "dependabot/cargo/x", paths: ["Cargo.lock"]}))
    assert l["status"] == "held"
    assert l["repo"] == "hyperpolymath/panll"
    assert l["territory"]["branch"] == "dependabot/cargo/x"
    assert l["territory"]["is_meta"] == false
    assert l["expires_at"] > l["acquired_at"]
  end

  test "a meta change -> control, and the lease marks is_meta and stays owner-unauthorised (LE2)" do
    {_d, m, l} = Dispatcher.dispatch(ctx(%{change_level: :meta}))
    assert m["tier"] == "control"
    assert l["territory"]["is_meta"] == true
    assert l["owner_authorized"] == false
  end
end
