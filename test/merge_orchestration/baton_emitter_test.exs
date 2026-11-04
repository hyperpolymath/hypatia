# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.BatonEmitterTest do
  use ExUnit.Case, async: true
  alias Hypatia.MergeOrchestration.BatonEmitter

  test "even an armed, revision-bound model decision cannot manufacture a qualified executor" do
    decision = %{
      pr: %{repo: "hyperpolymath/a", number: 7},
      safety: :arm_auto,
      head_sha: String.duplicate("a", 40),
      event_id: "test:baton:1",
      method: :squash,
      pool: :p2,
      route: %{authority_bot: "ci"}
    }

    assert {:error, :qualified_merge_executor_unavailable} =
             BatonEmitter.to_spec(decision, %{"lease_id" => "L1"})

    for method <- [:rebase, :merge_commit, :unknown] do
      assert {:error, :qualified_merge_executor_unavailable} =
               BatonEmitter.to_spec(%{decision | method: method})
    end
  end

  test "descriptive verifier metadata never invokes submission or authorizes mutation" do
    decision = %{
      pr: %{repo: "hyperpolymath/a", number: 7},
      safety: :arm_auto,
      verifier: %{by: "git-private-farm:decide_action", verified: true}
    }

    results = [
      %{gate: {:armed, %{"lease_id" => "L1"}}, decision: decision},
      %{gate: :no_gate, decision: decision},
      %{gate: {:deferred, %{}}, decision: decision}
    ]

    refused =
      BatonEmitter.emit(%{results: results}, submit: fn _ -> flunk("mutation submitted") end)

    assert [
             %{
               pr: %{number: 7},
               spec: nil,
               result: {:error, :qualified_merge_executor_unavailable}
             }
           ] = refused
  end
end
