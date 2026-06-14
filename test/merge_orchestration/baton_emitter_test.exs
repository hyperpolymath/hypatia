# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.BatonEmitterTest do
  use ExUnit.Case, async: true
  alias Hypatia.MergeOrchestration.BatonEmitter

  defp dec(o) do
    Map.merge(
      %{
        pr: %{repo: "hyperpolymath/a", number: 7},
        method: :squash,
        pool: :p2,
        route: %{authority_bot: "robot-repo-automaton", contributing_bots: ["ci"]},
        rationale: "chore/object -> arm_auto"
      },
      o
    )
  end

  test "to_spec builds a submit_planned spec routed to the token-bearing node, gated as mutating" do
    s = BatonEmitter.to_spec(dec(%{}), %{"lease_id" => "L1"})

    assert s.check_id == "merge-hyperpolymath__a-7"
    assert s.command == ["gh", "pr", "merge", "7", "--repo", "hyperpolymath/a", "--squash"]
    # routes to the only node holding the token capability; the brain lacks it.
    # underscore form — the bag-of-actions Zig bridge / Bag.Planner tag (a
    # hyphenated tag is unprovable and would route to no node).
    assert s.required_cap == "secret_access"
    # a merge mutates -> the planner gates it on a verifier (independent re-verify);
    # the spec MUST carry one or Bag.Planner returns {:rejected, :mutation_requires_verifier}
    assert s.mutating == true
    assert s.verifier.by == "git-private-farm:decide_action"
    assert s.verifier.reverifies == dec(%{}).route
    assert s.risk == :low
    assert s.attestation.lease_id == "L1"
    assert s.attestation.rationale =~ "arm_auto"
  end

  test "method maps to the gh flag; aggressive pools raise the planner risk" do
    assert BatonEmitter.to_spec(dec(%{method: :rebase})).command |> List.last() == "--rebase"
    assert BatonEmitter.to_spec(dec(%{method: :merge_commit})).command |> List.last() == "--merge"
    assert BatonEmitter.to_spec(dec(%{pool: :p3})).risk == :high
    assert BatonEmitter.to_spec(dec(%{pool: :mass_squash})).risk == :high
  end

  test "emit submits one Baton per ARMED entry only; the Bag.Mesh call is injected" do
    results = [
      %{gate: {:armed, %{"lease_id" => "L1"}}, decision: dec(%{}), entry: %{}},
      %{gate: :no_gate, decision: dec(%{pool: :p1}), entry: %{}},
      %{gate: {:deferred, %{"holder" => "x"}}, decision: dec(%{}), entry: %{}}
    ]

    submitted =
      BatonEmitter.emit(%{results: results},
        submit: fn spec -> {:pass, "mesh-github-runner", spec.check_id} end
      )

    assert length(submitted) == 1
    assert hd(submitted).spec.required_cap == "secret_access"
    assert {:pass, "mesh-github-runner", "merge-hyperpolymath__a-7"} = hd(submitted).result
  end
end
