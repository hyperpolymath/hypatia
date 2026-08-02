# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
defmodule Hypatia.MergeOrchestration.BatonEmitter do
  @moduledoc """
  Emit armed merge decisions onto the Bag-of-Actions mesh as Batons.

  The alternative actuation backend to `merge-decisions.jsonl` (artifact-7 design,
  `docs/design/merge-orchestration/05-execution-substrate-bag-of-actions.adoc`),
  built against `bag-of-actions`' real API: `Bag.Mesh.submit_planned/2` takes a
  spec `%{check_id, command, required_cap, mutating?, risk?}`, the planner routes
  it to the cheapest capable node, **gates `mutating: true` work on a verifier**
  (that is the independent re-verification), runs the `command` there, and returns
  a `Bag.ActionResult` — with zero GitHub Actions minutes.

  A merge is just a `command` (`gh pr merge …`) carrying `required_cap:
  "secret-access"` — the capability only `mesh-github-runner` holds in the estate
  `nodes.scm` (the token-bearer). The brain's node lacks it, so the Baton cannot
  execute on the brain and must migrate to the runner: the token-free-brain
  invariant expressed as capability routing. `mutating: true` makes the planner
  gate the merge on a verifier — defence in depth, the same posture as the
  `.git-private-farm` actuator's `decide_action`.

  `to_spec/2` is pure (data in, data out). `emit/2` submits one Baton per *armed*
  entry of a `Loop` result; the `Bag.Mesh` call is late-bound (`apply/3`) and
  injectable, so this stays compile-decoupled from `bag-of-actions` and the logic
  tests run without it.
  """

  # the capability `mesh-github-runner` holds in bag-of-actions' nodes.scm — i.e.
  # "can run gh with a GitHub token". Only that node has it; the brain does not.
  #
  # The string MUST match the capability tag the bag-of-actions Zig bridge parses
  # (`estate.Capability.fromString`) and the Elixir `Bag.Planner` matches on:
  # `"secret_access"` (underscore). An unknown tag is treated as unprovable and
  # NEVER satisfied (fail-closed), so a hyphenated `"secret-access"` would route
  # to *no* node and the merge would silently suspend — verified against
  # bag-of-actions `src/main.zig` (`match` subcommand) and `bag/test/planner_test.exs`.
  @runner_cap "secret_access"

  @doc "Build a `Bag.Mesh.submit_planned` spec for one armed decision (+ its lease)."
  def to_spec(decision, lease \\ %{}) do
    repo = decision.pr.repo
    number = to_string(decision.pr.number)

    %{
      check_id: "merge-" <> String.replace(repo, "/", "__") <> "-" <> number,
      command: ["gh", "pr", "merge", number, "--repo", repo, gh_flag(decision.method)],
      required_cap: @runner_cap,
      # a merge mutates -> the planner gates it on a verifier (independent re-verify)
      mutating: true,
      # The verifier the planner REQUIRES before it will plan mutating work
      # (`Bag.Planner`: `mutating and is_nil(verifier) -> {:rejected, ...}`). It
      # names the independent re-verification that runs on the token-bearing node
      # *before* the merge — the `.git-private-farm` actuator's `decide_action`
      # (veto / meta / not-armed / ci-green / P3 / mass_squash), re-checking the
      # council's signed verdict rather than trusting the brain's say-so.
      verifier: %{by: "git-private-farm:decide_action", reverifies: decision.route},
      risk: risk(decision.pool),
      # carried for the trust proof-chain / structured residue; the planner ignores
      # extra keys, but the verdict's residue can reference the council's verdict.
      attestation: %{
        lease_id: lease["lease_id"],
        route: decision.route,
        method: to_string(decision.method),
        pool: to_string(decision.pool),
        rationale: decision.rationale
      }
    }
  end

  @doc """
  Emit a Baton per *armed* entry of a `Loop` result (`%{results: [...]}`). Only
  gate-`{:armed, lease}` entries are submitted. `opts[:submit]` defaults to the
  late-bound `Bag.Mesh.submit_planned/1`. Returns `[%{pr, spec, result}]`.
  """
  def emit(%{results: results}, opts \\ []) do
    submit = Keyword.get(opts, :submit, &default_submit/1)

    for %{gate: {:armed, lease}, decision: decision} <- results do
      spec = to_spec(decision, lease)
      %{pr: decision.pr, spec: spec, result: submit.(spec)}
    end
  end

  defp default_submit(spec), do: apply(Bag.Mesh, :submit_planned, [spec])

  defp gh_flag(:squash), do: "--squash"
  defp gh_flag(:rebase), do: "--rebase"
  defp gh_flag(:merge_commit), do: "--merge"
  defp gh_flag(_), do: "--squash"

  # the aggressive pools carry higher planner risk (more verifier scrutiny)
  defp risk(pool) when pool in [:p3, :mass_squash], do: :high
  defp risk(_), do: :low
end
