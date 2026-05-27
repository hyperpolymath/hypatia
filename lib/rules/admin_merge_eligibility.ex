# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.AdminMergeEligibility do
  @moduledoc """
  Recognises PR shapes that are safe to admin-merge in bulk when the
  GitHub Actions budget has been exhausted and CI cannot complete.

  Derived from the 2026-05-27 estate sweep that admin-merged ~1213 PRs at
  98.7% success against a 1254-PR backlog. The categories below were
  empirically validated as "the merge IS the fix" — the PR exists to
  replace a heavy workflow with a thin wrapper, seed a doc, or apply a
  template-level chore, so blocking on CI provides no signal.

  Rule IDs AM001-AM009.

  Intended consumers:

    * `sustainabot` / `.git-private-farm` budget-resume sweep — call
      `eligible?/1` on each open PR to decide whether to admin-merge.
    * Hypatia self-scan reporting — surfaces "X PRs stuck on CI quota
      could be cleared by a budget-resume sweep" when the estate count
      exceeds 50 (DBA001 stall threshold).
    * Future tooling that wants the same recognition without rebuilding
      the title-shape regexes by hand.

  ## Safety envelope

  Every shape below is one of:

    1. **Workflow-replacement wrapper**: deletes a heavy per-repo
       workflow YAML and replaces it with a thin `uses:` wrapper that
       calls a SHA-pinned reusable. The merge IS the fix because the
       old workflow can't even run without the budget the user is
       trying to conserve.
    2. **Pure additive doc**: seeds a missing CHANGELOG, README, or
       docs-template skeleton. Zero risk to runtime behaviour.
    3. **Bounded chore**: license header migration, `.gitattributes`
       linguist exclusion, SPDX tag — purely metadata, no code path.

  Anything with code-path semantics, new abstractions, cross-cutting
  refactors, or proof work is explicitly out of scope and falls through
  to `:requires_review`.
  """

  # ─── Title-shape recognisers (AM001-AM009) ────────────────────────────

  @workflow_wrapper_patterns [
    {~r/^chore\(ci\): replace secret-scanner\.yml with reusable wrapper/, "AM001"},
    {~r/^chore\(ci\): replace scorecard\.yml with reusable wrapper/, "AM002"},
    {~r/^chore\(ci\): replace codeql\.yml with reusable wrapper/, "AM003"},
    {~r/^chore\(ci\): replace mirror\.yml with reusable wrapper/, "AM004"},
    {~r/^ci\(hypatia-scan\): repin reusable to merge-commit SHA/, "AM005"},
    {~r/^chore\(ci\): replace rust-ci\.yml with reusable wrapper/, "AM006"},
    {~r/^chore\(ci\): replace elixir-ci\.yml with reusable wrapper/, "AM006"}
  ]

  @additive_doc_patterns [
    {~r/^docs: seed CHANGELOG\.md \(Keep-a-Changelog format\)/, "AM007"},
    {~r/^docs: adopt docs-template\/ skeleton/, "AM008"},
    {~r/^docs\(readme\): expand stub README/, "AM008"},
    {~r/^docs: record tech-debt audit findings/, "AM008"}
  ]

  @bounded_chore_patterns [
    {~r/^chore\(linguist\): exclude /, "AM009"},
    {~r/^license\/migrate-to-mpl-2\.0|^chore: migrate.*MPL-2\.0/, "AM009"}
  ]

  @all_patterns @workflow_wrapper_patterns ++ @additive_doc_patterns ++ @bounded_chore_patterns

  @doc """
  Classify a PR title. Returns `{:eligible, rule_id}` for known safe
  shapes, `:requires_review` otherwise.

  This is a TITLE-only check by design — the runtime sweep needs to be
  fast (1k+ PRs at a time), and the canonical title shape is what the
  fan-out tooling (`standards`, dependabot, the `gh search` query) emits.
  Anything ambiguous falls through to require human review.
  """
  @spec classify(String.t()) :: {:eligible, String.t()} | :requires_review
  def classify(title) when is_binary(title) do
    case Enum.find(@all_patterns, fn {regex, _id} -> Regex.match?(regex, title) end) do
      {_regex, id} -> {:eligible, id}
      nil -> :requires_review
    end
  end

  def classify(_), do: :requires_review

  @doc """
  Check eligibility against a richer PR record (map with `:title`,
  `:mergeable`, `:mergeStateStatus`, `:isDraft`). Adds state-machine
  guards on top of title matching.

  An eligible PR must be:
    * non-draft
    * `mergeable: "MERGEABLE"` (not "CONFLICTING")
    * `mergeStateStatus` in `["BLOCKED", "CLEAN", "UNSTABLE"]` — BLOCKED
      is the common case (CI required-checks failing because budget
      exhausted; admin bypasses).
  """
  @spec eligible?(map()) :: {:eligible, String.t()} | :requires_review | :state_blocked
  def eligible?(pr) do
    cond do
      Map.get(pr, :isDraft, false) -> :state_blocked
      Map.get(pr, :mergeable) == "CONFLICTING" -> :state_blocked
      Map.get(pr, :mergeStateStatus) == "DIRTY" -> :state_blocked
      true -> classify(Map.get(pr, :title, ""))
    end
  end

  @doc """
  Enumerate the rule IDs and human-readable descriptions. Useful for
  reporting tooling.
  """
  def rule_catalog do
    %{
      "AM001" => "secret-scanner reusable-wrapper replacement",
      "AM002" => "scorecard reusable-wrapper replacement",
      "AM003" => "codeql reusable-wrapper replacement",
      "AM004" => "mirror reusable-wrapper replacement",
      "AM005" => "hypatia-scan SHA repin (orphan-SHA-from-squash-merge fix)",
      "AM006" => "language-CI (rust/elixir) reusable-wrapper replacement",
      "AM007" => "CHANGELOG.md seed in Keep-a-Changelog format",
      "AM008" => "docs-template / README / tech-debt-audit doc",
      "AM009" => "linguist gitattributes / license-header migration"
    }
  end

  # ─── Stall detection (DBA001) ─────────────────────────────────────────

  @doc """
  Detect a "dependabot stall" — open dependabot PR older than the
  given threshold whose only blocker is a review request from the repo
  owner. Root cause is almost always a `* @hyperpolymath` line in
  `.github/CODEOWNERS` auto-requesting a review that dependabot cannot
  satisfy on its own.

  Remediation: add a `.github/workflows/dependabot-automerge.yml`
  workflow that calls `gh pr merge --auto --squash` on
  `update-type:version-update:semver-patch` and `semver-minor` from
  `dependabot[bot]`. Major bumps and security advisories still gate.

  Returns `:stalled` if the heuristic fires, `:ok` otherwise.
  """
  @spec dependabot_stalled?(map(), pos_integer()) :: :stalled | :ok
  def dependabot_stalled?(%{author: %{login: "dependabot[bot]"}} = pr, days_threshold \\ 7) do
    created = Map.get(pr, :createdAt, "")
    has_review_requests = (Map.get(pr, :reviewRequests, []) != [])

    cond do
      created == "" -> :ok
      not has_review_requests -> :ok
      true ->
        case DateTime.from_iso8601(created) do
          {:ok, dt, _} ->
            age_days = DateTime.diff(DateTime.utc_now(), dt, :day)
            if age_days >= days_threshold, do: :stalled, else: :ok
          _ -> :ok
        end
    end
  end

  def dependabot_stalled?(_, _), do: :ok
end
