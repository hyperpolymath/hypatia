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

  # ─── Estate-wide backlog scan (DBA002) ────────────────────────────────

  @doc """
  Inventory open PRs across an estate and partition them by
  `AdminMergeEligibility.classify/1`. Returns
  `{:ok, %{eligible: [...], requires_review: [...], stalled_dependabot: [...]}}`.

  Intended consumers:

    * Periodic scan: when `length(eligible) > 50` the scanner emits an
      estate finding (`:backlog_exceeds_threshold`) that surfaces in
      hypatia's web dashboard + dispatches the
      `recipe-budget-resume-sweep` (which calls the
      `.git-private-farm/dispatch-templates/budget-resume-sweep.yml`
      workflow).

    * Manual inspection: the same partition runs as a one-off via
      `mix hypatia.pr_backlog` (script at
      `scripts/pr_backlog_report.sh` reads JSONL from
      `gh search prs --author=@me --state=open --owner=<owner>` and
      pipes through this function).

  The `prs` argument is a list of maps shaped like the
  `gh search prs ... --json url,title,isDraft,mergeable,mergeStateStatus,createdAt,author,reviewRequests`
  output (snake_case keys are accepted as well via the standard
  Map.get-with-default fallthrough).
  """
  @spec scan_estate_backlog(list(map())) :: {:ok, map()}
  def scan_estate_backlog(prs) when is_list(prs) do
    {eligible, others} =
      Enum.split_with(prs, fn pr ->
        case eligible?(pr) do
          {:eligible, _id} -> true
          _ -> false
        end
      end)

    stalled_dependabot =
      Enum.filter(others, fn pr ->
        dependabot_stalled?(pr) == :stalled
      end)

    requires_review = others -- stalled_dependabot

    {:ok,
     %{
       eligible: eligible,
       requires_review: requires_review,
       stalled_dependabot: stalled_dependabot,
       counts: %{
         eligible: length(eligible),
         requires_review: length(requires_review),
         stalled_dependabot: length(stalled_dependabot),
         total: length(prs)
       }
     }}
  end

  @doc """
  Detect an "obsolete supersedes" PR (ERR-PR-001): a PR whose only
  substantive change is to set a workflow `uses:` SHA, but main now
  carries a NEWER SHA at the same location. The PR's edit would
  REGRESS main. Close-as-superseded rather than rebase.

  Observed 2026-05-27: 6 such PRs across the estate
  (`somethings-fishy#26`, `nextgen-languages#55`, `stapeln#74`,
  `hypatia#349`, `typed-wasm#75`, plus the explicitly-confirmed
  `maa-framework#78` doc supersedes). All closed cleanly with a
  one-line "main already has newer X" comment.

  Inputs (all expected to be already-fetched via `gh` upstream):
    * `pr` — map with `:url`, `:title`, `:files` (list of `{path, patch}`)
    * `main_lookup` — fn that given a `path` returns the current main
      file contents (string) or nil if not present.

  Returns `{:obsolete, reason}` when the PR's edited line is older
  than main's current value at the same location; `:not_obsolete`
  otherwise. Caller is expected to gh-pr-close with the canonical
  comment template.
  """
  @spec obsolete_supersedes?(map(), (String.t() -> String.t() | nil)) ::
          {:obsolete, String.t()} | :not_obsolete
  def obsolete_supersedes?(%{files: files} = pr, main_lookup)
      when is_function(main_lookup, 1) do
    # Look at workflow YAML edits that change a `uses: ...@<sha>` line.
    Enum.find_value(files, :not_obsolete, fn file ->
      path = Map.get(file, :path, "")
      patch = Map.get(file, :patch, "")

      pr_targets = Regex.scan(~r/^\+\s+uses:\s+([^@\s]+)@([0-9a-f]{40})/m, patch)

      if pr_targets == [] do
        nil
      else
        case main_lookup.(path) do
          nil ->
            nil

          main_content ->
            main_targets =
              Regex.scan(~r/uses:\s+([^@\s]+)@([0-9a-f]{40})/, main_content)

            stale? =
              Enum.any?(pr_targets, fn [_, action, pr_sha] ->
                main_sha =
                  Enum.find_value(main_targets, fn [_, m_action, m_sha] ->
                    if m_action == action, do: m_sha
                  end)

                main_sha != nil and main_sha != pr_sha
              end)

            if stale? do
              {:obsolete, "main already carries a newer SHA for `uses:` line in #{path}"}
            else
              nil
            end
        end
      end
    end)
  end

  def obsolete_supersedes?(_, _), do: :not_obsolete

  @doc """
  Backlog-threshold finding. Returns `{:finding, ...}` when the
  eligible-PR queue exceeds `threshold` (default 50 — the 2026-05-26
  pilot's CI-blocked-but-mergeable count when GitHub Actions billing
  first cliff-hit). Returns `:below_threshold` otherwise.

  Wired into the periodic learning-loop sweep via `Hypatia.Rules` so a
  growing backlog becomes a first-class finding before it becomes a
  weekend-burner sweep.
  """
  @spec backlog_finding(map(), pos_integer()) ::
          {:finding, map()} | :below_threshold
  def backlog_finding(%{counts: %{eligible: count}}, threshold \\ 50) do
    if count >= threshold do
      {:finding,
       %{
         rule: "admin_merge_backlog_exceeds_threshold",
         rule_id: "DBA002",
         recipe_id: "recipe-budget-resume-sweep",
         severity: :medium,
         count: count,
         threshold: threshold,
         description:
           "Admin-merge-eligible PR queue is #{count} (threshold #{threshold}). " <>
             "Run `.git-private-farm/dispatch-templates/budget-resume-sweep.yml` to clear " <>
             "wrapper/docs/changelog/rescript-migration PRs in bulk before they backlog further."
       }}
    else
      :below_threshold
    end
  end
end
