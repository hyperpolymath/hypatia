# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.BaselineHealth do
  @moduledoc """
  Detects degraded `main`-branch baseline conditions that allow silent rot.

  Seven failure modes, each surfaced as a finding:

  - **BH001** — `main` branch protection has no `required_status_checks`
    block (or has it but with an empty `contexts` list). With this gap a
    PR with red CI can still be merged; subsequent PRs inherit the red
    baseline and look broken when their own diff is clean.

  - **BH002** — A `Cargo.toml` / `package.json` declares a dependency
    whose adjacent line contains a `TODO`/`FIXME`/`XXX` mentioning
    "migrate", "migration", "rename", or "API". This is the smell that
    PR #82 (`hyperpolymath/reasonably-good-token-vault`) left behind:
    `ureq = "3"  # TODO: migrate to 3.x API`. The bump landed, the
    migration didn't, main went red, every downstream dependabot PR
    inherited the failure.

  - **BH003** — A workflow run targeting `main` has been failing for
    longer than the staleness threshold (default: 24h). This is the
    persistent-red-baseline signal: an issue worth a maintainer escalation
    rather than auto-fix, because by the time it's >24h old it's not a
    flake.

  - **BH004** — A workflow `uses:` line references an action by full SHA
    that does not exist on the upstream repository. Every workflow run
    using the pin fails immediately on action resolution. Discovered
    2026-05-26 in `hyperpolymath/rsr-template-repo` and 9 other estate
    repos — a single dead SHA pin (`actions/upload-artifact@65c79d7f…`)
    was propagated by template scaffolding and broke main on each.

  - **BH005** — A required status check in branch protection corresponds
    to a workflow that only runs on `push` events (mirror jobs,
    `trigger-boj`, dispatch fan-outs, etc.), so it never reports on PRs.
    Every PR ends up permanently `BLOCKED` waiting for a check that
    can't appear. Discovered 2026-05-26 by an org-wide sweep that locked
    330 repos against their push-context checks; ~290 of them then
    needed correction to remove push-only contexts.

  - **BH006** — A required status check in branch protection corresponds
    to a workflow that no longer exists in `.github/workflows/`. The
    workflow was renamed or deleted, but the protection list wasn't
    updated. PRs block on a check that will never run. Discovered
    2026-05-26 in `hyperpolymath/rsr-template-repo` whose required list
    still referenced `analysis`, `dispatch`, `mirror-codeberg`, etc.
    after those workflows had been retired or renamed.

  - **BH007** — A merged commit's `author.email` does not match any UID
    on the GPG/SSH signing key that GitHub has registered for the
    committing user. The commit is signed (locally valid), but GitHub
    returns `verified: false, reason: bad_email`. Branch protection
    rules that require signed commits then block all subsequent PRs.
    Discovered 2026-05-26 after a session of estate-wide commits ran
    into `bad_email` on every output because the noreply UID was on the
    local GPG key but absent from the GitHub-registered public key.

  Dispatches to:
  - sustainabot: BH001/BH003/BH005/BH006/BH007 advisory (owner-level —
    branch protection / escalation / key management)
  - rhodibot: BH002/BH004 follow-up PR (mechanical fix is well-defined)

  Rule IDs: BH001-BH007

  ## Why this module exists

  Originally authored 2026-05-25 after a foundational fix to
  `hyperpolymath/reasonably-good-token-vault` (PR #89) where:

  1. PR #82 bumped `ureq` 2.12.1 → 3.3.0 and left a TODO instead of
     migrating. (BH002 detects this class.)
  2. Main went red and stayed red for 3+ days. (BH003 detects this.)
  3. Branch protection had no `required_status_checks`, so PR #82
     merged despite red CI. (BH001 detects this.)

  Extended 2026-05-26 with BH004-BH007 after a follow-on session
  surfaced four more propagation hazards across the estate (dead-SHA
  pins, push-only required checks, stale required-check names,
  GPG-UID-on-key mismatches). Same pattern: each was a single line of
  config that broke CI silently across many repos.

  Each finding is a propagation hazard — every repo in the estate
  might have the same gap. Hypatia surfaces them per-repo so the safety
  triangle can route the right action to the right bot.
  """

  require Logger

  @github_api_base "https://api.github.com"

  # Default staleness threshold for BH003 (hours).
  @default_baseline_red_threshold_hours 24

  # Patterns in TODO/FIXME comments that strongly suggest a deferred
  # major-version migration. Conservative — false positives are worse
  # than false negatives for this rule because the dispatch is "open a
  # PR", not "advise".
  @migration_pattern ~r/\b(TODO|FIXME|XXX)\b.*(migrat|rename|API|breaking|major)/i

  # Workflow YAML scanner for BH004 (dead SHA pin). Captures
  # `uses: <owner>/<repo>@<40-hex-sha>` (with or without a trailing
  # comment) for every action reference. Composite-action callouts
  # (`uses: ./...`) and docker-image refs (`docker://...`) are skipped.
  @uses_sha_pattern ~r/^\s*-?\s*uses:\s*([a-zA-Z0-9_.-]+\/[a-zA-Z0-9_.-]+)@([a-fA-F0-9]{40})\b/m

  # BH005/BH006 — distinguish workflows that ran on the latest PR vs
  # only on the main-branch push. A required check whose name appears
  # in `push_checks` but not in `pr_checks` is push-only (BH005). A
  # required check whose name appears in neither is stale (BH006).

  # BH007 — commit verification API reasons that indicate a UID gap on
  # the signing key (as opposed to a properly unsigned commit). `valid`
  # is the positive case; everything else is investigated.
  @bad_signing_reasons ~w[bad_email no_user unknown_signature_type bad_cert unsigned]

  # ─── BH001: Branch protection missing required_status_checks ──────────

  @doc """
  BH001: `main` branch protection has no `required_status_checks` block,
  or has the block but with no contexts listed.

  Severity: `:critical` — allows red-CI merges, which is how baseline
  rot enters main silently.

  Requires `GITHUB_TOKEN`. Returns `[]` cleanly if the token is missing.
  """
  def bh001_missing_required_status_checks(owner, repo) do
    case fetch_branch_protection(owner, repo, "main") do
      {:ok, %{"required_status_checks" => %{"contexts" => contexts}}}
      when is_list(contexts) and contexts != [] ->
        []

      {:ok, %{"required_status_checks" => %{"checks" => checks}}}
      when is_list(checks) and checks != [] ->
        []

      {:ok, _protection} ->
        [
          %{
            rule: "BH001",
            file: "#{owner}/#{repo}",
            severity: :critical,
            reason:
              "main branch protection has no required_status_checks " <>
                "-- red-CI PRs can be merged, baseline rot enters silently",
            action: :report,
            detail: %{
              branch: "main",
              fix:
                "Enable required status checks via Settings > Branches > main, " <>
                  "or `gh api -X PUT repos/#{owner}/#{repo}/branches/main/protection " <>
                  "...required_status_checks=...`"
            }
          }
        ]

      {:error, :no_token} ->
        []

      {:error, :not_protected} ->
        [
          %{
            rule: "BH001",
            file: "#{owner}/#{repo}",
            severity: :critical,
            reason: "main branch has no protection at all -- any push can land directly",
            action: :report,
            detail: %{branch: "main", fix: "Enable branch protection on main"}
          }
        ]

      {:error, _reason} ->
        []
    end
  end

  # ─── BH002: Major-bump TODO left in manifest ──────────────────────────

  @doc """
  BH002: Scan a repo's dependency manifests for lines whose comment
  contains a TODO/FIXME mentioning a deferred migration.

  This is a pure local file scan — no API calls, no token. Walks the
  repo for `Cargo.toml` and `package.json` files (extend as needed).

  Severity: `:high` — almost always indicates a live or imminent build
  break.
  """
  def bh002_migration_todo_in_manifest(repo_path) do
    manifest_paths =
      manifest_files(repo_path)
      |> Enum.filter(&File.regular?/1)

    manifest_paths
    |> Enum.flat_map(fn path ->
      path
      |> File.read!()
      |> String.split("\n")
      |> Enum.with_index(1)
      |> Enum.filter(fn {line, _} -> Regex.match?(@migration_pattern, line) end)
      |> Enum.map(fn {line, lineno} ->
        rel = Path.relative_to(path, repo_path)

        %{
          rule: "BH002",
          file: rel,
          severity: :high,
          reason:
            "manifest #{rel}:#{lineno} carries a deferred-migration TODO " <>
              "-- likely live or imminent build break",
          action: :open_followup_pr,
          detail: %{
            line: lineno,
            text: String.trim(line),
            fix:
              "Either complete the migration (update call sites to the " <>
                "new API) or roll the dependency back to the last working major."
          }
        }
      end)
    end)
  end

  # ─── BH003: Persistent red baseline on main ───────────────────────────

  @doc """
  BH003: Detect a CI workflow that has been failing on `main` longer
  than `threshold_hours` (default 24).

  Logic: pull the latest run for each workflow on `main`; if its
  conclusion is `failure` and it's older than the threshold, emit.

  Severity: `:critical` — at >24h, this is not a flake; it's an outage.

  Requires `GITHUB_TOKEN`. Returns `[]` cleanly if the token is missing.
  """
  def bh003_persistent_red_baseline(owner, repo, opts \\ []) do
    threshold = Keyword.get(opts, :threshold_hours, @default_baseline_red_threshold_hours)

    case fetch_workflow_runs(owner, repo, "main") do
      {:ok, runs} ->
        runs
        # Latest run per workflow_id
        |> Enum.group_by(& &1["workflow_id"])
        |> Enum.map(fn {_wf_id, group} ->
          Enum.max_by(group, & &1["created_at"], fn -> nil end)
        end)
        |> Enum.reject(&is_nil/1)
        |> Enum.filter(fn run ->
          run["conclusion"] == "failure" and
            age_in_hours(run["created_at"]) >= threshold
        end)
        |> Enum.map(fn run ->
          age = age_in_hours(run["created_at"])
          name = run["name"] || "(unknown workflow)"

          %{
            rule: "BH003",
            file: "#{owner}/#{repo}",
            severity: :critical,
            reason:
              "workflow `#{name}` has been failing on main for #{age}h " <>
                "(threshold: #{threshold}h) -- baseline is rotting",
            action: :escalate,
            detail: %{
              workflow: name,
              run_id: run["id"],
              head_sha: run["head_sha"],
              age_hours: age,
              threshold_hours: threshold,
              url: run["html_url"]
            }
          }
        end)

      {:error, _} ->
        []
    end
  end

  # ─── BH004: Dead action SHA pin in workflow YAML ──────────────────────

  @doc """
  BH004: For each `uses: <owner>/<repo>@<sha>` reference in workflow YAML,
  verify the SHA resolves to a real commit on the upstream action repo.

  Discovery 2026-05-26: a single dead pin
  (`actions/upload-artifact@65c79d7f54e76e4e3c7a8f34db0f4ac8b515c478`)
  was propagated by template scaffolding into ~62 workflow files across
  10 repos. Every workflow run using the pin fails at action-resolution
  time, before any step executes:

      Unable to resolve action `actions/upload-artifact@65c79d…`,
      unable to find version `65c79d…`

  Severity: `:critical` — the workflow is dead, not slow. Action:
  `:open_followup_pr` to bump to the current SHA (rhodibot territory).

  Requires `GITHUB_TOKEN` to confirm the SHA does not exist upstream.
  Returns `[]` cleanly without one.
  """
  def bh004_dead_action_sha_pin(repo_path) do
    workflow_files = find_workflow_files(repo_path)

    workflow_files
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      Regex.scan(@uses_sha_pattern, content, return: :index)
      |> Enum.flat_map(fn [{full_start, _}, {repo_start, repo_len}, {sha_start, sha_len}] ->
        action_repo = String.slice(content, repo_start, repo_len)
        sha = String.slice(content, sha_start, sha_len) |> String.downcase()
        line_no = line_number_for_offset(content, full_start)
        check_action_sha_alive(action_repo, sha, rel, line_no)
      end)
    end)
  end

  defp check_action_sha_alive(action_repo, sha, file, line_no) do
    case curl_github("repos/#{action_repo}/commits/#{sha}") do
      {:ok, %{"message" => "No commit found for SHA: " <> _}} ->
        [
          %{
            rule: "BH004",
            file: file,
            severity: :critical,
            reason:
              "workflow #{file}:#{line_no} pins #{action_repo}@#{String.slice(sha, 0, 8)}… " <>
                "which does not exist on the upstream action repository",
            action: :open_followup_pr,
            detail: %{
              line: line_no,
              action_repo: action_repo,
              dead_sha: sha,
              fix:
                "Bump to the current tag head: " <>
                  "`gh api repos/#{action_repo}/git/refs/tags/v<MAJOR> --jq .object.sha`"
            }
          }
        ]

      {:ok, %{"sha" => _real_sha}} ->
        # SHA resolves cleanly — no finding.
        []

      {:error, :no_token} ->
        # We can't verify without a token. Don't emit — false positives
        # are worse than false negatives for this rule (would be
        # disruptive to flag every pin as dead).
        []

      _other ->
        []
    end
  end

  # ─── BH005: Push-only required check ──────────────────────────────────

  @doc """
  BH005: A required status check corresponds to a workflow that triggers
  only on `push` events (no `pull_request` trigger), so it never reports
  on PRs. Every PR ends up permanently `BLOCKED` waiting for a check
  that cannot appear.

  Discovery 2026-05-26: an org-wide branch-protection sweep against
  330 hyperpolymath repos applied "lock currently-green checks on
  default branch" policy. It correctly captured 14 contexts on ephapax
  but 6 (`Dependabot`, `analysis`, `check-critical`, `dispatch`,
  `mirror-gitlab`, `trigger-boj`) were push-context-only workflows. The
  next PR opened immediately stuck in `BLOCKED / MERGEABLE`. Pattern
  replicated across all 330 protected repos.

  Logic: load branch protection's required `contexts`; load the latest
  PR-context check set (recent merged PRs); flag any required context
  that has no PR-side representative.

  Severity: `:high` — workflow itself is fine; the protection list is
  miscalibrated. Action: `:report` (sustainabot — owner is the only
  one who can edit protection).

  Requires `GITHUB_TOKEN`.
  """
  def bh005_push_only_required_check(owner, repo) do
    with {:ok, %{"required_status_checks" => %{"contexts" => contexts}}}
         when is_list(contexts) and contexts != [] <-
           fetch_branch_protection(owner, repo, "main"),
         {:ok, pr_checks} <- fetch_recent_pr_check_names(owner, repo) do
      contexts
      |> Enum.reject(&(&1 in pr_checks))
      |> Enum.map(fn name ->
        %{
          rule: "BH005",
          file: "#{owner}/#{repo}",
          severity: :high,
          reason:
            "required check `#{name}` did not run on any recent PR " <>
              "(likely push-only) — every PR will be permanently BLOCKED on it",
          action: :report,
          detail: %{
            context: name,
            branch: "main",
            fix:
              "Either remove `#{name}` from required_status_checks, or " <>
                "add a `pull_request:` trigger to its workflow file."
          }
        }
      end)
    else
      _ -> []
    end
  end

  # ─── BH006: Required-check name no longer corresponds to a workflow ──

  @doc """
  BH006: A required status check name does not correspond to any
  workflow currently in `.github/workflows/` (and did not appear on
  any recent main-branch push). The workflow was renamed or deleted
  but the protection list wasn't updated.

  Discovery 2026-05-26: `hyperpolymath/rsr-template-repo`'s required
  list still referenced `.github/dependabot.yml` (which is a path, not
  a check name), `analysis`, `antipattern-check`, `check`, `docs`,
  `lint`, `mirror-codeberg`, `mirror-gitlab`, `mirror-sourcehut`,
  `validate` — all stale or never-reporting names.

  Distinction from BH005: BH005 catches "ran on push, not on PR";
  BH006 catches "didn't run anywhere recently."

  Severity: `:high`. Action: `:report` (manual prune of protection
  list).

  Requires `GITHUB_TOKEN`.
  """
  def bh006_required_check_drift(owner, repo) do
    with {:ok, %{"required_status_checks" => %{"contexts" => contexts}}}
         when is_list(contexts) and contexts != [] <-
           fetch_branch_protection(owner, repo, "main"),
         {:ok, push_checks} <- fetch_recent_push_check_names(owner, repo),
         {:ok, pr_checks} <- fetch_recent_pr_check_names(owner, repo) do
      observed = MapSet.union(MapSet.new(push_checks), MapSet.new(pr_checks))

      contexts
      |> Enum.reject(&MapSet.member?(observed, &1))
      |> Enum.map(fn name ->
        %{
          rule: "BH006",
          file: "#{owner}/#{repo}",
          severity: :high,
          reason:
            "required check `#{name}` has not been emitted on any recent " <>
              "main-branch push OR pull-request — it appears stale or never existed",
          action: :report,
          detail: %{
            context: name,
            branch: "main",
            fix:
              "Verify the workflow still exists. If renamed, update " <>
                "required_status_checks. If retired, remove from the list."
          }
        }
      end)
    else
      _ -> []
    end
  end

  # ─── BH007: GPG/SSH key UID gap (signature bad_email) ────────────────

  @doc """
  BH007: A commit on the default branch (or open PR) is signed but
  GitHub returns `verified: false, reason: bad_email` (or similar
  UID-mismatch reasons). The signature itself is valid locally; the
  author email simply doesn't appear as a UID on the public key
  registered for that user on GitHub.

  Discovery 2026-05-26: an entire estate-wide session of commits was
  authored as the GitHub noreply email (`<id>+<user>@users.noreply.github.com`)
  to satisfy email-privacy push restrictions. The local GPG key
  contained both the noreply UID and the public UID, but the GitHub-
  registered public key only had the public UID exported. Every commit
  showed `verified: false / bad_email`. Branch protection requiring
  signed commits then blocked all PRs.

  Severity: `:high` — silently breaks branch protection that requires
  verified signatures. Action: `:report` (key management is owner-side).

  Logic: scan recent merge commits on main + open-PR head SHAs for
  `verification.reason ∈ #{inspect(@bad_signing_reasons)}`. Emit one
  finding per unique (author, reason) pair, not per commit, to avoid
  drowning the owner.

  Requires `GITHUB_TOKEN`.
  """
  def bh007_signing_key_uid_gap(owner, repo) do
    case fetch_recent_commit_verifications(owner, repo) do
      {:ok, commits} ->
        commits
        |> Enum.filter(fn c ->
          reason = get_in(c, ["commit", "verification", "reason"])
          verified = get_in(c, ["commit", "verification", "verified"])
          # Only emit for *signed but bad_email* — `unsigned` is a
          # different defect handled by a separate rule.
          reason in ["bad_email", "no_user", "unknown_signature_type"] and verified == false
        end)
        |> Enum.group_by(fn c ->
          {
            get_in(c, ["commit", "author", "email"]) || "(unknown)",
            get_in(c, ["commit", "verification", "reason"]) || "(unknown)"
          }
        end)
        |> Enum.map(fn {{email, reason}, group} ->
          shas = group |> Enum.map(&String.slice(&1["sha"], 0, 7)) |> Enum.uniq()

          %{
            rule: "BH007",
            file: "#{owner}/#{repo}",
            severity: :high,
            reason:
              "#{length(group)} commit(s) by #{email} signed but rejected " <>
                "with `#{reason}` — author email is not a UID on the GPG/SSH " <>
                "key registered for this user on GitHub",
            action: :report,
            detail: %{
              author_email: email,
              github_reason: reason,
              affected_shas: Enum.take(shas, 5),
              fix:
                "Add the author email as a UID to the key " <>
                  "(`gpg --quick-add-uid <fpr> '<name> <email>'`), re-export " <>
                  "(`gpg --armor --export <fpr>`), and re-upload to " <>
                  "https://github.com/settings/keys — or switch to SSH signing " <>
                  "and register the SSH public key as a Signing Key on GitHub."
            }
          }
        end)

      {:error, _} ->
        []
    end
  end

  # ─── BH008: Inherited main-branch debt ────────────────────────────────
  #
  # Pattern hit repeatedly during the 2026-06-02 sweeps: a single broken
  # check on `main` cascades to EVERY open PR in the same repo. Each PR
  # shows the same failing check, none of the PR diffs are causal, and
  # the right answer is one root-fix PR on `main` rather than N retries.
  #
  # Detection: across the open PRs in `{owner, repo}`, find any check
  # name that fails on `:threshold` (default 3) or more PRs. Emit one
  # `:warn` finding per inherited check name with the affected PR list
  # and a recommended action to file a root-fix PR on main.
  #
  # Crucially NOT auto-fixable: the root cause may be a workflow change,
  # a dep bump, a baseline regression — each needs owner direction.
  # See feedback in MEMORY.md: "C. NEW rule: inherited-main-branch-debt".

  @default_inherited_debt_threshold 3

  @doc """
  BH008: Find check names that fail across ≥ `threshold` open PRs in
  `{owner, repo}`. The shared failure is almost certainly inherited from
  a degraded `main` baseline rather than caused by any single PR's diff.

  Inputs (caller supplies; this rule is data-driven so the gh API
  fan-out lives in the caller):

      pr_check_data :: [
        %{number: integer,
          failed_checks: [String.t()]}
      ]

  Returns a list of findings, one per check name meeting the threshold.

  Opts:
    * `:threshold` — minimum PR count to flag (default 3)
  """
  def bh008_inherited_main_debt(owner, repo, pr_check_data, opts \\ [])
      when is_list(pr_check_data) do
    threshold = Keyword.get(opts, :threshold, @default_inherited_debt_threshold)

    pr_check_data
    |> Enum.flat_map(fn pr ->
      Enum.map(pr.failed_checks, fn check -> {check, pr.number} end)
    end)
    |> Enum.group_by(fn {check, _} -> check end, fn {_, num} -> num end)
    |> Enum.filter(fn {_check, prs} -> length(Enum.uniq(prs)) >= threshold end)
    |> Enum.map(fn {check, prs} ->
      pr_numbers = prs |> Enum.uniq() |> Enum.sort()

      %{
        rule: "BH008",
        file: "#{owner}/#{repo}",
        severity: :warn,
        auto_fixable: false,
        type: :inherited_main_debt,
        check_name: check,
        pr_count: length(pr_numbers),
        affected_prs: pr_numbers,
        reason:
          "Check `#{check}` is failing on #{length(pr_numbers)} open PRs " <>
            "(##{Enum.join(pr_numbers, ", #")}). When ≥#{threshold} PRs share " <>
            "the same failing check the cause is almost always inherited from " <>
            "a degraded main baseline rather than any single PR diff. " <>
            "Recommended action: file a single root-fix PR targeting main.",
        action: :file_root_fix_pr_on_main,
        detail: %{
          recommendation: "file_root_fix_pr_on_main",
          threshold: threshold
        }
      }
    end)
  end

  # ─── scan/2 facade ────────────────────────────────────────────────────

  @doc """
  Run all baseline-health checks. `owner` and `repo` may be `nil` if you
  only want the local (BH002, BH004 partially) checks; the API-backed
  checks will return empty in that case.
  """
  def scan(repo_path, opts \\ []) do
    {owner, repo} =
      case Keyword.get(opts, :owner_repo) do
        {o, r} when is_binary(o) and is_binary(r) -> {o, r}
        _ -> extract_owner_repo(repo_path)
      end

    api_findings =
      if owner && repo do
        bh001_missing_required_status_checks(owner, repo) ++
          bh003_persistent_red_baseline(owner, repo, opts) ++
          bh005_push_only_required_check(owner, repo) ++
          bh006_required_check_drift(owner, repo) ++
          bh007_signing_key_uid_gap(owner, repo)
      else
        []
      end

    findings =
      bh002_migration_todo_in_manifest(repo_path) ++
        bh004_dead_action_sha_pin(repo_path) ++
        api_findings

    %{
      findings: findings,
      total: length(findings),
      by_severity: group_by_severity(findings),
      dispatch: dispatch_recommendations(findings)
    }
  end

  # ─── GitHub API ───────────────────────────────────────────────────────

  defp fetch_branch_protection(owner, repo, branch) do
    case curl_github("repos/#{owner}/#{repo}/branches/#{branch}/protection") do
      {:ok, %{"message" => "Branch not protected"}} ->
        {:error, :not_protected}

      {:ok, %{"message" => msg}} ->
        {:error, "GitHub API: #{msg}"}

      {:ok, body} when is_map(body) ->
        {:ok, body}

      other ->
        other
    end
  end

  defp fetch_workflow_runs(owner, repo, branch) do
    qs = "branch=#{branch}&per_page=100"

    case curl_github("repos/#{owner}/#{repo}/actions/runs?#{qs}") do
      {:ok, %{"workflow_runs" => runs}} when is_list(runs) -> {:ok, runs}
      {:ok, %{"message" => msg}} -> {:error, "GitHub API: #{msg}"}
      {:ok, _} -> {:error, "unexpected GitHub response shape"}
      other -> other
    end
  end

  # BH005/BH006 helper — names of successful check-runs from the most
  # recent merged PRs (up to 3). Union across the three so a single
  # dependabot-only PR doesn't skew toward dependabot-only checks.
  defp fetch_recent_pr_check_names(owner, repo) do
    case curl_github("repos/#{owner}/#{repo}/pulls?state=closed&per_page=10") do
      {:ok, prs} when is_list(prs) ->
        recent_heads =
          prs
          |> Enum.filter(&(&1["merged_at"] != nil))
          |> Enum.take(3)
          |> Enum.map(& &1["head"]["sha"])

        names =
          recent_heads
          |> Enum.flat_map(fn sha ->
            case curl_github(
                   "repos/#{owner}/#{repo}/commits/#{sha}/check-runs?per_page=100"
                 ) do
              {:ok, %{"check_runs" => runs}} when is_list(runs) ->
                runs
                |> Enum.filter(&(&1["conclusion"] == "success"))
                |> Enum.map(& &1["name"])

              _ ->
                []
            end
          end)
          |> Enum.uniq()

        {:ok, names}

      _ ->
        {:error, :no_recent_prs}
    end
  end

  # BH006 helper — names of successful check-runs from the latest
  # push on the default branch.
  defp fetch_recent_push_check_names(owner, repo) do
    case curl_github("repos/#{owner}/#{repo}/commits/main/check-runs?per_page=100") do
      {:ok, %{"check_runs" => runs}} when is_list(runs) ->
        names =
          runs
          |> Enum.filter(&(&1["conclusion"] == "success"))
          |> Enum.map(& &1["name"])
          |> Enum.uniq()

        {:ok, names}

      _ ->
        {:error, :no_main_runs}
    end
  end

  # BH007 helper — latest ~30 commits on main with their verification
  # block. GitHub returns this in `/repos/.../commits` already.
  defp fetch_recent_commit_verifications(owner, repo) do
    case curl_github("repos/#{owner}/#{repo}/commits?per_page=30") do
      {:ok, commits} when is_list(commits) -> {:ok, commits}
      {:ok, %{"message" => msg}} -> {:error, "GitHub API: #{msg}"}
      other -> other
    end
  end

  defp curl_github(path) do
    token = System.get_env("GITHUB_TOKEN") || System.get_env("HYPATIA_DISPATCH_PAT")

    if token == nil or token == "" do
      {:error, :no_token}
    else
      url = "#{@github_api_base}/#{path}"

      case System.cmd(
             "curl",
             [
               "-s",
               "-H",
               "Accept: application/vnd.github+json",
               "-H",
               "Authorization: Bearer #{token}",
               "-H",
               "X-GitHub-Api-Version: 2022-11-28",
               url
             ],
             stderr_to_stdout: true
           ) do
        {body, 0} ->
          case Jason.decode(body) do
            {:ok, value} -> {:ok, value}
            {:error, _} -> {:error, "invalid JSON from GitHub API"}
          end

        {error, _} ->
          {:error, "curl failed: #{String.slice(error, 0, 200)}"}
      end
    end
  end

  # ─── Helpers ──────────────────────────────────────────────────────────

  defp manifest_files(repo_path) do
    # Walk the repo looking for Cargo.toml / package.json. Capped depth
    # so a monorepo doesn't pay for an unbounded find.
    case System.cmd(
           "find",
           [
             repo_path,
             "-maxdepth",
             "4",
             "-type",
             "f",
             "(",
             "-name",
             "Cargo.toml",
             "-o",
             "-name",
             "package.json",
             ")"
           ],
           stderr_to_stdout: true
         ) do
      {output, 0} ->
        output
        |> String.split("\n", trim: true)
        |> Enum.reject(&String.contains?(&1, "/target/"))
        |> Enum.reject(&String.contains?(&1, "/node_modules/"))

      _ ->
        []
    end
  end

  # BH004 helper — locate every active GitHub Actions workflow file.
  # Active = lives under `.github/workflows/` at the REPO ROOT (not
  # nested under subdirectories; GitHub Actions only consumes the
  # root location). Nested `.github/workflows/` in monorepo subtrees
  # are template scaffolds — they may still ship the broken pin but
  # they don't run CI, so BH004 leaves them to a separate rule.
  defp find_workflow_files(repo_path) do
    root = Path.join([repo_path, ".github", "workflows"])

    cond do
      not File.dir?(root) ->
        []

      true ->
        root
        |> File.ls!()
        |> Enum.filter(&(String.ends_with?(&1, ".yml") or String.ends_with?(&1, ".yaml")))
        |> Enum.map(&Path.join(root, &1))
        |> Enum.filter(&File.regular?/1)
    end
  end

  # BH004 helper — given a byte offset into a file's content, return
  # the 1-based line number.
  defp line_number_for_offset(content, offset) when offset >= 0 do
    content
    |> binary_part(0, min(offset, byte_size(content)))
    |> String.graphemes()
    |> Enum.count(&(&1 == "\n"))
    |> Kernel.+(1)
  end

  defp extract_owner_repo(repo_path) do
    case System.cmd("git", ["remote", "get-url", "origin"],
           cd: repo_path,
           stderr_to_stdout: true
         ) do
      {url, 0} ->
        trimmed = String.trim(url)

        cond do
          String.contains?(trimmed, "github.com:") ->
            [_, path] = String.split(trimmed, "github.com:", parts: 2)
            parse_owner_repo_from_path(path)

          String.contains?(trimmed, "github.com/") ->
            [_, path] = String.split(trimmed, "github.com/", parts: 2)
            parse_owner_repo_from_path(path)

          true ->
            {nil, nil}
        end

      _ ->
        {nil, nil}
    end
  end

  defp parse_owner_repo_from_path(path) do
    clean = path |> String.trim() |> String.trim_trailing(".git")

    case String.split(clean, "/", parts: 2) do
      [owner, repo] -> {owner, repo}
      _ -> {nil, nil}
    end
  end

  defp age_in_hours(nil), do: 0

  defp age_in_hours(iso_string) when is_binary(iso_string) do
    case DateTime.from_iso8601(iso_string) do
      {:ok, dt, _} -> DateTime.diff(DateTime.utc_now(), dt, :hour)
      _ -> 0
    end
  end

  defp group_by_severity(findings) do
    findings
    |> Enum.group_by(& &1.severity)
    |> Enum.map(fn {sev, items} -> {sev, length(items)} end)
    |> Map.new()
  end

  defp dispatch_recommendations(findings) do
    Enum.map(findings, fn finding ->
      bot =
        case finding.action do
          :open_followup_pr -> :rhodibot
          :escalate -> :sustainabot
          :report -> :sustainabot
          _ -> :sustainabot
        end

      # BH004 is mechanically fixable (one-line sed replace + PR), so
      # confidence is high; BH005-007 need owner judgement (which
      # contexts to keep, key-management) so we route to sustainabot
      # at calibrated confidence.
      confidence =
        case {finding.rule, finding.severity} do
          {"BH004", _} -> 0.93
          {"BH005", _} -> 0.85
          {"BH006", _} -> 0.85
          {"BH007", _} -> 0.80
          {_, :critical} -> 0.95
          {_, :high} -> 0.88
          {_, :medium} -> 0.80
          {_, :low} -> 0.65
          _ -> 0.50
        end

      %{
        bot: bot,
        confidence: confidence,
        rule: finding.rule,
        action: finding.action,
        reason: finding.reason
      }
    end)
  end
end
