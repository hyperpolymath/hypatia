# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.WorkflowAudit do
  @moduledoc """
  CI/CD workflow audit rules.

  Verifies that repositories have the standard RSR workflow set,
  checks SHA pins against known-good values, detects duplicates,
  and validates permissions declarations.
  """

  # ─── Standard RSR workflows (17 total) ─────────────────────────────────

  # These are workflows EVERY ecosystem repo is expected to carry, either as
  # a standalone YAML file or via the consolidated `governance.yml` shared
  # bundle. Eight workflows that USED to be in this list have been retired
  # by the consolidation (per `.github/workflows/governance.yml`'s docstring):
  #
  #   quality.yml, guix-nix-policy.yml, npm-bun-blocker.yml, ts-blocker.yml,
  #   security-policy.yml, rsr-antipattern.yml, wellknown-enforcement.yml,
  #   workflow-linter.yml
  #
  # All eight are now covered by `governance.yml` ->
  # hyperpolymath/standards/.github/workflows/governance-reusable.yml.
  # Flagging them as "missing" produces ~10 false-positive findings per
  # consuming repo (~2,900 across the estate); removed from the list.
  #
  # Three more removed: instant-sync.yml (per-repo opt-in for mirror sync,
  # not universal), jekyll.yml + jekyll-gh-pages.yml (only repos using
  # GitHub Pages need these), scorecard-enforcer.yml (consolidated into
  # scorecard.yml).
  @standard_workflows [
    "hypatia-scan.yml",
    "codeql.yml",
    "scorecard.yml",
    "mirror.yml",
    "governance.yml",
    "secret-scanner.yml"
  ]

  # ─── Known-good SHA pins ──────────────────────────────────────────────
  #
  # Consolidated 2026-05-28 (audit Part 3.7): this module previously
  # carried its own @known_good_shas with overlapping but DIFFERENT
  # entries from SecurityErrors.@sha_pins. The two maps had drifted to
  # different memberships, and every update had to touch both. Now
  # delegates to SecurityErrors.sha_pins/0 (the canonical source).
  # NOTE: slsa-framework/slsa-github-generator is deliberately omitted —
  # it is pin-exempt (self-verifies github.ref). See
  # SecurityErrors.@pin_exempt and hyperpolymath/hypatia#262. SHA-pinning
  # it breaks SLSA provenance.

  def standard_workflows, do: @standard_workflows

  def known_good_shas, do: Hypatia.Rules.SecurityErrors.sha_pins()

  # ─── Audit functions ───────────────────────────────────────────────────

  @doc """
  Audit workflow directory. Takes a list of workflow filenames and
  optionally their contents (as a map of filename => content).
  """
  def audit(workflow_files, workflow_contents \\ %{}, opts \\ []) do
    missing = check_missing_workflows(workflow_files, opts)
    unpinned = check_unpinned_actions(workflow_contents, opts)
    wrong_pins = check_wrong_pins(workflow_contents)
    permission_issues = check_permissions(workflow_contents)
    flawed_regexes = check_flawed_regex(workflow_contents)
    duplicates = check_duplicates(workflow_files, workflow_contents)
    caching_issues = check_caching(workflow_contents)
    run_context_issues = check_github_context_in_run(workflow_contents)
    download_then_run_issues = check_download_then_run(workflow_contents)
    nperm_typos = check_npermissions_typo(workflow_contents)
    codeql_lang_mismatch = check_codeql_language_matrix_mismatch(workflow_contents, opts)
    workflow_sha_foreign_ref = check_workflow_sha_as_foreign_ref(workflow_contents)

    reusable_caller_context_self_checkout =
      check_reusable_caller_context_self_checkout(workflow_contents)

    missing_timeouts = check_missing_timeout_minutes(workflow_contents)
    scorecard_publish_run = check_scorecard_publish_run_violation(workflow_contents)
    nonroot_container_eacces = check_nonroot_container_checkout_eacces(workflow_contents)
    orphan_reusable_pins = check_orphan_standards_reusable_pin(workflow_contents)
    ungated_secret_action = check_ungated_secret_action(workflow_contents)

    scorecard_wrapper_missing_perms =
      check_scorecard_wrapper_missing_job_permissions(workflow_contents)

    workflow_linter_self_ref = check_workflow_linter_self_reference(workflow_contents)
    codeql_missing_actions = check_codeql_missing_actions_language(workflow_contents)
    concurrency_missing = check_concurrency_missing_readonly(workflow_contents)
    heading_regex_issues = check_unanchored_heading_regex(workflow_contents)
    d_burn_issues = check_d_burn_double_trigger(workflow_contents)
    secret_history = check_secret_history_coverage(workflow_contents)
    toolchain_input_missing = check_sha_pinned_toolchain_without_input(workflow_contents)
    broken_reusable_revisions = check_known_broken_reusable_revision(workflow_contents)
    moved_reusable_calls = check_reusable_call_to_moved_repository(workflow_contents)

    %{
      # `flawed_regexes` was computed at the top of this function and
      # counted into `flawed_regex_count:` below, but was never added to
      # this list -- so the rule ran on every scan, reported a count
      # nothing consumes, and discarded every finding it produced. The
      # cli.ex normalizer's own comment names `flawed_regex` as one of the
      # `:rule`-keyed sources it handles, so the omission was accidental,
      # not a deliberate mute. Restoring it is gate-safe: the rule emits
      # `severity: :low` (rank 4), below the reusable's blocking threshold
      # of high/critical and below the default `--severity medium`.
      findings:
        missing ++
          unpinned ++
          wrong_pins ++
          permission_issues ++
          duplicates ++
          caching_issues ++
          run_context_issues ++
          download_then_run_issues ++
          nperm_typos ++
          codeql_lang_mismatch ++
          workflow_sha_foreign_ref ++
          reusable_caller_context_self_checkout ++
          missing_timeouts ++
          scorecard_publish_run ++
          nonroot_container_eacces ++
          orphan_reusable_pins ++
          ungated_secret_action ++
          scorecard_wrapper_missing_perms ++
          workflow_linter_self_ref ++
          codeql_missing_actions ++
          concurrency_missing ++
          heading_regex_issues ++
          d_burn_issues ++
          flawed_regexes ++
          secret_history ++
          toolchain_input_missing ++
          broken_reusable_revisions ++
          moved_reusable_calls,
      missing_count: length(missing),
      secret_history_count: length(secret_history),
      unpinned_count: length(unpinned),
      wrong_pin_count: length(wrong_pins),
      permission_issues: length(permission_issues),
      flawed_regex_count: length(flawed_regexes),
      duplicate_count: length(duplicates),
      caching_issues: length(caching_issues),
      run_context_issues: length(run_context_issues),
      download_then_run_issues: length(download_then_run_issues),
      npermissions_typo_count: length(nperm_typos),
      codeql_lang_mismatch_count: length(codeql_lang_mismatch),
      missing_timeout_count: length(missing_timeouts),
      scorecard_publish_run_count: length(scorecard_publish_run),
      nonroot_container_eacces_count: length(nonroot_container_eacces),
      orphan_reusable_pin_count: length(orphan_reusable_pins),
      ungated_secret_action_count: length(ungated_secret_action),
      scorecard_wrapper_missing_perms_count: length(scorecard_wrapper_missing_perms),
      workflow_linter_self_ref_count: length(workflow_linter_self_ref),
      codeql_missing_actions_count: length(codeql_missing_actions),
      concurrency_missing_count: length(concurrency_missing),
      heading_regex_issues_count: length(heading_regex_issues),
      d_burn_issues_count: length(d_burn_issues),
      toolchain_input_missing_count: length(toolchain_input_missing),
      broken_reusable_revision_count: length(broken_reusable_revisions),
      moved_reusable_call_count: length(moved_reusable_calls),
      workflow_count: length(workflow_files),
      standard_coverage: coverage_percentage(workflow_files)
    }
  end

  @doc """
  Check workflow jobs that lack `timeout-minutes:`. The GitHub Actions
  default is 6 hours, which lets a stuck job (commonly a `codeload`
  fetch hang — see standards#208) burn through monthly budget. Every
  job should declare an explicit timeout proportionate to its expected
  runtime; governance / lint jobs typically settle in 1-10 minutes.

  Detection is regex-based: scan for top-level `jobs:` keys, then
  verify the next ~20 lines after each job header carry a
  `timeout-minutes:` line before another top-level key (`runs-on`,
  `permissions`, `if`, `needs`, `outputs`, `name`, `steps`,
  `concurrency`, `services`, `environment`, `strategy`,
  `continue-on-error`, `defaults`, `container`). Emits one finding
  per job that lacks one.

  Recipe: `recipe-add-workflow-timeout-minutes` (gitbot-fleet script
  `fix-workflow-timeout-minutes.sh` — auto-fixable; default 10m for
  unrecognised jobs).
  """
  def check_missing_timeout_minutes(workflow_contents) when is_map(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      # Naïve YAML scan: any `^  <key>:$` under top-level `jobs:`
      # without a subsequent `^    timeout-minutes:` line before the
      # next sibling block.
      # Reusable-workflow caller jobs (a job-level `^    uses:` calling
      # `org/repo/.github/workflows/x.yml@ref`) are EXEMPT: a workflow_call
      # job cannot carry a job-level `timeout-minutes:` — the timeout lives
      # in the called workflow's own jobs. Flagging them is a false positive.
      lines = String.split(content, "\n")
      acc0 = {MapSet.new(), MapSet.new(), MapSet.new(), false, nil}

      {jobs_seen, jobs_with_timeout, reusable_jobs} =
        Enum.reduce(lines, acc0, fn line, {seen, with_to, reusable, in_j, curj} ->
          cond do
            String.match?(line, ~r/^jobs:\s*$/) ->
              {seen, with_to, reusable, true, nil}

            in_j and String.match?(line, ~r/^  [A-Za-z0-9_-]+:\s*$/) ->
              [_, name] = Regex.run(~r/^  ([A-Za-z0-9_-]+):/, line)
              {MapSet.put(seen, name), with_to, reusable, in_j, name}

            in_j and curj && String.match?(line, ~r/^    timeout-minutes:\s*\d+/) ->
              {seen, MapSet.put(with_to, curj), reusable, in_j, curj}

            in_j and curj && String.match?(line, ~r/^    uses:\s*\S/) ->
              {seen, with_to, MapSet.put(reusable, curj), in_j, curj}

            String.match?(line, ~r/^[A-Za-z]/) ->
              {seen, with_to, reusable, false, curj}

            true ->
              {seen, with_to, reusable, in_j, curj}
          end
        end)
        |> then(fn {s, w, r, _, _} -> {s, w, r} end)

      missing =
        jobs_seen
        |> MapSet.difference(jobs_with_timeout)
        |> MapSet.difference(reusable_jobs)

      Enum.map(missing, fn job_name ->
        %{
          rule: "missing_timeout_minutes",
          rule_id: "ERR-WF-013",
          recipe_id: "recipe-add-workflow-timeout-minutes",
          severity: :medium,
          file: filename,
          job: job_name,
          description:
            "Job `#{job_name}` in #{filename} has no `timeout-minutes:` declaration. " <>
              "Default is 6 hours — a stuck codeload fetch or runner hang can burn budget. " <>
              "Add `timeout-minutes: 10` (or proportional)."
        }
      end)
    end)
  end

  def check_missing_timeout_minutes(_), do: []

  @doc """
  Check which standard workflows are missing.

  `opts[:has_codeql_supported_language]` (boolean): when false, `codeql.yml`
  is dropped from the required set — CodeQL has nothing to scan in a repo
  with no JavaScript / Python / Go / Java / Ruby / C# / C++ / Swift files,
  so demanding the workflow would produce a perpetual missing-file finding
  that no one can usefully close. Hypatia's Rust + Elixir + Idris mix
  triggered this exact FP in the #237 self-scan.
  """
  def check_missing_workflows(workflow_files, opts \\ []) do
    requireds =
      if Keyword.get(opts, :has_codeql_supported_language, true) do
        @standard_workflows
      else
        @standard_workflows -- ["codeql.yml"]
      end

    Enum.flat_map(requireds, fn wf ->
      if wf in workflow_files do
        []
      else
        [
          %{
            type: :missing_workflow,
            file: wf,
            severity: severity_for_workflow(wf),
            action: :create,
            reason:
              "Required workflow `#{wf}` is absent from .github/workflows/. " <>
                "The estate baseline expects it; without it this repository is " <>
                "unscanned for whatever that workflow covers, and its absence is " <>
                "silent — no job fails, because no job runs."
          }
        ]
      end
    end)
  end

  @doc """
  Check for GitHub Actions that use tag references instead of SHA pins.

  Delegates the regex detection to `WorkflowHardening.wh004_scan_content/2`
  (canonical per audit 2026-05-28 Part 3.1) and post-processes the
  canonical findings to add this module's extras: `pin_exempt?` carve-
  outs (hypatia#262 self-verifying-ref reusables) and `known_sha`
  hints from the `@known_good_shas` table.
  """
  def check_unpinned_actions(workflow_contents, opts \\ []) do
    case parse_actions_lock(Keyword.get(opts, :actions_lock)) do
      {:invalid, finding} ->
        # A present-but-invalid lock is one indivisible integrity failure. Do
        # not also recommend inline SHA pins: `gh actions-lock` deliberately
        # owns those symbolic refs, and replacing them inline can remove their
        # transitive dependency and repository-identity coverage.
        [finding]

      {:usable, lock} ->
        scan_unpinned_actions(workflow_contents, lock)
    end
  end

  defp scan_unpinned_actions(workflow_contents, lock) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      Hypatia.Rules.WorkflowHardening.wh004_scan_content(filename, content)
      |> Enum.flat_map(fn finding ->
        slug = finding.detail.uses

        # Reconstruct action_ref + ref from the canonical "slug"
        # (e.g. "owner/repo@vN.N.N") emitted by WH004.
        [action_ref, ref] =
          case String.split(slug, "@", parts: 2) do
            [_a, r] -> [slug, r]
            [_a] -> [slug, ""]
          end

        cond do
          # A repository under GitHub Actions lockfile enforcement pins its
          # actions in `.github/workflows/actions.lock`, NOT inline. The
          # lockfile resolves each symbolic ref to a verified commit — plus
          # owner_id, repo_id and the transitive dependencies of composite
          # actions, which an inline SHA cannot express at all. On such a
          # repository `actions/checkout@v7.0.1` IS pinned.
          #
          # ⚠ The two mechanisms are mutually exclusive, not additive.
          # `gh actions-lock` REFUSES a ref that no tag or branch contains,
          # so inline SHA-pinning an action DROPS it from the lockfile and
          # REDUCES coverage. Reporting these as unpinned drives exactly
          # that regression. Established 2026-08-07 on this repository:
          # inline-pinning 40 refs put 14 workflows into startup_failure
          # and removed dtolnay/rust-toolchain from 7 lockfile entries.
          Hypatia.Rules.ActionsLock.pinned?(lock, filename, slug) ->
            []

          Hypatia.Rules.SecurityErrors.pin_exempt?(slug) ->
            [
              %{
                type: :pin_exempt_accepted,
                file: filename,
                action_ref: slug,
                severity: :info,
                action: :accept_with_rationale,
                rationale: Hypatia.Rules.SecurityErrors.pin_exemption_reason(slug),
                reason:
                  "Action `#{slug}` in #{filename} is not SHA-pinned, and that is " <>
                    "ACCEPTED: #{String.trim_trailing(Hypatia.Rules.SecurityErrors.pin_exemption_reason(slug), ".")}. " <>
                    "Recorded so the exemption is visible rather than " <>
                    "indistinguishable from an unreviewed unpinned action."
              }
            ]

          true ->
            severity = if ref in ["main", "master"], do: :high, else: :medium

            [
              %{
                type: :unpinned_action,
                file: filename,
                action_ref: slug,
                severity: severity,
                action: :pin_sha,
                known_sha: Map.get(Hypatia.Rules.SecurityErrors.sha_pins(), action_ref),
                reason:
                  unpinned_action_message(
                    filename,
                    slug,
                    ref,
                    Map.get(Hypatia.Rules.SecurityErrors.sha_pins(), action_ref)
                  )
              }
            ]
        end
      end)
    end)
  end

  # The remediation text for an unpinned action. This used to be omitted
  # entirely, so `cli.ex` substituted the generated string
  # "Action <slug> needs attention" -- which names the problem without saying
  # what it is or what to do, and is what the GitHub alert displayed.
  # `known_sha` and the mutability of `ref` are both already known here.
  defp unpinned_action_message(filename, slug, ref, known_sha) do
    mutability =
      if ref in ["main", "master"] do
        "`#{ref}` is a BRANCH: whatever it points at today can be replaced by " <>
          "the action's owner at any time, including after review"
      else
        "`#{ref}` is a tag, and a tag can be moved to a different commit"
      end

    fix =
      case known_sha do
        sha when is_binary(sha) -> "Pin it to the estate-canonical `#{sha}`."
        _ -> "Pin it to a full 40-character commit SHA, with the version in a trailing comment."
      end

    "Action `#{slug}` in #{filename} is not pinned to a commit SHA — #{mutability}. #{fix}"
  end

  @doc """
  Compatibility accessor for dependency refs in a fully valid lockfile.

  Do not use this global set for authorization: only
  `Hypatia.Rules.ActionsLock.pinned?/3` also proves workflow association.
  """
  def locked_refs(nil), do: MapSet.new()

  def locked_refs(lock_content) when is_binary(lock_content) do
    case Hypatia.Rules.ActionsLock.parse(lock_content) do
      {:ok, lock} -> Hypatia.Rules.ActionsLock.dependency_refs(lock)
      {:error, _reason} -> MapSet.new()
    end
  end

  def locked_refs(_), do: MapSet.new()

  @doc """
  Normalise an action ref to the form the lockfile keys on: `owner/repo@ref`,
  lowercased. Subpath actions (`github/codeql-action/init@v4`) and reusable
  workflow refs collapse to their root repository, which is how
  `gh actions-lock` records them.
  """
  def normalise_ref(slug) do
    Hypatia.Rules.ActionsLock.normalise_ref(slug)
  end

  defp parse_actions_lock(nil), do: {:usable, nil}

  defp parse_actions_lock(content) do
    case Hypatia.Rules.ActionsLock.parse(content) do
      {:ok, lock} ->
        {:usable, lock}

      {:error, reason} ->
        {:invalid,
         %{
           type: :invalid_actions_lock,
           file: "actions.lock",
           severity: :high,
           action: :regenerate,
           detail:
             "Invalid .github/workflows/actions.lock: #{inspect(reason)}. " <>
               "Regenerate and verify it with gh actions-lock."
         }}
    end
  end

  @doc """
  Check for SHA pins that don't match the local known-good table.

  Severity is `:info`, not `:medium`: a SHA that's not in the local table is
  *unverified*, not *wrong*. The known-good table can't keep pace with the
  full dependency graph, so when this rule fires at medium it generates
  more noise than signal (see hypatia#237 triage — 148 of 225 findings).
  Only escalate manually when the current SHA is on a known-vulnerable list.
  """
  def check_wrong_pins(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      # Match uses: owner/repo@sha
      Regex.scan(~r/uses:\s*([^\s#]+)@([0-9a-f]{40})/m, content)
      |> Enum.flat_map(fn [_full, action, sha] ->
        # Check if we have a known-good SHA for any version of this action
        matching =
          Enum.find(Hypatia.Rules.SecurityErrors.sha_pins(), fn {ref, _} ->
            String.starts_with?(ref, action <> "@")
          end)

        case matching do
          {_ref, known_sha} when known_sha != sha ->
            [
              %{
                type: :wrong_sha_pin,
                file: filename,
                action_ref: action,
                current_sha: sha,
                expected_sha: known_sha,
                severity: :info,
                fix: :update_pin,
                reason:
                  "Action `#{action}` in #{filename} is pinned to `#{sha}`, but the " <>
                    "estate canon for it is `#{known_sha}`. A divergent pin is still a " <>
                    "pin, so nothing fails — the repository is simply running a " <>
                    "different revision from the rest of the fleet."
              }
            ]

          _ ->
            []
        end
      end)
    end)
  end

  @doc """
  Check for missing or overly broad permissions declarations.
  """
  def check_permissions(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      findings = []

      # Check for write-all (should be read-all + specific)
      findings =
        if String.contains?(content, "permissions: write-all") do
          [
            %{
              type: :broad_permissions,
              file: filename,
              detail:
                "permissions: write-all should be permissions: read-all with specific overrides",
              severity: :high,
              action: :narrow_permissions
            }
            | findings
          ]
        else
          findings
        end

      # Check for a missing permissions declaration at EITHER level.
      #
      # ⚠ This test was historically anchored at column 0 (`~r/^permissions:/m`),
      # so it could only see workflow-level blocks. A job-level `permissions:`
      # block REPLACES the workflow-level one, so a workflow that scopes every
      # job individually and omits the top-level block is correctly hardened —
      # yet it was reported as having no permissions at all. That false reading
      # is what drove remediation to rewrite the workflow level only, narrowing
      # `contents: write` on workflows whose jobs relied on it. See WH002 in
      # `workflow_hardening.ex` for the three probes that judgement requires.
      findings =
        if not Regex.match?(~r/^[ \t]*permissions:/m, content) do
          [
            %{
              type: :missing_permissions,
              file: filename,
              detail: "No permissions declaration -- add permissions: read-all",
              severity: :medium,
              action: :add_permissions
            }
            | findings
          ]
        else
          findings
        end

      # Check for missing SPDX header
      findings =
        if not String.contains?(content, "SPDX-License-Identifier:") do
          [
            %{
              type: :missing_spdx,
              file: filename,
              detail: "No SPDX-License-Identifier header",
              severity: :low,
              action: :add_spdx
            }
            | findings
          ]
        else
          findings
        end

      findings
    end)
  end

  @doc """
  Check for setup actions that could use built-in caching but don't.
  """
  def check_caching(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      # Patterns for setup actions that support 'cache:'
      cacheable_actions = [
        {~r/uses:\s*actions\/setup-node@v\d+/, "setup-node"},
        {~r/uses:\s*actions\/setup-python@v\d+/, "setup-python"},
        {~r/uses:\s*actions\/setup-go@v\d+/, "setup-go"},
        {~r/uses:\s*actions\/setup-java@v\d+/, "setup-java"},
        {~r/uses:\s*mlugg\/setup-zig@v\d+/, "setup-zig"}
      ]

      Enum.flat_map(cacheable_actions, fn {pattern, name} ->
        if Regex.match?(pattern, content) and not String.contains?(content, "cache:") do
          [
            %{
              type: :missing_caching,
              file: filename,
              detail: "#{name} missing built-in caching (e.g., cache: 'npm' or cache: true)",
              severity: :low,
              action: :enable_caching
            }
          ]
        else
          []
        end
      end)
    end)
  end

  @doc """
  Detect potential duplicate workflows (same content, different names).
  """
  def check_duplicates(_workflow_files, workflow_contents) when map_size(workflow_contents) < 2,
    do: []

  def check_duplicates(_workflow_files, workflow_contents) do
    # Group by content hash
    contents_list =
      Enum.map(workflow_contents, fn {name, content} ->
        {name, :erlang.md5(content)}
      end)

    contents_list
    |> Enum.group_by(fn {_, hash} -> hash end)
    |> Enum.flat_map(fn {_hash, entries} ->
      if length(entries) > 1 do
        names = Enum.map(entries, fn {name, _} -> name end)

        [
          %{
            type: :duplicate_workflow,
            files: names,
            severity: :low,
            action: :consolidate,
            reason:
              "Workflows #{Enum.join(names, ", ")} are byte-identical. Every push " <>
                "runs all of them, spending the minutes more than once, and a fix " <>
                "applied to one copy leaves the others stale. Consolidate to one."
          }
        ]
      else
        []
      end
    end)
  end

  # ─── WF013: npermissions typo ───────────────────────────────────────────

  @doc """
  WF013: Detect 'npermissions:' typo in workflow files.
  GitHub Actions silently ignores unknown top-level keys, so a typo like
  'npermissions:' means the workflow runs with overly broad default permissions.
  Severity: high.
  Action: rename to 'permissions:'.
  """
  def check_npermissions_typo(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      if Regex.match?(~r/^npermissions:/m, content) do
        [
          %{
            rule: "WF013",
            type: :npermissions_typo,
            file: filename,
            severity: :high,
            reason:
              "Workflow has 'npermissions' typo -- should be 'permissions'. GitHub Actions silently ignores this, running with overly broad defaults.",
            action: :fix_typo
          }
        ]
      else
        []
      end
    end)
  end

  @doc """
  Detect direct GitHub context interpolation inside run blocks.

  Only fires for **actor-controllable** context expressions — the values a
  forked-PR contributor or issue/comment author can choose:

    * `github.head_ref` / `github.event.pull_request.head.ref`
    * `github.event.pull_request.title|body`
    * `github.event.issue.title|body`
    * `github.event.comment.body` / `github.event.review.body`
    * `github.event.workflow_run.head_branch` / `display_title`
    * `github.event.commits[*].message|author.name|author.email`

  Admin-controlled context (`github.repository`, `github.sha`, `github.ref_name`
  on a push event from a privileged author, `vars.*`, etc.) is not an
  injection vector — it can only be set by repo owners and is constrained by
  GitHub. Interpolating those is style guidance, not a security finding, so
  it doesn't fire here.

  Note: `github.actor` is *only* attacker-controllable when the workflow runs
  on `pull_request_target` from a fork. Pre-2026-05 audit was conservative
  (always flagged); we narrow to the actor-controlled set to restore signal.
  """
  def check_github_context_in_run(workflow_contents) do
    # Actor-controllable expressions. Anchored loosely so we catch the
    # expression wherever it appears in a run block — the previous full
    # `run: |` block-shape match missed inline `run:` single-liners.
    # `[\s\S]*?` previously spanned step boundaries: a `run:` in one step and a
    # context expression in a LATER step's `env:` block matched as though the
    # value were interpolated into the script. Measured 2026-07-29 on
    # gitbot-fleet's repo-integrity-guard.yml -- flagged :critical although the
    # untrusted values are bound via `env:` and consumed as quoted shell vars
    # through `grep -F`, i.e. exactly the mitigation this rule asks for.
    #
    # `[^\n]` per line, and no line may start a new step (`- name:`/`- uses:`)
    # or open a sibling mapping key at step indent, so the match stays inside
    # one run block.
    run_context_re =
      ~r/run:(?:[^\n]*\n(?!\s*-\s+(?:name|uses|id):|\s{0,10}(?:env|with|if|name|uses):\s))*?[^\n]*\$\{\{\s*github\.(?:head_ref|event\.pull_request\.(?:title|body|head\.ref)|event\.issue\.(?:title|body)|event\.comment\.body|event\.review\.body|event\.workflow_run\.(?:head_branch|display_title)|event\.commits)[^}]*\}\}/m

    unsafe_json_payload_re =
      ~r/-d\s*".*\$\{\{\s*github\.(?:head_ref|event\.pull_request\.(?:title|body|head\.ref)|event\.issue\.(?:title|body)|event\.comment\.body|event\.review\.body|event\.workflow_run\.(?:head_branch|display_title)|event\.commits)/s

    Enum.flat_map(workflow_contents, fn {filename, content} ->
      content = strip_comments(content)
      findings = []

      findings =
        if Regex.match?(run_context_re, content) do
          [
            %{
              type: :actions_expression_injection,
              file: filename,
              detail:
                "GitHub context value is interpolated directly inside a run block. Move context into env and use jq/quoted variables.",
              severity: :critical,
              action: :sanitize_context
            }
            | findings
          ]
        else
          findings
        end

      findings =
        if Regex.match?(unsafe_json_payload_re, content) do
          [
            %{
              type: :unsafe_curl_payload,
              file: filename,
              detail:
                "curl JSON payload is assembled with inline GitHub context interpolation. Build payload with jq --arg.",
              severity: :high,
              action: :use_jq_payload
            }
            | findings
          ]
        else
          findings
        end

      findings
    end)
  end

  @doc """
  Detect download-and-execute shell patterns in workflow run blocks.

  Comment-aware: YAML comment lines (`^# ...`) and shell comments inside
  run blocks are stripped before matching, so a workflow that says
  `# safer than curl | sh` doesn't get flagged for the pattern in its own
  description (see hypatia#237 triage — docs.yml and mirror.yml both
  baselined this FP).
  """
  def check_download_then_run(workflow_contents) do
    download_then_run_re = ~r/\b(?:curl|wget)\b[^\n|;]*\|\s*(?:sh|bash)\b/

    Enum.flat_map(workflow_contents, fn {filename, content} ->
      stripped = strip_comments(content)

      if Regex.match?(download_then_run_re, stripped) do
        [
          %{
            type: :download_then_run,
            file: filename,
            detail:
              "Workflow executes remote script directly (curl/wget piped to shell). Download, verify checksum/signature, then execute.",
            severity: :high,
            action: :verify_download_integrity
          }
        ]
      else
        []
      end
    end)
  end

  @doc """
  Detect codeql.yml whose language matrix lists a CodeQL source-scanning
  language (`javascript-typescript`, `python`, `go`, etc.) when the repo
  has no source files in any such language.

  Caught 12 repos across the fleet at the time of writing — each one had
  `language: javascript-typescript` on a Julia / Rust / Coq / Idris repo,
  so the CodeQL `analyze` job exited "configuration error: no source
  files" on every Dependabot PR and blocked the merge.

  The `actions` language is always safe (scans workflow files which every
  repo has). Only the source-scanning languages need the repo-side
  guarantee.

  `opts[:has_codeql_supported_language]` — same boolean threaded through
  from `Hypatia.CLI.has_codeql_supported_language?/1`. When `false` and
  codeql.yml lists a scanning language, this rule fires.
  """
  def check_codeql_language_matrix_mismatch(workflow_contents, opts \\ []) do
    if Keyword.get(opts, :has_codeql_supported_language, true) do
      []
    else
      # Languages CodeQL scans by reading source files in the repo (as
      # opposed to `actions`, which scans workflow YAML).
      scanning_langs =
        ~w(javascript-typescript javascript typescript python go java ruby csharp c-cpp swift kotlin)

      Enum.flat_map(workflow_contents, fn {filename, content} ->
        if codeql_workflow?(filename) do
          Enum.flat_map(scanning_langs, fn lang ->
            if Regex.match?(~r/language:\s*#{Regex.escape(lang)}(?:\s|$)/m, content) do
              [
                %{
                  type: :codeql_language_matrix_mismatch,
                  file: filename,
                  detail:
                    "codeql.yml lists `language: #{lang}` but the repo has no source files in any CodeQL-scannable language. The analyze job will exit 'no source files' on every run. Switch the matrix to `actions` (which scans workflow files — every repo has those).",
                  severity: :high,
                  action: :switch_codeql_matrix_to_actions
                }
              ]
            else
              []
            end
          end)
        else
          []
        end
      end)
    end
  end

  defp codeql_workflow?(filename) do
    base = Path.basename(filename)
    base in ["codeql.yml", "codeql.yaml", "codeql-analysis.yml", "codeql-analysis.yaml"]
  end

  # ─── WF014: OSSF Scorecard publish-with-run job-shape violation ───────

  @doc """
  WF014: Detect a workflow job that runs `ossf/scorecard-action` with
  `publish_results: true` AND contains a `run:` step in the same job.

  The OSSF publish endpoint enforces a hard contract on the job that
  publishes:

      webapp: scorecard job must only have steps with uses

  Any `run:` step in the same job (typically a "Check minimum score"
  threshold gate, or an SBOM dump) causes the publish step to fail and
  the whole workflow run to be marked failed. Caught 49 estate repos on
  the 2026-05-30 audit.

  Fix recipe: split the threshold check into a downstream job that
  `needs: scorecard` and consumes the SARIF via `actions/upload-artifact`
  → `actions/download-artifact`. The publishing job stays `uses:`-only.

  Sensitivity / specificity:
    * Specific — only fires when ALL THREE markers are present in the
      same file: `uses: ossf/scorecard-action…`, `publish_results: true`,
      and at least one `run:` step. A job that omits `publish_results`
      or omits the `run:` step does not trigger the OSSF endpoint
      contract, so no finding.
    * Sensitive — file-level detection catches the canonical single-job
      `scorecard-enforcer.yml` shape and any clone of it; the multi-job
      variant (already-split) does not have `run:` in the same workflow
      file alongside `publish_results: true` *unless* the threshold gate
      is co-located, which is exactly the bug we want to flag.
  """
  def check_scorecard_publish_run_violation(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      stripped = strip_comments(content)

      uses_scorecard? = Regex.match?(~r/uses:\s*ossf\/scorecard-action@/, stripped)

      publish_true? = Regex.match?(~r/publish_results:\s*true/, stripped)

      # Only emit if the job that runs `ossf/scorecard-action` with `publish_results: true`
      # itself contains a `run:` step. We need to check per-job, not per-file.
      # Split content by job boundaries (job names start at column 0 or 2 spaces)
      # and find which job has the scorecard action with publish_results.

      # Find all job sections and check if the scorecard+publish job has run steps
      job_sections = Regex.split(~r/\n(?:^|  )[a-zA-Z_][a-zA-Z0-9_-]*:\s*\n/m, stripped)

      scorecard_job_has_run =
        Enum.any?(job_sections, fn section ->
          scorecard_in_job? = Regex.match?(~r/uses:\s*ossf\/scorecard-action@/, section)
          publish_in_job? = Regex.match?(~r/publish_results:\s*true/, section)

          if scorecard_in_job? and publish_in_job? do
            # Check if this same job section has a run: step
            Regex.match?(~r/^\s+- name:[^\n]*\n\s+run:/m, section) or
              Regex.match?(~r/^\s+- run:\s*[|>\n]/m, section) or
              Regex.match?(~r/^\s+run:\s*[|>]/m, section)
          else
            false
          end
        end)

      if uses_scorecard? and publish_true? and scorecard_job_has_run do
        [
          %{
            rule: "WF014",
            type: :scorecard_publish_with_run_step,
            file: filename,
            severity: :high,
            reason:
              "Job runs `ossf/scorecard-action` with `publish_results: true` AND " <>
                "contains a `run:` step. The OSSF publish endpoint enforces " <>
                "\"scorecard job must only have steps with uses\" — the run-step " <>
                "presence will fail the publish step and the whole workflow. " <>
                "Move any `run:` step (e.g. threshold gate) into a `needs: scorecard` " <>
                "downstream job that consumes the SARIF via upload/download-artifact.",
            action: :split_scorecard_publish_job
          }
        ]
      else
        []
      end
    end)
  end

  # ─── WF015: Non-root container image with actions/checkout EACCES ─────

  # Container images that default to a non-root user. `actions/checkout`'s
  # post-step writes save_state files to `/__w/_temp/_runner_file_commands`
  # — a runner-host path mounted into the container. Non-root containers
  # cannot write there and the checkout post-step fails with EACCES,
  # killing the job before any user step runs.
  #
  # The fix is `container.options: --user root` on the job, OR a
  # post-checkout chown step. The first is simpler and is the canonical
  # recipe; the in-container `root` user is still mapped to the runner
  # host's unprivileged runner user by the Actions sandbox.
  @nonroot_container_images [
    "coqorg/coq",
    "leanprover/lean4",
    "leanprover/elan",
    "makarius/isabelle",
    "haskell:",
    "haskell/cabal:",
    "rocker/r-",
    "jekyll/jekyll"
  ]

  @doc """
  WF015: Detect a workflow job whose `container.image:` is on the known
  non-root-user image list, WITHOUT `container.options: --user root` (or
  equivalent `--user 0`) and WITHOUT a post-checkout chown of `/__w/_temp`.

  Root cause: `actions/checkout` writes save_state files to
  `/__w/_temp/_runner_file_commands/save_state_<uuid>` as part of its
  post-step. Non-root container users (UID ≠ 0) cannot open that path
  for write, and Node.js throws EACCES before any user step runs.

  Caught ephapax `coq-build.yml` on the 2026-05-30 audit — the merge-
  oracle workflow itself couldn't run, defeating the formal/ hard-gate
  guarantee in `[[feedback_proof_pr_build_oracle_is_only_truth]]`.

  Sensitivity / specificity:
    * Specific — only fires for images on the well-known non-root-user
      list (coqorg/coq, leanprover/*, makarius/isabelle, etc.). Generic
      `ubuntu:22.04`, `alpine:latest`, `node:18`, `python:3.10`,
      `debian:bookworm` etc. all default to root and do not need the
      override, so they do NOT fire.
    * Sensitive — fires regardless of where the `--user root` would
      appear (anywhere in the same file's `container:` block).
  """
  def check_nonroot_container_checkout_eacces(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      stripped = strip_comments(content)

      uses_checkout? = Regex.match?(~r/uses:\s*actions\/checkout@/, stripped)

      has_user_root? = Regex.match?(~r/options:\s*[^\n]*--user\s+(?:root|0)\b/, stripped)

      if uses_checkout? and not has_user_root? do
        Enum.flat_map(@nonroot_container_images, fn img ->
          if Regex.match?(~r/image:\s*#{Regex.escape(img)}/, stripped) do
            [
              %{
                rule: "WF015",
                type: :nonroot_container_checkout_eacces,
                file: filename,
                severity: :critical,
                container_image: img,
                reason:
                  "Job uses container `#{img}*` (non-root default user) + " <>
                    "`actions/checkout` without `--user root`. The checkout " <>
                    "post-step writes to `/__w/_temp/_runner_file_commands` " <>
                    "as the runner host user; the container's non-root user " <>
                    "lacks write permission and the job dies with EACCES " <>
                    "before any user step runs. Add `container.options: " <>
                    "--user root` (or a post-checkout chown step).",
                action: :add_container_user_root
              }
            ]
          else
            []
          end
        end)
      else
        []
      end
    end)
  end

  # ─── WF016: Orphan reusable-workflow SHA pin ──────────────────────────

  # SHAs of `hyperpolymath/standards/.github/workflows/*-reusable.yml`
  # commits that have been REMOVED from main (force-pushed branch tip,
  # garbage-collected via repo rewrite, etc.). GitHub's read-only API
  # still resolves the blob content, so a caller pinning to one of these
  # SHAs appears to validate at code-review time — but the `workflow_call`
  # resolution at run-time refuses commits not reachable from any branch
  # HEAD and emits "workflow file issue" with zero jobs created.
  #
  # Audit reference:
  #   hyperpolymath/standards/docs/audits/audit-hypatia-pin-orphan-2026-05-27.adoc
  #
  # Append to this list when a fresh orphan is discovered. The detector
  # is data-driven so the rule does not need a code change to absorb a
  # newly-rotated reusable.
  @orphan_reusable_shas %{
    "hyperpolymath/standards/.github/workflows/hypatia-scan-reusable.yml" => [
      # 97df762... orphan — superseded by 915139d73560e65a8240b8fc7768698658502c89
      "97df7621",
      "97df76210d966e0e1a89d5e5e9bcf66e16cea5e8"
    ],
    "hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml" => [
      # 4fdf4314... orphan — superseded by cc5a372af1 (merge commit SHA)
      "4fdf4314",
      "4fdf43147f2ad6d96f9c849c2f81e3a5fad9a8c0"
    ]
  }

  @doc """
  WF016: Detect callers pinning to a known-orphan reusable-workflow SHA.

  Orphan SHA = a commit that was once the tip of `standards/main` but
  has since been removed from main (force-push, branch rewrite, GC).
  The `gh api repos/.../contents/...?ref=SHA` call still succeeds (read-
  only blob lookup is cache-friendly), so code-review tooling does not
  catch this. Run-time `workflow_call` resolution, on the other hand,
  requires the SHA to be reachable from a branch HEAD and refuses
  orphans — emitting "workflow file issue" and creating zero jobs.

  Audit recipe:
  `recipe-rewrite-orphan-reusable-pin` (gitbot-fleet script
  `rewrite-orphan-reusable-pin.sh` — auto-fixable; replaces the orphan
  SHA with the canonical merge-commit SHA published by the standards
  audit). Caught 178 estate repos on the 2026-05-30 sweep (95 hypatia-
  scan + 83 rust-ci).

  Sensitivity / specificity:
    * Specific — only fires on `uses: <owner>/<repo>/.github/workflows/
      <name>-reusable.yml@<orphan_sha>` where the owner/repo/name + SHA
      pair are in the maintained `@orphan_reusable_shas` map. Currency-
      checking is by static membership, not by API call, so the rule
      is safe under rate-limit pressure (callable across the estate
      without a quota hit). Both prefix (8-char) and full (40-char)
      SHAs match.
    * Sensitive — runs against the `uses:` line as it appears in YAML,
      so caller wrappers, fan-out templates, and one-off pins all match.
  """
  def check_orphan_standards_reusable_pin(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      stripped = strip_comments(content)

      Enum.flat_map(@orphan_reusable_shas, fn {reusable_path, orphan_shas} ->
        # Match: `uses: owner/repo/.github/workflows/x-reusable.yml@SHA`
        # Under either name of the standards repository (StandardsRepo).
        ref_re =
          ~r/uses:\s*#{Hypatia.Rules.StandardsRepo.path_regex_source(reusable_path)}@([0-9a-f]{8,40})/

        Regex.scan(ref_re, stripped)
        |> Enum.flat_map(fn [_full, sha] ->
          if Enum.any?(orphan_shas, fn orphan -> String.starts_with?(sha, orphan) end) do
            [
              %{
                rule: "WF016",
                type: :orphan_reusable_sha_pin,
                file: filename,
                reusable: reusable_path,
                current_sha: sha,
                severity: :critical,
                reason:
                  "`#{reusable_path}` is pinned at `@#{sha}` which is no longer " <>
                    "reachable from any branch in the standards repo. Code-review " <>
                    "tooling resolves the blob (cached) but `workflow_call` at " <>
                    "run-time refuses orphan SHAs, emitting \"workflow file issue\" " <>
                    "with zero jobs created. Re-pin to the canonical merge-commit " <>
                    "SHA from the standards audit.",
                action: :rewrite_orphan_reusable_pin
              }
            ]
          else
            []
          end
        end)
      end)
    end)
  end

  # ─── WF026: SHA-pinned dtolnay/rust-toolchain with no toolchain: input ──

  # The 40 hex characters must end the ref: `@<sha>-patched` is a tag that
  # merely begins with one, and a tag carries its own channel. Git resolves
  # a commit ID in either case, so upper-case hex is the same pin.
  # The owner/repository is matched in either case — GitHub resolves
  # `Dtolnay/Rust-Toolchain` to the same action — and the SHA as written.
  @toolchain_sha_pin ~r/^\s*(?:-\s+)?["']?uses["']?\s*:\s*(?:[&!]\S*\s+)*["']?(?i:dtolnay\/rust-toolchain)@([0-9a-fA-F]{40})(?=["'\s#,}\]]|$)/

  # The same key written inside a flow mapping on the line that opens it:
  # `- { uses: dtolnay/rust-toolchain@<sha> }`, `steps: [ { uses: ... } ]`.
  @toolchain_sha_pin_inline ~r/[{,]\s*["']?uses["']?\s*:\s*(?:[&!]\S*\s+)*["']?(?i:dtolnay\/rust-toolchain)@([0-9a-fA-F]{40})(?=["'\s#,}\]]|$)/

  @doc """
  WF026: a SHA-pinned `dtolnay/rust-toolchain` step that passes no
  `toolchain:` input.

  The action reads the toolchain from its `@`-ref (`@stable`, `@1.85.0`).
  A full-SHA ref carries no channel, so a SHA-pinned step with no
  `with: toolchain:` dies in its first step with
  `'toolchain' is a required input` — before any cargo command runs — and
  the red reads as a code failure. plasma-parser-writer's
  `rust-ci / Cargo check + clippy + fmt` was red this way from 2026-09-29
  to 2026-10-14 (plasma-parser-writer#105); standards' rust-ci-reusable.yml
  carried the defect from d73c93e0 to 49ed4715 (standards#439 fixed it).
  WF027 covers the CALLER side of that window; this rule is the defect
  itself, wherever it is written — a reusable, a plain workflow, a fork.

  Sensitivity / specificity:
    * Fires only on `uses: dtolnay/rust-toolchain@<40-hex>` whose step —
      the whole list item, from its opening `- ` line (a bare `-` on its own
      line opens a step too), so a `with:` written above `uses:` counts
      (YAML mapping keys carry no order) — has no `with:` mapping naming
      `toolchain:`, in block form (a direct child of `with:`) or flow form
      (`with: { toolchain: stable }`). A tag-pinned step carries its channel
      in the ref and never fires. Keys may be quoted (`"uses":`): the same
      key to GitHub, the same key here.
    * Only a `uses:` that is a KEY OF THE STEP MAPPING is the step's action.
      The same text nested under `env:`, inside a `run: |` script that
      writes a fixture, or under any other key is data: no finding, so the
      fleet is never sent to rewrite script text.
    * Only `with:` decides: `toolchain:` under `env:` or any other key is
      not an action input and does not rescue the step.
    * The step is bounded by indentation, so a later step's `toolchain:`
      cannot rescue an earlier step.
    * Severity is low and the finding is `advisory: true`: the rule reports
      and routes to the fleet; it is never a blocking gate (AGENTIC
      `must-not-promote-to-blocking`). The CLI never counts it toward exit 1
      and reports it below the threshold under `--advisory`
      (`Hypatia.CLI.report_sets/2`).
    * A KYAML workflow (standards YAML-POLICY Y-3, the output of
      `yq -o kyaml`) is judged as its block equivalent, line for line
      (`ResearchExtensions.flow_to_block/1`), so a quoted, braced step is
      the same step. A flow-style step written inline in a block document
      (`- { uses: ..., with: { toolchain: stable } }`,
      `steps: [ { uses: ... } ]`, or a `- {` item spread over lines) is read
      through its braces: its `with` entry decides, quotes and nested
      braces respected, and a `uses:` inside a quoted string is text. A
      workflow written in flow style on one line (`jobs: { build: { steps:
      [ { uses: ... } ] } }`, with or without a root `{ }`) is read the same
      way: the keys opening the brackets around the step must be `steps`
      in a job in `jobs`.
    * `with:` may be an anchor (`with: &inputs`, block or flow), an alias
      (`with: *inputs`) or carry a merge key (`<<: *inputs`): the anchored
      mapping is resolved from the document — as YAML resolves an alias, to
      the latest `&inputs` before it — and its direct entries decide. An
      alias with no anchor before it in the file cannot be read; the step is
      reported so, not passed silently.
    * A `uses:` whose value YAML continues below its key (`- uses:`, then —
      past any blank or comment-only lines — the reference on a deeper line)
      is the step's key all the same.

  Auto-fixable: gitbot-fleet `scripts/fix-rust-toolchain-input.sh` inserts
  `toolchain: stable` (the channel the estate's reusables pass) into a
  block-style step whose `with:` is absent, a block mapping or a flow
  mapping. The fixer refuses every other shape rather than guess (exit 2,
  file untouched), so a finding on a KYAML document, a flow-style step, a
  `uses:` value continued below its key, or a `with:` that goes through an
  anchor or alias carries `auto_fixable: false` and routes to review instead
  of to the fixer.
  """
  def check_sha_pinned_toolchain_without_input(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      # KYAML is read as the block layout it denotes; line numbers survive.
      normalised = Hypatia.Rules.ResearchExtensions.flow_to_block(content)
      kyaml? = normalised != content
      lines = String.split(normalised, "\n")
      continued = wf_continued_uses(lines)
      # A step inside an open flow collection is a flow-style step even when
      # its `{` stands alone on a line above (`steps: [`, `{`, `uses: ...`).
      flow = wf_flow_lines(normalised)
      # A step given whole as an alias (`- *step`, `steps: [*step]`) runs the
      # mapping its anchor holds, wherever that mapping sits (a matrix entry):
      # its `uses:` line is a live step's, judged as one, and routes to
      # review — the fixer does not rewrite anchored steps.
      aliased =
        for %{key_line: n} <- Hypatia.Rules.PinSites.node_alias_sites(normalised),
            into: MapSet.new(),
            do: n

      lines
      |> Enum.with_index(1)
      |> Enum.flat_map(fn {line, number} ->
        # `- uses:` with the reference below it, or `- uses: *name` naming an
        # anchored one, is read as the one-line key it stands for.
        {line, resolved} =
          case Map.fetch(continued, number) do
            {:ok, {joined, how}} -> {joined, how}
            :error -> {line, nil}
          end

        live? = number in aliased

        case Regex.run(@toolchain_sha_pin, line) do
          [_full, sha] ->
            # The fleet fixer matches the identity in lower case only, and a
            # value on its key's line only; any other spelling or layout is
            # reported for review rather than handed to it.
            review =
              cond do
                kyaml? -> :kyaml
                resolved != nil -> resolved
                live? -> :node_alias
                wf_node_properties?(line) -> :properties
                not String.contains?(line, "dtolnay/rust-toolchain@") -> :spelling
                true -> nil
              end

            if wf_in_flow?(flow, number),
              do: wf026_flow_line_finding(filename, lines, flow, number, sha, review, 0, live?),
              else: wf026_block_step_finding(filename, lines, line, number, sha, review, live?)

          _ ->
            # Several flow-style steps may share one physical line
            # (`steps: [ { uses: ... }, { uses: ... } ]`): each is judged.
            @toolchain_sha_pin_inline
            |> Regex.scan(line, return: :index)
            |> Enum.flat_map(fn [{at, _}, {sha_at, 40}] ->
              sha = binary_part(line, sha_at, 40)

              review =
                cond do
                  kyaml? -> :kyaml
                  live? -> :node_alias
                  true -> nil
                end

              # On a line inside a flow collection opened above, the step's
              # ancestry is in the brackets, not the line's indentation.
              if wf_in_flow?(flow, number) do
                wf026_flow_line_finding(filename, lines, flow, number, sha, review, at + 1, live?)
              else
                wf026_inline_step_finding(
                  filename,
                  lines,
                  line,
                  number,
                  at + 1,
                  sha,
                  review,
                  live?
                )
              end
            end)
        end
      end)
    end)
  end

  # The block-style path: `uses:` is a key on its own line, outside any flow
  # collection (one inside is read by `wf026_flow_line_finding/7`). A step
  # whose opener is `- {` is a flow-style step: the fixer refuses it.
  defp wf026_block_step_finding(filename, lines, line, number, sha, review, live?) do
    {step, key_indent, opener} = wf026_step(lines, number)
    flow_item? = Regex.match?(~r/^\s*-\s+\{/, Enum.at(lines, opener - 1))

    cond do
      # `uses:` nested under `env:`, inside a `run: |` script or under
      # any other key is data, not the step's action.
      wf026_key_indent(line) != key_indent ->
        []

      # A quoted scalar the step opened above (`- run: "echo start`) and has
      # not closed by this line makes the `uses:` line part of the string.
      # The scan starts at the step's opener or, where the string opens on
      # the line that holds the step (`steps: [{"run":"echo start`, no list
      # item of its own), at the line's YAML parent.
      wf_in_open_quote?(lines, min(opener, wf_parent_line(lines, number)), number) ->
        []

      # A list item is a step only under `steps:`: one inside a block
      # scalar (a fixture kept in `env:`, a `run: |` script) is text. A
      # mapping a step alias runs (`live?`) is a step wherever it sits.
      not live? and not wf026_under_steps?(lines, opener) ->
        []

      true ->
        case wf026_with(step, key_indent, lines, opener) do
          {:toolchain, _form} ->
            []

          verdict ->
            form = if flow_item?, do: :flow_step, else: elem(verdict, 1)
            [wf026_finding(filename, number, sha, verdict, form, review)]
        end
    end
  end

  # The inline path: `uses:` sits inside a flow mapping on the line that
  # opens it. The mapping is read through its braces (quote-aware, across
  # lines) and its `with` entry decides.
  defp wf026_inline_step_finding(filename, lines, line, number, uses_at, sha, review, live?) do
    with {:ok, open_col} <- wf_flow_opener_before(line, uses_at),
         true <- live? or wf026_inline_step_at?(lines, line, number, open_col),
         {:ok, interior, _} <- wf_flow_interior(lines, number, open_col) do
      # Where the step's `with` value stands, line and column: an alias
      # there names the latest anchor before it, not one later on its line.
      pos =
        case wf_flow_entry_value_at(interior, "with") do
          nil -> {number, :eol}
          at -> wf_interior_pos(interior, at, number, open_col)
        end

      case wf026_with_entry(wf_flow_entries(interior), lines, pos) do
        {:toolchain, _} -> []
        verdict -> [wf026_finding(filename, number, sha, verdict, :flow_step, review)]
      end
    else
      _ -> []
    end
  end

  # A `uses:` key inside a flow collection opened on an earlier line —
  # KYAML-like layout, indented or not (`steps: [`, `{`, `uses: ...`) — at
  # the line's start, or at byte `at` of it (`{ uses: ...`, `name: x,
  # uses: ...`). Its ancestry is the brackets still open before it
  # (`wf_flow_context/4`), not its indentation: it is a step's own key when
  # the innermost one is a mapping that is an item of the `[` a `steps:` in
  # a job opens, and the step is read through that mapping's braces.
  defp wf026_flow_line_finding(filename, lines, flow, number, sha, review, at, live?) do
    with {:ok, keys, kinds, {brace_line, brace_col}} <- wf_flow_context(lines, flow, number, at),
         true <- live? or match?([nil, "steps", job, "jobs" | _] when is_binary(job), keys),
         # The `{` is an item of the `[` that `steps:` opens, or of the
         # block sequence under it (`- {` on a line of its own); a mapping
         # a step alias runs is the step wherever it sits.
         true <-
           match?([:map, :seq | _], kinds) or kinds == [:map] or
             (live? and match?([:map | _], kinds)),
         {:ok, interior, _} <- wf_flow_interior(lines, brace_line, brace_col) do
      pos =
        case wf_flow_entry_value_at(interior, "with") do
          nil -> {brace_line, :eol}
          at -> wf_interior_pos(interior, at, brace_line, brace_col)
        end

      case wf026_with_entry(wf_flow_entries(interior), lines, pos) do
        {:toolchain, _} -> []
        verdict -> [wf026_finding(filename, number, sha, verdict, :flow_step, review)]
      end
    else
      _ -> []
    end
  end

  # Is the flow mapping opening at `open_col` a step? Either it sits in a
  # sequence position of a block document (below), or the line itself
  # writes the path to it in flow style.
  defp wf026_inline_step_at?(lines, line, number, open_col) do
    (wf026_inline_item?(line, open_col) and
       wf026_inline_under_steps?(lines, line, number, open_col)) or
      wf026_flow_path_step?(lines, line, number, open_col)
  end

  # A workflow written in flow style on one line — `jobs: { build: { steps:
  # [ { uses: ... } ] } }`, the same under a root `{ ... }`, or a job line
  # `build: { steps: [ ... ] }` under a block `jobs:`. The mapping must sit
  # directly in a `[` that `steps:` opens, in a mapping a job key opens, in
  # the one `jobs:` opens: the keys that open the brackets still unclosed on
  # the line, nearest first, then the block ancestors of the line. Each key
  # is read quote-aware, so a `name: "CI, build"` sibling separates nothing.
  defp wf026_flow_path_step?(lines, line, number, open_col) do
    case {wf_flow_opener_stack(line, open_col), wf_ancestor_keys(lines, number)} do
      {{:ok, [{:seq, _} | _] = stack}, ancestors} when is_list(ancestors) ->
        keys = wf_flow_opener_keys(line, stack) ++ ancestors
        match?(["steps", job, "jobs" | _] when is_binary(job), keys)

      _ ->
        false
    end
  end

  # The key that opens each bracket of an opener stack (innermost first), or
  # nil for a bracket no key opens (a root `{`, a `{` item in a sequence).
  defp wf_flow_opener_keys(line, stack) do
    cols =
      Enum.map(stack, fn
        {:seq, col} -> col
        col -> col
      end)

    froms = cols |> Enum.drop(1) |> Enum.map(&(&1 + 1)) |> Kernel.++([0])

    Enum.zip_with(cols, froms, fn col, from -> wf027_key_before(line, from, col) end)
  end

  # The text before the mapping's `{` must be a sequence position: a list
  # dash, the `steps:` key opening its sequence, a `[`, a comma, or nothing.
  # Anything else (`run: "echo { uses: ... }"`) is a scalar carrying braces.
  # An earlier sibling on the same line (`[ { uses: a }, { uses: b } ]`) is
  # a closed mapping before this one; it is removed before the test.
  # Node properties may stand before the mapping or the sequence it sits in
  # (`- &step {`, `steps: &s [ &step {`): they name the node, not a key.
  defp wf026_inline_item?(line, open_col) do
    prefix =
      line
      |> binary_part(0, open_col)
      |> wf_drop_closed_mappings()
      |> String.replace(~r/(^|[\s\[{,:-])[&!]\S*(?=\s)/, "\\1")

    Regex.match?(~r/^\s*(?:-\s*)?(?:["']?steps["']?\s*:\s*)?[\[,\s]*$/, prefix)
  end

  defp wf_drop_closed_mappings(text) do
    case Regex.replace(~r/\{[^{}]*\}/, text, "") do
      ^text -> text
      shorter -> wf_drop_closed_mappings(shorter)
    end
  end

  defp wf026_inline_under_steps?(lines, line, number, open_col) do
    prefix = binary_part(line, 0, open_col)

    if Regex.match?(~r/["']?steps["']?\s*:/, prefix) do
      # The line is the `steps:` key itself; it belongs to a job.
      match?([_job, "jobs" | _], wf_ancestor_keys(lines, number) || [])
    else
      wf026_under_steps?(lines, number)
    end
  end

  # The finding. Only a plain block-style step is handed to the fixer:
  # the fleet script refuses flow-style steps, anchors and aliases (exit
  # 2, file untouched), and a KYAML document is a flow-style document.
  # `review` says why a finding must route to review whatever its form:
  # :kyaml (a flow-style document) or :spelling (the action named in a case
  # the fixer does not match); nil when only the form decides.
  defp wf026_finding(filename, number, sha, verdict, form, review) do
    repairable? = review == nil and form in [:none, :block, :flow]

    shape =
      case {verdict, form} do
        {{:unreadable, :alias, name}, _} ->
          " Its `with:` is the alias `*#{name}` and no `&#{name}` anchor is defined before " <>
            "it in the file, so the inputs cannot be read; the alias must be resolved by hand."

        {{:unreadable, :toolchain_alias, name}, _} ->
          " Its `toolchain:` input is the alias `*#{name}`, and no `&#{name}` scalar this " <>
            "reader can read is defined before it, so whether the action receives a channel " <>
            "cannot be told; the alias must be checked by hand."

        {_, :flow_step} ->
          " The step is written in flow style; the repair script rewrites block-style " <>
            "steps only, so this one routes to review."

        {_, f} when f in [:anchor, :alias, :merge] ->
          " Its `with:` goes through a YAML anchor or alias; the repair script does not " <>
            "rewrite anchored mappings, so this one routes to review."

        _ when review == :kyaml ->
          " The workflow is KYAML; the repair script rewrites block-style steps only, " <>
            "so this one routes to review."

        _ when review == :properties ->
          " The `uses:` value carries a YAML anchor or tag; the repair script reads a " <>
            "bare reference only, so this one routes to review."

        _ when review == :continued ->
          " The `uses:` value is continued below its key; the repair script reads a " <>
            "reference on the key's own line only, so this one routes to review."

        _ when review == :node_alias ->
          " The step is given as an alias (`- *name`) of a mapping anchored elsewhere; " <>
            "the repair script does not rewrite anchored steps, so this one routes to review."

        _ when review == :alias ->
          " The `uses:` value is an alias of an anchored reference; the repair script " <>
            "does not rewrite anchored values, so this one routes to review."

        _ when review == :spelling ->
          " The action is not spelled `dtolnay/rust-toolchain`; GitHub resolves the name in " <>
            "either case, but the repair script matches it as written in lower case, so this " <>
            "one routes to review."

        _ ->
          ""
      end

    %{
      rule: "WF026",
      type: :sha_pinned_toolchain_without_input,
      file: filename,
      line: number,
      current_sha: sha,
      # Report and route, never block (AGENTIC `must-not-promote-to-blocking`):
      # `:low` (rank 4) sits below the CLI's default `--severity medium` and
      # the reusable's high/critical threshold, as `flawed_regex` does. The
      # fleet routing reads `fix_script`/`auto_fixable`, not the severity.
      # `advisory: true` keeps it out of the CLI's exit code at any
      # threshold and lets `--advisory` report it below one.
      severity: :low,
      advisory: true,
      auto_fixable: repairable?,
      fix_script: if(repairable?, do: "fix-rust-toolchain-input.sh", else: nil),
      reason:
        "`dtolnay/rust-toolchain` is pinned at `@#{sha}` with no `toolchain:` " <>
          "input. The action reads the channel from its `@`-ref; a commit SHA " <>
          "carries none, so the step exits 1 with \"'toolchain' is a required " <>
          "input\" before any cargo command runs and the red reads as a code " <>
          "failure. Add `with: toolchain: stable` (or the channel you mean)." <> shape,
      action: :add_toolchain_input
    }
  end

  # Indentation width of a line (spaces only; workflow YAML never uses tabs).
  defp wf026_indent(line), do: String.length(line) - String.length(String.trim_leading(line, " "))

  # A line belongs to the step opened at `step_indent` until the next list item
  # at that indentation or shallower, or a shallower key. Blank lines and
  # comment lines belong to whatever step they sit in.
  defp wf026_step_line?(line, step_indent) do
    trimmed = String.trim(line)

    cond do
      trimmed == "" -> true
      String.starts_with?(trimmed, "#") -> true
      Regex.match?(~r/^\s*-(?:\s|$)/, line) and wf026_indent(line) <= step_indent -> false
      wf026_indent(line) < step_indent -> false
      true -> true
    end
  end

  defp wf026_plain?(line) do
    trimmed = String.trim(line)
    trimmed != "" and not String.starts_with?(trimmed, "#")
  end

  # The whole list item the `uses:` line at `number` (1-based) belongs to:
  # from its opening `- ` line — which may sit above `uses:`, since YAML
  # mapping keys carry no order, and may be a bare `-` on its own line — to
  # the next list item at that indentation or shallower, or a shallower key.
  # Returns the step's lines and the column of its keys, so a caller can tell
  # a key of the step mapping from the same text nested under another key.
  defp wf026_step(lines, number) do
    uses_line = Enum.at(lines, number - 1)
    uses_indent = wf026_indent(uses_line)

    {opener, step_indent} =
      if Regex.match?(~r/^\s*-(?:\s|$)/, uses_line) do
        {number, uses_indent}
      else
        lines
        |> Enum.take(number - 1)
        |> Enum.with_index(1)
        |> Enum.reverse()
        |> Enum.find(fn {l, _} ->
          Regex.match?(~r/^\s*-(?:\s|$)/, l) and wf026_indent(l) < uses_indent
        end)
        |> case do
          {l, i} -> {i, wf026_indent(l)}
          nil -> {number, uses_indent}
        end
      end

    rest =
      lines
      |> Enum.drop(opener)
      |> Enum.take_while(&wf026_step_line?(&1, step_indent))

    step = [Enum.at(lines, opener - 1) | rest]
    {step, wf026_step_key_indent(step), opener}
  end

  # A list item is a step only when its parent key is a job's `steps:`, or
  # a composite action's under `runs:`. An item inside `run: |`, inside a
  # fixture kept in `env:`, or of a `steps:` some other mapping holds (a
  # matrix entry's `fixture: { steps: ... }`) is data however much it looks
  # like a step, and must never send the fleet to rewrite it. A `steps:`
  # with nothing above it (a fragment) is still read as a step list.
  defp wf026_under_steps?(lines, opener) do
    case wf_ancestor_keys(lines, opener) do
      ["steps"] -> true
      ["steps", "runs" | _] -> true
      ["steps", job, "jobs" | _] when is_binary(job) -> true
      _ -> false
    end
  end

  # The mapping keys enclosing the line at `number`, nearest first — list
  # items are nodes, not keys, and are skipped — or nil when an ancestor
  # opens a block scalar: the line is then text, not YAML structure. The
  # parent of a list item may sit at the item's own indentation (YAML's
  # `steps:\n- uses:` shape); every further ancestor is strictly shallower.
  defp wf_ancestor_keys(lines, number) do
    line = Enum.at(lines, number - 1)
    indent = wf026_indent(line)
    list_item? = Regex.match?(~r/^\s*-(?:\s|$)/, line)
    limit = if list_item?, do: indent, else: indent - 1

    lines
    |> Enum.take(number - 1)
    |> Enum.reverse()
    |> Enum.filter(&wf026_plain?/1)
    |> wf_walk_up(limit, list_item?, [])
  end

  defp wf_walk_up([], _limit, _sibling_ok?, keys), do: Enum.reverse(keys)

  defp wf_walk_up([line | rest], limit, sibling_ok?, keys) do
    indent = wf026_indent(line)
    list_item? = Regex.match?(~r/^\s*-(?:\s|$)/, line)

    cond do
      indent > limit ->
        wf_walk_up(rest, limit, sibling_ok?, keys)

      # A sibling item of the one being placed.
      list_item? and sibling_ok? and indent == limit ->
        wf_walk_up(rest, limit, sibling_ok?, keys)

      # A flow closer (`}`, `],`) on its own line ends a collection some
      # earlier line opened; it names nothing and bounds nothing.
      Regex.match?(~r/^\s*[}\]]/, line) ->
        wf_walk_up(rest, limit, sibling_ok?, keys)

      Regex.match?(~r/:\s*[|>][-+0-9]*\s*(?:#.*)?$/, line) ->
        nil

      # A list item, or a `{`/`[` opening a flow item on its own line, is a
      # node, not a key.
      list_item? or Regex.match?(~r/^\s*[{\[]/, line) ->
        wf_walk_up(rest, indent - 1, false, keys)

      true ->
        wf_walk_up(rest, indent - 1, false, [wf_key_name(line) | keys])
    end
  end

  # The flow depth and the structure of each line of a file
  # (`PinSites.line_depths/1`, `PinSites.structure_lines/1`), read once.
  defp wf_flow_lines(content) do
    %{
      depths: content |> Hypatia.Rules.PinSites.line_depths() |> List.to_tuple(),
      text: content |> Hypatia.Rules.PinSites.structure_lines() |> List.to_tuple()
    }
  end

  defp wf_in_flow?(flow, number), do: wf_flow_at(flow.depths, number, 0) > 0

  defp wf_flow_at(tuple, number, default) do
    if number >= 1 and number <= tuple_size(tuple), do: elem(tuple, number - 1), else: default
  end

  # For line `number` inside a flow collection — its depth at the start
  # (`PinSites.line_depths/1`) above 0 — read up to byte `at` of it, the
  # position judged: `{:ok, keys, kinds, {line, col}}`, the keys opening the
  # brackets still open there, innermost first (nil for one no key opens, a
  # `{` item of a sequence), then the block ancestors of the line that
  # opened the outermost; each bracket's kind (:map for `{`, :seq for `[`);
  # and where the innermost one stands. Read from the structure of the lines
  # since that one (`PinSites.structure_lines/1`: comments cut and quoted
  # scalars blanked across lines, so a `# world"` continuing a string is
  # text), so a collection spread over lines is one however its lines are
  # indented — column 0 included. nil at depth 0, past the line's comment,
  # or when that text cannot be read.
  defp wf_flow_context(lines, flow, number, at \\ 0) do
    start =
      if number > 1,
        do: Enum.find((number - 1)..1//-1, &(wf_flow_at(flow.depths, &1, 0) == 0))

    here = wf_flow_at(flow.text, number, "")

    with true <- wf_in_flow?(flow, number),
         s when is_integer(s) <- start,
         true <- at <= byte_size(here),
         # The key itself must be structure: a `uses:` the structure view
         # blanks is inside a quoted scalar continued from above.
         true <-
           Regex.match?(
             ~r/^\s*(?:-\s+)?["']?uses["']?\s*:/,
             binary_part(here, at, byte_size(here) - at)
           ),
         text =
           Enum.map_join(s..(number - 1), "\n", &wf_flow_at(flow.text, &1, "")) <>
             "\n" <> binary_part(here, 0, at),
         {:ok, [inner | _] = stack} <- wf_flow_opener_stack(text, byte_size(text)) do
      keys = wf_flow_opener_keys(text, stack) ++ (wf_ancestor_keys(lines, s) || [])
      kinds = Enum.map(stack, &if(is_integer(&1), do: :map, else: :seq))
      at = if is_integer(inner), do: inner, else: elem(inner, 1)
      {:ok, keys, kinds, wf_interior_pos(text, at, s, -1)}
    else
      _ -> nil
    end
  end

  defp wf_key_name(line) do
    line
    |> String.replace(~r/^\s*(?:-\s+)?/, "")
    |> String.replace(~r/\s*:.*$/, "")
    |> String.trim("\"")
    |> String.trim("'")
  end

  # The column of the step's keys: the opener's key (`- uses:` -> the column
  # of `uses`), or for a bare `-` — or a `- {` opening a flow-style item — the
  # indentation of the first plain line under it.
  defp wf026_step_key_indent([opener | rest]) do
    cond do
      Regex.match?(~r/^\s*-\s+\S/, opener) and not Regex.match?(~r/^\s*-\s+\{\s*$/, opener) ->
        wf026_key_indent(opener)

      Regex.match?(~r/^\s*-\s*(?:\{\s*)?$/, opener) ->
        case Enum.find(rest, &wf026_plain?/1) do
          nil -> wf026_indent(opener) + 2
          l -> wf026_indent(l)
        end

      true ->
        wf026_indent(opener)
    end
  end

  # What the step's `with:` says about `toolchain:`, as a verdict:
  # `{:toolchain, form}`, `{:missing, form}` or `{:unreadable, :alias, name}`,
  # where form is :none (no `with:`), :block, :flow, :anchor (`with: &x`),
  # :alias (`with: *x`) or :merge (a `<<: *x` child). Only a `with:` at the
  # step's key column is the step's own; `toolchain:` under `env:` or any
  # other key is not an action input.
  # `step` starts at line `opener`, so its line i is line `opener + i`: where
  # an alias in `with:` stands, which decides the anchor it names.
  defp wf026_with(step, key_indent, lines, opener) do
    step
    |> Enum.with_index()
    |> Enum.find_value({:missing, :none}, fn {line, i} ->
      if wf026_key_indent(line) == key_indent do
        case Regex.run(~r/^\s*(?:-\s+)?["']?with["']?\s*:(.*)$/, line) do
          [_, value] ->
            value = value |> wf_strip_comment() |> String.trim()
            wf026_with_value(value, line, Enum.drop(step, i + 1), key_indent, lines, opener + i)

          nil ->
            nil
        end
      end
    end)
  end

  defp wf026_with_value(value, line, rest, key_indent, lines, number) do
    cond do
      # `with: *inputs` — the mapping the latest `&inputs` before it anchors.
      match?([_ | _], Regex.run(~r/^\*([^\s,\[\]{}]+)$/, value)) ->
        [_, name] = Regex.run(~r/^\*([^\s,\[\]{}]+)$/, value)
        [{col, _}] = Regex.run(~r/\*/, line, return: :index)
        wf026_alias_verdict(name, lines, :alias, {number, col})

      # `with: {` / `with: &inputs {` — a flow mapping, read to its brace.
      String.contains?(value, "{") ->
        form = if wf026_anchored?(value), do: :anchor, else: :flow
        [{at, _}] = Regex.run(~r/\{/, line, return: :index)

        case wf_flow_interior([line | rest], 1, at) do
          {:ok, interior, _} ->
            wf026_entries_verdict(wf_flow_entries(interior), lines, form, {number, :eol})

          :error ->
            {:missing, form}
        end

      # `with:` / `with: &inputs` — a block mapping: only a DIRECT child is
      # an input; the contents of another input's block scalar
      # (`components: |` carrying `toolchain: stable`) sit deeper and are text.
      true ->
        form = if wf026_anchored?(value), do: :anchor, else: :block
        pos = {:block, number, wf_block_child_lines(rest, key_indent, number)}
        wf026_entries_verdict(wf_block_children(rest, key_indent), lines, form, pos)
    end
  end

  # Does a `with:` value carry an anchor? Node properties come in either
  # order: `&inputs`, `&inputs !!map`, `!!map &inputs`.
  defp wf026_anchored?(value), do: Regex.match?(~r/^(?:!\S*\s+)*&/, value)

  # The `with` entry of a flow-style step's own entries; `pos` is where its
  # value stands, which decides the anchor an alias there names.
  defp wf026_with_entry(entries, lines, pos) do
    case List.keyfind(entries, "with", 0) do
      nil ->
        {:missing, :none}

      {_, value} ->
        value = String.trim(value)

        cond do
          match?([_ | _], Regex.run(~r/^\*([^\s,\[\]{}]+)$/, value)) ->
            [_, name] = Regex.run(~r/^\*([^\s,\[\]{}]+)$/, value)
            wf026_alias_verdict(name, lines, :alias, pos)

          String.contains?(value, "{") ->
            form = if wf026_anchored?(value), do: :anchor, else: :flow
            [{at, _}] = Regex.run(~r/\{/, value, return: :index)

            case wf_flow_interior([value], 1, at) do
              {:ok, interior, _} ->
                wf026_entries_verdict(wf_flow_entries(interior), lines, form, pos)

              :error ->
                {:missing, form}
            end

          true ->
            {:missing, :flow}
        end
    end
  end

  # Does a list of `{key, value}` entries name a toolchain? A `<<: *x` merge
  # key brings the anchored mapping's entries in, as does each alias of a
  # `<<: [*base, *rust]` sequence, but an explicit key wins over a merged one
  # (YAML merge semantics): `toolchain:` written empty beside `<<: *inputs`
  # is the empty value, whatever the anchor names. A merge this reader does
  # not resolve (`<<: { ... }`, a block sequence) leaves the inputs unknown:
  # the finding routes to review (`:merge`), never to the fixer.
  # The key present with no value (`toolchain:`, `null`, `~`) is not an input
  # the action can read; the fleet fixer fills such a key in place. `pos`
  # (`{line, column}`, column `:eol` for the whole line) is where the
  # mapping stands: an alias in it names the latest anchor before it.
  defp wf026_entries_verdict(entries, lines, form, pos, seen \\ []) do
    case wf026_toolchain_in(entries, lines, seen, pos) do
      {:set, :own} -> {:toolchain, form}
      {:set, :merge} -> {:toolchain, :merge}
      :null -> {:missing, form}
      :absent -> {:missing, form}
      :unknown -> {:missing, :merge}
      {:unreadable, _, _} = unreadable -> unreadable
    end
  end

  # Where a mapping's `toolchain` comes from, under YAML merge semantics: its
  # own key first; else each merged mapping in order, the first that defines
  # the key deciding — an explicit `toolchain: null` in `*empty` of
  # `<<: [*empty, *rust]` is the input, `*rust`'s value is not. Returns
  # {:set, :own | :merge}, :null, :absent, :unknown (a merge this reader
  # does not resolve) or {:unreadable, :alias, name}.
  defp wf026_toolchain_in(entries, lines, seen, pos) do
    case Enum.find(entries, fn {k, _} -> k == "toolchain" end) do
      {_, v} when is_binary(v) ->
        case Regex.run(~r/^\s*\*([^\s,\[\]{}]+)\s*$/, v) do
          [_, name] -> wf026_scalar_alias(name, lines, wf_entry_pos(pos, "toolchain"))
          nil -> if wf026_null_input?(v), do: :null, else: {:set, :own}
        end

      _ ->
        entries
        |> Enum.filter(fn {k, _} -> k == "<<" end)
        |> Enum.find_value(:absent, fn {_, v} ->
          case wf026_merge_aliases(v) do
            {:ok, names} ->
              Enum.find_value(names, fn name ->
                case wf026_alias_lookup(name, lines, seen, wf_entry_pos(pos, "<<")) do
                  :absent -> nil
                  {:set, _} -> {:set, :merge}
                  other -> other
                end
              end)

            :error ->
              :unknown
          end
        end)
    end
  end

  defp wf026_alias_lookup(name, lines, seen, pos) do
    if name in seen do
      {:unreadable, :alias, name}
    else
      case wf_anchor_entries(lines, name, pos) do
        {:ok, entries, def_pos} -> wf026_toolchain_in(entries, lines, [name | seen], def_pos)
        :error -> {:unreadable, :alias, name}
      end
    end
  end

  # The aliases a merge key's value names: `*x`, or a flow sequence of
  # aliases `[*a, *b]`, in order; :error for anything else.
  defp wf026_merge_aliases(value) do
    v = String.trim(value)

    cond do
      m = Regex.run(~r/^\*([^\s,\[\]{}]+)$/, v) ->
        {:ok, [Enum.at(m, 1)]}

      m = Regex.run(~r/^\[\s*(\*[^\s,\[\]{}]+(?:\s*,\s*\*[^\s,\[\]{}]+)*)\s*,?\s*\]$/, v) ->
        names =
          m
          |> Enum.at(1)
          |> String.split(",")
          |> Enum.map(&(&1 |> String.trim() |> String.trim_leading("*")))

        {:ok, names}

      true ->
        :error
    end
  end

  # Where an entry's value stands, for an alias in it: a block mapping's
  # position (`{:block, line, %{key => line}}`) is each child's own line —
  # `source: &channel nightly` then `toolchain: *channel` names the anchor
  # one line above it — any other position is the mapping's.
  defp wf_entry_pos({:block, number, child_lines}, key),
    do: {Map.get(child_lines, key, number), :eol}

  defp wf_entry_pos(pos, _key), do: pos

  # `toolchain: *name` is the scalar the latest `&name` before it anchors:
  # null or empty there is no input, exactly as if written in place
  # (`x: &empty ""`). An anchor this reader cannot find, or one that names a
  # mapping, a sequence or a value continued on the lines below, leaves the
  # input unreadable, and the finding routes to review.
  defp wf026_scalar_alias(name, lines, pos) do
    case wf_anchor_scalar(lines, name, pos) do
      {:ok, value} -> if wf026_null_input?(value), do: :null, else: {:set, :own}
      :error -> {:unreadable, :toolchain_alias, name}
    end
  end

  # Is a `toolchain` value, as written, no input the action can read? Its
  # node properties come off first: an anchor names the node, not its value
  # (`&channel ""` is the empty string, `&channel null` null), and a tag
  # says how the scalar resolves — `!!null` always to null, `!!str` to the
  # string it spells, so `!!str ""` is still empty but `!!str null` is the
  # string null, an input.
  defp wf026_null_input?(value) do
    [_, props, scalar] = Regex.run(~r/^\s*((?:[&!]\S*(?:\s+|$))*)(.*)$/s, value)
    tags = Regex.scan(~r/!\S*/, props) |> List.flatten()

    cond do
      "!!null" in tags -> true
      "!!str" in tags -> String.trim(scalar) in ["", ~s(""), "''"]
      true -> wf026_null_value?(scalar)
    end
  end

  # YAML's null: nothing, `~`, or `null` in the three spellings the core
  # schema admits, unquoted. `"null"` is the string null, an input the
  # action receives; `""` and `''` are the empty string, which it does not.
  defp wf026_null_value?(value) do
    case String.trim(value) do
      "" -> true
      "~" -> true
      ~s("") -> true
      "''" -> true
      bare -> bare in ["null", "Null", "NULL"]
    end
  end

  # An alias that reaches itself (`&x` whose mapping merges `*x`) is as
  # unreadable as one with no anchor.
  defp wf026_alias_verdict(name, lines, form, pos, seen \\ []) do
    if name in seen do
      {:unreadable, :alias, name}
    else
      case wf_anchor_entries(lines, name, pos) do
        {:ok, entries, def_pos} ->
          wf026_entries_verdict(entries, lines, form, def_pos, [name | seen])

        :error ->
          {:unreadable, :alias, name}
      end
    end
  end

  # The entries of the mapping anchored `&name` that an alias at `pos`
  # (`{line, column}`, column `:eol` for anywhere on the line) names. YAML
  # resolves an alias to the latest `&name` before it, and a document may
  # define one again (`defaults: &inputs {...}`, then `other: &inputs {}`):
  # the last definition before `pos` decides, and one only after it defines
  # nothing for it. Definitions are read outside comments, quoted scalars
  # and block-scalar content (a `run: |` fixture writing `fake: &inputs` is
  # text; so is a `note: "see &name"`), and the mapping as a flow mapping
  # when a `{` follows the anchor, else as the block mapping under its line.
  # Returns `{:ok, entries, pos}` with the definition's own position (where
  # an alias inside the anchored mapping stands), or :error.
  defp wf_anchor_entries(lines, name, pos) do
    case wf_anchor_def(lines, name, pos) do
      nil ->
        :error

      {line, masked, number, at, len} ->
        after_anchor = binary_part(masked, at + len, byte_size(masked) - at - len)

        case Regex.run(~r/\{/, after_anchor, return: :index) do
          [{brace, _}] ->
            case wf_flow_interior(lines, number, at + len + brace) do
              {:ok, interior, _} -> {:ok, wf_flow_entries(interior), {number, :eol}}
              :error -> :error
            end

          nil ->
            # A bare sequence item carrying the anchor (`- &inputs`, its keys
            # on the lines below) holds the mapping under the dash, not under
            # the column its first key would take.
            parent =
              if Regex.match?(~r/^\s*-\s+(?:[&!]\S*\s*)+$/, wf_strip_comment(line)),
                do: wf026_indent(line),
                else: wf026_key_indent(line)

            rest = Enum.drop(lines, number)
            children = wf_block_children(rest, parent)
            {:ok, children, {:block, number, wf_block_child_lines(rest, parent, number)}}
        end
    end
  end

  # The scalar the anchor `&name` an alias at `pos` names holds, as written
  # (quoted or plain, "" for none): `{:ok, value}`, or :error when there is
  # no such anchor, or it names a mapping or sequence, or its value is
  # continued on the lines below.
  defp wf_anchor_scalar(lines, name, pos) do
    with {line, masked, number, at, len} <- wf_anchor_def(lines, name, pos),
         rest = binary_part(line, at + len, byte_size(line) - at - len),
         false <- Regex.match?(~r/^\s*(?:![^\s]*\s+)*[\[{*&|>]/, rest) do
      cond do
        m =
            Regex.run(
              ~r/^\s+((?:![^\s]*\s+)*)("(?:[^"\\]|\\.)*"|'(?:[^']|'')*'|[^\s,}\]#][^,}\]#]*?)\s*(?:[,}\]]|\s#|$)/,
              rest
            ) ->
          # The scalar with its tag (`&x !!str null` is the string null).
          {:ok, Enum.at(m, 1) <> Enum.at(m, 2)}

        # Nothing after the anchor but a tag or a comment: null, unless a
        # deeper line continues the scalar.
        Regex.match?(
          ~r/^\s*(?:![^\s]*\s*)*(?:#.*)?$/,
          binary_part(masked, at + len, byte_size(masked) - at - len)
        ) ->
          next = lines |> Enum.drop(number) |> Enum.find(&wf026_plain?/1)
          if next && wf026_indent(next) > wf026_indent(line), do: :error, else: {:ok, ""}

        true ->
          :error
      end
    else
      _ -> :error
    end
  end

  # The latest definition of `&name` before `pos`, as
  # `{line, masked line, number, offset, length}`, or nil.
  # An anchor is `&name` where YAML lets a node start: at the line's start or
  # after its list dashes, after `key: `, or — inside a flow collection (one
  # the line opens, or the line sits in) — after `[`, `{` or `,`, each past
  # any tag. `NOTE: foo&channel nightly` is one plain scalar, no anchor.
  defp wf_anchor_def(lines, name, {pos_line, pos_col}) do
    re = ~r/&#{Regex.escape(name)}(?=[\s,{}\[\]]|$)/

    content = Enum.join(lines, "\n")
    states = Hypatia.Rules.PinSites.line_states(content)
    depths = Hypatia.Rules.PinSites.line_depths(content)

    lines
    |> Enum.zip(Enum.zip(states, depths))
    |> Enum.with_index(1)
    |> Enum.take(pos_line)
    |> Enum.flat_map(fn {{l, {{_, state}, depth}}, number} ->
      if state not in [:text, :value] and wf026_plain?(l) do
        masked = wf_mask_quoted(l)

        re
        |> Regex.scan(masked, return: :index)
        |> Enum.filter(fn [{at, _}] -> number < pos_line or pos_col == :eol or at < pos_col end)
        |> Enum.filter(fn [{at, _}] -> wf_node_start?(binary_part(masked, 0, at), depth) end)
        |> Enum.map(fn [{at, len}] -> {l, masked, number, at, len} end)
      else
        []
      end
    end)
    |> List.last()
  end

  # Inside a flow collection a node also starts directly after the colon of
  # a quoted key, JSON-style (`{"BAD":&bad "a/b@x"}`).
  defp wf_node_start?(before, depth) do
    Regex.match?(~r/(?:^\s*(?:-\s+)*|:\s+)(?:![^\s]*\s+)*$/, before) or
      ((depth > 0 or String.contains?(before, ["[", "{"])) and
         Regex.match?(~r/(?:[\[{,]|["']:)\s*(?:![^\s]*\s+)*$/, before))
  end

  # The line of each direct child of the block mapping whose key sits at
  # `key_indent`, `rest` being the lines after the key's line `number`, as
  # `%{key => line}` — the children `wf_block_children/2` reads, the first
  # of a key spelled twice.
  defp wf_block_child_lines(rest, key_indent, number) do
    block =
      rest
      |> Enum.with_index(number + 1)
      |> Enum.take_while(fn {l, _} ->
        trimmed = String.trim(l)
        trimmed == "" or String.starts_with?(trimmed, "#") or wf026_indent(l) > key_indent
      end)
      |> Enum.filter(fn {l, _} -> wf026_plain?(l) end)

    case block do
      [] ->
        %{}

      [{first, _} | _] ->
        child_indent = wf026_indent(first)

        block
        |> Enum.filter(fn {l, _} -> wf026_indent(l) == child_indent end)
        |> Enum.reduce(%{}, fn {l, n}, acc ->
          case Regex.run(~r/^\s*(?:-\s+)?["']?([^"':\s]+)["']?\s*:/, l) do
            [_, k] -> Map.put_new(acc, k, n)
            nil -> acc
          end
        end)
    end
  end

  # The direct children of a block mapping whose key sits at `key_indent`,
  # as `{key, value}` entries: the plain lines deeper than the key, at the
  # first child's indentation. Deeper lines are nested mappings or scalar
  # text, not entries; under a child with no value on its own line they are
  # that value (`toolchain:` then `nightly` indented is `toolchain: nightly`).
  defp wf_block_children(rest, key_indent) do
    children =
      rest
      |> Enum.take_while(fn l ->
        trimmed = String.trim(l)
        trimmed == "" or String.starts_with?(trimmed, "#") or wf026_indent(l) > key_indent
      end)
      |> Enum.filter(&wf026_plain?/1)

    case children do
      [] ->
        []

      [first | _] ->
        child_indent = wf026_indent(first)

        children
        |> Enum.with_index()
        |> Enum.filter(fn {l, _} -> wf026_indent(l) == child_indent end)
        |> Enum.flat_map(fn {l, i} ->
          case Regex.run(~r/^\s*(?:-\s+)?["']?([^"':\s]+)["']?\s*:(.*)$/, l) do
            [_, k, v] ->
              v = v |> wf_strip_comment() |> String.trim()

              # A value on the lines below — nothing on the key's line, or a
              # block scalar's indicator (`toolchain: >-`) — is what those
              # lines hold: an empty block scalar is the empty string.
              # So is one with nothing on the line but node properties
              # (`toolchain: &channel`), kept in front of what is below.
              props = Regex.run(~r/^(?:[&!]\S*(?:\s+|$))*/, v) |> hd()
              continued? = v == "" or Regex.match?(~r/^(?:[&!]\S*\s+)*[|>][-+0-9]*$/, v)

              cond do
                continued? -> [{k, wf_continued_value(children, i, child_indent)}]
                props == v -> [{k, props <> " " <> wf_continued_value(children, i, child_indent)}]
                true -> [{k, v}]
              end

            nil ->
              []
          end
        end)
    end
  end

  # The value of the child at `i` continued on the lines below it, deeper
  # than its key and up to the next child; "" when there are none.
  defp wf_continued_value(children, i, child_indent) do
    children
    |> Enum.drop(i + 1)
    |> Enum.take_while(&(wf026_indent(&1) > child_indent))
    |> Enum.map_join(" ", &(&1 |> wf_strip_comment() |> String.trim()))
  end

  # ─── flow-mapping reading (quote- and depth-aware) ──────────────────

  # Byte column of the innermost `{` still open at byte `at` of `line`, read
  # from the start with quotes and comments respected; :error when `at` is
  # not inside a flow mapping opened on this line (it is inside a quoted
  # scalar, or no brace precedes it).
  defp wf_flow_opener_before(line, at) do
    line
    |> binary_part(0, at)
    |> :binary.bin_to_list()
    |> wf_flow_prefix(0, nil, [], ?\s)
  end

  # State: nil (plain text), ?" or ?' (inside that quote), :comment. The
  # stack holds the byte column of each open `{` (`:seq` for a `[`); the
  # innermost must be a mapping for `at` to be a key position.
  # Every `{`/`[` still open before byte `at` of `line`, innermost first:
  # the column of each `{`, `{:seq, column}` for a `[`. Quoted scalars and
  # comments are skipped, as in wf_flow_opener_before/2; :error inside one.
  defp wf_flow_opener_stack(line, at) do
    line
    |> binary_part(0, at)
    |> :binary.bin_to_list()
    |> wf_flow_stack(0, nil, [], ?\s)
  end

  defp wf_flow_stack([], _pos, nil, stack, _prev), do: {:ok, stack}
  defp wf_flow_stack([], _pos, _state, _stack, _prev), do: :error

  defp wf_flow_stack([c | rest], pos, nil, stack, prev) do
    cond do
      c == ?# and prev in [?\s, ?\t] -> wf_flow_stack(rest, pos + 1, :comment, stack, c)
      c in [?", ?'] -> wf_flow_stack(rest, pos + 1, c, stack, c)
      c == ?{ -> wf_flow_stack(rest, pos + 1, nil, [pos | stack], c)
      c == ?[ -> wf_flow_stack(rest, pos + 1, nil, [{:seq, pos} | stack], c)
      c in [?}, ?]] -> wf_flow_stack(rest, pos + 1, nil, Enum.drop(stack, 1), c)
      true -> wf_flow_stack(rest, pos + 1, nil, stack, c)
    end
  end

  defp wf_flow_stack([?\\, c | rest], pos, ?", stack, _prev),
    do: wf_flow_stack(rest, pos + 2, ?", stack, c)

  defp wf_flow_stack([q | rest], pos, q, stack, _prev),
    do: wf_flow_stack(rest, pos + 1, nil, stack, q)

  defp wf_flow_stack([c | rest], pos, state, stack, _prev),
    do: wf_flow_stack(rest, pos + 1, state, stack, c)

  defp wf_flow_prefix([], _pos, nil, [col | _], _prev) when is_integer(col), do: {:ok, col}
  defp wf_flow_prefix([], _pos, _state, _stack, _prev), do: :error

  defp wf_flow_prefix([c | rest], pos, nil, stack, prev) do
    cond do
      c == ?# and prev in [?\s, ?\t] -> wf_flow_prefix(rest, pos + 1, :comment, stack, c)
      c in [?", ?'] -> wf_flow_prefix(rest, pos + 1, c, stack, c)
      c == ?{ -> wf_flow_prefix(rest, pos + 1, nil, [pos | stack], c)
      c == ?[ -> wf_flow_prefix(rest, pos + 1, nil, [:seq | stack], c)
      c in [?}, ?]] -> wf_flow_prefix(rest, pos + 1, nil, Enum.drop(stack, 1), c)
      true -> wf_flow_prefix(rest, pos + 1, nil, stack, c)
    end
  end

  defp wf_flow_prefix([?\\, c | rest], pos, ?", stack, _prev),
    do: wf_flow_prefix(rest, pos + 2, ?", stack, c)

  defp wf_flow_prefix([q | rest], pos, q, stack, _prev),
    do: wf_flow_prefix(rest, pos + 1, nil, stack, q)

  defp wf_flow_prefix([c | rest], pos, state, stack, _prev),
    do: wf_flow_prefix(rest, pos + 1, state, stack, c)

  # The interior of the flow collection whose opener sits at byte `open_col`
  # of line `number` (1-based): the text between that brace and the one
  # closing it, which may be lines later. Quoted scalars and comments are
  # skipped for depth, comments are dropped from the text. :error when the
  # document ends first.
  defp wf_flow_interior(lines, number, open_col) do
    first = Enum.at(lines, number - 1)
    head = binary_part(first, open_col + 1, byte_size(first) - open_col - 1)

    text =
      [head | Enum.drop(lines, number)]
      |> Enum.join("\n")
      |> :binary.bin_to_list()

    wf_flow_take(text, 1, nil, [], 0)
  end

  defp wf_flow_take([], _depth, _state, _acc, _nl), do: :error

  defp wf_flow_take([c | rest], depth, nil, acc, nl) do
    cond do
      c == ?# and (acc == [] or hd(acc) in [?\s, ?\t, ?\n]) ->
        wf_flow_take(rest, depth, :comment, acc, nl)

      c in [?", ?'] ->
        wf_flow_take(rest, depth, c, [c | acc], nl)

      c in [?{, ?[] ->
        wf_flow_take(rest, depth + 1, nil, [c | acc], nl)

      c in [?}, ?]] and depth == 1 ->
        {:ok, acc |> Enum.reverse() |> List.to_string(), nl}

      c in [?}, ?]] ->
        wf_flow_take(rest, depth - 1, nil, [c | acc], nl)

      c == ?\n ->
        wf_flow_take(rest, depth, nil, [c | acc], nl + 1)

      true ->
        wf_flow_take(rest, depth, nil, [c | acc], nl)
    end
  end

  defp wf_flow_take([?\\, c | rest], depth, ?", acc, nl),
    do: wf_flow_take(rest, depth, ?", [c, ?\\ | acc], nl)

  defp wf_flow_take([?\n | rest], depth, :comment, acc, nl),
    do: wf_flow_take(rest, depth, nil, [?\n | acc], nl + 1)

  defp wf_flow_take([_ | rest], depth, :comment, acc, nl),
    do: wf_flow_take(rest, depth, :comment, acc, nl)

  defp wf_flow_take([q | rest], depth, q, acc, nl),
    do: wf_flow_take(rest, depth, nil, [q | acc], nl)

  defp wf_flow_take([c | rest], depth, q, acc, nl),
    do: wf_flow_take(rest, depth, q, [c | acc], nl)

  # The top-level `{key, value}` entries of a flow mapping's interior:
  # commas inside nested `{}`/`[]` or quoted scalars do not split, and the
  # key ends at the first top-level colon.
  defp wf_flow_entries(interior) do
    interior
    |> wf_flow_split(?,)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.map(fn entry ->
      case wf_flow_split(entry, ?:) do
        [key] -> {wf_unquote(key), ""}
        [key | value] -> {wf_unquote(key), value |> Enum.join(":") |> String.trim()}
      end
    end)
  end

  # Split at `sep` where it sits at depth 0 and outside quotes.
  defp wf_flow_split(text, sep) do
    text
    |> :binary.bin_to_list()
    |> wf_flow_split(sep, 0, nil, [], [])
  end

  defp wf_flow_split([], _sep, _depth, _q, cur, out),
    do: Enum.reverse([cur |> Enum.reverse() |> List.to_string() | out])

  defp wf_flow_split([c | rest], sep, depth, nil, cur, out) do
    cond do
      c == sep and depth == 0 ->
        wf_flow_split(rest, sep, depth, nil, [], [cur |> Enum.reverse() |> List.to_string() | out])

      c in [?", ?'] ->
        wf_flow_split(rest, sep, depth, c, [c | cur], out)

      c in [?{, ?[] ->
        wf_flow_split(rest, sep, depth + 1, nil, [c | cur], out)

      c in [?}, ?]] ->
        wf_flow_split(rest, sep, max(depth - 1, 0), nil, [c | cur], out)

      true ->
        wf_flow_split(rest, sep, depth, nil, [c | cur], out)
    end
  end

  defp wf_flow_split([?\\, c | rest], sep, depth, ?", cur, out),
    do: wf_flow_split(rest, sep, depth, ?", [c, ?\\ | cur], out)

  defp wf_flow_split([q | rest], sep, depth, q, cur, out),
    do: wf_flow_split(rest, sep, depth, nil, [q | cur], out)

  defp wf_flow_split([c | rest], sep, depth, q, cur, out),
    do: wf_flow_split(rest, sep, depth, q, [c | cur], out)

  # Drop a trailing ` # comment` that sits outside quotes.
  # The byte offset in a flow mapping's interior at which the value of its
  # top-level `key` entry starts, or nil. Entries are split at commas outside
  # quotes and nested brackets.
  defp wf_flow_entry_value_at(interior, key) do
    re = ~r/^\s*["']?#{Regex.escape(key)}["']?\s*:\s*/

    interior
    |> wf_flow_entry_starts()
    |> Enum.find_value(fn at ->
      case Regex.run(re, binary_part(interior, at, byte_size(interior) - at), return: :index) do
        [{0, len}] -> at + len
        _ -> nil
      end
    end)
  end

  # Where each top-level entry of a flow interior starts: byte 0, and past
  # every comma outside quotes and nested brackets. Inside double quotes a
  # `\` escapes the byte after it (`"say \"hi\", then"` is one scalar); a
  # doubled `''` inside single quotes closes and reopens with nothing
  # between, so it hides no comma either.
  defp wf_flow_entry_starts(interior),
    do: interior |> :binary.bin_to_list() |> wf_entry_starts(0, 0, nil, [0]) |> Enum.reverse()

  defp wf_entry_starts([], _i, _d, _q, acc), do: acc

  defp wf_entry_starts([?\\, _ | rest], i, d, ?", acc),
    do: wf_entry_starts(rest, i + 2, d, ?", acc)

  defp wf_entry_starts([q | rest], i, d, q, acc), do: wf_entry_starts(rest, i + 1, d, nil, acc)

  defp wf_entry_starts([_ | rest], i, d, q, acc) when q != nil,
    do: wf_entry_starts(rest, i + 1, d, q, acc)

  defp wf_entry_starts([c | rest], i, d, nil, acc) when c in [?", ?'],
    do: wf_entry_starts(rest, i + 1, d, c, acc)

  defp wf_entry_starts([c | rest], i, d, nil, acc) when c in [?{, ?[],
    do: wf_entry_starts(rest, i + 1, d + 1, nil, acc)

  defp wf_entry_starts([c | rest], i, d, nil, acc) when c in [?}, ?]],
    do: wf_entry_starts(rest, i + 1, d - 1, nil, acc)

  defp wf_entry_starts([?, | rest], i, 0, nil, acc),
    do: wf_entry_starts(rest, i + 1, 0, nil, [i + 1 | acc])

  defp wf_entry_starts([_ | rest], i, d, nil, acc), do: wf_entry_starts(rest, i + 1, d, nil, acc)

  # Offset `at` of a flow interior that `wf_flow_interior/3` read from the
  # `{` at `open_col` of line `number`, as `{line, column}` in the file.
  defp wf_interior_pos(interior, at, number, open_col) do
    case :binary.matches(binary_part(interior, 0, at), "\n") do
      [] ->
        {number, open_col + 1 + at}

      nls ->
        {nl, _} = List.last(nls)
        {number + length(nls), at - nl - 1}
    end
  end

  # Does the line's own `uses:` value carry YAML node properties — an anchor
  # or tag before the reference (`uses: &rust a/b@x`, `uses: !!str a/b@x`)?
  defp wf_node_properties?(line),
    do: Regex.match?(~r/^\s*(?:-\s+)?["']?uses["']?\s*:\s*[&!]/, line)

  # The `uses:` keys whose reference is not written on their own line, as
  # `%{key_line => {joined, how}}`: the key's line with the reference in
  # place, which the rules read as they read a one-line key. `how` is
  # :continued for a reference YAML continues below the key (`- uses:`, then
  # — past any blank or comment-only lines — the reference on a deeper line;
  # or a block scalar, `- uses: >-`, whose one content line is the reference)
  # and :alias for `uses: *name`, the reference the latest `&name` before it
  # anchors (`PinSites.alias_sites/1`). Found on the masked lines
  # (`PinSites.mask_lines/1`), so a `uses:` in a `run: |` script or a quoted
  # string stands for nothing.
  defp wf_continued_uses(lines) do
    content = Enum.join(lines, "\n")
    masked = Hypatia.Rules.PinSites.mask_lines(content)
    prefixes = Hypatia.Rules.PinSites.flow_prefixes(content)

    continued =
      for {key, _value, site} <- Hypatia.Rules.PinSites.continuations(masked), into: %{} do
        # A block scalar's indicator (`>-`, `|`) is no part of the value.
        head =
          lines
          |> Enum.at(key - 1)
          |> Hypatia.Rules.PinSites.head()
          |> String.trim_trailing()
          |> String.replace(~r/\s+[|>][-+0-9]*$/, "")

        {key, {head <> " " <> site.action <> "@" <> site.ref, :continued}}
      end

    for %{line: n, at: at, len: len, site: site} <-
          Hypatia.Rules.PinSites.alias_sites(masked, prefixes),
        not Map.has_key?(continued, n),
        into: continued do
      line = Enum.at(lines, n - 1)
      tail = binary_part(line, at + len, byte_size(line) - at - len)
      {n, {binary_part(line, 0, at) <> site.action <> "@" <> site.ref <> tail, :alias}}
    end
  end

  defp wf_strip_comment(text) do
    text
    |> :binary.bin_to_list()
    |> wf_strip_comment(nil, [])
  end

  defp wf_strip_comment([], _q, acc), do: acc |> Enum.reverse() |> List.to_string()

  defp wf_strip_comment([?# | _], nil, acc) when acc == [] or hd(acc) in [?\s, ?\t],
    do: acc |> Enum.reverse() |> List.to_string()

  defp wf_strip_comment([c | rest], nil, acc) when c in [?", ?'],
    do: wf_strip_comment(rest, c, [c | acc])

  defp wf_strip_comment([?\\, c | rest], ?", acc), do: wf_strip_comment(rest, ?", [c, ?\\ | acc])
  defp wf_strip_comment([q | rest], q, acc), do: wf_strip_comment(rest, nil, [q | acc])
  defp wf_strip_comment([c | rest], q, acc), do: wf_strip_comment(rest, q, [c | acc])

  # Is line `number` (1-based) inside a quoted scalar that a line from `from`
  # up to it opened and did not close? A double- or single-quoted value may
  # run over several physical lines (`- run: "echo start` … `echo end"`), and
  # a `uses:` written on one of them is string text, not a key. The content
  # of a block scalar (`run: |`) is skipped: a quote there is script text and
  # opens nothing. Only the lines of the one step or job are read, so a quote
  # the scan misreads elsewhere in the file cannot hide a real pin.
  defp wf_in_open_quote?(lines, from, number) when from < number do
    lines
    |> Enum.slice((from - 1)..(number - 2)//1)
    |> Enum.reduce({nil, nil, 0}, &wf_quote_line/2)
    |> elem(0)
    |> Kernel.!=(nil)
  end

  defp wf_in_open_quote?(_lines, _from, _number), do: false

  # One line of that scan: `{open_quote | nil, block_scalar_key_indent | nil,
  # flow_depth}`. The depth of the flow collections still open is carried from
  # line to line, as the shared pin reader carries it: a `{"run":"echo` whose
  # `{` (or `steps: [`) opened on an earlier line is still inside one.
  defp wf_quote_line(line, {nil, block, depth}) when is_integer(block) do
    if String.trim(line) == "" or wf026_indent(line) > block,
      do: {nil, block, depth},
      else: wf_quote_line(line, {nil, nil, depth})
  end

  defp wf_quote_line(line, {q, _, depth}) do
    case wf_quote_scan(line, q, "", depth) do
      {nil, outside} ->
        depth = max(depth + wf_bracket_delta(outside), 0)

        if Regex.match?(~r/:\s*[|>][-+0-9]*\s*$/, outside),
          do: {nil, wf026_key_indent(line), depth},
          else: {nil, nil, depth}

      {open, outside} ->
        {open, nil, max(depth + wf_bracket_delta(outside), 0)}
    end
  end

  # Walk one line from quote state `q` (nil outside a quoted scalar), keeping
  # the text outside quotes; `depth` is the flow depth the line starts at. A
  # quote opens a scalar only where a scalar can start (wf_scalar_start?/2);
  # `echo it's` is a plain scalar. A comment ends the line.
  defp wf_quote_scan(<<>>, q, out, _depth), do: {q, out}

  defp wf_quote_scan(<<?\\, _, rest::binary>>, ?", out, depth),
    do: wf_quote_scan(rest, ?", out, depth)

  defp wf_quote_scan(<<?', ?', rest::binary>>, ?', out, depth),
    do: wf_quote_scan(rest, ?', out, depth)

  defp wf_quote_scan(<<c, rest::binary>>, q, out, depth) when c == q,
    do: wf_quote_scan(rest, nil, out <> "x", depth)

  defp wf_quote_scan(<<_, rest::binary>>, q, out, depth) when q != nil,
    do: wf_quote_scan(rest, q, out, depth)

  defp wf_quote_scan(<<?#, _::binary>>, nil, out, _depth)
       when out == "" or binary_part(out, byte_size(out) - 1, 1) in [" ", "\t"],
       do: {nil, out}

  defp wf_quote_scan(<<c, rest::binary>>, nil, out, depth) when c in [?", ?'] do
    if wf_scalar_start?(out, depth),
      do: wf_quote_scan(rest, c, out <> "x", depth),
      else: wf_quote_scan(rest, nil, out <> <<c>>, depth)
  end

  defp wf_quote_scan(<<c, rest::binary>>, nil, out, depth),
    do: wf_quote_scan(rest, nil, out <> <<c>>, depth)

  # Can a scalar start after `out` (the line so far, quote bodies dropped)?
  # After `: `, a list dash, `[`, `{` or `,`, or at the line's start; inside
  # a flow collection, opened on this line or one before it (`depth`), also
  # directly after `:` (JSON-style `{"run":"echo`); in each case past any
  # anchor or tag (`run: &cmd "echo`), as Hypatia.Rules.PinSites reads a file.
  defp wf_scalar_start?(out, depth) do
    Regex.match?(~r/(?:^\s*(?:-\s+)*|:\s+|[\[{,]\s*)(?:[&!]\S*\s+)*$/, out) or
      (depth + wf_bracket_delta(out) > 0 and Regex.match?(~r/:\s*(?:[&!]\S*\s+)*$/, out))
  end

  # Flow brackets opened minus closed in text with its quote bodies dropped.
  defp wf_bracket_delta(out),
    do: length(Regex.scan(~r/[\[{]/, out)) - length(Regex.scan(~r/[\]}]/, out))

  # The nearest line above `number` that is shallower than it: the key whose
  # value holds it (a job's key line for a job-level `uses:`).
  defp wf_parent_line(lines, number) do
    indent = wf026_indent(Enum.at(lines, number - 1))

    (number - 1)..1//-1
    |> Enum.find(number, fn n ->
      l = Enum.at(lines, n - 1)
      wf026_plain?(l) and wf026_indent(l) < indent
    end)
  end

  # The line with its comment dropped and every quoted scalar's body blanked
  # to `x` byte for byte, so `&name` or `{` written inside a string is not
  # structure, and an offset into the result is an offset into the line.
  defp wf_mask_quoted(text), do: wf_mask_quoted(text, nil, "")

  defp wf_mask_quoted(<<>>, _q, out), do: out

  defp wf_mask_quoted(<<?#, _::binary>>, nil, out)
       when out == "" or binary_part(out, byte_size(out) - 1, 1) in [" ", "\t"],
       do: out

  defp wf_mask_quoted(<<c, rest::binary>>, nil, out) when c in [?", ?'],
    do: wf_mask_quoted(rest, c, out <> <<c>>)

  defp wf_mask_quoted(<<c, rest::binary>>, nil, out), do: wf_mask_quoted(rest, nil, out <> <<c>>)

  defp wf_mask_quoted(<<?\\, _, rest::binary>>, ?", out),
    do: wf_mask_quoted(rest, ?", out <> "xx")

  defp wf_mask_quoted(<<q, rest::binary>>, q, out), do: wf_mask_quoted(rest, nil, out <> <<q>>)
  defp wf_mask_quoted(<<_, rest::binary>>, q, out), do: wf_mask_quoted(rest, q, out <> "x")

  defp wf_unquote(text) do
    text
    |> String.trim()
    |> String.replace(~r/^"(.*)"$/s, "\\1")
    |> String.replace(~r/^'(.*)'$/s, "\\1")
    |> String.trim()
  end

  # Column of a key, with a list dash counted as indentation (`- with:` is
  # the column of `with`).
  defp wf026_key_indent(line) do
    line
    |> String.replace(~r/^(\s*)-\s/, "\\1  ")
    |> wf026_indent()
  end

  # ─── WF027: caller pinned at a reusable revision the estate has measured dead ──

  # One entry per (reusable, window). The SHAs are the commits of
  # hyperpolymath/standards main whose rust-ci-reusable.yml pins
  # dtolnay/rust-toolchain by SHA and passes no `toolchain:` input — a walk of
  # every first-parent commit from d73c93e0 (2026-06-01) to 49ed4715
  # (2026-06-24), each one's file content checked, not the dates. The
  # replacement is the current standards main whose own actions.lock GitHub
  # accepts (see pr-automerge-policy.json PIN-003 for the lock-side window,
  # which kills EVERY standards reusable and so lives in the pin denylist
  # rather than here). gitbot-fleet scripts/lib/stale-reusable-pins.tsv carries
  # the same rows (Window A) for the repair; regenerate both together.
  @broken_reusable_revisions [
    %{
      reusable: "hyperpolymath/standards/.github/workflows/rust-ci-reusable.yml",
      window: "standards main 2026-06-01..2026-06-24 (d73c93e0..49ed4715)",
      fixed_by: "standards#439 (9fe0d8ffa33a6fa0c6ba64117bef9981dacd3020)",
      replacement: "75bd5994470661a099df562d6d5ad2fe744dbd66",
      reason:
        "rust-ci-reusable.yml at this revision pins dtolnay/rust-toolchain by SHA and " <>
          "passes no toolchain: input, so every caller fails in its first step with " <>
          "\"'toolchain' is a required input\" before any cargo command runs.",
      shas: [
        "bf32531befa4bb7f692a5f7ecac1eab71af20506",
        "583a9346ebf28e0fd8114f22aef140eaa1be4ac0",
        "d7c22711e830e1f383846472f6e9b99debdb201e",
        "747b2f0f690505b24c90991a030a01e564ff59c6",
        "509142da4ec04d74dccfc9e52813aeabc2c6fa02",
        "e2ef79eecdd623ff631fcbf3fe0268ba4f4ce166",
        "0597948e81c0be7570f805d7ec49f64fa0eeefd6",
        "da1ee5ebaec4de70e7530a9fcb4b2c693434bcc1",
        "0f49246bc5055f7159c92d649c4d3fb97b75cf66",
        "f15ced3f44146de9931bcb70157e1825d07b83ed",
        "5e29ef7923aff017419bdce9354e9df4c3823cb5",
        "ad8c45387878f2b00217330b0e1260edc6f878b5",
        "3ce22c244bd4b31ed99397a585cd458735a79819",
        "8c785d41adbbee534b158747a5c6796402e450f0",
        "c1070339c80c4f1af71bb35cacc58c19f611f3a2",
        "49ed471515bdf587aeb9931af2d1314cd6aa5de2",
        "7282b1ac828d89eaecd3403b59591975746ec3ff",
        "d135b05bfc647d0c0fbfedc7e80f37ea50f49236",
        "4762ba66441de05bccb36e074ba3d2202219126a",
        "59429dc666d347d030114cd6ef6f4ff7b7f6a6b4",
        "e8fe153b568af5e3534a261eee0258fa84fa6c5e",
        "280cb7e3f2db8b0e5f26c42877ea5fa80243b06f",
        "c6c095b38f7e729d3d355629b80e771dfe1a351b",
        "09e7023d24682621bea4e11965a1ef5e87d86c3b",
        "42455b19727fa0ccf73a9b914f0e9781d4c34577",
        "4ddc926b4b86451c2711a16bf927ed16ad2db45e",
        "0809b8e3630d38229b6c3629956e0dff4b937c33",
        "d72fe5a14e841ac6d78514b53624b6173038ee20",
        "5a93d9d57cc04de4002d6d0ecd336fc7a8698910",
        "3e57141d38672438a7a9110bd10ea0f76e6099a7",
        "78b29005efe954822c86c553b40523b9fdae78d4",
        "f290d4a696b8af78f9141daeced486b7b8a85d11",
        "a8c69ecfcd87d5418dd4559019ac860fff20e834",
        "a04c1d9f32e3c6cb5e91b7ba220080475449a4d2",
        "c43aafaa2d707f3f195cad9742d3ccc8d068e3db",
        "a955527810b82dbf7a9bc541502c5235f119499d",
        "2ff85f8f84212ae2cbedb1ea232c6f5a470acef3",
        "8e6ba7d4a0890d25425855a77404d4690d0ab4b5",
        "426368b72949d8235c8cf47f8bdc5e0e5cd5b54f",
        "025bba3f35f5040bf76d4954f02c744d61f68e8f",
        "8466cee8adbb814a2507a9a33e36ba415c5d7eba",
        "f5f0506a6ec88e574753eee701a268e0d4b3a7f2",
        "ae5b6731e0c1ce192edc14737a226d6b201341aa",
        "742424ec9cffeb414174276d9321325859e3aa64",
        "19995ace5f1179e9e2d4783fe1d36df0f343492d",
        "524523c1a6057942aadaef5999912fbf13a355c5",
        "e4c7d968fa9aad6fcecab23bd60d1025679d1ad9",
        "e96096c6614469e29e02edcc923fdacc0e611b34",
        "ca29b1950ac493e4a29db3b61b6991fbdc8a57b2",
        "e59affd85741ac786867a667bc1526ade31b1dd4",
        "7694d58cd4c82d6d706ef9653bcf545e28126794",
        "33a82ee5e3635bd137b2bc8ef2b1d71367195546",
        "52db79bee40380820e01c454efe6e85ae6115b2c",
        "6022e498a06552d71e51662283b9453855185b94",
        "c97b2bdd12aeaff4624efcaec874ea7665dadae6",
        "9726a3814c895bf531f22c2b82cc15a3b1efa1f3",
        "bec61613c821fdb1035f18b88e6e5d19d23328f7",
        "4236493691a06a6393b5ec4480f38e5d804f5b69",
        "6cd3772824e59c8c9affeab66061e25383544242",
        "e763d82b93f46e7201d95e6d817ac61d739a8cc1",
        "aa665acc04fff66632ede8368dbc9c2b5a630d3c",
        "3a5c23cb5888eca79df090fde6d52a8dc4820285",
        "0b96f61a3f043bd5409b913e526b29f3fd38a217",
        "ae6619ecddd0eff0f957c73280efc36bb9302d75",
        "3b3549e293f87be99d7dbb939d380c8128842c8e",
        "2b88f34c97a9478b7a96b07425bb37ee5822bb8e",
        "9bdaff9479d53328ed907bdd5ba885398800fb00",
        "142dd7a0b9cd6101de8c4279c55b7051c537f76b",
        "ac6b3252e7170d9459779c03bf31ae04c3629a61",
        "f2b9d5eb5fb68a60b30468b8289a0f8cefbdd558",
        "e401e47106ce0b0e32e3d0e2b7d910410e5b165e",
        "e3ad4ca266e4dc93b7cebce5e3b3b48c6d523e45",
        "74748546395bf3dcedd5b660f58feb36be9d53d6",
        "fc7abf589b93f9a33b8af74dbfe98ba22707156f",
        "7814dc67a489afc39f0289af5d3b6c3e23645156",
        "c98915a225c98f2b90980cf045a00eb603e6af0f",
        "18dd92c19d5b9e50740826a4be8301afbab91bae",
        "82a8ac8e2fd446b710139a759bf22f175dc5e299",
        "55da6b331922f0ff7389c783d363ae12d8c61068",
        "1376eb6251fa9beeaea75241739b1d00dd6b4028",
        "28946811fb7fe2d05e9ac11c9ebee7295e966d40",
        "d7c3b8f75d70ab640e6f37a4c8f9e6e8f2b6df56",
        "d73c93e0e7d38f673b68e616d51a62f5860081fd"
      ]
    }
  ]

  @doc "The (reusable, window) table WF027 fires on."
  def broken_reusable_revisions, do: @broken_reusable_revisions

  @doc """
  WF027: a caller pins an estate reusable workflow at a revision the estate
  has MEASURED dead for that reusable.

  Unlike WF016 (a SHA no branch reaches any more), these commits are
  reachable and resolve: the run starts, the reusable's first step fails,
  and the failure names something the caller never wrote. The table is
  data, keyed by reusable path, so a newly measured window is a row, not a
  rule. Full 40-hex pins only — that is the only form sha_pinning_required
  lets a caller write.

  A job's `uses:` whose value YAML continues below its key (past any blank
  or comment-only lines, or as a block scalar) is the call all the same. So
  is the `uses:` of a mapping a job is given whole as an alias (`rust:
  *job`), wherever that mapping is anchored (`PinSites.node_alias_sites/1`).

  Auto-fixable: gitbot-fleet `scripts/fix-stale-reusable-pin.sh` rewrites
  the pin to the replacement the table names, never to "newest". It refuses
  a value continued below its key (exit 2, file untouched), so that finding
  carries `auto_fixable: false` and routes to review, as does a call made
  through a job alias whose mapping is no job's own: the fixer rewrites the
  keys of `jobs:` only.

  Severity is low and the finding is `advisory: true`: like WF026 the rule
  reports and routes to the fleet; it is never a blocking gate (AGENTIC
  `must-not-promote-to-blocking`).
  """
  def check_known_broken_reusable_revision(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      lines = String.split(content, "\n")
      continued = wf_continued_uses(lines)
      flow = wf_flow_lines(content)
      # A job given whole as an alias (`rust: *job`) runs the mapping its
      # anchor holds: that mapping's `uses:` line is a call wherever it sits.
      aliased =
        for %{key_line: n} <- Hypatia.Rules.PinSites.node_alias_sites(content),
            into: MapSet.new(),
            do: n

      Enum.flat_map(@broken_reusable_revisions, fn rev ->
        # Anchored to a `uses:` key at the start of a line (quoted or not) that
        # is a key of a job under `jobs:` — the only place a reusable is
        # called — or to the same key inside the flow mapping a job opens on
        # its own line (`rust: { uses: ... }`). The reusable's path inside a
        # `run: |` script, under `env:`, in a string value or in a comment is
        # not a call, so the fleet is never sent to rewrite script data.
        # The 40 hex characters end the ref (`@<sha>-patched` is a tag), in
        # either case: git resolves the commit ID case-insensitively.
        reusable = wf027_reusable_re(rev.reusable)

        ref_re =
          ~r/^\s*(?:-\s+)?["']?uses["']?\s*:\s*(?:[&!]\S*\s+)*["']?#{reusable}@([0-9a-fA-F]{40})(?=["'\s#,}\]]|$)/

        inline_re =
          ~r/[{,]\s*["']?uses["']?\s*:\s*(?:[&!]\S*\s+)*["']?#{reusable}@([0-9a-fA-F]{40})(?=["'\s#,}\]]|$)/

        lines
        |> Enum.with_index(1)
        |> Enum.flat_map(fn {line, number} ->
          # A job's `uses:` with the reference below it, or naming an anchored
          # one (`uses: *call`), is the one-line key it stands for.
          {line, resolved} =
            case Map.fetch(continued, number) do
              {:ok, {joined, how}} -> {joined, how}
              :error -> {line, nil}
            end

          live? = number in aliased

          # Each call with the layout the fleet fixer would meet: it rewrites
          # a bare reference on a block key's own line, nothing else. A call
          # only a job alias makes (its mapping anchored outside `jobs:`) is
          # no key the fixer places.
          shas =
            case Regex.run(ref_re, line) do
              [_full, sha] ->
                review =
                  cond do
                    resolved != nil -> resolved
                    wf_node_properties?(line) -> :properties
                    true -> nil
                  end

                case wf027_job_key(lines, flow, number) do
                  :block -> [{sha, review}]
                  :flow -> [{sha, review || :flow}]
                  nil when live? -> [{sha, review || :node_alias}]
                  nil -> []
                end

              nil ->
                inline_re
                |> Regex.scan(line, return: :index)
                |> Enum.flat_map(fn [{at, _}, {sha_at, 40}] ->
                  cond do
                    wf027_inline_job?(lines, flow, line, number, at + 1) -> [:flow]
                    live? -> [:node_alias]
                    true -> []
                  end
                  |> Enum.map(&{binary_part(line, sha_at, 40), &1})
                end)
            end

          Enum.flat_map(shas, fn {sha, review} ->
            if String.downcase(sha) in rev.shas do
              [wf027_finding(filename, number, sha, rev, review)]
            else
              []
            end
          end)
        end)
      end)
    end)
  end

  # Is the `uses:` that starts line `number` a key of a job under `jobs:`?
  # :block when its indentation says so, outside any quoted scalar the job
  # opened above it — the reading the fleet fixer makes. A line inside a flow
  # collection opened on an earlier line is judged by the brackets still open
  # before it (`wf_flow_context/4`), however it is indented: the innermost
  # must be the mapping a job key opens in `jobs`. Such a call is :block too
  # when its indentation agrees (KYAML-style `rust: {` / `  uses: ...`, which
  # the fixer rewrites), and :flow when only the brackets say so (`jobs: {`,
  # `rust: {`, `uses: ...` all in column 0, which the fixer cannot place).
  # nil when it is no job's key.
  defp wf027_job_key(lines, flow, number) do
    block? =
      match?([_job, "jobs" | _], wf_ancestor_keys(lines, number)) and
        not wf_in_open_quote?(lines, wf_parent_line(lines, number), number)

    cond do
      not wf_in_flow?(flow, number) -> if block?, do: :block
      not wf027_flow_job?(wf_flow_context(lines, flow, number)) -> nil
      block? -> :block
      true -> :flow
    end
  end

  # Is the innermost bracket of a `wf_flow_context/4` the mapping a job key
  # opens in `jobs`?
  defp wf027_flow_job?({:ok, [job, "jobs" | _], [:map | _], _}) when is_binary(job), do: true
  defp wf027_flow_job?(_), do: false

  # The reusable's identity as a regex source. GitHub resolves the owner and
  # the repository name in either case (`Hyperpolymath/Standards` is
  # `hyperpolymath/standards`); the workflow path inside the repository is a
  # file path and is matched as written. The fleet fixer reads the pin
  # case-insensitively and rewrites it as the caller spells it.
  defp wf027_reusable_re(reusable) do
    case String.split(reusable, "/", parts: 3) do
      [owner, repo, path] ->
        if Hypatia.Rules.StandardsRepo.name?(owner <> "/" <> repo) do
          # A standards reusable is the same call under either name of the
          # repository (Hypatia.Rules.StandardsRepo).
          Hypatia.Rules.StandardsRepo.path_regex_source(reusable)
        else
          "(?i:#{Regex.escape(owner <> "/" <> repo)})/#{Regex.escape(path)}"
        end

      _ ->
        Regex.escape(reusable)
    end
  end

  # Is the `uses` at `uses_at` the entry of a flow mapping a job key opens?
  # The job's own line under `jobs:` (`rust: { uses: ... }`), the whole
  # `jobs:` mapping written on one line (`jobs: { rust: { uses: ... } }`), or
  # the same inside a root `{ ... }`: the keys that open the brackets still
  # unclosed before `uses` (nearest first), then the line's block ancestors,
  # must read a job key, then `jobs`. Each key is the last entry before its
  # brace, split quote-aware, so a sibling's `name: "CI, build"` separates
  # nothing.
  defp wf027_inline_job?(lines, flow, line, number, uses_at) do
    # On a line inside a flow collection opened above, the brackets before
    # `uses` are read from where the collection began.
    if wf_in_flow?(flow, number),
      do: wf027_flow_job?(wf_flow_context(lines, flow, number, uses_at)),
      else: wf027_line_job?(lines, line, number, uses_at)
  end

  defp wf027_line_job?(lines, line, number, uses_at) do
    case {wf_flow_opener_stack(line, uses_at), wf_ancestor_keys(lines, number)} do
      {{:ok, [open | _] = stack}, ancestors} when is_integer(open) and is_list(ancestors) ->
        keys = wf_flow_opener_keys(line, stack) ++ ancestors
        match?([job, "jobs" | _] when is_binary(job), keys)

      _ ->
        false
    end
  end

  # The key whose value is the flow mapping opening at byte `open`: the last
  # top-level entry of `line[from, open)` must read `key:` and nothing else.
  defp wf027_key_before(line, from, open) do
    # Node properties before the bracket name the collection, not its key:
    # `rust: &job {` is the job `rust`, `[ &step {` an anonymous step.
    entry =
      line
      |> binary_part(from, open - from)
      |> wf_flow_split(?,)
      |> List.last("")
      |> String.trim()
      |> String.replace(~r/(?:^|\s+)(?:[&!]\S*)(?:\s+[&!]\S*)*$/, "")
      |> String.trim()

    case Regex.run(~r/^["']?([\w.-]+)["']?\s*:$/, entry) do
      [_, key] -> key
      _ -> nil
    end
  end

  # The fleet fixer refuses (exit 2, file untouched) a value continued below
  # its key, one carrying an anchor or tag, and a flow-style job mapping: a
  # finding on any of them (`review`) routes to review.
  defp wf027_finding(filename, number, sha, rev, review) do
    %{
      rule: "WF027",
      type: :known_broken_reusable_revision,
      file: filename,
      line: number,
      reusable: rev.reusable,
      current_sha: sha,
      replacement_sha: rev.replacement,
      # Advisory, as WF026: reported and routed, never a blocking finding.
      severity: :low,
      advisory: true,
      auto_fixable: review == nil,
      fix_script: if(review == nil, do: "fix-stale-reusable-pin.sh", else: nil),
      reason:
        "`#{rev.reusable}` is pinned at `@#{sha}`, inside #{rev.window}: " <>
          rev.reason <>
          " Fixed by #{rev.fixed_by}; move the pin to " <>
          "`@#{rev.replacement}`." <> wf027_review_note(review),
      action: :rewrite_stale_reusable_pin
    }
  end

  defp wf027_review_note(nil), do: ""

  defp wf027_review_note(:continued),
    do:
      " The `uses:` value is continued below its key; the repair script rewrites " <>
        "a reference on the key's own line only, so this one routes to review."

  defp wf027_review_note(:alias),
    do:
      " The `uses:` value is an alias of an anchored reference; the repair script " <>
        "does not rewrite anchored values, so this one routes to review."

  defp wf027_review_note(:properties),
    do:
      " The `uses:` value carries a YAML anchor or tag; the repair script rewrites " <>
        "a bare reference only, so this one routes to review."

  defp wf027_review_note(:flow),
    do:
      " The job is written as a flow mapping; the repair script rewrites block-style " <>
        "jobs only, so this one routes to review."

  defp wf027_review_note(:node_alias),
    do:
      " The call is made by a job given as an alias of a mapping anchored outside " <>
        "`jobs:`; the repair script rewrites the keys of `jobs:` only, so this one " <>
        "routes to review."

  # ─── WF028: Reusable-workflow call into a moved repository ────────────

  # Repositories that moved to another owner (transfer or rename). GitHub
  # redirects git and the REST API for the old name, but does not follow the
  # redirect for a reusable workflow's `uses:`: the caller ends in
  # `startup_failure` ("workflow was not found") before any job is created.
  # Measured for standards on 2026-10-26 (hypatia governance runs
  # 40424594888, 40424740139, before and after #958 re-pointed it).
  #
  # One row per move. A transfer into the org (standards D97) is a row, not
  # a rule: append it when the move lands, never before.
  @moved_repositories Enum.map(Hypatia.Rules.StandardsRepo.former(), fn former ->
                        %{
                          former: former,
                          current: Hypatia.Rules.StandardsRepo.current(),
                          moved: "2026-10-25"
                        }
                      end)

  @doc "The (former, current) table WF028 fires on."
  def moved_repositories, do: @moved_repositories

  @doc """
  WF028: a job calls a reusable workflow by a repository name that has
  moved to another owner.

  Every such call is a certain `startup_failure`: the redirect GitHub keeps
  for the old name is not followed when a reusable is resolved. Re-pointing
  the owner keeps the pinned revision, since a move keeps the history.

  The call is found where WF027 finds it — a `uses:` key of a job under
  `jobs:`, in block or flow style, continued below its key, or made through
  a job alias — so a path inside a `run:` script, an `env:` value or a
  comment never fires. A step's `uses:` (an action, not a reusable) is out
  of scope: the redirect is unmeasured there.

  No fleet script rewrites the owner yet, so the finding carries
  `auto_fixable: false`. It is `advisory: true` with severity low, as WF026
  and WF027: reported and routed, never a blocking gate (AGENTIC
  `must-not-promote-to-blocking`).
  """
  def check_reusable_call_to_moved_repository(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      lines = String.split(content, "\n")
      continued = wf_continued_uses(lines)
      flow = wf_flow_lines(content)

      # Only an alias given as a job's value (`rust: *job`, `jobs: { a: *j }`)
      # makes a reusable call; `- *step` and `steps: [ *step ]` are steps.
      # PinSites classifies each alias where it stands, not by its line.
      aliased =
        for %{key_line: n, kind: :job} <- Hypatia.Rules.PinSites.node_alias_sites(content),
            into: MapSet.new(),
            do: n

      Enum.flat_map(@moved_repositories, fn move ->
        # The value of a `uses:` key, as YAML reads the scalar: a quoted one
        # runs to its closing quote, whatever it holds; an unquoted one ends
        # at whitespace in block style (a branch or tag may hold `,` `}` `]`
        # `#`) and, inside a flow collection, at the delimiters too.
        key = "[\"']?uses[\"']?\\s*:\\s*(?:[&!]\\S*\\s+)*"
        quoted = "\"([^\"]*)\"|'([^']*)'"
        block_value = "(?:" <> quoted <> "|([^\\s\"']+))"
        flow_value = "(?:" <> quoted <> "|([^\\s\"'#,}\\]]+))"

        block_re = Regex.compile!("^\\s*(?:-\\s+)?" <> key <> block_value)
        leading_flow_re = Regex.compile!("^\\s*(?:-\\s+)?" <> key <> flow_value)
        inline_re = Regex.compile!("[{,]\\s*" <> key <> flow_value)

        # The path ends at the workflow's extension: a file name may hold
        # `@` (`build@nightly.yml@release`), and so may a branch.
        call_re =
          Regex.compile!(
            "^(?i:#{Regex.escape(move.former)})/(\\.github/workflows/.+?\\.ya?ml)@(.+)$"
          )

        lines
        |> Enum.with_index(1)
        |> Enum.flat_map(fn {line, number} ->
          line =
            case Map.fetch(continued, number) do
              {:ok, {joined, _how}} -> joined
              :error -> line
            end

          live? = number in aliased
          leading_re = if wf_in_flow?(flow, number), do: leading_flow_re, else: block_re

          # The `uses:` the line opens with, and every one a later flow entry
          # on the same line opens (`uses: a@x }, b: { uses: a@y`).
          leading =
            case Regex.run(leading_re, line, return: :index) do
              [_full | groups] ->
                if wf027_job_key(lines, flow, number) != nil or live?,
                  do: [wf028_scalar(line, groups)],
                  else: []

              nil ->
                []
            end

          inline =
            inline_re
            |> Regex.scan(line, return: :index)
            |> Enum.flat_map(fn [{at, _} | groups] ->
              if wf027_inline_job?(lines, flow, line, number, at + 1) or live?,
                do: [wf028_scalar(line, groups)],
                else: []
            end)

          for value <- leading ++ inline,
              [_, path, ref] <- [Regex.run(call_re, value)] do
            wf028_finding(filename, number, path, ref, move)
          end
        end)
      end)
    end)
  end

  # The scalar a value match captured: whichever of the double-quoted,
  # single-quoted and plain groups took part.
  defp wf028_scalar(line, groups) do
    {at, len} = Enum.find(groups, {0, 0}, fn {at, _} -> at >= 0 end)
    binary_part(line, at, len)
  end

  defp wf028_finding(filename, number, path, ref, move) do
    %{
      rule: "WF028",
      type: :reusable_call_to_moved_repository,
      file: filename,
      line: number,
      reusable: move.former <> "/" <> path,
      current_sha: ref,
      replacement: move.current <> "/" <> path <> "@" <> ref,
      # Advisory, as WF026/WF027: reported and routed, never a blocking finding.
      severity: :low,
      advisory: true,
      auto_fixable: false,
      fix_script: nil,
      reason:
        "`#{move.former}` moved to `#{move.current}` on #{move.moved}. GitHub does not " <>
          "follow the redirect for a reusable workflow's `uses:`, so this call ends in " <>
          "`startup_failure` (\"workflow was not found\") before any job runs. Re-point " <>
          "it to `#{move.current}/#{path}@#{ref}` (same revision), regenerate " <>
          "`actions.lock` with its tool, and make sure the repository's Actions " <>
          "allow-list admits the new owner.",
      action: :repoint_moved_reusable
    }
  end

  # ─── WF017: Secret-consuming action without secret-presence gate ──────

  # Actions whose primary input is a secret and that fail outright when
  # the secret is empty. Each entry: `{action_prefix, param_name}` —
  # the `param_name` is the `with:` key the action reads from.
  #
  # When the param value interpolates `${{ secrets.X }}` and the step has
  # no `if: secrets.X != ''` gate (or equivalent env-based gate), the
  # workflow fails every run on repos where the secret hasn't been
  # propagated. The Mirror radicle (26 repos) + Instant Sync (39 repos)
  # bugs caught by the 2026-05-30 audit were both this pattern.
  @secret_consuming_actions [
    {"webfactory/ssh-agent", "ssh-private-key"},
    {"peter-evans/repository-dispatch", "token"},
    {"peter-evans/create-pull-request", "token"},
    {"actions-ecosystem/action-create-comment", "github_token"}
  ]

  @doc """
  WF017: Detect a workflow step that uses a known secret-consuming
  action with `${{ secrets.X }}` as its primary input, but lacks
  an `if: secrets.X != ''` gate.

  Caught Mirror radicle (\`webfactory/ssh-agent\` per-forge × 7 jobs) and
  Instant Sync (\`peter-evans/repository-dispatch\`) on the 2026-05-30
  audit — combined 65 estate repos failing on every push because the
  required secret was missing on those repos. Fix recipe is in
  hyperpolymath/standards#305 (source-level gate in the reusable).

  Sensitivity / specificity:
    * Specific — only fires for the curated `@secret_consuming_actions`
      list. Generic `uses: actions/checkout` with no secret param does
      not fire. Actions that gracefully no-op on empty secret (rare —
      most error out) are not in the list. The list is data-driven so
      a new entry does not require a code change.
    * Sensitive — fires regardless of where the `if:` would appear
      (anywhere in the step block), and works for both single-line
      `if: secrets.X != ''` and YAML-block `if: \${{ secrets.X != '' }}`.
      Both quote styles (\` ` `\`` and \` ' `\`) match.
  """
  def check_ungated_secret_action(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      stripped = strip_comments(content)

      stripped
      |> extract_steps_using_known_actions()
      |> Enum.flat_map(fn step_block ->
        Enum.flat_map(@secret_consuming_actions, fn {action_prefix, param} ->
          if Regex.match?(~r/uses:\s*#{Regex.escape(action_prefix)}@/, step_block) do
            # Pull the secret name out of the `with: <param>: ${{ secrets.X }}` line.
            case Regex.run(
                   ~r/#{Regex.escape(param)}:\s*\$\{\{\s*secrets\.([A-Z_][A-Z0-9_]*)\s*\}\}/,
                   step_block
                 ) do
              [_full, secret_name] ->
                # Step is "gated" if there is an `if:` line in the step block
                # that references the secret being empty/non-empty. The `if:`
                # can be the step's leading key (`  - if: ...`) or a regular
                # interior key (`    if: ...` after a `- uses:` head). Both
                # quote-styles + both block / `${{ … }}` expression forms
                # count.
                gate_re =
                  ~r/(?:^|\n)\s*(?:-\s+)?if:[^\n]*secrets\.#{Regex.escape(secret_name)}[^\n]*!=\s*['"]['"]?/

                # Also accept the valid env+output cross-step gate: a prior step
                # reads the secret into `env:` and writes a presence boolean to
                # $GITHUB_OUTPUT, and the consuming step is gated with
                # `if: steps.<id>.outputs.<name>`. The `secrets` context is NOT
                # available in step-level `if:`, so the same-step
                # `if: secrets.X != ''` form above is itself unusable — this
                # env+output pattern is the correct GitHub Actions idiom.
                env_output_gate? =
                  Regex.match?(~r/if:[^\n]*steps\.[A-Za-z0-9_-]+\.outputs\./, step_block) and
                    String.contains?(stripped, "GITHUB_OUTPUT")

                # Third valid idiom, previously unrecognised: a job-level
                # `env:` binds a presence boolean from the secret and steps gate
                # on it. Measured 2026-07-29 on gitbot-fleet's instant-sync.yml,
                # which does exactly this on every step plus an else branch, and
                # was still reported :high.
                #
                #     env:
                #       HAS_TOKEN: ${{ secrets.FARM_DISPATCH_TOKEN != '' }}
                #     steps:
                #       - if: env.HAS_TOKEN == 'true'
                env_boolean_gate? =
                  case Regex.run(
                         ~r/(\w+):\s*\$\{\{\s*secrets\.#{Regex.escape(secret_name)}\s*!=\s*['"]['"]?\s*\}\}/,
                         stripped
                       ) do
                    [_, env_name] ->
                      Regex.match?(~r/if:[^\n]*env\.#{Regex.escape(env_name)}\b/, stripped)

                    _ ->
                      false
                  end

                # The RAW-MAPPING form, which is what the estate actually uses:
                #
                #     env:
                #       TOKEN: ${{ secrets.X }}     # raw secret, no comparison
                #     steps:
                #       - if: env.TOKEN != ''       # comparison lives in the if
                #
                # The clause above only matches when the comparison is inside
                # the env assignment. Both forms are correct, and this one is
                # the one the estate documents, so without this clause the rule
                # reported every correctly-gated workflow — 7 such alerts in
                # `standards` alone, on files whose comments explain the pattern.
                #
                # ⚠ Why the indirection exists at all, and why the rule must not
                # ask for `if: secrets.X != ''` directly: the `secrets` context
                # is NOT available in `if:`. A workflow that uses it there is
                # REJECTED BEFORE ANY JOB IS CREATED — a startup_failure with no
                # log and no check run, which silently killed CI in 51 estate
                # repositories. Flagging the env form pushes authors toward the
                # one construction that cannot work.
                env_raw_gate? =
                  case Regex.run(
                         ~r/(\w+):\s*\$\{\{\s*secrets\.#{Regex.escape(secret_name)}\s*\}\}/,
                         stripped
                       ) do
                    [_, env_name] ->
                      Regex.match?(
                        ~r/if:[^\n]*env\.#{Regex.escape(env_name)}\b[^\n]*(!=|==)/,
                        stripped
                      )

                    _ ->
                      false
                  end

                if Regex.match?(gate_re, step_block) or env_output_gate? or
                     env_boolean_gate? or env_raw_gate? do
                  []
                else
                  [
                    %{
                      rule: "WF017",
                      type: :secret_action_without_presence_gate,
                      file: filename,
                      action: action_prefix,
                      secret: secret_name,
                      severity: :high,
                      reason:
                        "Step uses `#{action_prefix}` with `#{param}: " <>
                          "\${{ secrets.#{secret_name} }}` but has no " <>
                          "`if: secrets.#{secret_name} != ''` gate. On repos " <>
                          "where the secret hasn't been propagated the action " <>
                          "fails on every push, red-maining the repo. Add the " <>
                          "step-level gate (or env+if pattern) so the missing-" <>
                          "secret path is a clean skip instead of a red.",
                      fix_recipe: :add_secret_presence_gate
                    }
                  ]
                end

              _ ->
                []
            end
          else
            []
          end
        end)
      end)
    end)
  end

  # Like `extract_checkout_blocks/1` but yields step blocks that use any
  # of the known secret-consuming actions. Re-uses the same column-aware
  # step-boundary detection; just changes the final filter predicate.
  defp extract_steps_using_known_actions(content) do
    prefixes =
      Enum.map(@secret_consuming_actions, fn {p, _} -> Regex.escape(p) end) |> Enum.join("|")

    filter_re = ~r/uses:\s*(?:#{prefixes})@/

    content
    |> String.split("\n")
    |> Enum.reduce({[], nil}, fn line, {steps, current} ->
      cond do
        matches = Regex.run(~r/^(\s*)-\s+/, line) ->
          indent = matches |> Enum.at(1) |> String.length()
          {flush_step(steps, current), {indent, [line]}}

        is_tuple(current) ->
          {step_indent, lines_acc} = current

          if String.trim(line) == "" or leading_indent(line) > step_indent do
            {steps, {step_indent, [line | lines_acc]}}
          else
            {flush_step(steps, current), nil}
          end

        true ->
          {steps, current}
      end
    end)
    |> then(fn {steps, current} -> flush_step(steps, current) end)
    |> Enum.reverse()
    |> Enum.filter(&Regex.match?(filter_re, &1))
  end

  # ─── Helpers ───────────────────────────────────────────────────────────

  # Strip YAML / shell line comments from a workflow body before pattern
  # matching. Used by the security-pattern checks (download-then-run,
  # actions-expression-injection, unsafe-curl-payload) so a workflow that
  # *describes* the bad pattern in a comment ("safer than curl | sh") is
  # not itself flagged for it. Leaves shebangs and inline trailing-comments
  # untouched — only full-line comments are removed, which is the most
  # common false-positive case.
  defp strip_comments(content) do
    Regex.replace(~r/^[ \t]*#[^\n]*$/m, content, "")
  end

  defp coverage_percentage(workflow_files) do
    present = Enum.count(@standard_workflows, &(&1 in workflow_files))
    round(present / length(@standard_workflows) * 100)
  end

  defp severity_for_workflow(wf) do
    case wf do
      "hypatia-scan.yml" -> :critical
      "codeql.yml" -> :high
      "scorecard.yml" -> :high
      "quality.yml" -> :high
      "mirror.yml" -> :high
      "secret-scanner.yml" -> :high
      "security-policy.yml" -> :medium
      _ -> :low
    end
  end

  @doc """
  Detect `actions/checkout` steps that use `${{ github.workflow_sha }}`
  as `ref:` for a `repository:` that is NOT the caller's own repository.

  Root cause caught: in any workflow (especially reusable), the
  `github.workflow_sha` context resolves to the *caller's* commit SHA,
  not the SHA of the workflow YAML itself. Passing it as `ref:` to
  `actions/checkout` of a foreign repository — e.g. checking out
  `hyperpolymath/standards` at the caller's SHA — fails with
  `git fetch ... exit code 128` ("No commit found for SHA"), and after
  three retries the step (and the whole job) fails.

  This was the root cause of the estate-wide stuck-PR cascade on
  2026-05-26 (governance / Language / package anti-pattern policy
  failing on every PR after `governance-reusable.yml` was updated to
  fetch its own scripts dir via the foreign-checkout pattern with
  `ref: ${{ github.workflow_sha }}`). See `hyperpolymath/standards`
  PR fixing `governance-reusable.yml:155`.

  Sensitivity / specificity:

  * Specific — only fires when `repository:` is a literal owner/name
    OR `${{ inputs.* }}` (i.e. NOT `${{ github.repository }}`). The
    common safe pattern `repository: ${{ github.repository }}` +
    `ref: ${{ github.workflow_sha }}` is fine (or at least
    consistent — both refer to the caller).
  * Sensitive — catches the pattern regardless of which job/step it
    lives in. Operates on the parsed `with:` block keys so it does
    not depend on key ordering.
  """
  def check_workflow_sha_as_foreign_ref(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      content
      |> strip_comments()
      |> extract_checkout_blocks()
      |> Enum.flat_map(fn block ->
        repo_value = block_value(block, ~r/^\s*repository:\s*([^\n]+?)\s*$/m)
        ref_value = block_value(block, ~r/^\s*ref:\s*([^\n]+?)\s*$/m)

        cond do
          is_nil(repo_value) or is_nil(ref_value) ->
            []

          true ->
            uses_workflow_sha? = Regex.match?(~r/\$\{\{\s*github\.workflow_sha\s*\}\}/, ref_value)

            caller_repo? = Regex.match?(~r/\$\{\{\s*github\.repository\s*\}\}/, repo_value)

            if uses_workflow_sha? and not caller_repo? do
              [
                %{
                  type: :workflow_sha_as_foreign_ref,
                  file: filename,
                  detail:
                    "actions/checkout uses `ref: ${{ github.workflow_sha }}` " <>
                      "for `repository: #{repo_value}` (not the caller's own " <>
                      "repo). `github.workflow_sha` resolves to the caller's " <>
                      "commit SHA, which does not exist in `#{repo_value}` — " <>
                      "the fetch fails with exit code 128. Pin `ref:` to a " <>
                      "branch (e.g. `main`), tag, or explicit SHA in the " <>
                      "target repo, or expose it as an input on the reusable.",
                  severity: :critical,
                  action: :pin_external_checkout_ref
                }
              ]
            else
              []
            end
        end
      end)
    end)
  end

  @doc """
  Detect reusable workflows (`on: workflow_call`) that check out their
  *own* repository at a caller-derived ref (`github.ref`,
  `github.head_ref`, `github.sha`).

  In a reusable-workflow context, all `github.*` ref variables resolve
  to the *caller*'s values, not the reusable repo's. A reusable that
  does:

      uses: actions/checkout@…
      with:
        repository: hyperpolymath/standards   # this repo
        ref: ${{ github.ref }}                # caller's branch ref

  …will try to fetch the caller's branch from the reusable's repo,
  which almost always fails (the caller's branch name doesn't exist
  here). The two safe shapes are:

    1. `ref:` omitted (defaults to the reusable's default branch), or
    2. `ref:` pinned to a specific branch/tag/SHA of the reusable's
       repo, or
    3. an explicit `inputs.*_ref` threaded through from the caller.

  This is the structural cousin of the `workflow_sha` bug — same
  failure mode (`exit code 128 — No commit found`), different context
  variable.
  """
  def check_reusable_caller_context_self_checkout(workflow_contents) do
    caller_ref_re = ~r/\$\{\{\s*github\.(?:ref|head_ref|sha|workflow_sha)\s*\}\}/

    Enum.flat_map(workflow_contents, fn {filename, content} ->
      stripped = strip_comments(content)
      reusable? = Regex.match?(~r/^\s*workflow_call:/m, stripped)

      if reusable? do
        stripped
        |> extract_checkout_blocks()
        |> Enum.flat_map(fn block ->
          repo_value = block_value(block, ~r/^\s*repository:\s*([^\n]+?)\s*$/m)
          ref_value = block_value(block, ~r/^\s*ref:\s*([^\n]+?)\s*$/m)

          cond do
            is_nil(repo_value) or is_nil(ref_value) ->
              []

            true ->
              caller_repo_var? = Regex.match?(~r/\$\{\{\s*github\.repository\s*\}\}/, repo_value)

              foreign_literal_self? =
                not caller_repo_var? and
                  String.contains?(repo_value, "/") and
                  not String.starts_with?(repo_value, "${{")

              caller_derived_ref? = Regex.match?(caller_ref_re, ref_value)

              if foreign_literal_self? and caller_derived_ref? do
                [
                  %{
                    type: :reusable_caller_context_self_checkout,
                    file: filename,
                    detail:
                      "Reusable workflow (`on: workflow_call`) checks out a " <>
                        "literal foreign repo `#{repo_value}` at " <>
                        "`ref: #{ref_value}` — a caller-context variable. In a " <>
                        "reusable workflow, `github.ref` / `head_ref` / `sha` / " <>
                        "`workflow_sha` resolve to the *caller's* values, which " <>
                        "do not exist in `#{repo_value}`, so the fetch fails " <>
                        "with exit code 128. Pin `ref:` to a literal branch/tag/SHA " <>
                        "in `#{repo_value}`, omit it (defaults to the default " <>
                        "branch), or thread an explicit input through from the caller.",
                    severity: :critical,
                    action: :pin_reusable_self_checkout_ref
                  }
                ]
              else
                []
              end
          end
        end)
      else
        []
      end
    end)
  end

  # ─── Helpers for the two checkout-shape rules above ────────────────────
  #
  # `extract_checkout_blocks/1` splits the file into YAML steps (each
  # starting at a `- key:` line), then returns only the steps whose body
  # contains `uses: actions/checkout@…`. This handles both shapes a
  # checkout step can take:
  #
  #     - uses: actions/checkout@SHA          # uses-first
  #       with: { … }
  #
  #     - name: Check out X                   # name-first
  #       uses: actions/checkout@SHA
  #       with: { … }
  #
  # Unlike a single multi-line regex, it does not get tripped by interior
  # `with:` / `repository:` / `ref:` / `path:` sub-keys.
  defp extract_checkout_blocks(content) do
    content
    |> String.split("\n")
    |> Enum.reduce({[], nil}, fn line, {steps, current} ->
      cond do
        # A step starts at any line of shape `  - key:` (the `-` is the
        # list-item marker; whatever follows is the first key of the
        # step). We anchor on the `-` column so we know when the step
        # ends.
        matches = Regex.run(~r/^(\s*)-\s+/, line) ->
          indent = matches |> Enum.at(1) |> String.length()
          {flush_step(steps, current), {indent, [line]}}

        is_tuple(current) ->
          {step_indent, lines_acc} = current

          if String.trim(line) == "" or leading_indent(line) > step_indent do
            {steps, {step_indent, [line | lines_acc]}}
          else
            # Less- or equal-indented line that isn't a new `- key:`
            # closes the current step (and we don't start a new one).
            {flush_step(steps, current), nil}
          end

        true ->
          {steps, current}
      end
    end)
    |> then(fn {steps, current} -> flush_step(steps, current) end)
    |> Enum.reverse()
    |> Enum.filter(&Regex.match?(~r/uses:\s*actions\/checkout@/, &1))
  end

  defp flush_step(steps, nil), do: steps

  defp flush_step(steps, {_indent, lines_acc}) do
    [lines_acc |> Enum.reverse() |> Enum.join("\n") | steps]
  end

  defp leading_indent(line) do
    case Regex.run(~r/^(\s*)/, line) do
      [_, ws] -> String.length(ws)
      _ -> 0
    end
  end

  defp block_value(block, regex) do
    case Regex.run(regex, block) do
      [_, value] -> String.trim(value)
      _ -> nil
    end
  end

  @doc """
  Check for flawed regex patterns in workflow files.
  Detects common mistakes like unescaped dots, overly broad matches,
  and regex patterns that could cause false positives in CI checks.
  """
  def check_flawed_regex(workflow_contents) when is_map(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      flaws = []

      # Detect unescaped dots in grep patterns (e.g., grep "foo.bar" matches "fooXbar")
      flaws =
        if Regex.match?(~r/grep\s+(-[a-zA-Z]*\s+)*["'][^"']*(?<!\\)\.[^"']*["']/, content) do
          [
            %{
              rule: "flawed_regex",
              severity: :low,
              file: filename,
              description: "grep with unescaped dot -- may match unintended characters"
            }
            | flaws
          ]
        else
          flaws
        end

      # Detect grep -E with * quantifier without preceding atom
      flaws =
        if Regex.match?(~r/grep\s+-[a-zA-Z]*E[a-zA-Z]*\s+["'][^"']*(?<![.\w\\])\*/, content) do
          [
            %{
              rule: "flawed_regex",
              severity: :low,
              file: filename,
              description: "grep -E with bare * quantifier -- likely needs .* or \\w*"
            }
            | flaws
          ]
        else
          flaws
        end

      flaws
    end)
  end

  def check_flawed_regex(_), do: []

  # ─── Secret-history coverage (Phase 1c) ───────────────────────────────

  # A repository is covered when it calls the estate rail, which is the only
  # invocation whose history behaviour is guaranteed by construction:
  # `standards/.github/workflows/secret-scanner-reusable.yml` checks out at
  # `fetch-depth: 0`, runs a working-tree pass AND a full-history pass, and the
  # history pass already refuses to report a pass on a shallow checkout.
  @secret_history_rail "secret-scanner-reusable.yml"

  # A direct invocation counts as coverage too -- the estate does not mandate
  # the rail -- but only its presence is asserted here, not its depth, which is
  # what `secret_scan_without_history` below is for.
  @direct_secret_scanners ~r/\b(gitleaks|trufflehog)\b/

  @doc """
  Check that the repository actually has a secret-history gate.

  This is a **coverage** assertion, not a reimplementation. Full-history secret
  scanning already exists on ~398 repositories via the estate rail; duplicating
  `gitleaks detect` in Elixir alongside it would add cost and a second thing to
  keep correct. What was missing is the assertion that the gate is *there*.

  Measured across 573 deduplicated local clones on 2026-09-15: 409 call the
  hypatia scan rail, 398 call the secret-scanner rail, and **11 run hypatia with
  no secret-history gate of any kind** -- no rail call and no direct `gitleaks`
  or `trufflehog` invocation. Those 11 are the fire population.

  Severity is `:medium` by measurement, not by taste. The reusable's blocking
  gate refuses `high` and `critical`; at `:high` this would put 11 repositories'
  `main` into a permanently-red required check the moment it shipped, with no
  PR to review, because hypatia is resolved by `git ls-remote ... HEAD` at
  runtime. Medium is visible at the default `--severity medium` threshold and
  blocks nothing.

  Deliberately NOT asserted: `secrets: inherit` on the caller. The rail's own
  header (lines 28-33) requires it so the gitleaks-*action*'s `GITHUB_TOKEN`
  reference resolves -- but the same header records that #500 replaced that
  action with a pinned, checksum-verified binary, which removed the reason.
  Probing for a requirement the artefact itself documents as superseded would
  false-positive across the fleet.

  This check reads the repository's own workflow files only. Whether the
  *history* a scan can see is real is a property of the checkout, and is
  reported separately by `git_state`'s GS008 shallow-clone probe -- a
  history-dependent finding produced under a truncated history is vacuous, and
  the two findings are meant to be read together.
  """
  def check_secret_history_coverage(workflow_contents) when is_map(workflow_contents) do
    # An absence finding has no file to name, so it anchors on the directory
    # that is its subject. Do not contort this into a filename to satisfy a
    # fixture-matching gate.
    contents = Map.values(workflow_contents)

    calls_rail? = Enum.any?(contents, &String.contains?(&1, @secret_history_rail))

    direct_files =
      Enum.filter(workflow_contents, fn {_f, c} -> Regex.match?(@direct_secret_scanners, c) end)

    cond do
      calls_rail? ->
        []

      direct_files == [] ->
        [
          %{
            rule: "missing_secret_history_scan",
            severity: :medium,
            file: ".github/workflows",
            description:
              "No secret-history scan is wired in this repository -- no call to " <>
                "`#{@secret_history_rail}` and no direct `gitleaks`/`trufflehog` invocation. " <>
                "A secret committed and later deleted is invisible to every working-tree " <>
                "scan, so this repository has no coverage for the class of leak that " <>
                "matters most. Fix: add a caller for " <>
                "`hyperpolymath/standards/.github/workflows/#{@secret_history_rail}`, which " <>
                "checks out at `fetch-depth: 0` and runs both a working-tree and a " <>
                "full-history pass.",
            action: :add_secret_history_scan
          }
        ]

      true ->
        # A direct scanner exists. It only provides history coverage if the
        # checkout feeding it is unshallow AND the scanner is not restricted to
        # working-tree mode. `gitleaks --no-git` reads the filesystem only, so a
        # deleted-but-committed secret is missed however deep the checkout is.
        Enum.flat_map(direct_files, fn {filename, content} ->
          no_git_only? =
            String.contains?(content, "--no-git") and
              not Regex.match?(~r/gitleaks\s+detect(?![^\n]*--no-git)/, content)

          deep_checkout? = Regex.match?(~r/fetch-depth:\s*0\b/, content)

          cond do
            no_git_only? ->
              [
                %{
                  rule: "secret_scan_without_history",
                  severity: :low,
                  file: filename,
                  description:
                    "`#{filename}` runs a secret scanner in working-tree mode only " <>
                      "(`--no-git`), so a secret that was committed and later deleted is " <>
                      "never examined. Fix: add a second pass without `--no-git`, or call " <>
                      "`#{@secret_history_rail}`, which runs both passes.",
                  action: :add_history_pass
                }
              ]

            not deep_checkout? ->
              [
                %{
                  rule: "secret_scan_without_history",
                  severity: :low,
                  file: filename,
                  description:
                    "`#{filename}` invokes a secret scanner but its checkout does not set " <>
                      "`fetch-depth: 0`, so the scan sees a shallow history and cannot find " <>
                      "a secret that was committed and later deleted. A history scan " <>
                      "without history reports a pass it did not earn. Fix: set " <>
                      "`fetch-depth: 0` on the checkout step, or call " <>
                      "`#{@secret_history_rail}`.",
                  action: :set_fetch_depth_zero
                }
              ]

            true ->
              []
          end
        end)
    end
  end

  def check_secret_history_coverage(_), do: []

  # ─── WF018: Scorecard wrapper missing job-level permissions ───────────
  #
  # Caller-of-`scorecard-reusable.yml` workflow without job-level
  # `security-events: write`. Reusable called-workflow permissions are
  # CAPPED by the caller's grant: even though the reusable re-asserts
  # the grant on its own analysis job, the cap silently zeros it out.
  # ossf/scorecard-action then cannot upload SARIF and the run fails
  # with `startup_failure` — no logs, no findings, the silent-CI-
  # failure class hypatia is best positioned to catch.
  #
  # Estate baseline 2026-05-30: 81 of 88 wrappers across the estate
  # were in this state (see standards#303 / #282). Canonical fix shape
  # in standards/.github/workflows/scorecard-reusable.yml docstring.
  # See hyperpolymath/hypatia#390 + memory
  # feedback_scorecard_wrapper_caller_permissions.md.

  @doc """
  WF018: Detect a `scorecard.yml` wrapper that delegates to
  `hyperpolymath/standards`'s `scorecard-reusable.yml` but lacks
  `security-events: write`.

  Sensitivity / specificity:
    * Specific — only fires when the file references
      `scorecard-reusable.yml`. A standalone scorecard workflow is not
      flagged.
    * Sensitive — looks for the literal `security-events: write` token
      anywhere in the file (workflow-level is enough since called-
      workflow permissions inherit from there, but the fix recipe
      recommends job-level for clarity).
  """
  def check_scorecard_wrapper_missing_job_permissions(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      base = Path.basename(filename)

      cond do
        base not in ["scorecard.yml", "scorecard.yaml"] ->
          []

        not String.contains?(content, "scorecard-reusable.yml") ->
          []

        Regex.match?(~r/security-events:\s*write/, content) ->
          []

        true ->
          [
            %{
              rule: "WF018",
              type: :scorecard_wrapper_missing_job_permissions,
              file: filename,
              severity: :high,
              reason:
                "scorecard.yml delegates to hyperpolymath/standards " <>
                  "`scorecard-reusable.yml` but the file does not declare " <>
                  "`security-events: write`. Reusable called-workflow " <>
                  "permissions are CAPPED by the caller's grants; the " <>
                  "reusable's own job-level grant cannot exceed what the " <>
                  "caller provides. Result: ossf/scorecard-action cannot " <>
                  "upload SARIF and the run fails with `startup_failure` " <>
                  "(no logs, no findings). Add `permissions: " <>
                  "{security-events: write, id-token: write}` at the job " <>
                  "level (preferred) or workflow level.",
              fix_recipe: :add_job_level_scorecard_perms
            }
          ]
      end
    end)
  end

  # ─── WF019: workflow-linter.yml self-referential `uses:` grep ─────────
  #
  # Repos carrying the legacy in-tree `workflow-linter.yml` (rather than
  # the consolidated `governance.yml` → standards reusable) often
  # contain a shell step that `grep`s for `uses:` across all workflow
  # files. The linter's own comments + grep command line contain
  # literal `uses:` tokens, so the linter flags itself. Fix is to
  # exempt `workflow-linter.yml` (and the sibling
  # `scorecard-enforcer.yml`) from the grep, or to migrate to the
  # consolidated governance reusable.
  #
  # Observed 4 repos in this state on the 2026-05-30 sweep
  # (ipv6-only#9 / #10, file-soup#44, fireflag#30). See
  # hyperpolymath/hypatia#337.

  @doc """
  WF019: Detect `workflow-linter.yml` that greps for `uses:` across all
  workflow files without exempting itself or the canonical
  `scorecard-enforcer.yml`.

  Sensitivity / specificity:
    * Specific — fires only when all three markers are present: file
      basename is `workflow-linter.yml`, file contains a
      `grep ... "uses:"` invocation, file does NOT contain a string
      naming `workflow-linter.yml` or `scorecard-enforcer.yml`
      (which would suggest a `grep -v` exemption is in place).
    * Sensitive — works regardless of whether the grep is in a `run:`
      block or a heredoc.
  """
  def check_workflow_linter_self_reference(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      base = Path.basename(filename)

      cond do
        base not in ["workflow-linter.yml", "workflow-linter.yaml"] ->
          []

        not Regex.match?(~r/grep[^\n]*["']uses:["']/, content) ->
          []

        Regex.match?(~r/workflow-linter\.ya?ml|scorecard-enforcer\.ya?ml/, content) ->
          []

        true ->
          [
            %{
              rule: "WF019",
              type: :workflow_linter_self_reference,
              file: filename,
              severity: :medium,
              reason:
                "workflow-linter.yml greps for `uses:` across all workflow " <>
                  "files but does not exempt itself or the sibling " <>
                  "`scorecard-enforcer.yml`. Its own comments + grep " <>
                  "command line contain literal `uses:` tokens, so the " <>
                  "linter flags itself on every run. Either add " <>
                  "`grep -v workflow-linter.yml | grep -v scorecard-enforcer.yml` " <>
                  "to the pipeline, or migrate to the consolidated " <>
                  "`governance.yml` -> standards reusable.",
              fix_recipe: :exempt_linter_from_self_grep
            }
          ]
      end
    end)
  end

  # ─── WF020: CodeQL workflow missing `language: actions` matrix entry ──
  #
  # Companion to check_codeql_language_matrix_mismatch (which catches
  # the OPPOSITE: a codeql.yml that lists a *source-scanning* language
  # on a repo with no matching source). WF020 catches the positive
  # case: almost every repo has .github/workflows/*.yml, so almost
  # every repo SHOULD declare `language: actions` in its CodeQL matrix.
  #
  # See hyperpolymath/hypatia#338.

  @doc """
  WF020: Detect a `codeql.yml` that does not list `language: actions`
  in its matrix, when the repo has workflow files.

  Companion to (not replacement for) `check_codeql_language_matrix_mismatch`.
  This rule says "you SHOULD also scan workflow YAML"; the other says
  "you should NOT pretend to scan a source language you don't have".

  Sensitivity / specificity:
    * Specific — only fires when codeql.yml exists AND lacks
      `language: actions` AND the workflow_contents map contains at
      least one non-codeql workflow.
    * Sensitive — handles both YAML list-style and inline-string-style
      `language: actions`.
  """
  def check_codeql_missing_actions_language(workflow_contents) do
    has_other_workflows? =
      Enum.any?(workflow_contents, fn {f, _} ->
        base = Path.basename(f)
        String.ends_with?(base, ".yml") and not codeql_workflow?(f)
      end)

    if not has_other_workflows? do
      []
    else
      Enum.flat_map(workflow_contents, fn {filename, content} ->
        if codeql_workflow?(filename) and
             not Regex.match?(~r/language:\s*actions(?:\s|$)/m, content) do
          [
            %{
              rule: "WF020",
              type: :codeql_missing_actions_language,
              file: filename,
              severity: :medium,
              reason:
                "codeql.yml does not list `language: actions` in its " <>
                  "matrix, but the repo has workflow files. CodeQL's " <>
                  "`actions` language scans workflow YAML for injection " <>
                  "and other CI/CD-specific weaknesses — every repo with " <>
                  "workflows benefits. Add an entry to `matrix.include` " <>
                  "with `language: actions` + `build-mode: none`.",
              fix_recipe: :add_codeql_actions_language
            }
          ]
        else
          []
        end
      end)
    end
  end

  # ─── WF021: Read-only check workflow missing `concurrency:` ───────────
  #
  # A workflow that runs on `pull_request`/`push` and is read-only (a
  # `permissions:` block with no `: write` scope) wastes runner minutes on
  # rapid-push PRs when it lacks a top-level `concurrency:` block with
  # `cancel-in-progress`. The read-only condition is the safety gate:
  # cancelling an in-flight run is only safe when the workflow neither
  # publishes nor mutates. Publishers/mutators (release, npm/jsr publish,
  # Pages deploy, repo mirror/sync, `git push`) are skipped.
  #
  # See hyperpolymath/hypatia#365 (cohort hypatia#333, pattern 6).

  @wf021_skip ~r/npm publish|jsr publish|gh-release|softprops|publish_results|peaceiris|deploy-pages|pages-build|mirror|GITLAB_SSH|BITBUCKET_SSH|git push|release:/i

  @doc """
  WF021: Detect a read-only check workflow (runs on pull_request/push, has a
  `permissions:` block with no `: write` scope) that lacks a top-level
  `concurrency:` block.

  Sensitivity / specificity:
    * Specific — the read-only gate (no `: write` anywhere) plus a
      publisher/mutator skip-list keep release/publish/Pages/mirror
      workflows out, where `cancel-in-progress` would be unsafe.
    * Sensitive — fires on any PR/push read-only workflow missing a
      top-level `concurrency:` key.
  """
  def check_concurrency_missing_readonly(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      triggers? = Regex.match?(~r/pull_request|^\s+push:/m, content)
      no_concurrency? = not Regex.match?(~r/^[ \t]*concurrency:/m, content)

      read_only? =
        Regex.match?(~r/^[ \t]*permissions:/m, content) and
          not String.contains?(content, ": write")

      if triggers? and no_concurrency? and read_only? and not Regex.match?(@wf021_skip, content) do
        [
          %{
            rule: "WF021",
            type: :concurrency_missing_readonly,
            file: filename,
            severity: :low,
            reason:
              "read-only check workflow runs on pull_request/push but has no top-level " <>
                "`concurrency:` block; rapid-push PRs queue redundant runs. Add " <>
                "`concurrency: {group: \"${{ github.workflow }}-${{ github.ref }}\", " <>
                "cancel-in-progress: true}`. Skipped for publishers/mutators where " <>
                "cancelling is unsafe.",
            fix_recipe: :add_workflow_concurrency
          }
        ]
      else
        []
      end
    end)
  end

  # ─── ERR-WF-014: bare [push, pull_request] double trigger (D-BURN) ─────
  #
  # `on:` firing both push and pull_request with the push trigger unscoped
  # runs every PR-branch push twice — once as push, once as pull_request —
  # doubling Actions burn for zero extra signal. The ci-health sweep calls
  # this class D-BURN; scripts/ci-health/remediate.sh auto-fixes it (scope
  # push to the default branch + concurrency-cancel).

  @doc """
  ERR-WF-014: Detect the D-BURN double-trigger shape on the raw YAML string
  (same convention as every other check in this module):

    * flow form  — `on: [push, pull_request]` (either order, any spacing)
    * block form — an `on:` block carrying a *bare* `push:` (no sub-keys at
      all) alongside a `pull_request:` key

  Conservative by design: a `push:` scoped by `branches:`/`paths:`/`tags:`
  is left alone — that mirrors the ci-health sweep's D-BURN definition, and
  remediation of scoped-but-still-overlapping triggers needs human eyes.
  """
  def check_d_burn_double_trigger(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      if d_burn_double_trigger?(content) do
        [
          %{
            rule: "ERR-WF-014",
            type: :d_burn_double_trigger,
            file: filename,
            severity: :medium,
            reason:
              "`on:` fires both push and pull_request with the push trigger unscoped — " <>
                "every PR-branch push runs this workflow twice (D-BURN). Scope push to " <>
                "the default branch and add a `concurrency:` cancel block " <>
                "(scripts/ci-health/remediate.sh D-BURN applies both).",
            fix_recipe: :scope_push_and_concurrency_cancel
          }
        ]
      else
        []
      end
    end)
  end

  @d_burn_flow ~r/^on:[ \t]*\[[^\]]*\bpush\b[^\]]*\bpull_request\b[^\]]*\]|^on:[ \t]*\[[^\]]*\bpull_request\b[^\]]*\bpush\b[^\]]*\]/m
  # A bare `push:` — the next line is not indented deeper, so no sub-keys.
  @d_burn_bare_push ~r/^([ \t]+)push:[ \t]*\n(?!\1[ \t])/m

  defp d_burn_double_trigger?(content) do
    Regex.match?(@d_burn_flow, content) or
      (Regex.match?(~r/^on:[ \t]*$/m, content) and
         Regex.match?(~r/^[ \t]+pull_request:/m, content) and
         Regex.match?(@d_burn_bare_push, content))
  end

  # ─── WF022: Unanchored heading regex in inline-python workflow scripts ─
  #
  # An inline-python `run:` step that detects a markdown section heading with
  # `re.search(r'TitleCase Phrase', line)` — without anchoring to `^#` /
  # `^#{1,4}\s+` — also matches *prose* mentions of the same phrase, so the
  # parser silently walks the wrong table. This was the multi-week silent
  # gate failure on affinescript's governance check (standards#183 anchored
  # the regex). HIGH severity: estate-wide and invisible in logs.
  #
  # See hyperpolymath/hypatia#360 (cohort hypatia#333, pattern 1).

  @wf022_re_call ~r/re\.(?:search|match)\(\s*r(['"])(.*?)\1/
  @wf022_reason "inline-python heading-detection regex is not anchored to `^#` / `^\#{1,4}\\s+`; it also matches prose mentions of the heading phrase, so the parser can silently walk the wrong section. Anchor the regex to the heading shape (e.g. `^\#{1,4}\\s+.*Phrase`) and name the intended heading in a comment."

  @doc """
  WF022: Detect a heading-detection regex used unanchored inside inline
  Python in a workflow `run:` block.

  Heuristic: in a file that uses `python3`, find `re.search`/`re.match` whose
  literal pattern has a markdown-heading shape (a Title-Case word followed by
  another Title-Case word or a `[Xx]`-style char class) and does NOT start
  with `^#`.

  Sensitivity / specificity:
    * Specific — gated on `python3` presence, the heading-phrase shape, and
      absence of `^#` anchoring; single-word matches (e.g. `Error: (.+)`) and
      already-anchored patterns are ignored.
    * Sensitive — fires on any such unanchored heading regex in the file.
  """
  def check_unanchored_heading_regex(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      if String.contains?(content, "python3") do
        @wf022_re_call
        |> Regex.scan(content)
        |> Enum.map(fn [_, _q, pat] -> pat end)
        |> Enum.filter(&wf022_heading_unanchored?/1)
        |> Enum.map(fn pat ->
          %{
            rule: "WF022",
            type: :unanchored_heading_regex,
            file: filename,
            severity: :high,
            pattern: pat,
            reason: @wf022_reason,
            fix_recipe: :anchor_heading_regex
          }
        end)
      else
        []
      end
    end)
  end

  defp wf022_heading_unanchored?(pat) do
    heading_like? = Regex.match?(~r/[A-Z][a-z]+\s+(?:[A-Z][a-z]+|\[[A-Za-z])/, pat)
    heading_like? and not String.starts_with?(pat, "^#")
  end

  # ─── WF023: Stale continue-on-error mask tied to a closed issue (#364) ─
  #
  # A job carrying `continue-on-error: true` with a nearby comment like
  # "remove when #N" / "until #N" is a deliberate temporary mask. Once #N
  # closes, the mask is stale and erodes gate quality. `closed_issues` (a
  # list of closed issue numbers) is injected by the caller. Standalone (not
  # wired into audit/3 — it needs the issue-state signal). Cohort #333 p5.
  @doc """
  WF023: flag `continue-on-error: true` workflows whose "until/remove when
  #N" comment names an issue in `closed_issues`.
  """
  def check_stale_continue_on_error(workflow_contents, closed_issues) do
    closed = MapSet.new(closed_issues)

    Enum.flat_map(workflow_contents, fn {file, content} ->
      if Regex.match?(~r/continue-on-error:\s*true/, content) do
        ~r/(?:until|remove when|gated on|pending|once)\s+#(\d+)/i
        |> Regex.scan(content)
        |> Enum.map(fn [_, n] -> String.to_integer(n) end)
        |> Enum.filter(&MapSet.member?(closed, &1))
        |> Enum.uniq()
        |> Enum.map(fn n ->
          %{
            rule: "WF023",
            type: :stale_continue_on_error,
            file: file,
            severity: :medium,
            reason:
              "`continue-on-error: true` is gated on ##{n}, which is closed; drop the mask and let the job report accurately",
            issue: n
          }
        end)
      else
        []
      end
    end)
  end

  # ─── WF025: CodeQL cron-canonical conformance ─────────────────────────
  #
  # standards#286 fixed the canonical CodeQL schedule at `0 6 1 * *` (monthly,
  # 06:00 UTC on the 1st of each month) — the estate-wide cron drift sweep
  # that triggered this rule is tracked at standards#288 / standards#323 / #324.
  #
  # Two failure shapes (a sub-category drives gitbot-fleet routing):
  #
  #   :weekly_to_monthly      — `0 6 * * 1` style (any dow != *). Maps to the
  #                              #288-class follow-up (weekly → monthly).
  #
  #   :non_canonical_monthly  — monthly but at a different (HH:MM) / day-of-
  #                              month. Maps to the #324-class follow-up
  #                              (non-canonical monthly → canonical monthly).
  #
  # Both are auto-fixable via the same recipe `codeql-cron-monthly` —
  # rewrite to `0 6 1 * *`. Severity `:warn` (drift, not breakage).

  @codeql_canonical_cron "0 6 1 * *"

  @doc """
  WF025: Detect `.github/workflows/codeql.yml` cron entries that are NOT the
  canonical `0 6 1 * *` monthly schedule.

  Sub-category drives downstream routing:
    `:weekly_to_monthly`      — #288-class follow-up
    `:non_canonical_monthly`  — #324-class follow-up

  See standards#286 (canonical fix), standards#288 (estate cron drift sweep),
  standards#323 (recurring drift-detection), standards#324 (non-canonical
  one-off fan-out).
  """
  def check_codeql_cron_drift(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      if codeql_workflow_file?(filename) do
        stripped = strip_comments(content)
        crons = extract_cron_expressions(stripped)

        crons
        |> Enum.reject(&(&1 == @codeql_canonical_cron))
        |> Enum.map(fn raw ->
          subcat = cron_drift_subcategory(raw)

          %{
            rule: "WF025",
            rule_id: "WF-025",
            type: :codeql_cron_drift,
            sub_category: subcat,
            file: filename,
            severity: :warn,
            auto_fixable: true,
            recipe_id: "codeql-cron-monthly",
            canonical: @codeql_canonical_cron,
            observed: raw,
            reason:
              "CodeQL workflow uses non-canonical cron `#{raw}`. The estate-wide " <>
                "canonical (per standards#286) is `#{@codeql_canonical_cron}` " <>
                "(monthly, 06:00 UTC on the 1st). Sub-category " <>
                "`#{subcat}` — routes through #{routing_label(subcat)}.",
            action: :rewrite_cron_to_canonical
          }
        end)
      else
        []
      end
    end)
  end

  @doc """
  Public for testability: classify a cron expression against the canonical
  `0 6 1 * *` shape.

    `:canonical`              — exact match
    `:weekly_to_monthly`      — DOW field is not `*` (fires N times per week)
    `:non_canonical_monthly`  — DOW is `*` but HH/MM/DOM differ
    `:malformed`              — not a valid 5-field cron expression
  """
  def cron_drift_subcategory(@codeql_canonical_cron), do: :canonical

  def cron_drift_subcategory(raw) when is_binary(raw) do
    case String.split(String.trim(raw), ~r/\s+/, trim: true) do
      [_min, _hour, _dom, _mon, dow] when dow != "*" -> :weekly_to_monthly
      [_min, _hour, _dom, _mon, _dow] -> :non_canonical_monthly
      _ -> :malformed
    end
  end

  defp routing_label(:weekly_to_monthly), do: "standards#288 (weekly→monthly fan-out)"

  defp routing_label(:non_canonical_monthly),
    do: "standards#324 (non-canonical→canonical fan-out)"

  defp routing_label(:malformed), do: "manual triage (malformed cron)"
  defp routing_label(_), do: "manual triage"

  defp codeql_workflow_file?(filename) do
    base = Path.basename(filename)
    base == "codeql.yml" or base == "codeql.yaml"
  end

  defp extract_cron_expressions(stripped) do
    ~r/^\s*-\s*cron:\s*["']?([^"'\n#]+?)["']?\s*$/m
    |> Regex.scan(stripped, capture: :all_but_first)
    |> List.flatten()
    |> Enum.map(&String.trim/1)
  end

  # ─── WF024: Chapel ≥2.8.0 ABI / runs-on mismatch ──────────────────────

  # Chapel debian packages (`chapel-*.deb` from chapel-lang/chapel releases,
  # also fetched via `CHAPEL_DEB_URL`) link against `libclang-cpp.so.14`.
  # That SONAME ships on ubuntu-22.04 (LLVM 14, the default toolchain).
  # ubuntu-24.04 / ubuntu-latest (currently noble) carries `libclang-cpp.so.18`
  # and the chpl binary aborts at start-up with the classic
  #   "error while loading shared libraries: libclang-cpp.so.14: cannot open"
  # message. Caught proven#141 (Chapel pilot CI fix) + panic-attack#99
  # (Chapel Wave 2 — 6 cold-cache CI iterations debugging this exact ABI
  # mismatch + 3 more Chapel-2.8.0 sharp edges). The fix is `runs-on:
  # ubuntu-22.04` for any job invoking a pre-built Chapel deb.
  @chapel_marker_patterns [
    ~r/CHAPEL_DEB_URL\b/,
    ~r/chapel-[0-9]+\.[0-9]+(?:\.[0-9]+)?[-_a-zA-Z0-9]*\.deb\b/
  ]

  @doc """
  WF024: Detect a workflow job that downloads/installs a Chapel ≥2.8.0
  debian package but runs on `ubuntu-latest` or `ubuntu-24.04`, where the
  binary's `libclang-cpp.so.14` SONAME dependency is unsatisfiable.

  Heuristic: workflow content contains `CHAPEL_DEB_URL` OR a `chapel-*.deb`
  filename, AND `runs-on:` resolves to anything other than `ubuntu-22.04`.

  Recipe (from panic-attack#99): pin `runs-on: ubuntu-22.04`. Bumping
  Chapel itself to a 24.04-built artifact would also work but isn't
  upstream-available as of 2026-06-02.
  """
  def check_chapel_abi_runs_on_mismatch(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      stripped = strip_comments(content)

      chapel? = Enum.any?(@chapel_marker_patterns, &Regex.match?(&1, stripped))

      if chapel? do
        runs_on_vals =
          ~r/runs-on:\s*([^\n#]+)/
          |> Regex.scan(stripped, capture: :all_but_first)
          |> List.flatten()
          |> Enum.map(&String.trim/1)
          |> Enum.map(&String.trim(&1, "\""))
          |> Enum.map(&String.trim(&1, "'"))

        bad =
          Enum.filter(runs_on_vals, fn v ->
            v in ["ubuntu-latest", "ubuntu-24.04"] or
              String.starts_with?(v, "ubuntu-24") or
              String.starts_with?(v, "ubuntu-25")
          end)

        case bad do
          [] ->
            []

          _ ->
            [
              %{
                rule: "WF024",
                type: :chapel_abi_runs_on_mismatch,
                file: filename,
                severity: :high,
                runs_on: bad,
                auto_fixable: true,
                recipe_id: "chapel-runs-on-pin-22-04",
                reason:
                  "Workflow installs a Chapel ≥2.8.0 .deb (CHAPEL_DEB_URL or " <>
                    "chapel-*.deb), but `runs-on:` is `#{Enum.join(bad, ", ")}`. " <>
                    "The chpl binary links against libclang-cpp.so.14 (LLVM 14, " <>
                    "ubuntu-22.04 only) — newer Ubuntu images carry .so.18 and " <>
                    "chpl aborts with `cannot open shared object file` before " <>
                    "any user step runs. Pin `runs-on: ubuntu-22.04`. See " <>
                    "panic-attack#99 / proven#141.",
                action: :pin_runs_on_ubuntu_2204
              }
            ]
        end
      else
        []
      end
    end)
  end

  # ─── WF024 sub-patterns: 4 prior Chapel-2.8.0 sharp edges ─────────────
  #
  # Surfaced by panic-attack#99 (Chapel Wave 2 — 6 cold-cache CI iterations).
  # Each sub-pattern has its own fix recipe so future sessions can route
  # them through the gitbot-fleet auto-remediation pipeline without rebuilding
  # the diagnostic from scratch.
  #
  #   A. `MANPATH` is unset on minimal runners — Chapel install scripts
  #      that do `MANPATH=...:$MANPATH` produce `unbound variable` under
  #      `set -u`. Recipe: guard with `${MANPATH:-}`.
  #
  #   B. `CHPL_LLVM` is unset — chpl needs `CHPL_LLVM=bundled` (or `system`)
  #      to find its backend. Pre-2.8.0 builds defaulted; 2.8.0 errors.
  #      Recipe: export `CHPL_LLVM=bundled` (or `system`) before running.
  #
  #   C. `chpl --about` dropped in 2.8.0 — diagnostics that scrape `--about`
  #      output now fail. Recipe: switch to `chpl --version`.
  #
  #   D. `printchplenv --simple` output reformatted — old globs that match
  #      `comm-gasnet*` against the simple output now miss. Recipe: switch
  #      to `find <CHPL_HOME> -name comm-gasnet` (path-walk, not output-grep).

  @doc """
  WF024 sub-patterns: detect the 4 additional Chapel-2.8.0 sharp edges
  alongside the runs-on ABI mismatch. Each emits its own finding with a
  distinct `recipe_id` so the gitbot-fleet auto-remediation pipeline can
  apply the right targeted fix without re-deriving the diagnostic.

  Returned shapes share `rule: "WF024"`, `auto_fixable: true`, and
  `severity: :medium` (the sharp edges are CI-fatal but each has a
  one-line workaround once recognised — promoted from `:high` reserved
  for the ABI mismatch itself).
  """
  def check_chapel_sharp_edges(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      stripped = strip_comments(content)

      # Gate accepts any Chapel tell, not just a bare `chpl` invocation: a
      # workflow can drive Chapel entirely through `printchplenv` and
      # CHPL_* env vars (the exact shape check D targets) without ever
      # writing the word `chpl` — the old gate skipped those files whole.
      chapel? =
        Enum.any?(@chapel_marker_patterns, &Regex.match?(&1, stripped)) or
          Regex.match?(~r/\bchpl\b|\bprintchplenv\b|\bCHPL_[A-Z]/, stripped)

      if chapel? do
        []
        |> append_if(
          # A. MANPATH unset under set -u
          Regex.match?(~r/\bMANPATH=[^\n]*\$MANPATH\b/, stripped) and
            not Regex.match?(~r/\$\{MANPATH:-\}/, stripped),
          %{
            rule: "WF024",
            type: :chapel_manpath_unset,
            file: filename,
            severity: :medium,
            auto_fixable: true,
            recipe_id: "chapel-manpath-guard",
            reason:
              "Workflow extends `MANPATH` without guarding for the unset case " <>
                "(`$MANPATH` on a minimal runner triggers `unbound variable` under " <>
                "`set -u`). Replace with `${MANPATH:-}`. See panic-attack#99.",
            action: :guard_manpath_default
          }
        )
        |> append_if(
          # B. CHPL_LLVM unset (no export anywhere in workflow)
          not Regex.match?(~r/\bCHPL_LLVM\s*[:=]/, stripped),
          %{
            rule: "WF024",
            type: :chapel_chpl_llvm_unset,
            file: filename,
            severity: :medium,
            auto_fixable: true,
            recipe_id: "chapel-chpl-llvm-export",
            reason:
              "Workflow invokes `chpl` without exporting `CHPL_LLVM`. Chapel 2.8.0 " <>
                "no longer auto-detects the backend; add `export CHPL_LLVM=bundled` " <>
                "(or `system`) before any chpl invocation. See panic-attack#99.",
            action: :export_chpl_llvm_bundled
          }
        )
        |> append_if(
          # C. `chpl --about` was dropped in 2.8.0
          Regex.match?(~r/\bchpl\s+--about\b/, stripped),
          %{
            rule: "WF024",
            type: :chapel_chpl_about_dropped,
            file: filename,
            severity: :medium,
            auto_fixable: true,
            recipe_id: "chapel-replace-chpl-about-with-version",
            reason:
              "`chpl --about` was removed in Chapel 2.8.0. Diagnostics that scrape " <>
                "`--about` output now fail. Replace with `chpl --version`. " <>
                "See panic-attack#99.",
            action: :replace_chpl_about_with_version
          }
        )
        |> append_if(
          # D. `printchplenv --simple | grep comm-gasnet*`  (brittle glob)
          Regex.match?(
            ~r/printchplenv\s+--simple[^|\n]*\|[^|\n]*\bcomm-gasnet/,
            stripped
          ),
          %{
            rule: "WF024",
            type: :chapel_printchplenv_glob_brittle,
            file: filename,
            severity: :medium,
            auto_fixable: true,
            recipe_id: "chapel-find-comm-gasnet",
            reason:
              "`printchplenv --simple` output reformatted in Chapel 2.8.0; " <>
                "globs that grep its output for `comm-gasnet*` now miss. Switch to " <>
                "`find \"$CHPL_HOME\" -name comm-gasnet` (path-walk, not output-grep). " <>
                "See panic-attack#99.",
            action: :replace_printchplenv_grep_with_find
          }
        )
      else
        []
      end
    end)
  end

  defp append_if(list, true, finding), do: list ++ [finding]
  defp append_if(list, false, _finding), do: list
end
