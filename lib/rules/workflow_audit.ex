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
    unpinned = check_unpinned_actions(workflow_contents)
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
    reusable_caller_context_self_checkout = check_reusable_caller_context_self_checkout(workflow_contents)
    missing_timeouts = check_missing_timeout_minutes(workflow_contents)
    scorecard_publish_run = check_scorecard_publish_run_violation(workflow_contents)
    nonroot_container_eacces = check_nonroot_container_checkout_eacces(workflow_contents)
    orphan_reusable_pins = check_orphan_standards_reusable_pin(workflow_contents)
    ungated_secret_action = check_ungated_secret_action(workflow_contents)
    scorecard_wrapper_missing_perms = check_scorecard_wrapper_missing_job_permissions(workflow_contents)
    workflow_linter_self_ref = check_workflow_linter_self_reference(workflow_contents)
    codeql_missing_actions = check_codeql_missing_actions_language(workflow_contents)

    %{
      findings:
        missing ++ unpinned ++ wrong_pins ++ permission_issues ++ duplicates ++
          caching_issues ++ run_context_issues ++ download_then_run_issues ++ nperm_typos ++
          codeql_lang_mismatch ++ workflow_sha_foreign_ref ++
          reusable_caller_context_self_checkout ++ missing_timeouts ++
          scorecard_publish_run ++ nonroot_container_eacces ++ orphan_reusable_pins ++
          ungated_secret_action ++ scorecard_wrapper_missing_perms ++
          workflow_linter_self_ref ++ codeql_missing_actions,
      missing_count: length(missing),
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
      lines = String.split(content, "\n")
      in_jobs = false
      jobs_with_timeout = MapSet.new()
      jobs_seen = MapSet.new()
      current_job = nil

      {jobs_seen, jobs_with_timeout} =
        Enum.reduce(lines, {jobs_seen, jobs_with_timeout, false, nil}, fn line, {seen, with_to, in_j, curj} ->
          cond do
            String.match?(line, ~r/^jobs:\s*$/) ->
              {seen, with_to, true, nil}

            in_j and String.match?(line, ~r/^  [A-Za-z0-9_-]+:\s*$/) ->
              [_, name] = Regex.run(~r/^  ([A-Za-z0-9_-]+):/, line)
              {MapSet.put(seen, name), with_to, in_j, name}

            in_j and curj && String.match?(line, ~r/^    timeout-minutes:\s*\d+/) ->
              {seen, MapSet.put(with_to, curj), in_j, curj}

            String.match?(line, ~r/^[A-Za-z]/) ->
              {seen, with_to, false, curj}

            true ->
              {seen, with_to, in_j, curj}
          end
        end)
        |> then(fn {s, w, _, _} -> {s, w} end)

      missing = MapSet.difference(jobs_seen, jobs_with_timeout)

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
        [%{type: :missing_workflow, file: wf,
           severity: severity_for_workflow(wf),
           action: :create}]
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
  def check_unpinned_actions(workflow_contents) do
    Enum.flat_map(workflow_contents, fn {filename, content} ->
      Hypatia.Rules.WorkflowHardening.wh004_scan_content(filename, content)
      |> Enum.map(fn finding ->
        slug = finding.detail.uses
        # Reconstruct action_ref + ref from the canonical "slug"
        # (e.g. "owner/repo@vN.N.N") emitted by WH004.
        [action_ref, ref] =
          case String.split(slug, "@", parts: 2) do
            [_a, r] -> [slug, r]
            [_a]    -> [slug, ""]
          end
        _ = action_ref

        if Hypatia.Rules.SecurityErrors.pin_exempt?(slug) do
          %{
            type: :pin_exempt_accepted,
            file: filename,
            action_ref: slug,
            severity: :info,
            action: :accept_with_rationale,
            rationale: Hypatia.Rules.SecurityErrors.pin_exemption_reason(slug)
          }
        else
          severity = if ref in ["main", "master"], do: :high, else: :medium

          %{
            type: :unpinned_action,
            file: filename,
            action_ref: slug,
            severity: severity,
            action: :pin_sha,
            known_sha: Map.get(Hypatia.Rules.SecurityErrors.sha_pins(), action_ref)
          }
        end
      end)
    end)
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
        matching = Enum.find(Hypatia.Rules.SecurityErrors.sha_pins(), fn {ref, _} ->
          String.starts_with?(ref, action <> "@")
        end)

        case matching do
          {_ref, known_sha} when known_sha != sha ->
            [%{type: :wrong_sha_pin, file: filename, action_ref: action,
               current_sha: sha, expected_sha: known_sha,
               severity: :info, fix: :update_pin}]
          _ -> []
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
      findings = if String.contains?(content, "permissions: write-all") do
        [%{type: :broad_permissions, file: filename,
           detail: "permissions: write-all should be permissions: read-all with specific overrides",
           severity: :high, action: :narrow_permissions} | findings]
      else
        findings
      end

      # Check for missing permissions declaration entirely
      findings = if not Regex.match?(~r/^permissions:/m, content) do
        [%{type: :missing_permissions, file: filename,
           detail: "No permissions declaration -- add permissions: read-all",
           severity: :medium, action: :add_permissions} | findings]
      else
        findings
      end

      # Check for missing SPDX header
      findings = if not String.contains?(content, "SPDX-License-Identifier:") do
        [%{type: :missing_spdx, file: filename,
           detail: "No SPDX-License-Identifier header",
           severity: :low, action: :add_spdx} | findings]
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
          [%{type: :missing_caching, file: filename,
             detail: "#{name} missing built-in caching (e.g., cache: 'npm' or cache: true)",
             severity: :low, action: :enable_caching}]
        else
          []
        end
      end)
    end)
  end

  @doc """
  Detect potential duplicate workflows (same content, different names).
  """
  def check_duplicates(_workflow_files, workflow_contents) when map_size(workflow_contents) < 2, do: []
  def check_duplicates(_workflow_files, workflow_contents) do
    # Group by content hash
    contents_list = Enum.map(workflow_contents, fn {name, content} ->
      {name, :erlang.md5(content)}
    end)

    contents_list
    |> Enum.group_by(fn {_, hash} -> hash end)
    |> Enum.flat_map(fn {_hash, entries} ->
      if length(entries) > 1 do
        names = Enum.map(entries, fn {name, _} -> name end)
        [%{type: :duplicate_workflow, files: names,
           severity: :low, action: :consolidate}]
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
        [%{
          rule: "WF013",
          type: :npermissions_typo,
          file: filename,
          severity: :high,
          reason: "Workflow has 'npermissions' typo -- should be 'permissions'. GitHub Actions silently ignores this, running with overly broad defaults.",
          action: :fix_typo
        }]
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
    run_context_re =
      ~r/run:[\s\S]*?\$\{\{\s*github\.(?:head_ref|event\.pull_request\.(?:title|body|head\.ref)|event\.issue\.(?:title|body)|event\.comment\.body|event\.review\.body|event\.workflow_run\.(?:head_branch|display_title)|event\.commits)[^}]*\}\}/m

    unsafe_json_payload_re =
      ~r/-d\s*".*\$\{\{\s*github\.(?:head_ref|event\.pull_request\.(?:title|body|head\.ref)|event\.issue\.(?:title|body)|event\.comment\.body|event\.review\.body|event\.workflow_run\.(?:head_branch|display_title)|event\.commits)/s

    Enum.flat_map(workflow_contents, fn {filename, content} ->
      content = strip_comments(content)
      findings = []

      findings =
        if Regex.match?(run_context_re, content) do
          [%{
            type: :actions_expression_injection,
            file: filename,
            detail:
              "GitHub context value is interpolated directly inside a run block. Move context into env and use jq/quoted variables.",
            severity: :critical,
            action: :sanitize_context
          } | findings]
        else
          findings
        end

      findings =
        if Regex.match?(unsafe_json_payload_re, content) do
          [%{
            type: :unsafe_curl_payload,
            file: filename,
            detail:
              "curl JSON payload is assembled with inline GitHub context interpolation. Build payload with jq --arg.",
            severity: :high,
            action: :use_jq_payload
          } | findings]
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
        [%{
          type: :download_then_run,
          file: filename,
          detail:
            "Workflow executes remote script directly (curl/wget piped to shell). Download, verify checksum/signature, then execute.",
          severity: :high,
          action: :verify_download_integrity
        }]
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
      scanning_langs = ~w(javascript-typescript javascript typescript python go java ruby csharp c-cpp swift kotlin)

      Enum.flat_map(workflow_contents, fn {filename, content} ->
        if codeql_workflow?(filename) do
          Enum.flat_map(scanning_langs, fn lang ->
            if Regex.match?(~r/language:\s*#{Regex.escape(lang)}(?:\s|$)/m, content) do
              [%{
                type: :codeql_language_matrix_mismatch,
                file: filename,
                detail:
                  "codeql.yml lists `language: #{lang}` but the repo has no source files in any CodeQL-scannable language. The analyze job will exit 'no source files' on every run. Switch the matrix to `actions` (which scans workflow files — every repo has those).",
                severity: :high,
                action: :switch_codeql_matrix_to_actions
              }]
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

      uses_scorecard? =
        Regex.match?(~r/uses:\s*ossf\/scorecard-action@/, stripped)

      publish_true? =
        Regex.match?(~r/publish_results:\s*true/, stripped)

      # Filter out `run:` keys that appear inside top-level `defaults:` /
      # `with:` / commented blocks. Step-level `run:` is the only flavour
      # the OSSF endpoint cares about — anchored at `^\s+-\s+run:` or
      # `^\s+run:\s*\|` inside a `steps:` list.
      has_step_run? =
        Regex.match?(~r/^\s+- name:[^\n]*\n\s+run:/m, stripped) or
          Regex.match?(~r/^\s+- run:\s*[|>\n]/m, stripped) or
          Regex.match?(~r/^\s+run:\s*[|>]/m, stripped)

      if uses_scorecard? and publish_true? and has_step_run? do
        [%{
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
        }]
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
      has_user_root? =
        Regex.match?(~r/options:\s*[^\n]*--user\s+(?:root|0)\b/, stripped)

      if uses_checkout? and not has_user_root? do
        Enum.flat_map(@nonroot_container_images, fn img ->
          if Regex.match?(~r/image:\s*#{Regex.escape(img)}/, stripped) do
            [%{
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
            }]
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
        ref_re = ~r/uses:\s*#{Regex.escape(reusable_path)}@([0-9a-f]{8,40})/

        Regex.scan(ref_re, stripped)
        |> Enum.flat_map(fn [_full, sha] ->
          if Enum.any?(orphan_shas, fn orphan -> String.starts_with?(sha, orphan) end) do
            [%{
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
            }]
          else
            []
          end
        end)
      end)
    end)
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

                if Regex.match?(gate_re, step_block) do
                  []
                else
                  [%{
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
                  }]
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
    prefixes = Enum.map(@secret_consuming_actions, fn {p, _} -> Regex.escape(p) end) |> Enum.join("|")
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
            uses_workflow_sha? =
              Regex.match?(~r/\$\{\{\s*github\.workflow_sha\s*\}\}/, ref_value)

            caller_repo? =
              Regex.match?(~r/\$\{\{\s*github\.repository\s*\}\}/, repo_value)

            if uses_workflow_sha? and not caller_repo? do
              [%{
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
              }]
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
    caller_ref_re =
      ~r/\$\{\{\s*github\.(?:ref|head_ref|sha|workflow_sha)\s*\}\}/

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
              caller_repo_var? =
                Regex.match?(~r/\$\{\{\s*github\.repository\s*\}\}/, repo_value)

              foreign_literal_self? =
                not caller_repo_var? and
                  String.contains?(repo_value, "/") and
                  not String.starts_with?(repo_value, "${{")

              caller_derived_ref? = Regex.match?(caller_ref_re, ref_value)

              if foreign_literal_self? and caller_derived_ref? do
                [%{
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
                }]
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
          [%{rule: "flawed_regex", severity: :low, file: filename,
             description: "grep with unescaped dot -- may match unintended characters"} | flaws]
        else
          flaws
        end

      # Detect grep -E with * quantifier without preceding atom
      flaws =
        if Regex.match?(~r/grep\s+-[a-zA-Z]*E[a-zA-Z]*\s+["'][^"']*(?<![.\w\\])\*/, content) do
          [%{rule: "flawed_regex", severity: :low, file: filename,
             description: "grep -E with bare * quantifier -- likely needs .* or \\w*"} | flaws]
        else
          flaws
        end

      flaws
    end)
  end

  def check_flawed_regex(_), do: []

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
          [%{
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
          }]
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
          [%{
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
          }]
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
          [%{
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
          }]
        else
          []
        end
      end)
    end
  end
end
