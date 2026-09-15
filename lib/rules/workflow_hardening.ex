# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.WorkflowHardening do
  @moduledoc """
  Static detection of dangerous GitHub Actions workflow patterns.

  This module catches the high-severity defects that `actionlint` and
  `zizmor` ship as built-in audits, plus a few from the GitHub Actions
  hardening guide and recent academic literature. It does **not**
  duplicate `BaselineHealth` (those rules concern baseline drift, not
  workflow content).

  Rule IDs WH001-WH012.

  ## Provenance map

  Each rule lists the upstream tool / spec that catches the same
  defect. Where two tools cover the same defect with slightly different
  framings (e.g. actionlint's `untrusted-inputs` vs zizmor's
  `template-injection`), the upstream tools are both cited.

  - **WH001** — Template injection from `${{ github.event.* }}` into
    `run:` blocks. Same as zizmor `template-injection` and actionlint
    `untrusted-inputs`. The single highest-impact GHA defect class
    (Benedetti et al. 2022 found ~7% of public workflows reachable).
  - **WH002** — Workflow-level `permissions:` block missing, set to
    `write-all`, or has top-level `contents: write`. Cassel et al. (MSR 2024)
    measured ~74% of public workflows at default permissions. Same as scorecard
    `Token-Permissions` (TokenPermissionsID) and zizmor `excessive-permissions`.
  - **WH003** — `pull_request_target` (or `workflow_run`) trigger
    combined with checkout of PR head ref. The infamous fork-PR
    credential-leak pattern. Same as zizmor `dangerous-triggers`
    + scorecard `Dangerous-Workflow`.
  - **WH004** — `uses:` reference not pinned to a full 40-char commit
    SHA. Same as zizmor `unpinned-uses` and scorecard
    `Pinned-Dependencies`.
  - **WH005** — `credentials.password:` or similar literal credential
    in a workflow `services:` / `container:` block. Same as actionlint
    `hardcoded-credentials` and zizmor `hardcoded-container-credentials`.
  - **WH006** — Job missing `timeout-minutes`. Default GHA timeout is
    360 minutes; runaway jobs waste runner-minutes and can mask hangs.
    From Datadog CI Visibility best-practices.
  - **WH007** — Workflow missing top-level `concurrency:` group on a
    PR-triggered run. Same as zizmor `concurrency-limits`. Costs money
    and risks races on the same head.
  - **WH008** — Reusable workflow call uses `secrets: inherit`. Forwards
    every secret in the caller's scope. Same as zizmor `secrets-inherit`.
  - **WH009** — `${{ toJSON(secrets) }}` (or whole-context `${{ secrets }}`)
    dumps every secret to the runner / logs. Same as zizmor
    `overprovisioned-secrets`.
  - **WH010** — Use of deprecated workflow commands (`::set-output::`,
    `::save-state::`, `::set-env::`, `::add-path::`). GitHub has
    deprecated all four since 2022; they still partially function but
    are removed periodically. Same as actionlint
    `deprecated-workflow-commands`.
  - **WH011** — `curl … | sh` / `wget … | sh` in a `run:` block. Pipes
    network content to a shell — a classic supply-chain ingress. From
    Semgrep CI rules and OWASP CICD-SEC-3.
  - **WH012** — Untrusted input written to `$GITHUB_ENV` or
    `$GITHUB_PATH`. Persists attacker-controlled value into later steps
    of the same job. Same as zizmor `github-env`.

  ## Architecture

  All rules are pure-local file scans (no GitHub API). Each takes a
  `repo_path` and returns a list of finding maps in the standard shape:

      %{rule: "WHNNN", file: "...", severity: ..., reason: "...",
        action: ..., detail: %{...}}

  Workflow file discovery uses the same scope as BH004: only
  `<repo>/.github/workflows/*.yml`/`*.yaml` (root-level — GitHub Actions
  ignores nested `.github/workflows/` in monorepo subtrees).

  ## Implementation note

  We use regex-on-YAML-text rather than full YAML parsing because:

  1. The scanning corpus is ~370 repos × ~5 workflow files each ≈ 1800
     files. Regex is acceptably fast and easy to reason about.
  2. The patterns we care about are syntactically distinctive
     (`${{ ... }}` interpolation, `uses:` slugs, `run:` block prefix).
  3. False positives are caught downstream when the safety triangle
     routes through review (the same way `BaselineHealth` BH002 errs
     on side of caution).

  Where full YAML parsing would meaningfully improve precision
  (e.g. inter-step `$GITHUB_ENV` taint flow for WH012), the rule is
  scoped narrowly enough to make regex tractable.
  """

  require Logger

  # ─── Workflow-file discovery ────────────────────────────────────────

  @doc false
  def workflow_files(repo_path) do
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

  # Left-hand sides of `${{ ... }}` that are ATTACKER-CONTROLLED and so are
  # script-injection sources for WH001/WH012. Entries are deliberate
  # PREFIXES (`github.event.commits` must cover `...commits.*.message`), so
  # the pattern below appends a trailing boundary: without it every entry
  # also matches a LONGER identifier, and bare `github.ref` swallowed the
  # benign `github.ref_name` — 449 of 450 estate criticals were that alias.
  # `github.ref`/`github.ref_name` are NOT here: on `pull_request` they are
  # `refs/pull/N/merge` (GitHub-generated) and on `push` a ref only someone
  # with push access can set, so neither is attacker-controlled in the sense
  # GitHub's script-injection guidance means. A fork PR's branch name reaches
  # a workflow as `github.head_ref` / `event.pull_request.head.ref`, both
  # of which ARE listed.
  @untrusted_contexts ~w[
    github.event.issue.title github.event.issue.body
    github.event.pull_request.title github.event.pull_request.body
    github.event.pull_request.head.ref github.event.pull_request.head.label
    github.event.comment.body github.event.review.body
    github.event.commits github.event.head_commit.message
    github.event.head_commit.author github.event.pages
    github.head_ref
  ]
  @untrusted_pattern Regex.compile!(
                       "\\$\\{\\{\\s*(" <>
                         (@untrusted_contexts |> Enum.map(&Regex.escape/1) |> Enum.join("|")) <>
                         ")(?![A-Za-z0-9_])"
                     )

  @doc """
  The attacker-controlled contexts WH001/WH012 match on.

  Exposed so the boundary invariant can be asserted against the REAL list
  rather than a copy that silently drifts: entries are joined into one
  alternation and matched as prefixes, so ANY new member is a prefix trap
  until the trailing boundary proves otherwise.
  """
  def untrusted_contexts, do: @untrusted_contexts

  # ─── WH001: Template injection ──────────────────────────────────────

  @doc """
  WH001: `${{ github.event.* }}` (or other attacker-controlled context)
  interpolated directly into a `run:` block.

  The fix is to bind the value to an `env:` variable, then reference it
  via `"$VAR"` inside the shell:

      env:
        TITLE: ${{ github.event.pull_request.title }}
      run: echo "$TITLE"
  """
  def wh001_template_injection(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      scan_run_blocks(content)
      |> Enum.flat_map(fn {line_no, run_text} ->
        if Regex.match?(@untrusted_pattern, run_text) do
          [
            %{
              rule: "WH001",
              file: rel,
              severity: :critical,
              reason:
                "workflow #{rel}:#{line_no} interpolates an attacker-controlled " <>
                  "${{ github.event.* }} context directly into a `run:` shell " <>
                  "block — script-injection vector",
              action: :report,
              detail: %{
                line: line_no,
                fix:
                  "Bind to an `env:` variable then reference as `\"$VAR\"` " <>
                    "inside the shell. See github.com/en/actions/security-guides."
              }
            }
          ]
        else
          []
        end
      end)
    end)
  end

  # ─── WH002: Excessive workflow permissions ──────────────────────────

  @doc """
  WH002: Workflow has no top-level `permissions:` block at all, OR has
  `permissions: write-all`, OR has top-level `contents: write`. Per Cassel
  et al. 2024, ~74% of public workflows are at the default (write-all-equivalent
  for many scopes). This catches Scorecard TokenPermissionsID alerts.
  """
  def wh002_excessive_permissions(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      cond do
        Regex.match?(~r/^permissions:\s*write-all\b/m, content) ->
          [finding_wh002(rel, "set to `write-all`", :high)]

        Regex.match?(~r/^permissions:\s*\n\s+contents:\s*write/m, content) ->
          [wh002_contents_write_finding(rel, content)]

        Regex.match?(~r/^permissions:\s*\n\s+write-all:\s*true/m, content) ->
          [finding_wh002(rel, "with `write-all: true`", :high)]

        # ⚠ A job-level `permissions:` block REPLACES the workflow-level one,
        # so a workflow that scopes every job individually and omits the
        # top-level block is CORRECTLY hardened, not unhardened. Anchoring
        # this test at column 0 (the historical bug) reported those as
        # "permissions absent" and drove remediation that rewrote the
        # workflow level only.
        not permissions_declared_anywhere?(content) ->
          [finding_wh002(rel, "absent (defaults to broad permissions)", :warn)]

        true ->
          []
      end
    end)
  end

  @doc """
  Return true when a `permissions:` block is declared at EITHER the workflow
  level (column 0) or the job level (indented). Comments are not matched:
  `#` is not whitespace.
  """
  def permissions_declared_anywhere?(content) when is_binary(content) do
    Regex.match?(~r/^[ \t]*permissions:/m, content)
  end

  # Operations that consume `contents: write`. If a workflow performs one of
  # these, narrowing its effective permission to `read` does not harden it —
  # it BREAKS it, silently, at the next run rather than at PR time. Nothing in
  # CI exercises the token, so the YAML stays valid and every check stays
  # green while the capability is gone.
  @contents_write_operations [
    ~r/\bgit\s+push\b/,
    ~r/\bgit\s+commit\b/,
    ~r/\bgh\s+pr\s+(create|merge)\b/,
    ~r/\bgh\s+release\s+(create|upload|edit)\b/,
    ~r/\bsoftprops\/action-gh-release\b/,
    ~r/\bpeter-evans\/create-pull-request\b/,
    ~r/\bactions\/create-release\b/,
    ~r/\bncipollo\/release-action\b/,
    ~r/\bstefanzweifel\/git-auto-commit-action\b/
  ]

  @doc """
  Return true when the workflow text performs an operation that actually
  requires `contents: write`.
  """
  def performs_contents_write?(content) when is_binary(content) do
    Enum.any?(@contents_write_operations, &Regex.match?(&1, content))
  end

  @doc """
  Return true when at least one JOB declares its own `permissions:` block.
  A job-level block replaces the workflow-level one, so its presence means
  a workflow-level narrowing may be correct hardening rather than a break.
  """
  def job_level_permissions?(content) when is_binary(content) do
    Regex.match?(~r/^[ \t]+permissions:/m, content)
  end

  # The three probes, applied together. Judging a top-level `contents: write`
  # needs all of them:
  #   (1) is the workflow-level grant present  — the cond clause above;
  #   (2) does the file actually perform a write;
  #   (3) does a job carry its own `permissions:`.
  # Only (1) alone is NOT a finding — that was the defect that made this rule
  # recommend breaking narrowings.
  defp wh002_contents_write_finding(rel, content) do
    writes? = performs_contents_write?(content)
    job_scoped? = job_level_permissions?(content)

    cond do
      # Writes, and no job-level elevation to fall back on. Narrowing the
      # workflow level here REMOVES a capability the workflow uses.
      writes? and not job_scoped? ->
        %{
          rule: "WH002",
          file: rel,
          severity: :warn,
          reason:
            "workflow #{rel} has top-level `permissions:` with `contents: write` " <>
              "AND performs a write (push/commit/release/PR). It is over-broad, but " <>
              "narrowing the workflow level alone WOULD BREAK IT — no job declares " <>
              "its own `permissions:`.",
          action: :report,
          fix_recipe: "add-job-level-contents-write-then-narrow-workflow-level",
          detail: %{
            fix:
              "Two steps, in this order: (1) add `permissions: {contents: write}` to " <>
                "the job that performs the write; (2) only then narrow the " <>
                "workflow-level block to `contents: read`. Doing (2) without (1) " <>
                "fails at the next write, not at PR time — nothing in CI exercises " <>
                "the token, so every check stays green."
          }
        }

      # Writes, but jobs are individually scoped — narrowing is safe for any
      # job carrying its own block. Still worth reporting, at low severity.
      writes? and job_scoped? ->
        finding_wh002(
          rel,
          "with `contents: write` (workflow performs a write, but jobs carry " <>
            "their own `permissions:` — verify the WRITING job is one of them " <>
            "before narrowing)",
          :warn
        )

      # No write performed: narrowing is genuine least-privilege hardening.
      true ->
        finding_wh002(
          rel,
          "with `contents: write` (no write operation found — safe to narrow)",
          :high
        )
    end
  end

  defp finding_wh002(file, why, sev) do
    %{
      rule: "WH002",
      file: file,
      severity: sev,
      reason:
        "workflow #{file} has top-level `permissions:` #{why} — " <>
          "GITHUB_TOKEN should be scoped to least-privilege",
      action: :report,
      detail: %{
        fix:
          "Add `permissions: contents: read` at the top of the workflow " <>
            "and grant per-job escalation only where needed."
      }
    }
  end

  # ─── WH003: Dangerous trigger + PR head checkout ────────────────────

  @doc """
  WH003: A workflow triggered by `pull_request_target` (or `workflow_run`)
  that ALSO checks out the head ref of the fork PR. This combination
  runs untrusted code with secrets in scope — the SolarWinds / Codecov
  attack vector.
  """
  def wh003_dangerous_trigger_with_checkout(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      dangerous_trigger? =
        Regex.match?(~r/^\s*on:\s*$/m, content) and
          (Regex.match?(~r/^\s+pull_request_target:/m, content) or
             Regex.match?(~r/^\s+workflow_run:/m, content))

      # Inline form: `on: pull_request_target`
      dangerous_inline? =
        Regex.match?(~r/^\s*on:\s*pull_request_target\b/m, content) or
          Regex.match?(~r/^\s*on:\s*workflow_run\b/m, content)

      pr_head_checkout? =
        Regex.match?(
          ~r/uses:\s*actions\/checkout@.*\n\s+with:\s*\n(?:.*\n)*?\s+ref:\s*\$\{\{\s*github\.event\.pull_request\.head/m,
          content
        ) or
          Regex.match?(
            ~r/ref:\s*\$\{\{\s*github\.event\.pull_request\.head\.(sha|ref)/m,
            content
          )

      if (dangerous_trigger? or dangerous_inline?) and pr_head_checkout? do
        [
          %{
            rule: "WH003",
            file: rel,
            severity: :critical,
            reason:
              "workflow #{rel} uses `pull_request_target`/`workflow_run` " <>
                "AND checks out the fork PR head — runs untrusted code with " <>
                "secrets in scope",
            action: :report,
            detail: %{
              fix:
                "Replace with `pull_request` trigger (no secret access), OR " <>
                  "do not check out the head ref under the privileged trigger."
            }
          }
        ]
      else
        []
      end
    end)
  end

  # ─── WH004: Unpinned action `uses:` ─────────────────────────────────

  @doc """
  WH004: `uses: owner/repo@<ref>` where `<ref>` is a branch or tag
  rather than a 40-char commit SHA. Tag refs are mutable; a malicious
  release can substitute the implementation.
  """
  def wh004_unpinned_uses(repo_path) do
    lock_path = Path.join([repo_path, ".github", "workflows", "actions.lock"])

    case File.read(lock_path) do
      {:error, :enoent} ->
        wh004_scan_repo(repo_path, nil)

      {:error, reason} ->
        [invalid_actions_lock_finding(reason)]

      {:ok, content} ->
        case Hypatia.Rules.ActionsLock.parse(content) do
          {:ok, lock} -> wh004_scan_repo(repo_path, lock)
          {:error, reason} -> [invalid_actions_lock_finding(reason)]
        end
    end
  end

  defp wh004_scan_repo(repo_path, lock) do
    # Path-walking wrapper: enumerate workflow files and delegate per-file
    # scanning to `wh004_scan_content/2` so the same detection logic is
    # callable both from a repo-path walker (this function) and from a
    # pre-loaded `{filename, content}` mapper (workflow_audit). See audit
    # 2026-05-28 Part 3.1 — WH004 is now the canonical unpinned-action
    # source; workflow_audit/check_unpinned_actions delegates here.
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      wh004_scan_content(rel, content)
      |> Enum.reject(fn finding ->
        Hypatia.Rules.ActionsLock.pinned?(lock, rel, finding.detail.uses)
      end)
    end)
  end

  defp invalid_actions_lock_finding(reason) do
    %{
      rule: "invalid_actions_lock",
      file: ".github/workflows/actions.lock",
      severity: :high,
      reason: "actions.lock failed closed: #{inspect(reason)}",
      action: :regenerate,
      detail: %{
        kind: :invalid_actions_lock,
        fix: "Regenerate and verify the lock with `gh actions-lock`."
      }
    }
  end

  @doc """
  WH004 core scanner: detect unpinned `uses:` references in a single
  workflow file's content. Returns the canonical WH004 finding shape
  with severity `:warn` (no special-casing of main/master here; callers
  that want main-vs-tag severity bumps can post-process).

  Exempts: local-action refs (`./...`), docker images (`docker://...`),
  already-SHA-pinned refs (40 hex chars after `@`).

  This is the canonical unpinned-action detection per audit 2026-05-28
  Part 3.1. Direct consumers:

    * `wh004_unpinned_uses/1` — repo-path walker (this module)
    * `Hypatia.Rules.WorkflowAudit.check_unpinned_actions/1` — pre-
      loaded-content path; post-processes findings to add
      `pin_exempt?` carve-outs and `@known_good_shas` lookups.

  Both should be the only sites doing unpinned-action regex matching.
  """
  def wh004_scan_content(filename, content) do
    Regex.scan(~r/^\s*-?\s*uses:\s*(\S+)/m, content, return: :index)
    |> Enum.flat_map(fn [{full_start, _}, {slug_start, slug_len}] ->
      # `return: :index` yields BYTE offsets; String.slice/3 counts
      # graphemes. Any multi-byte character earlier in the file (em-dash
      # in a comment, emoji) shifted the slice, producing mangled slugs
      # ("tions/checkout@…", "urin 21 JRE…") that bypassed the
      # 40-hex-pinned exemption below and flagged SHA-pinned actions as
      # unpinned. binary_part/3 is the byte-correct slice.
      slug = binary_part(content, slug_start, slug_len)

      cond do
        String.starts_with?(slug, "./") or String.starts_with?(slug, "docker://") ->
          []

        # Already SHA-pinned (40 hex chars after @)
        Regex.match?(~r/@[a-fA-F0-9]{40}\b/, slug) ->
          []

        # Has an @ but it's a tag or branch
        String.contains?(slug, "@") ->
          line_no = line_number_for_offset(content, full_start)

          [
            %{
              rule: "WH004",
              file: filename,
              severity: :warn,
              reason:
                "workflow #{filename}:#{line_no} pins `#{slug}` to a tag/branch — " <>
                  "mutable ref allows upstream takeover",
              action: :report,
              detail: %{
                line: line_no,
                uses: slug,
                fix:
                  "Replace tag/branch ref with a 40-char commit SHA: " <>
                    "`gh api repos/<owner>/<repo>/git/refs/tags/<tag> --jq .object.sha`. " <>
                    "Append `# <tag>` as a comment for readability."
              }
            }
          ]

        true ->
          []
      end
    end)
  end

  # ─── WH005: Hardcoded credentials in services/container ─────────────

  @doc """
  WH005: A `credentials.password:` (or `services.<n>.credentials.password:`)
  block contains a literal string instead of a `${{ secrets.X }}` reference.
  """
  def wh005_hardcoded_credentials(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      Regex.scan(~r/(?m)^\s*password:\s*(\S.*)$/, content, return: :index)
      |> Enum.flat_map(fn [{full_start, _}, {val_start, val_len}] ->
        # byte offsets from return: :index — see wh004_scan_content
        val = binary_part(content, val_start, val_len)
        line_no = line_number_for_offset(content, full_start)

        if String.contains?(val, "${{ secrets.") or String.contains?(val, "${{secrets.") do
          []
        else
          [
            %{
              rule: "WH005",
              file: rel,
              severity: :critical,
              reason:
                "workflow #{rel}:#{line_no} contains a literal `password:` " <>
                  "value — credential is now in git history",
              action: :report,
              detail: %{
                line: line_no,
                fix:
                  "Move the value to a repo or org secret, reference as " <>
                    "`${{ secrets.X }}`, and rotate the leaked credential immediately."
              }
            }
          ]
        end
      end)
    end)
  end

  # ─── WH006: Missing job timeout ─────────────────────────────────────

  @doc """
  WH006: A job has no `timeout-minutes:` declaration. The default is
  360 minutes (6h) — runaway jobs waste runner-minutes and mask hangs.
  """
  def wh006_missing_job_timeout(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      # Find every `jobs.<id>:` entry and check whether its block
      # contains `timeout-minutes:` before the next job or end of file.
      job_blocks = extract_job_blocks(content)

      job_blocks
      |> Enum.flat_map(fn {job_id, line_no, body} ->
        if Regex.match?(~r/^\s+timeout-minutes:/m, body) do
          []
        else
          [
            %{
              rule: "WH006",
              file: rel,
              severity: :warn,
              reason:
                "workflow #{rel}:#{line_no} job `#{job_id}` has no " <>
                  "`timeout-minutes:` — defaults to 360 min on hang",
              action: :report,
              detail: %{
                line: line_no,
                job: job_id,
                fix: "Add `timeout-minutes: <N>` (typical: 5-30) under the job."
              }
            }
          ]
        end
      end)
    end)
  end

  # ─── WH007: Missing concurrency on PR-triggered workflow ────────────

  @doc """
  WH007: A workflow that triggers on `pull_request` has no top-level
  `concurrency:` group. Pushing many commits to the same PR head in
  rapid succession queues N runs that mostly waste runner-minutes.
  """
  def wh007_missing_concurrency(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      pr_triggered? =
        Regex.match?(~r/^\s+pull_request\b/m, content) or
          Regex.match?(~r/^\s*on:\s*pull_request\b/m, content)

      has_concurrency? = Regex.match?(~r/^[ \t]*concurrency:/m, content)

      if pr_triggered? and not has_concurrency? do
        [
          %{
            rule: "WH007",
            file: rel,
            severity: :info,
            reason:
              "workflow #{rel} triggers on `pull_request` but has no " <>
                "top-level `concurrency:` group — successive pushes queue " <>
                "duplicate runs",
            action: :report,
            detail: %{
              fix:
                "Add at the top:\n" <>
                  "  concurrency:\n" <>
                  "    group: ${{ github.workflow }}-${{ github.ref }}\n" <>
                  "    cancel-in-progress: true"
            }
          }
        ]
      else
        []
      end
    end)
  end

  # ─── WH008: secrets: inherit in reusable-workflow call ──────────────

  @doc """
  WH008: A `jobs.<id>.secrets: inherit` line — forwards every secret
  in the caller's scope to the reusable workflow, breaking scope
  isolation. Should be `secrets:` with explicit per-name forwarding.
  """
  def wh008_secrets_inherit(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      Regex.scan(~r/(?m)^\s+secrets:\s*inherit\b/, content, return: :index)
      |> Enum.map(fn [{idx, _}] ->
        line_no = line_number_for_offset(content, idx)

        %{
          rule: "WH008",
          file: rel,
          severity: :warn,
          reason:
            "workflow #{rel}:#{line_no} uses `secrets: inherit` — " <>
              "forwards every caller secret to the reusable workflow",
          action: :report,
          detail: %{
            line: line_no,
            fix:
              "Replace with explicit per-name forwarding:\n" <>
                "  secrets:\n" <>
                "    MY_SECRET: ${{ secrets.MY_SECRET }}"
          }
        }
      end)
    end)
  end

  # ─── WH009: Overprovisioned secrets ─────────────────────────────────

  @doc """
  WH009: `${{ toJSON(secrets) }}` (or whole-context `${{ secrets }}`)
  dumps every secret as a single JSON blob — typically into a step
  output or env var, making redaction unreliable.
  """
  def wh009_overprovisioned_secrets(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      Regex.scan(~r/\$\{\{\s*(toJSON|toJson|fromJSON|fromJson)\(\s*secrets\s*\)\s*\}\}/, content,
        return: :index
      )
      |> Enum.map(fn [{idx, _} | _] ->
        line_no = line_number_for_offset(content, idx)

        %{
          rule: "WH009",
          file: rel,
          severity: :critical,
          reason:
            "workflow #{rel}:#{line_no} serialises the entire `secrets` " <>
              "context — every secret is exposed to the runner",
          action: :report,
          detail: %{
            line: line_no,
            fix:
              "Reference specific secrets by name: `${{ secrets.X }}` per " <>
                "named credential. Never serialise the whole context."
          }
        }
      end)
    end)
  end

  # ─── WH010: Deprecated workflow commands ────────────────────────────

  @doc """
  WH010: Use of `::set-output::`, `::save-state::`, `::set-env::`, or
  `::add-path::`. All four are deprecated and removed-then-restored
  several times since 2022; rely on `$GITHUB_OUTPUT` / `$GITHUB_ENV`
  / `$GITHUB_PATH` instead.
  """
  def wh010_deprecated_workflow_commands(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      Regex.scan(~r/::(?:set-output|save-state|set-env|add-path)\b/, content, return: :index)
      |> Enum.map(fn [{idx, _}] ->
        line_no = line_number_for_offset(content, idx)
        slice = byte_preview(content, idx, 20)

        %{
          rule: "WH010",
          file: rel,
          severity: :warn,
          reason:
            "workflow #{rel}:#{line_no} uses deprecated workflow command " <>
              "`#{slice}…` — replace with $GITHUB_OUTPUT / $GITHUB_ENV / $GITHUB_PATH",
          action: :report,
          detail: %{
            line: line_no,
            fix:
              "Migrate:\n" <>
                "  echo \"key=value\" >> \"$GITHUB_OUTPUT\"   (was ::set-output::)\n" <>
                "  echo \"key=value\" >> \"$GITHUB_ENV\"      (was ::set-env::)\n" <>
                "  echo \"/path\" >> \"$GITHUB_PATH\"          (was ::add-path::)"
          }
        }
      end)
    end)
  end

  # ─── WH011: Curl-pipe-shell in run: ─────────────────────────────────

  @doc """
  WH011: `curl … | sh` / `wget … | bash` in a `run:` block. Pipes
  network content to a shell — supply-chain ingress that bypasses
  every checksum/signature gate.
  """
  def wh011_curl_pipe_shell(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      Regex.scan(
        ~r/(?:curl|wget)\s[^|\n]*\|\s*(?:bash|sh|zsh)(?:\s|$)/,
        content,
        return: :index
      )
      |> Enum.map(fn [{idx, _}] ->
        line_no = line_number_for_offset(content, idx)
        slice = byte_preview(content, idx, 60)

        %{
          rule: "WH011",
          file: rel,
          severity: :high,
          reason:
            "workflow #{rel}:#{line_no} pipes network content to a shell " <>
              "(`#{slice}…`) — bypasses every integrity gate",
          action: :report,
          detail: %{
            line: line_no,
            fix:
              "Download with `curl -fsSL -o /tmp/install.sh URL`, verify " <>
                "SHA256, then `bash /tmp/install.sh` — or use a versioned " <>
                "action that handles signing."
          }
        }
      end)
    end)
  end

  # ─── WH012: Untrusted input written to $GITHUB_ENV ──────────────────

  @doc """
  WH012: An `echo "<key>=${{ github.event.* }}" >> $GITHUB_ENV` (or
  similar) — persists attacker-controlled value into later steps of
  the same job, bypassing the per-step taint boundary.
  """
  def wh012_untrusted_to_github_env(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      # Find every line that writes to $GITHUB_ENV or $GITHUB_PATH and
      # contains an untrusted-context interpolation.
      Regex.scan(
        ~r/(?m)^.*\$\{\{[^}]*(?:github\.event\.|github\.head_ref).*\}\}.*>>\s*"?\$GITHUB_(?:ENV|PATH)"?/,
        content,
        return: :index
      )
      |> Enum.map(fn [{idx, _}] ->
        line_no = line_number_for_offset(content, idx)

        %{
          rule: "WH012",
          file: rel,
          severity: :critical,
          reason:
            "workflow #{rel}:#{line_no} writes a github.event.* / " <>
              "github.head_ref value into $GITHUB_ENV / $GITHUB_PATH — " <>
              "attacker-controlled value persists across steps",
          action: :report,
          detail: %{
            line: line_no,
            fix:
              "Read the untrusted value into a bound `env:` first, " <>
                "validate/escape, then write the cleaned value to GITHUB_ENV."
          }
        }
      end)
    end)
  end

  # ─── Scan facade ────────────────────────────────────────────────────

  @doc """
  Run every workflow-hardening check on `repo_path` and return the
  standard scan-result map.
  """
  def scan(repo_path, _opts \\ []) do
    findings =
      wh001_template_injection(repo_path) ++
        wh002_excessive_permissions(repo_path) ++
        wh003_dangerous_trigger_with_checkout(repo_path) ++
        wh004_unpinned_uses(repo_path) ++
        wh005_hardcoded_credentials(repo_path) ++
        wh006_missing_job_timeout(repo_path) ++
        wh007_missing_concurrency(repo_path) ++
        wh008_secrets_inherit(repo_path) ++
        wh009_overprovisioned_secrets(repo_path) ++
        wh010_deprecated_workflow_commands(repo_path) ++
        wh011_curl_pipe_shell(repo_path) ++
        wh012_untrusted_to_github_env(repo_path) ++
        wh013_permission_starved_write(repo_path) ++
        wh014_masked_scanner_upload(repo_path)

    %{
      findings: findings,
      total: length(findings),
      by_severity: group_by_severity(findings),
      dispatch: dispatch_recommendations(findings)
    }
  end

  # ─── WH013: Permission-starved write ────────────────────────────────

  @doc """
  WH013: the workflow PERFORMS an operation requiring `contents: write`
  (push / commit / release / PR create) but neither the workflow level nor
  any job grants it.

  This is the inverse of WH002 and it detects a REAL, SILENT BREAKAGE rather
  than a hardening opportunity. It exists because an over-narrow remediation
  is invisible to CI: the YAML stays valid, nothing in a PR run exercises the
  token, so `statusCheckRollup` reads SUCCESS and the workflow looks healthy.
  The failure surfaces only at the next write, possibly weeks later — and if
  the write is written defensively, e.g.

      git push origin HEAD 2>/dev/null || echo "::warning::Could not push"

  then a DENIED PUSH EMITS A WARNING, NOT A FAILURE. The workflow stays green
  forever while recording nothing. That shape is scored `:high`, not `:warn`,
  because the masking is what makes it undetectable.

  Judging this needs all three probes — a workflow-level grant alone is not
  the answer, because a job-level `permissions:` block REPLACES the
  workflow-level one:

    1. is there a workflow-level `contents: write`;
    2. does the file actually perform a write;
    3. does a job carry its own `permissions:` (and so possibly its own grant).
  """
  def wh013_permission_starved_write(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      cond do
        not performs_contents_write?(content) ->
          []

        # A grant exists at either level. This rule deliberately does not try
        # to prove the grant is on the RIGHT job — that needs a real YAML
        # parse, and a false "you are broken" is worse than a missed one.
        grants_contents_write_anywhere?(content) ->
          []

        # No grant anywhere, and the write is masked so it can never go red.
        masked_write?(content) ->
          [
            %{
              rule: "WH013",
              file: rel,
              severity: :high,
              reason:
                "workflow #{rel} performs a write (push/commit/release/PR) with NO " <>
                  "`contents: write` at the workflow level or any job level, AND the " <>
                  "write is masked (`2>/dev/null`, `|| true`, or `|| echo`). The push " <>
                  "is denied, the error is swallowed, and the workflow stays GREEN " <>
                  "while recording nothing.",
              action: :report,
              fix_recipe: "grant-contents-write-to-writing-job-and-unmask",
              detail: %{
                fix:
                  "Add `permissions: {contents: write}` to the job that performs the " <>
                    "write, and remove the `2>/dev/null` / `|| echo` mask so a denied " <>
                    "push fails the job instead of warning."
              }
            }
          ]

        true ->
          [
            %{
              rule: "WH013",
              file: rel,
              severity: :high,
              reason:
                "workflow #{rel} performs a write (push/commit/release/PR) but grants " <>
                  "no `contents: write` at the workflow level or any job level — the " <>
                  "write will be denied at run time.",
              action: :report,
              fix_recipe: "grant-contents-write-to-writing-job",
              detail: %{
                fix:
                  "Add `permissions: {contents: write}` to the job that performs the " <>
                    "write. Do not widen the workflow level if other jobs are correctly " <>
                    "scoped — a job-level block replaces the workflow-level one."
              }
            }
          ]
      end
    end)
  end

  @doc """
  Return true when `contents: write` (or `write-all`) is granted at the
  workflow level or at any job level.
  """
  def grants_contents_write_anywhere?(content) when is_binary(content) do
    Regex.match?(~r/^[ \t]*contents:\s*write\b/m, content) or
      Regex.match?(~r/^[ \t]*permissions:\s*write-all\b/m, content) or
      Regex.match?(~r/^[ \t]*write-all:\s*true\b/m, content)
  end

  # A write whose failure is swallowed: stderr redirected away, or the
  # command `||`-chained into a warning or a no-op. AGENTS.md §5 — never
  # `2>/dev/null` the thing under test.
  @write_masks [
    ~r/(?:git\s+push|git\s+commit|gh\s+pr\s+(?:create|merge))[^\n]*2>\s*\/dev\/null/,
    ~r/(?:git\s+push|git\s+commit|gh\s+pr\s+(?:create|merge))[^\n]*\|\|\s*(?:true|:)\s*$/m,
    ~r/(?:git\s+push|git\s+commit|gh\s+pr\s+(?:create|merge))[^\n]*\|\|\s*echo/
  ]

  @doc """
  Return true when a write operation's failure is masked, so a denied write
  cannot turn the job red.
  """
  def masked_write?(content) when is_binary(content) do
    Enum.any?(@write_masks, &Regex.match?(&1, content))
  end

  # ─── Internals ──────────────────────────────────────────────────────

  # Extract every `run:` block from the workflow content. Returns
  # `[{line_no, block_text}]`. Both single-line (`run: cmd`) and
  # multi-line (`run: |\n  cmd1\n  cmd2`) forms are captured.
  defp scan_run_blocks(content) do
    lines = String.split(content, "\n")

    single_line =
      lines
      |> Enum.with_index(1)
      |> Enum.filter(fn {line, _} -> Regex.match?(~r/^\s*-?\s*run:\s+\S/, line) end)

    multi_line = extract_multiline_runs(lines)

    Enum.map(single_line ++ multi_line, fn {line, no} -> {no, line} end)
  end

  defp extract_multiline_runs(lines) do
    {acc, pending} =
      Enum.with_index(lines, 1)
      |> Enum.reduce({[], nil}, fn {line, no}, {acc, current} ->
        cond do
          Regex.match?(~r/^\s*-?\s*run:\s*[|>]/, line) ->
            # Start of a block scalar. Capture until indent drops.
            {acc, {no, [line], indent_of(line)}}

          current != nil ->
            {start_no, block_lines, start_indent} = current

            if line == "" or indent_of(line) > start_indent do
              {acc, {start_no, [line | block_lines], start_indent}}
            else
              merged = block_lines |> Enum.reverse() |> Enum.join("\n")
              {[{merged, start_no} | acc], nil}
            end

          true ->
            {acc, nil}
        end
      end)

    # A block scalar that runs to EOF is still a block. The reduce above only
    # emits when a LATER line's indent drops back, so without this flush the
    # final `run: |` of every workflow file was silently never scanned —
    # a false NEGATIVE across every rule built on scan_run_blocks/1.
    case pending do
      nil ->
        acc

      {start_no, block_lines, _indent} ->
        [{block_lines |> Enum.reverse() |> Enum.join("\n"), start_no} | acc]
    end
  end

  defp indent_of(line) do
    case Regex.run(~r/^(\s*)/, line) do
      [_, ws] -> String.length(ws)
      _ -> 0
    end
  end

  # Extract every job definition from the workflow content. Returns
  # `[{job_id, line_no, body}]`. Approximate — assumes 2-space indent
  # under `jobs:`.
  defp extract_job_blocks(content) do
    lines = String.split(content, "\n")
    in_jobs? = false
    jobs_indent = nil

    {acc, _, _, _} =
      Enum.with_index(lines, 1)
      |> Enum.reduce({[], in_jobs?, jobs_indent, nil}, fn
        {line, _no}, {acc, false, _ji, _current} ->
          if Regex.match?(~r/^jobs:\s*$/, line) do
            {acc, true, nil, nil}
          else
            {acc, false, nil, nil}
          end

        {line, no}, {acc, true, nil, current} ->
          case Regex.run(~r/^(\s+)([a-zA-Z0-9_-]+):\s*$/, line) do
            [_, ws, job_id] ->
              ji = String.length(ws)
              acc2 = flush(acc, current)
              {acc2, true, ji, {job_id, no, [line]}}

            _ ->
              {acc, true, nil, current}
          end

        {line, _no}, {acc, true, jobs_indent, current} ->
          case Regex.run(~r/^(\s+)([a-zA-Z0-9_-]+):\s*$/, line) do
            [_, ws, job_id] ->
              this_indent = String.length(ws)

              if this_indent == jobs_indent do
                acc2 = flush(acc, current)
                {acc2, true, jobs_indent, {job_id, current_line_no(current, line), [line]}}
              else
                # Still in current job body
                {acc, true, jobs_indent, append_line(current, line)}
              end

            _ ->
              cond do
                line == "" or indent_of(line) >= jobs_indent + 1 ->
                  {acc, true, jobs_indent, append_line(current, line)}

                true ->
                  acc2 = flush(acc, current)
                  {acc2, false, nil, nil}
              end
          end
      end)

    Enum.reverse(acc)
  end

  defp current_line_no(nil, _line), do: 0
  defp current_line_no({_id, no, _body}, _line), do: no

  defp append_line(nil, _), do: nil
  defp append_line({id, no, body}, line), do: {id, no, [line | body]}

  defp flush(acc, nil), do: acc

  defp flush(acc, {id, no, body}) do
    [{id, no, body |> Enum.reverse() |> Enum.join("\n")} | acc]
  end

  defp line_number_for_offset(content, offset) when offset >= 0 do
    content
    |> binary_part(0, min(offset, byte_size(content)))
    |> String.graphemes()
    |> Enum.count(&(&1 == "\n"))
    |> Kernel.+(1)
  end

  # Fixed-length excerpt starting at a BYTE offset (regex match start, so
  # always on a character boundary). Clamps to the remaining bytes —
  # binary_part/3 raises on overrun — and trims any trailing partial
  # UTF-8 sequence so the preview stays Jason-encodable.
  defp byte_preview(content, idx, len) do
    avail = max(byte_size(content) - idx, 0)
    trim_partial_utf8(binary_part(content, idx, min(len, avail)))
  end

  defp trim_partial_utf8(bin) do
    if String.valid?(bin) or byte_size(bin) == 0 do
      bin
    else
      trim_partial_utf8(binary_part(bin, 0, byte_size(bin) - 1))
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
      # Workflow-hardening findings are not auto-fixable in the general
      # case (the fix shape varies per defect class). Always route to
      # sustainabot for advisory.
      confidence =
        case finding.severity do
          :critical -> 0.92
          :high -> 0.85
          :warn -> 0.75
          :info -> 0.60
          _ -> 0.50
        end

      %{
        bot: :sustainabot,
        confidence: confidence,
        rule: finding.rule,
        action: finding.action,
        reason: finding.reason
      }
    end)
  end

  # ── WH014 ───────────────────────────────────────────────────────────────
  # Measured 2026-09-14: this shape DELETED 84 real code-scanning alerts from
  # hyperpolymath/academic-workflow-suite while every run reported success.

  @sarif_upload_re ~r{codeql-action/upload-sarif}
  # A scanner invocation whose failure is swallowed: `… > something.json || true`
  @masked_scan_re ~r/>\s*[^\s|;&]*\.json\s*(?:2>[^\s|;&]*\s*)?\|\|\s*true/
  # A count that manufactures a zero when the artefact is unparseable.
  @masked_count_re ~r/\|\|\s*echo\s+0\b/
  # An assertion that the findings artefact is a NON-EMPTY array. `jq -e` sets a
  # non-zero exit on false/null, and `length > 0` is the estate's canonical form
  # (standards/.github/workflows/hypatia-scan-reusable.yml).
  @findings_assertion_re ~r/length\s*>\s*0|jq\s+-e/

  @doc """
  WH014 — a scanner whose failure is masked, feeding an upload to GitHub code
  scanning, with nothing asserting the findings artefact is non-empty.

  This is not merely a permissive gate. It **deletes security alerts**.

  GitHub reconciles each code-scanning upload against the previous analysis
  carrying the same tool name and category: any alert absent from the new
  analysis is AUTO-CLOSED. So when a scanner crashes and its failure is
  swallowed:

      hypatia-cli.sh scan . --exit-zero > hypatia-findings.json || true
      FINDING_COUNT=$(jq '. | length' hypatia-findings.json 2>/dev/null || echo 0)

  the workflow proceeds with a count of zero, renders a syntactically valid
  SARIF containing **zero results**, uploads it, and every previously-open
  alert for that category is closed. The job is green at every step. Nothing
  in `statusCheckRollup`, the run conclusion, or the check rollup shows that
  anything went wrong — the only visible trace is `results_count` falling to 0
  in `code-scanning/analyses`.

  Measured on 2026-09-14 in `hyperpolymath/academic-workflow-suite`: an analysis
  carrying **84** findings on 09-11 was followed by six green runs uploading
  **0** results, closing all 84.

  The rule fires only when all three hold, which keeps it conservative:

    1. the workflow uploads SARIF to code scanning;
    2. a scanner invocation or its count is masked (`|| true`, `|| echo 0`);
    3. nothing asserts the findings artefact is a non-empty array.

  Condition 3 is what exempts a correctly-written caller. The estate's
  `hypatia-scan-reusable.yml` performs exactly that assertion
  (`type == "array" and length > 0`, `exit 2` otherwise) and so never fires.
  """
  def wh014_masked_scanner_upload(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      uploads? = Regex.match?(@sarif_upload_re, content)
      masked? = Regex.match?(@masked_scan_re, content) or Regex.match?(@masked_count_re, content)
      asserts? = Regex.match?(@findings_assertion_re, content)

      if uploads? and masked? and not asserts? do
        [
          %{
            rule: "WH014",
            file: rel,
            severity: :high,
            reason:
              "workflow #{rel} uploads SARIF to code scanning but masks the scanner's " <>
                "failure (`|| true` / `|| echo 0`) and never asserts the findings " <>
                "artefact is a non-empty array. When the scanner fails, this uploads a " <>
                "SARIF with zero results, and GitHub AUTO-CLOSES every previously-open " <>
                "alert for that category — silently, with the job green.",
            action: :report,
            fix_recipe: "assert-findings-before-sarif-upload",
            detail: %{
              fix:
                "Remove the `|| true` and the `|| echo 0` fallback, then validate before " <>
                  "uploading: `jq -e 'type == \"array\" and length > 0' findings.json` and " <>
                  "exit non-zero if it fails. Prefer calling " <>
                  "hyperpolymath/standards/.github/workflows/hypatia-scan-reusable.yml, " <>
                  "which already performs this assertion."
            }
          }
        ]
      else
        []
      end
    end)
  end
end
