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
  - **WH002** — Workflow-level `permissions:` block missing or set to
    `write-all`. Cassel et al. (MSR 2024) measured ~74% of public
    workflows at default permissions. Same as scorecard `Token-Permissions`
    and zizmor `excessive-permissions`.
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
      not File.dir?(root) -> []
      true ->
        root
        |> File.ls!()
        |> Enum.filter(&(String.ends_with?(&1, ".yml") or String.ends_with?(&1, ".yaml")))
        |> Enum.map(&Path.join(root, &1))
        |> Enum.filter(&File.regular?/1)
    end
  end

  # Trusted left-hand sides of `${{ ... }}` (NOT script-injection
  # sources). Anything in github.event.*, github.head_ref, etc. is
  # treated as attacker-controlled in WH001/WH012.
  @untrusted_contexts ~w[
    github.event.issue.title github.event.issue.body
    github.event.pull_request.title github.event.pull_request.body
    github.event.pull_request.head.ref github.event.pull_request.head.label
    github.event.comment.body github.event.review.body
    github.event.commits github.event.head_commit.message
    github.event.head_commit.author github.event.pages
    github.head_ref github.ref
  ]
  @untrusted_pattern Regex.compile!(
    "\\$\\{\\{\\s*(" <>
      (@untrusted_contexts |> Enum.map(&Regex.escape/1) |> Enum.join("|")) <>
      ")"
  )

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
  `permissions: write-all`. Per Cassel et al. 2024, ~74% of public
  workflows are at the default (write-all-equivalent for many scopes).
  """
  def wh002_excessive_permissions(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      cond do
        Regex.match?(~r/^\s*permissions:\s*write-all\b/m, content) ->
          [finding_wh002(rel, "set to `write-all`", :high)]

        not Regex.match?(~r/^\s*permissions:/m, content) ->
          [finding_wh002(rel, "absent (defaults to broad permissions)", :warn)]

        true ->
          []
      end
    end)
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
    end)
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
      slug = String.slice(content, slug_start, slug_len)

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
        val = String.slice(content, val_start, val_len)
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

      has_concurrency? = Regex.match?(~r/^concurrency:/m, content)

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

      Regex.scan(~r/\$\{\{\s*(toJSON|toJson|fromJSON|fromJson)\(\s*secrets\s*\)\s*\}\}/, content, return: :index)
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
        slice = String.slice(content, idx, 20)

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
        slice = String.slice(content, idx, 60)

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
        wh012_untrusted_to_github_env(repo_path)

    %{
      findings: findings,
      total: length(findings),
      by_severity: group_by_severity(findings),
      dispatch: dispatch_recommendations(findings)
    }
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
    {acc, _} =
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

    acc
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
end
