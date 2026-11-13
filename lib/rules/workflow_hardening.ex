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

  alias Hypatia.WorkflowYaml

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
  WH002: Workflow has no `permissions:` declaration at any level, OR has
  top-level `permissions: write-all`, `write-all: true` in a top-level
  permissions block, or top-level `contents: write`. Per Cassel
  et al. 2024, ~74% of public workflows are at the default (write-all-equivalent
  for many scopes). This catches Scorecard TokenPermissionsID alerts.

  Return a list of findings with repo-relative file paths. Missing permissions
  produce `:warn`; write-all grants produce `:high`. For `contents: write`,
  scan the workflow and referenced local scripts: detected writes or unresolved
  script references produce `:warn`, otherwise `:high`. Return `[]` when no
  workflow matches. Job-level permissions alone do not produce a finding.

  Raises `File.Error` if workflow directory listing, workflow reading or
  reading a selected local script fails.
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
          [wh002_contents_write_finding(rel, local_script_scan(content, repo_path))]

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
    content = strip_foreign_pushes(content)
    Enum.any?(@contents_write_operations, &Regex.match?(&1, content))
  end

  # `git push <remote>` where <remote> is a NAMED remote other than `origin`
  # (`gitlab`, `codeberg`, `backup`, …). `contents: write` governs the job's
  # GITHUB_TOKEN, i.e. pushes to THIS repository; a mirror push to another
  # forge authenticates with its own SSH key or token and needs no grant.
  # Flags before the remote (`--force`, `-u`, `--mirror`) are skipped. A bare
  # `git push`, `git push origin …` and `git push "$REMOTE"` are all kept —
  # the last because the remote cannot be known statically (standards#943).
  @foreign_push ~r/\bgit\s+push\b(?:\s+-[-\w=]*)*\s+(?!origin\b)[A-Za-z][\w.-]*/

  @doc """
  Remove foreign-remote `git push` invocations, leaving any other operation on
  the same line (`git push gitlab main && git push origin main`) intact.
  """
  def strip_foreign_pushes(content) when is_binary(content) do
    Regex.replace(@foreign_push, content, "")
  end

  # A `run:` that calls a repo-local script (`bash scripts/wiki-sync.sh`,
  # `./ci/release.sh`) performs whatever that script performs. Reading only the
  # workflow text made WH002 call absolute-zero's wiki-sync.yml "no write
  # operation found — safe to narrow" while scripts/wiki-sync.sh does the
  # `git push` that needs the grant: the recommended narrowing breaks the sync.
  @local_script_ref ~r/(?<![\w\/.-])((?:\.\/)?[\w.-]+(?:\/[\w.-]+)*\.(?:sh|bash))\b/

  # Literal `working-directory:` values, at step level or under
  # `defaults: run:` (workflow or job). Expression values (`${{ … }}`, `$X`)
  # cannot be resolved statically and are not used as bases.
  @working_directory ~r/^\s*working-directory:\s*["']?([^"'\s#$]+)["']?\s*(?:#.*)?$/m

  @doc """
  Append each selected local shell script's text once to `content`, separated
  by newlines. Return `%{content: combined_text, unresolved: references}`.
  References ending in `.sh` or `.bash` are recognised anywhere in the input
  text, including comments; scripts are not executed or scanned recursively.

  Resolve references against the repo root and every recognised literal
  `working-directory:` in the workflow. For example, `working-directory: scripts`
  with `bash wiki-sync.sh` selects `scripts/wiki-sync.sh`. All matching bases
  are considered, without pairing references with individual steps.

  Candidates must expand beneath the repo root and have no symbolic links
  below that root. Missing paths, non-regular files and paths whose metadata
  cannot be read are rejected. `unresolved` contains each reference with
  in-repo candidates but no accepted candidate, in first-occurrence order.
  References whose candidates all escape the repo are ignored.

  Raises `File.Error` if reading an accepted script fails; this error is not
  converted into an unresolved reference.
  """
  def local_script_scan(content, repo_path) when is_binary(content) do
    root = Path.expand(repo_path)

    bases =
      [
        root
        | Enum.map(Regex.scan(@working_directory, content, capture: :all_but_first), fn [d] ->
            Path.expand(d, root)
          end)
      ]
      |> Enum.uniq()
      |> Enum.filter(&inside?(&1, root))

    {scripts, unresolved} =
      @local_script_ref
      |> Regex.scan(content, capture: :all_but_first)
      |> Enum.map(fn [ref] -> ref end)
      |> Enum.uniq()
      |> Enum.reduce({[], []}, fn ref, {found, missing} ->
        candidates =
          bases
          |> Enum.map(&Path.expand(ref, &1))
          |> Enum.uniq()
          |> Enum.filter(&(&1 != root and inside?(&1, root)))

        readable = Enum.filter(candidates, &unlinked_regular_file?(&1, root))

        cond do
          # Every candidate escapes the repo: not a repo-local script.
          candidates == [] -> {found, missing}
          readable == [] -> {found, [ref | missing]}
          true -> {readable ++ found, missing}
        end
      end)

    texts = scripts |> Enum.uniq() |> Enum.map(&File.read!/1)
    %{content: Enum.join([content | texts], "\n"), unresolved: Enum.reverse(unresolved)}
  end

  @doc """
  Return the combined text from `local_script_scan/2`, discarding its unresolved
  references. Uses the same reference selection and propagates `File.Error`
  if reading a selected script fails.
  """
  def with_local_scripts(content, repo_path) when is_binary(content) do
    local_script_scan(content, repo_path).content
  end

  defp inside?(path, root), do: path == root or String.starts_with?(path, root <> "/")

  # lstat every component from the root down; any symbolic link rejects.
  defp unlinked_regular_file?(path, root) do
    parts = path |> Path.relative_to(root) |> Path.split()

    parts
    |> Enum.scan(root, &Path.join(&2, &1))
    |> Enum.with_index(1)
    |> Enum.all?(fn {p, i} ->
      case File.lstat(p) do
        {:ok, %File.Stat{type: :regular}} -> i == length(parts)
        {:ok, %File.Stat{type: :directory}} -> i < length(parts)
        _ -> false
      end
    end)
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
  defp wh002_contents_write_finding(rel, %{content: content, unresolved: unresolved}) do
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

      # A called script could not be read, so "no write found" is unproven.
      # Never tell the owner narrowing is safe on an unread script.
      unresolved != [] ->
        finding_wh002(
          rel,
          "with `contents: write` (calls #{Enum.join(unresolved, ", ")}, which " <>
            "could not be read (missing, or behind a symbolic link) — verify it " <>
            "performs no push/commit/release before narrowing)",
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
        # A reusable-workflow caller (`job: uses: ./.github/workflows/x.yml`)
        # cannot carry `timeout-minutes:` — GitHub rejects the key there; the
        # called workflow's own jobs own their timeouts (standards#943).
        if Regex.match?(~r/^\s+timeout-minutes:/m, body) or reusable_caller_job?(body) do
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

  Defers to WF021 (owner ruling, 2026-11-13): it fires only where WF021's
  safety gate holds, i.e. the workflow is read-only and not a publisher or
  mutator. Cancelling an in-flight release, Pages deploy, mirror or push is
  unsafe, so `cancel-in-progress: true` is never advised there.
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

      # WF021 is the single safety gate for advising cancel-in-progress.
      cancel_safe? =
        Hypatia.Rules.WorkflowAudit.check_concurrency_missing_readonly(%{rel => content}) != []

      if pr_triggered? and not has_concurrency? and cancel_safe? do
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

  ## Writes that do not ride on `GITHUB_TOKEN`

  `contents: write` governs the job's `GITHUB_TOKEN` only. A job that brings
  its own credential (a PAT or app token as `actions/checkout`'s `token:`, or
  as the `GH_TOKEN`/`GITHUB_TOKEN` env of its steps) writes with THAT
  credential, so writes are judged per job and a credentialed job is never
  "starved" by a read-only grant. This repository's `estate-rescan.yml` (a
  PAT clone of another repository) and `inbox-steward-intake.yml` (checkout
  with `FARM_PAT`) were both reported as starved for this reason.

  Masking stays a defect there: `git push … 2>/dev/null || echo` cannot turn
  the job red when the credential expires or loses scope. That shape is
  reported at `:warn` with its own fix recipe, because no grant is missing.
  """
  def wh013_permission_starved_write(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = WorkflowYaml.read_workflow(path)
      rel = Path.relative_to(path, repo_path)

      {credentialed, token_writes} =
        content
        |> writing_jobs()
        |> Enum.split_with(& &1.credentialed?)

      masked_credentialed =
        if Enum.any?(credentialed, &masked_job?/1),
          do: [wh013_masked_credentialed_finding(rel)],
          else: []

      wh013_starved_findings(rel, content, token_writes) ++ masked_credentialed
    end)
  end

  # Each job that performs a write, as `%{text, steps, credentialed?}`, read
  # by the shared workflow reader: comment lines dropped, aliases expanded,
  # steps only under the job's own `steps:` key. A job is credentialed by its
  # own text or by a workflow-level `env:` it inherits. A file whose jobs the
  # reader cannot split is judged as one body and is never credentialed: one
  # job's PAT must not excuse another job's write.
  defp writing_jobs(content) do
    case content |> WorkflowYaml.active_lines() |> WorkflowYaml.workflow_job_lines() do
      [] ->
        if performs_contents_write?(content),
          do: [%{text: content, steps: [], credentialed?: false}],
          else: []

      jobs ->
        inherited? = own_credential_job?(workflow_preamble(content))

        for job <- jobs, text = lines_text(job), performs_contents_write?(text) do
          %{
            text: text,
            steps: WorkflowYaml.job_steps(job),
            credentialed?: inherited? or own_credential_job?(text)
          }
        end
    end
  end

  defp lines_text(lines), do: Enum.map_join(lines, "\n", &elem(&1, 0))

  # The text of each step of one job body (header line first, as the job
  # extractor gives it), read by the shared reader: only the items of the
  # job's own `steps:` key, comment lines dropped.
  defp job_step_texts(body) do
    body
    |> String.split("\n")
    |> Enum.with_index(1)
    |> Enum.reject(fn {line, _} -> String.starts_with?(String.trim_leading(line), "#") end)
    |> WorkflowYaml.job_steps()
    |> Enum.map(&lines_text/1)
  end

  defp blank_or_comment?(line) do
    trimmed = String.trim(line)
    trimmed == "" or String.starts_with?(trimmed, "#")
  end

  # Everything above the top-level `jobs:` key (name, on, permissions, env).
  # The key may be quoted (`"jobs":`, `'jobs':`), as WorkflowYaml reads it.
  defp workflow_preamble(content) do
    content |> String.split(~r/^(?:jobs|"jobs"|'jobs')\s*:/m, parts: 2) |> hd()
  end

  # `token:` / `GH_TOKEN:` / `GITHUB_TOKEN:` set to a secret other than
  # `secrets.GITHUB_TOKEN` (dot or index form), or to a step-minted token.
  @own_credential ~r/^[ \t]*(?:-[ \t]+)?(?:token|GH_TOKEN|GITHUB_TOKEN):[ \t]*["']?\$\{\{\s*(?:secrets\.(?!GITHUB_TOKEN\b)[A-Za-z_][A-Za-z0-9_]*|secrets\[\s*['"](?!GITHUB_TOKEN['"])[A-Za-z_][A-Za-z0-9_]*['"]\s*\]|steps\.[A-Za-z0-9_-]+\.outputs\.token)\s*\}\}/m

  @doc """
  Return true when a job body supplies its own write credential instead of
  relying on `GITHUB_TOKEN`: `token:` (e.g. on `actions/checkout`), `GH_TOKEN:`
  or `GITHUB_TOKEN:` set to a secret other than `secrets.GITHUB_TOKEN` (as
  `secrets.NAME` or `secrets['NAME']`), or to a step-minted
  `steps.<id>.outputs.token`.

  Approximate in the safe direction for WH013: a job that sets a PAT and then
  writes with `GITHUB_TOKEN` anyway is not reported as starved.
  """
  def own_credential_job?(body) when is_binary(body) do
    Regex.match?(@own_credential, body)
  end

  defp wh013_masked_credentialed_finding(rel) do
    %{
      rule: "WH013",
      file: rel,
      severity: :warn,
      reason:
        "workflow #{rel} performs a write with its own credential (PAT or app " <>
          "token), and the write is masked (`2>/dev/null`, `|| true`, `|| echo`, or the " <>
          "step's `continue-on-error: true`). " <>
          "If the credential expires or loses scope, the write fails and the " <>
          "workflow stays green.",
      action: :report,
      fix_recipe: "unmask-credentialed-write",
      detail: %{
        fix:
          "Remove the `2>/dev/null` / `|| true` / `|| echo` mask, or the step's " <>
            "`continue-on-error: true`, so a failed write " <>
            "fails the job. Skip an empty commit with `git diff --cached --quiet` " <>
            "rather than swallowing `git commit`'s exit."
      }
    }
  end

  # The grant is judged file-wide (`content`); starvation and masking only
  # over the jobs whose writes ride on GITHUB_TOKEN (`token_writes`).
  defp wh013_starved_findings(rel, content, token_writes) do
    cond do
      token_writes == [] ->
        []

      # A grant exists at either level. This rule deliberately does not try
      # to prove the grant is on the RIGHT job — that needs a real YAML
      # parse, and a false "you are broken" is worse than a missed one.
      grants_contents_write_anywhere?(content) ->
        []

      # No grant anywhere, and the write is masked so it can never go red.
      Enum.any?(token_writes, &masked_job?/1) ->
        [
          %{
            rule: "WH013",
            file: rel,
            severity: :high,
            reason:
              "workflow #{rel} performs a write (push/commit/release/PR) with NO " <>
                "`contents: write` at the workflow level or any job level, AND the " <>
                "write is masked (`2>/dev/null`, `|| true`, `|| echo`, or the step's " <>
                "`continue-on-error: true`). The push " <>
                "is denied, the error is swallowed, and the workflow stays GREEN " <>
                "while recording nothing.",
            action: :report,
            fix_recipe: "grant-contents-write-to-writing-job-and-unmask",
            detail: %{
              fix:
                "Add `permissions: {contents: write}` to the job that performs the " <>
                  "write, and remove the `2>/dev/null` / `|| echo` mask (or the step's " <>
                  "`continue-on-error: true`) so a denied " <>
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
    ~r/(?:git\s+push|git\s+commit|gh\s+pr\s+(?:create|merge)|gh\s+release\s+(?:create|upload|edit))[^\n]*2>\s*\/dev\/null/,
    ~r/(?:git\s+push|git\s+commit|gh\s+pr\s+(?:create|merge)|gh\s+release\s+(?:create|upload|edit))[^\n]*\|\|\s*(?:true|:)\s*$/m,
    ~r/(?:git\s+push|git\s+commit|gh\s+pr\s+(?:create|merge)|gh\s+release\s+(?:create|upload|edit))[^\n]*\|\|\s*echo/
  ]

  # `continue-on-error: true` as a key of the step, quoted or not. The prefix
  # (indent, plus `- ` on the step's opening line) is captured so the key's
  # column can be compared with the step's own key column.
  @step_continue_on_error ~r/^(\s*(?:-\s+)?)(?:continue-on-error|"continue-on-error"|'continue-on-error')\s*:\s*true\b/

  @doc """
  Return true when a write operation's failure is masked, so a denied write
  cannot turn the job red: a shell mask on the write itself, or
  `continue-on-error: true` on the very step that performs the write. A flag
  on a sibling step does not mask this write, and a job-level
  `continue-on-error` is not counted: it is a deliberate, visible job setting
  (standards' github-backup-mirror-reusable.yml documents and surfaces it).

  `content` is a workflow (or a fragment of one). Its steps are read with
  `Hypatia.WorkflowYaml`, so only the items of each job's own `steps:` count;
  a fragment without `jobs:` is judged by its shell masks alone.
  """
  def masked_write?(content) when is_binary(content) do
    shell_masked_write?(content) or
      content
      |> WorkflowYaml.active_lines()
      |> WorkflowYaml.workflow_job_lines()
      |> Enum.flat_map(&WorkflowYaml.job_steps/1)
      |> Enum.any?(&masked_step?/1)
  end

  defp masked_job?(%{text: text, steps: steps}) do
    shell_masked_write?(text) or Enum.any?(steps, &masked_step?/1)
  end

  defp shell_masked_write?(text), do: Enum.any?(@write_masks, &Regex.match?(&1, text))

  defp masked_step?(step) do
    step_continue_on_error?(step) and performs_contents_write?(lines_text(step))
  end

  # `continue-on-error: true` at the step's own key column. Deeper text (a
  # `run: |` script that writes a workflow file, a `with:` value) is not the
  # step's flag.
  defp step_continue_on_error?(step) do
    key_column = WorkflowYaml.step_key_column(step)

    Enum.any?(step, fn {line, _} ->
      case Regex.run(@step_continue_on_error, line) do
        [_, prefix] -> String.length(prefix) == key_column
        nil -> false
      end
    end)
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

  @doc """
  Return true when a job body (as produced by the job extractor, header line
  first) has a JOB-LEVEL `uses:` key, i.e. it calls a reusable workflow.
  Step-level `uses:` sits deeper (`- uses:` or under `- name:`) and is not
  matched: only a `uses:` at the job's own key indentation counts.
  """
  def reusable_caller_job?(body) when is_binary(body) do
    keys =
      body
      |> String.split("\n")
      |> Enum.drop(1)
      |> Enum.reject(&(String.trim(&1) == "" or String.starts_with?(String.trim(&1), "#")))

    case keys do
      [] ->
        false

      [first | _] ->
        key_indent = indent_of(first)

        Enum.any?(keys, fn line ->
          indent_of(line) == key_indent and Regex.match?(~r/^\s*uses:\s*\S/, line)
        end)
    end
  end

  # Extract every job definition from the workflow content. Returns
  # `[{job_id, line_no, body}]`. Approximate — assumes 2-space indent
  # under `jobs:`.
  defp extract_job_blocks(content) do
    lines = String.split(content, "\n")
    in_jobs? = false
    jobs_indent = nil

    # The final job is still in flight when the lines run out; it must be
    # flushed too, or the last job of every workflow is never checked.
    {acc, _, _, last} =
      Enum.with_index(lines, 1)
      |> Enum.reduce({[], in_jobs?, jobs_indent, nil}, fn
        {line, _no}, {acc, false, _ji, _current} ->
          if Regex.match?(~r/^jobs:\s*(?:#.*)?$/, line) do
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

        {line, no}, {acc, true, jobs_indent, current} ->
          case Regex.run(~r/^(\s+)([a-zA-Z0-9_-]+):\s*$/, line) do
            [_, ws, job_id] ->
              this_indent = String.length(ws)

              if this_indent == jobs_indent do
                acc2 = flush(acc, current)
                {acc2, true, jobs_indent, {job_id, no, [line]}}
              else
                # Still in current job body
                {acc, true, jobs_indent, append_line(current, line)}
              end

            _ ->
              cond do
                # A comment, at any indent, neither ends `jobs:` nor joins a
                # job body: WH013 reads bodies as raw text, and
                # `# the publisher used to git push` is not a write.
                String.starts_with?(String.trim_leading(line), "#") ->
                  {acc, true, jobs_indent, current}

                line == "" or indent_of(line) >= jobs_indent + 1 ->
                  {acc, true, jobs_indent, append_line(current, line)}

                true ->
                  acc2 = flush(acc, current)
                  {acc2, false, nil, nil}
              end
          end
      end)

    acc |> flush(last) |> Enum.reverse()
  end

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

  # WH014 is a bounded literal-path check, not a shell interpreter. Tokens
  # keep quoted data separate from control operators. Command substitutions
  # are analysed as commands; quoted examples and comments are not commands.
  @wh014_report_path ~r/\A[A-Za-z0-9_.\/-]+\.(?:json|sarif)\z/
  @wh014_upload ~r/\Agithub\/codeql-action\/upload-sarif@[^ \t]+\z/
  @wh014_workspace "/__wh014_workspace__"

  defp wh014_masked_upload?(body, inherited_directory) do
    # The job's keys above its `steps` key, quoted or not, as the shared
    # reader recognises it.
    [preamble | _] = String.split(body, ~r/^[ \t]*(?:steps|"steps"|'steps')[ \t]*:/m, parts: 2)
    directory = wh014_default_directory(preamble, inherited_directory)

    body
    |> job_step_texts()
    |> Enum.reduce_while(MapSet.new(), fn step, tainted ->
      upload = wh014_step_value(step, "uses")
      report = wh014_step_input(step, "sarif_file")
      report = wh014_path(report, @wh014_workspace)

      if is_binary(upload) and Regex.match?(@wh014_upload, upload) and
           not is_nil(report) and MapSet.member?(tainted, report) do
        {:halt, :masked_upload}
      else
        step_directory =
          case wh014_step_value(step, "working-directory") do
            nil -> directory
            value -> wh014_directory(value, @wh014_workspace)
          end

        next =
          if is_nil(step_directory) do
            tainted
          else
            step
            |> scan_run_blocks()
            |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
            |> Enum.sort_by(&elem(&1, 0))
            |> Enum.reduce(tainted, fn {_line, runs}, paths ->
              # scan_run_blocks returns the scalar header and its body at
              # the same line. Use the complete body once, in source order.
              run = Enum.max_by(runs, &byte_size/1)

              case wh014_run_tokens(run) do
                {:ok, tokens} -> wh014_follow_commands(tokens, paths, step_directory)
                :unknown -> paths
              end
            end)
          end

        {:cont, next}
      end
    end)
    |> Kernel.==(:masked_upload)
  end

  defp wh014_step_lines(step) do
    [item | rest] = String.split(step, "\n")

    if Regex.match?(~r/^[ \t]*-[ \t]*(?:#.*)?$/, item) do
      # A bare sequence marker has no key column. Read it from the first
      # real mapping entry, keeping nested scalar text at its own indentation.
      case Enum.reject(rest, &blank_or_comment?/1) do
        [first | _] ->
          column = indent_of(first)
          if column > indent_of(item), do: {column, rest}, else: {nil, []}

        [] ->
          {nil, []}
      end
    else
      case Regex.run(~r/^([ \t]*-[ \t]+)(.*)$/, item) do
        [_, prefix, value] ->
          column = String.length(prefix)
          {column, [String.duplicate(" ", column) <> value | rest]}

        _ ->
          {nil, []}
      end
    end
  end

  defp wh014_step_value(step, key) do
    {column, lines} = wh014_step_lines(step)
    wh014_yaml_value(lines, column, key)
  end

  defp wh014_step_input(step, key) do
    {column, lines} = wh014_step_lines(step)

    case Enum.drop_while(lines, fn line ->
           indent_of(line) != column or not Regex.match?(~r/^[ \t]*with:[ \t]*$/, line)
         end) do
      [_with | rest] ->
        inputs = Enum.take_while(rest, &(blank_or_comment?(&1) or indent_of(&1) > column))

        case Enum.reject(inputs, &blank_or_comment?/1) do
          [first | _] -> wh014_yaml_value(inputs, indent_of(first), key)
          [] -> nil
        end

      [] ->
        nil
    end
  end

  defp wh014_yaml_value(lines, column, key) do
    pattern = Regex.compile!("^[ \\t]*" <> Regex.escape(key) <> ":[ \\t]*(.*)$")

    Enum.find_value(lines, fn line ->
      if indent_of(line) == column do
        case Regex.run(pattern, line) do
          [_, value] -> wh014_literal_scalar(value)
          _ -> nil
        end
      end
    end)
  end

  defp wh014_literal_scalar(value) do
    case Regex.run(~r/\A(?:"([^"]*)"|'([^']*)'|([^ \t#]+))[ \t]*(?:#.*)?\z/, String.trim(value)) do
      nil -> nil
      captures -> captures |> tl() |> Enum.find(&(&1 != ""))
    end
  end

  defp wh014_default_directory(content, inherited) do
    if Regex.match?(~r/^[ \t]*working-directory:/m, content) do
      case Regex.run(@working_directory, content) do
        [_, value] -> wh014_directory(value, @wh014_workspace)
        _ -> nil
      end
    else
      inherited
    end
  end

  defp wh014_directory(value, base) do
    if is_binary(value) and Regex.match?(~r/\A[A-Za-z0-9_.\/-]+\z/, value),
      do: Path.expand(value, base),
      else: nil
  end

  defp wh014_path(value, directory) do
    if is_binary(value) and Regex.match?(@wh014_report_path, value),
      do: Path.expand(value, directory),
      else: nil
  end

  defp wh014_run_tokens(run) do
    [header | lines] = String.split(run, "\n")

    with [_, value] <- Regex.run(~r/^[ \t]*(?:-[ \t]+)?run:[ \t]*(.*)$/, header) do
      script =
        cond do
          Regex.match?(~r/\A\|[-+]?[ \t]*(?:#.*)?\z/, value) -> Enum.join(lines, "\n")
          String.starts_with?(value, [">", "'", "\""]) -> nil
          true -> value
        end

      if is_binary(script) do
        case wh014_lex(script, :plain, nil, [], false) do
          {:ok, tokens, ""} -> {:ok, tokens}
          _ -> :unknown
        end
      else
        :unknown
      end
    else
      _ -> :unknown
    end
  end

  defp wh014_flush(tokens, nil), do: tokens
  defp wh014_flush(tokens, word), do: [{:word, word} | tokens]

  defp wh014_lex("", :plain, word, tokens, false),
    do: {:ok, Enum.reverse(wh014_flush(tokens, word)), ""}

  defp wh014_lex("", _quote, _word, _tokens, _nested), do: :unknown

  defp wh014_lex(<<")", rest::binary>>, :plain, word, tokens, true),
    do: {:ok, Enum.reverse(wh014_flush(tokens, word)), rest}

  defp wh014_lex(<<"'", rest::binary>>, :single, word, tokens, nested),
    do: wh014_lex(rest, :plain, word, tokens, nested)

  defp wh014_lex(<<"'", rest::binary>>, :plain, word, tokens, nested),
    do: wh014_lex(rest, :single, word || "", tokens, nested)

  defp wh014_lex(<<"\"", rest::binary>>, :double, word, tokens, nested),
    do: wh014_lex(rest, :plain, word, tokens, nested)

  defp wh014_lex(<<"\"", rest::binary>>, :plain, word, tokens, nested),
    do: wh014_lex(rest, :double, word || "", tokens, nested)

  defp wh014_lex(<<"\\\r\n", rest::binary>>, quote, word, tokens, nested)
       when quote in [:plain, :double],
       do: wh014_lex(rest, quote, word, tokens, nested)

  defp wh014_lex(<<"\\\n", rest::binary>>, quote, word, tokens, nested)
       when quote in [:plain, :double],
       do: wh014_lex(rest, quote, word, tokens, nested)

  defp wh014_lex(<<"\\", char::utf8, rest::binary>>, :plain, word, tokens, nested),
    do: wh014_lex(rest, :plain, (word || "") <> <<char::utf8>>, tokens, nested)

  defp wh014_lex(<<"\\", char::utf8, rest::binary>>, :double, word, tokens, nested)
       when char in [36, 96, 34, 92],
       do: wh014_lex(rest, :double, (word || "") <> <<char::utf8>>, tokens, nested)

  # $$ expands to the shell PID. Its second dollar is not the start of a
  # command substitution in printed text such as "$$(jq ... || echo 0)".
  defp wh014_lex(<<"$$", rest::binary>>, quote, word, tokens, nested)
       when quote in [:plain, :double],
       do: wh014_lex(rest, quote, (word || "") <> "$$", tokens, nested)

  # Arithmetic expansion has different token rules and is outside the
  # command-substitution grammar. Do not interpret its data as commands.
  defp wh014_lex(<<"$((", _rest::binary>>, quote, _word, _tokens, _nested)
       when quote in [:plain, :double],
       do: :unknown

  defp wh014_lex(<<"$(", rest::binary>>, quote, word, tokens, nested)
       when quote in [:plain, :double] do
    case wh014_lex(rest, :plain, nil, [], true) do
      {:ok, inner, tail} ->
        wh014_lex(tail, quote, (word || "") <> "$()", [{:nested, inner} | tokens], nested)

      _ ->
        :unknown
    end
  end

  defp wh014_lex(<<"(", rest::binary>>, :plain, word, tokens, nested) do
    case wh014_lex(rest, :plain, nil, [], true) do
      {:ok, inner, tail} ->
        wh014_lex(tail, :plain, nil, [{:nested, inner} | wh014_flush(tokens, word)], nested)

      _ ->
        :unknown
    end
  end

  # A here-document needs its own grammar. Do not read its data as shell
  # commands or claim that this run's dynamic flow has been analysed.
  defp wh014_lex(<<"<<", _rest::binary>>, :plain, _word, _tokens, _nested), do: :unknown

  defp wh014_lex(<<"#", rest::binary>>, :plain, nil, tokens, nested) do
    case String.split(rest, "\n", parts: 2) do
      [_comment, tail] -> wh014_lex("\n" <> tail, :plain, nil, tokens, nested)
      [_comment] -> wh014_lex("", :plain, nil, tokens, nested)
    end
  end

  defp wh014_lex(<<char, rest::binary>>, :plain, word, tokens, nested)
       when char in [?\s, ?\t, ?\r],
       do: wh014_lex(rest, :plain, nil, wh014_flush(tokens, word), nested)

  defp wh014_lex(<<char, rest::binary>>, :plain, word, tokens, nested)
       when char in [?\n, ?;],
       do: wh014_lex(rest, :plain, nil, [:sequence | wh014_flush(tokens, word)], nested)

  defp wh014_lex(<<"||", rest::binary>>, :plain, word, tokens, nested),
    do: wh014_lex(rest, :plain, nil, [:or | wh014_flush(tokens, word)], nested)

  defp wh014_lex(<<"&&", rest::binary>>, :plain, word, tokens, nested),
    do: wh014_lex(rest, :plain, nil, [:and | wh014_flush(tokens, word)], nested)

  defp wh014_lex(<<"&", rest::binary>>, :plain, word, tokens, nested),
    do: wh014_lex(rest, :plain, nil, [:sequence | wh014_flush(tokens, word)], nested)

  defp wh014_lex(<<"|", rest::binary>>, :plain, word, tokens, nested),
    do: wh014_lex(rest, :plain, nil, [:pipe | wh014_flush(tokens, word)], nested)

  # Descriptor duplication is part of this command, not a background '&'.
  # Preserve the boundary without inferring a dynamic descriptor-to-file flow.
  defp wh014_lex(<<redirect, ?&, rest::binary>>, :plain, word, tokens, nested)
       when redirect in [?<, ?>] do
    words =
      if is_binary(word) and Regex.match?(~r/\A[0-9]+\z/, word),
        do: tokens,
        else: wh014_flush(tokens, word)

    wh014_lex(rest, :plain, nil, [:fd_duplicate | words], nested)
  end

  defp wh014_lex(<<">", rest::binary>>, :plain, word, tokens, nested) do
    {kind, rest} =
      if String.starts_with?(rest, ">"),
        do: {:append, String.slice(rest, 1..-1//1)},
        else: {:out, rest}

    {descriptor, words} =
      if word in ["1", "2"],
        do: {word, tokens},
        else: {"1", wh014_flush(tokens, word)}

    wh014_lex(rest, :plain, nil, [{kind, descriptor} | words], nested)
  end

  defp wh014_lex(<<"<", rest::binary>>, :plain, word, tokens, nested),
    do: wh014_lex(rest, :plain, nil, [:input | wh014_flush(tokens, word)], nested)

  defp wh014_lex(<<char::utf8, rest::binary>>, quote, word, tokens, nested),
    do: wh014_lex(rest, quote, (word || "") <> <<char::utf8>>, tokens, nested)

  defp wh014_segments(tokens) do
    {segments, current} =
      Enum.reduce(tokens, {[], []}, fn token, {segments, current} ->
        if token in [:sequence, :and, :or, :pipe] do
          if current == [] do
            {segments, current}
          else
            {[{Enum.reverse(current), token} | segments], []}
          end
        else
          {segments, [token | current]}
        end
      end)

    Enum.reverse([{Enum.reverse(current), :sequence} | segments])
    |> Enum.reject(&(elem(&1, 0) == []))
  end

  defp wh014_follow_commands(tokens, tainted, directory) do
    segments = wh014_segments(tokens)

    segments
    |> Enum.with_index()
    |> Enum.reduce({tainted, []}, fn {{command, separator}, index}, {paths, piped_inputs} ->
      paths =
        Enum.reduce(command, paths, fn
          {:nested, inner}, acc -> wh014_follow_commands(inner, acc, directory)
          _, acc -> acc
        end)

      {words, redirects} = wh014_command_parts(command)
      {inputs, copy_target} = wh014_inputs(words)

      inputs =
        if List.first(words) in ["jq", "cat"] do
          redirected = for {:input, "0", path} <- redirects, do: path
          inputs ++ redirected
        else
          inputs
        end

      inputs = Enum.map(inputs, &wh014_path(&1, directory)) |> Enum.reject(&is_nil/1)
      inputs = if List.first(words) in ["jq", "cat"], do: inputs ++ piped_inputs, else: inputs

      outputs =
        for {kind, "1", value} <- redirects,
            path = wh014_path(value, directory),
            not is_nil(path),
            do: {kind, path}

      outputs =
        case wh014_path(copy_target, directory) do
          nil -> outputs
          path -> [{:out, path} | outputs]
        end

      next_words =
        case Enum.at(segments, index + 1) do
          {next, _} -> next |> wh014_command_parts() |> elem(0)
          nil -> []
        end

      count_mask? = List.first(words) == "jq" and separator == :or and next_words == ["echo", "0"]

      paths = if count_mask?, do: Enum.reduce(inputs, paths, &MapSet.put(&2, &1)), else: paths

      inherited? = Enum.any?(inputs, &MapSet.member?(paths, &1))
      output_mask? = separator == :or and next_words == ["true"]

      paths =
        Enum.reduce(outputs, paths, fn {kind, path}, acc ->
          # Outputs have already passed the JSON/SARIF literal-path boundary.
          if inherited? or output_mask? or (kind == :append and MapSet.member?(acc, path)),
            do: MapSet.put(acc, path),
            else: MapSet.delete(acc, path)
        end)

      {paths, if(separator == :pipe, do: inputs, else: [])}
    end)
    |> elem(0)
  end

  defp wh014_command_parts(tokens), do: wh014_command_parts(tokens, [], [])

  defp wh014_command_parts([{kind, descriptor}, {:word, path} | rest], words, redirects)
       when kind in [:out, :append],
       do: wh014_command_parts(rest, words, [{kind, descriptor, path} | redirects])

  # The target of >& / <& is a descriptor, not an input/output filename.
  defp wh014_command_parts([:fd_duplicate, {:word, _descriptor} | rest], words, redirects),
    do: wh014_command_parts(rest, words, redirects)

  defp wh014_command_parts([:input, {:word, path} | rest], words, redirects),
    do: wh014_command_parts(rest, words, [{:input, "0", path} | redirects])

  defp wh014_command_parts([{:word, word} | rest], words, redirects),
    do: wh014_command_parts(rest, [word | words], redirects)

  defp wh014_command_parts([_ | rest], words, redirects),
    do: wh014_command_parts(rest, words, redirects)

  defp wh014_command_parts([], words, redirects),
    do: {Enum.reverse(words), Enum.reverse(redirects)}

  defp wh014_inputs(["jq" | args]), do: {wh014_jq_inputs(args, false, []), nil}

  defp wh014_inputs(["cat" | args]),
    do: {Enum.reject(args, &String.starts_with?(&1, "-")), nil}

  defp wh014_inputs([command | args]) when command in ["cp", "mv"] do
    case Enum.reject(args, &(&1 in ["-f", "-v", "--"])) do
      [source, target] -> {[source], target}
      _ -> {[], nil}
    end
  end

  defp wh014_inputs(_), do: {[], nil}

  defp wh014_jq_inputs([option, _name, path | rest], filter?, inputs)
       when option in ["--slurpfile", "--rawfile"],
       do: wh014_jq_inputs(rest, filter?, [path | inputs])

  defp wh014_jq_inputs([option, _name, _value | rest], filter?, inputs)
       when option in ["--arg", "--argjson"],
       do: wh014_jq_inputs(rest, filter?, inputs)

  defp wh014_jq_inputs([option, _program | rest], _filter?, inputs)
       when option in ["-f", "--from-file"],
       do: wh014_jq_inputs(rest, true, inputs)

  defp wh014_jq_inputs([option | _], _filter?, _inputs)
       when option in ["--args", "--jsonargs"],
       do: []

  defp wh014_jq_inputs([arg | rest], filter?, inputs) do
    cond do
      String.starts_with?(arg, "-") -> wh014_jq_inputs(rest, filter?, inputs)
      not filter? -> wh014_jq_inputs(rest, true, inputs)
      true -> wh014_jq_inputs(rest, true, [arg | inputs])
    end
  end

  defp wh014_jq_inputs([], _filter?, inputs), do: Enum.reverse(inputs)

  # ── WH014 ───────────────────────────────────────────────────────────────
  # Measured 2026-09-14: this shape DELETED 84 real code-scanning alerts from
  # hyperpolymath/academic-workflow-suite while every run reported success.

  @doc """
  WH014 — a scanner or findings-count failure is masked in a workflow that
  uploads SARIF to GitHub code scanning.

  A failed or incomplete scan must not become a complete empty analysis.
  GitHub reconciles code-scanning uploads against previous analyses for the
  same tool and category, so an empty analysis can close existing alerts.
  For example:

      hypatia-cli.sh scan . --exit-zero > hypatia-findings.json || true
      FINDING_COUNT=$(jq '. | length' hypatia-findings.json 2>/dev/null || echo 0)

  can proceed with a manufactured zero, render a valid SARIF with no results,
  and upload it despite the scanner having failed.

  Measured on 2026-09-14 in `hyperpolymath/academic-workflow-suite`: an analysis
  carrying **84** findings on 09-11 was followed by six green runs uploading
  **0** results, closing all 84.

  The rule reports a masked JSON/SARIF-producing invocation or count (`|| true`,
  `|| echo 0`) only when a visible literal-path chain carries that report
  to the named `sarif_file` of a later code-scanning upload in the same job.
  A masked count of unrelated optional metadata does not establish that
  relationship. Neither does an external converter's name alone.

  Neither `jq -e` nor a non-empty array establishes scanner completion: the
  assertion may inspect unrelated data, or a failed scan may leave partial
  valid findings. Conversely, a complete clean scan may legitimately produce
  an empty array. Preserve the scanner's exit status and findings, reject
  execution or coverage errors, then validate the expected report shape.

  This report-only rule follows literal JSON/SARIF paths through `jq`, `cat`,
  `cp`, `mv` and shell redirections, in extracted step order. It recognises
  `$(...)` command substitutions and backslash-newline continuations, keeps
  independent commands separate, and ignores mask text inside comments or quoted data.
  A known overwrite replaces the earlier path relationship. Literal working
  directories are resolved relative to the workspace.

  This is deliberately bounded: dynamic paths, external script internals,
  cross-job artefacts, arithmetic expansions, here-documents, folded or quoted
  YAML run scalars and general shell control flow are not analysed. Missing literal-flow evidence
  means this rule cannot establish its pattern; it is never a scan-completion
  or safety verdict. Do not treat absence of WH014 as assurance.
  """
  def wh014_masked_scanner_upload(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      affected_jobs =
        content
        |> extract_job_blocks()
        |> Enum.filter(fn {_job, _line, body} ->
          directory = wh014_default_directory(workflow_preamble(content), @wh014_workspace)
          wh014_masked_upload?(body, directory)
        end)

      if affected_jobs != [] do
        [
          %{
            rule: "WH014",
            file: rel,
            severity: :high,
            reason:
              "workflow #{rel} has a literal report path from a masked JSON/SARIF-producing " <>
                "command or findings count (`|| true` / `|| echo 0`) to a SARIF upload. A failed or " <>
                "incomplete scan can become an empty SARIF upload that closes existing " <>
                "alerts. An unrelated assertion or non-empty findings array does not " <>
                "establish scanner completion.",
            action: :report,
            fix_recipe: "assert-findings-before-sarif-upload",
            detail: %{
              jobs: Enum.map(affected_jobs, fn {job, _line, _body} -> job end),
              analysis_scope:
                "literal report paths in the same job; not scan-completion evidence",
              fix:
                "Preserve the scanner exit status and reject execution or coverage " <>
                  "errors before uploading. For Hypatia, exit 0 is a successful scan, " <>
                  "exit 1 carries findings, and exit 2 is an error; retain real findings. " <>
                  "Then validate `jq -e 'type == \"array\"' findings.json`, allowing an " <>
                  "empty array from a complete scan. Remove `|| true` / `|| echo 0`; " <>
                  "never turn a scanner or parsing failure into zero findings."
            }
          }
        ]
      else
        []
      end
    end)
  end
end
