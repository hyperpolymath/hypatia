# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.ResearchExtensions do
  @moduledoc """
  Static detection of GitHub Actions workflow defects sourced from
  commercial CI/CD security tools (Snyk, StepSecurity, Endor Labs,
  Datadog CI Visibility) and recent academic literature on CI/CD
  attack surface.

  This module is complementary to `WorkflowHardening` (which draws on
  the open-source `actionlint`/`zizmor` audit cluster) and
  `BaselineHealth` (which detects baseline-drift conditions). The
  rules here represent the *extension* of the open-source set with
  the patterns surfaced by commercial / academic research that aren't
  yet covered by the OSS auditors.

  Rule IDs RE001-RE010.

  ## Provenance map

  Each rule cites its upstream source (tool name and/or paper).

  - **RE001** — Workflow accesses `secrets.*` but does not install
    `step-security/harden-runner`. StepSecurity's Harden-Runner
    provides outbound-egress monitoring/blocking for workflows; its
    absence on a secrets-touching workflow leaves a clear telemetry
    gap. From StepSecurity Harden-Runner deployment guide.
  - **RE002** — Harden-Runner is installed but configured with
    `egress-policy: audit` (telemetry-only, never blocks). On a
    protected-branch workflow this provides no actual containment.
    From StepSecurity production-deployment guidance.
  - **RE003** — `actions/cache` key interpolates `github.head_ref`
    (or PR title/body). Forked PRs can poison the cache by picking a
    head ref that overlaps a privileged restore key. Snyk's dataflow
    auditor catches this; same defect class as the 2022 GitHub cache-
    poisoning advisory.
  - **RE004** — A container `uses: docker://image:tag` reference (or
    `container: image:tag`) pinned by tag, not by digest. Tag mutation
    re-points the image at attacker-controlled bytes. Same defect as
    Snyk container scanning's tag-pin warning.
  - **RE005** — A test step swallows non-zero exit (`|| true`, or
    `continue-on-error: true` on a step whose name contains
    `test`/`spec`/`check`). Datadog CI Visibility catalogues this as
    the canonical flake-masking pattern: the suite goes green while
    real failures pile up.
  - **RE006** — A composite action (`action.yml` with
    `runs.using: composite`) calls a third-party `uses:` reference
    that is not SHA-pinned. Endor Labs' R-END-05 surfaces this as a
    *nested* pinning gap — the consuming workflow may pin the
    composite, but the composite then loads tags transitively.
  - **RE007** — Workflow-level `env:` block contains a
    `${{ secrets.* }}` reference. Endor Labs' R-END-01 (least-
    privilege secrets scope): a workflow-scoped env exposes the
    secret to every step, including untrusted third-party actions
    that shouldn't see it. Per-step or per-job `env:` is the fix.
  - **RE008** — `github.actor == 'dependabot[bot]'` (or any other
    spoofable bot identity) used as a trust gate. `github.actor` is
    *the user who triggered the run* — on `pull_request_target` from
    a fork, the attacker controls the value. From zizmor's
    `bot-conditions` audit and Koishybayev et al. (USENIX Security
    2022). The correct gate is `github.actor_id == <numeric_id>` or
    `github.event.pull_request.user.login`.
  - **RE009** — `${{ fromJSON(secrets.X) }}` — parsing a secret as
    structured JSON. Each parsed field becomes a separate runner-
    side value, and GitHub's runner-side redaction operates on the
    *literal* secret text, not its parsed fields. The parsed fields
    then leak into logs unredacted. From zizmor's
    `unredacted-secrets`.
  - **RE010** — `workflow_run` trigger that consumes artifacts from
    the upstream workflow without provenance verification. The
    cross-workflow trust crossing is the EuroS&P 2023
    (Kermabon-Bobinnec et al.) attack surface — a successful PR
    workflow can publish a malicious artifact that the privileged
    `workflow_run` then trusts. Mitigation: verify artifact provenance
    or `actions/download-artifact@v4`'s `run-id` constraint.

  ## Architecture

  All rules are pure-local file scans (no GitHub API). Each takes a
  `repo_path` and returns a list of finding maps in the standard
  shape:

      %{rule: "RENNN", file: "...", severity: :critical | :high | :warn | :info,
        reason: "...", action: :report, detail: %{fix: "...", ...}}

  Workflow file discovery uses the same scope as `WorkflowHardening`
  and `BaselineHealth` BH004: only `<repo>/.github/workflows/*.yml`
  / `*.yaml` at the repo root. RE006 additionally inspects any local
  composite `action.yml` files found under the repo (those *can*
  live anywhere — that's how composite actions are addressed via
  `uses: ./path/to/action`).

  ## Implementation note

  Regex on YAML text rather than parsed YAML, for the same reasons
  given in `Hypatia.Rules.WorkflowHardening`: corpus is small,
  patterns are syntactically distinctive, false positives are caught
  downstream by safety-triangle review routing.
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

  @doc false
  def composite_action_files(repo_path) do
    # Composite action files live at `action.yml` or `action.yaml`
    # anywhere in the repo. Walk via `find` with depth cap so a
    # monorepo doesn't pay for unbounded traversal.
    case System.cmd(
           "find",
           [
             repo_path,
             "-maxdepth",
             "6",
             "-type",
             "f",
             "(",
             "-name",
             "action.yml",
             "-o",
             "-name",
             "action.yaml",
             ")"
           ],
           stderr_to_stdout: true
         ) do
      {output, 0} ->
        output
        |> String.split("\n", trim: true)
        |> Enum.reject(&String.contains?(&1, "/node_modules/"))
        |> Enum.reject(&String.contains?(&1, "/.git/"))
        |> Enum.filter(&File.regular?/1)

      _ ->
        []
    end
  end

  # ─── RE001: Harden-Runner absent on secrets-touching workflow ─────────

  @doc """
  RE001: Workflow references `${{ secrets.* }}` but does not install
  `step-security/harden-runner`. Provenance: StepSecurity Harden-Runner
  deployment guide.

  Severity: `:warn`. Action: `:report`.
  """
  def re001_missing_harden_runner(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      touches_secrets? =
        Regex.match?(~r/\$\{\{\s*secrets\.[A-Za-z_][A-Za-z0-9_]*/, content)

      installs_harden? =
        Regex.match?(~r/uses:\s*step-security\/harden-runner/, content)

      if touches_secrets? and not installs_harden? do
        [
          %{
            rule: "RE001",
            file: rel,
            severity: :warn,
            reason:
              "workflow #{rel} references `secrets.*` but does not install " <>
                "`step-security/harden-runner` — no outbound-egress telemetry",
            action: :report,
            detail: %{
              fix:
                "Add as the first step of each job:\n" <>
                  "  - uses: step-security/harden-runner@<SHA>\n" <>
                  "    with:\n" <>
                  "      egress-policy: block\n" <>
                  "      allowed-endpoints: >\n" <>
                  "        github.com:443"
            }
          }
        ]
      else
        []
      end
    end)
  end

  # ─── RE002: Harden-Runner in audit-only mode ─────────────────────────

  @doc """
  RE002: Harden-Runner is installed but configured with
  `egress-policy: audit`. Provenance: StepSecurity production-deployment
  guidance — audit mode is a deliberate first-rollout setting that
  becomes a finding when it persists on a workflow that touches
  protected branches (or runs on `push` to default).

  Severity: `:high`. Action: `:report`.
  """
  def re002_harden_runner_audit_mode(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      installs_harden? =
        Regex.match?(~r/uses:\s*step-security\/harden-runner/, content)

      audit_mode? =
        Regex.match?(~r/egress-policy:\s*audit\b/, content)

      protected_branch_trigger? =
        Regex.match?(~r/^\s*on:\s*push\b/m, content) or
          Regex.match?(~r/^\s+push:/m, content) or
          Regex.match?(~r/^\s+release:/m, content)

      if installs_harden? and audit_mode? and protected_branch_trigger? do
        [
          %{
            rule: "RE002",
            file: rel,
            severity: :high,
            reason:
              "workflow #{rel} runs harden-runner in `egress-policy: audit` — " <>
                "telemetry-only, never blocks; on a protected-branch trigger this is not containment",
            action: :report,
            detail: %{
              fix:
                "Switch `egress-policy: audit` to `egress-policy: block` once " <>
                  "the allow-list is stable. Keep `audit` only for first-rollout windows."
            }
          }
        ]
      else
        []
      end
    end)
  end

  # ─── RE003: cache key interpolates attacker-controlled input ─────────

  @doc """
  RE003: `actions/cache` key interpolates `github.head_ref` (or PR
  title/body / branch-name fragment). Forked PRs can choose a head
  ref that overlaps a privileged restore key — cache poisoning.
  Provenance: Snyk dataflow auditor + 2022 GitHub cache-poisoning
  advisory.

  Severity: `:high`. Action: `:report`.
  """
  def re003_cache_key_poisoning(repo_path) do
    untrusted_key_re =
      ~r/key:\s*[^\n]*\$\{\{\s*github\.(?:head_ref|event\.pull_request\.(?:title|body|head\.ref))/

    restore_keys_re =
      ~r/restore-keys:[\s\S]{0,200}?\$\{\{\s*github\.(?:head_ref|event\.pull_request\.(?:title|body|head\.ref))/

    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      uses_cache? =
        Regex.match?(~r/uses:\s*actions\/cache(?:@|\s)/, content)

      if uses_cache? and
           (Regex.match?(untrusted_key_re, content) or
              Regex.match?(restore_keys_re, content)) do
        # Locate first match for line number.
        idx =
          case Regex.run(untrusted_key_re, content, return: :index) do
            [{i, _} | _] -> i
            _ ->
              case Regex.run(restore_keys_re, content, return: :index) do
                [{i, _} | _] -> i
                _ -> 0
              end
          end

        line_no = line_number_for_offset(content, idx)

        [
          %{
            rule: "RE003",
            file: rel,
            severity: :high,
            reason:
              "workflow #{rel}:#{line_no} caches with a key derived from an " <>
                "attacker-controlled context (`github.head_ref` / PR title/body) — cache poisoning vector",
            action: :report,
            detail: %{
              line: line_no,
              fix:
                "Derive cache key from immutable inputs only: " <>
                  "`${{ hashFiles('**/Cargo.lock') }}` or `${{ github.sha }}`. " <>
                  "Never key off head_ref / PR title / branch name."
            }
          }
        ]
      else
        []
      end
    end)
  end

  # ─── RE004: container action pinned by tag, not digest ────────────────

  @doc """
  RE004: A `docker://image:tag` reference (in `uses:` or `container:`)
  is pinned by tag rather than a SHA256 digest. Tag mutation re-points
  the image at attacker-controlled bytes. Provenance: Snyk container
  scanning's tag-pin warning + GitHub's pin-by-digest hardening guide.

  Severity: `:warn`. Action: `:report`.
  """
  def re004_container_tag_pin(repo_path) do
    # `docker://owner/image:tag` (no @sha256:)
    docker_uses_re =
      ~r/uses:\s*(docker:\/\/\S+?)(?:\s|$)/

    container_re =
      ~r/^\s*(?:image|container):\s*(\S+)\s*$/m

    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      uses_findings =
        Regex.scan(docker_uses_re, content, return: :index)
        |> Enum.flat_map(fn [{full_start, _}, {ref_start, ref_len}] ->
          ref = String.slice(content, ref_start, ref_len)

          if String.contains?(ref, "@sha256:") do
            []
          else
            line_no = line_number_for_offset(content, full_start)
            [finding_re004(rel, line_no, ref)]
          end
        end)

      container_findings =
        Regex.scan(container_re, content, return: :index)
        |> Enum.flat_map(fn [{full_start, _}, {ref_start, ref_len}] ->
          ref = String.slice(content, ref_start, ref_len)

          cond do
            String.contains?(ref, "@sha256:") ->
              []

            # Plausibility filter: must contain ":" (tag) and look like
            # an image reference, not a free-text YAML value.
            not String.contains?(ref, ":") ->
              []

            String.starts_with?(ref, "${{") ->
              []

            true ->
              line_no = line_number_for_offset(content, full_start)
              [finding_re004(rel, line_no, ref)]
          end
        end)

      uses_findings ++ container_findings
    end)
  end

  defp finding_re004(file, line_no, ref) do
    %{
      rule: "RE004",
      file: file,
      severity: :warn,
      reason:
        "workflow #{file}:#{line_no} pins container `#{ref}` by tag — " <>
          "tag is mutable; pin by sha256 digest instead",
      action: :report,
      detail: %{
        line: line_no,
        image: ref,
        fix:
          "Resolve the tag to a digest:\n" <>
            "  docker buildx imagetools inspect #{ref} --format '{{.Manifest.Digest}}'\n" <>
            "Then pin as `#{ref}@sha256:<digest>` (drop or keep the tag for readability)."
      }
    }
  end

  # ─── RE005: test step swallows non-zero exit ─────────────────────────

  @doc """
  RE005: A step whose name contains `test`/`spec`/`check` either uses
  `continue-on-error: true` or pipes the command to `|| true`. The
  suite goes green while real failures pile up. Provenance: Datadog
  CI Visibility flake-masking patterns.

  Severity: `:warn`. Action: `:report`.
  """
  def re005_test_swallows_exit(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      content
      |> step_blocks()
      |> Enum.flat_map(fn {name, line_no, body} ->
        is_test? =
          is_binary(name) and
            Regex.match?(~r/(?:test|spec|check|lint|verify)/i, name)

        has_continue_on_error? =
          Regex.match?(~r/continue-on-error:\s*true\b/, body)

        has_or_true? =
          Regex.match?(~r/\|\|\s*true\b/, body)

        if is_test? and (has_continue_on_error? or has_or_true?) do
          mechanism =
            cond do
              has_continue_on_error? and has_or_true? ->
                "`continue-on-error: true` AND `|| true`"

              has_continue_on_error? ->
                "`continue-on-error: true`"

              true ->
                "`|| true`"
            end

          [
            %{
              rule: "RE005",
              file: rel,
              severity: :warn,
              reason:
                "workflow #{rel}:#{line_no} step `#{name}` swallows non-zero " <>
                  "exit via #{mechanism} — failures will be masked",
              action: :report,
              detail: %{
                line: line_no,
                step_name: name,
                fix:
                  "Remove the failure-swallow. If a single test is genuinely " <>
                    "flaky, retry it explicitly (e.g. `nextest --retries 2`) — never " <>
                    "swallow the exit of the whole step."
              }
            }
          ]
        else
          []
        end
      end)
    end)
  end

  # ─── RE006: composite action calls unpinned third-party action ───────

  @doc """
  RE006: A composite `action.yml` (with `runs.using: composite`) has a
  `uses:` reference to a third-party action that is not SHA-pinned.
  The composite's consumers may pin the composite itself, but the
  composite then transitively loads a tag. Provenance: Endor Labs
  R-END-05.

  Severity: `:warn`. Action: `:report`.
  """
  def re006_composite_unpinned_nested(repo_path) do
    repo_path
    |> composite_action_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      composite? = Regex.match?(~r/using:\s*['"]?composite['"]?/, content)

      if composite? do
        Regex.scan(~r/^\s*-?\s*uses:\s*(\S+)/m, content, return: :index)
        |> Enum.flat_map(fn [{full_start, _}, {slug_start, slug_len}] ->
          slug = String.slice(content, slug_start, slug_len)

          cond do
            String.starts_with?(slug, "./") or String.starts_with?(slug, "docker://") ->
              []

            Regex.match?(~r/@[a-fA-F0-9]{40}\b/, slug) ->
              []

            String.contains?(slug, "@") ->
              line_no = line_number_for_offset(content, full_start)

              [
                %{
                  rule: "RE006",
                  file: rel,
                  severity: :warn,
                  reason:
                    "composite action #{rel}:#{line_no} loads `#{slug}` by " <>
                      "tag/branch — nested-pinning gap; the composite's consumers " <>
                      "cannot enforce immutability transitively",
                  action: :report,
                  detail: %{
                    line: line_no,
                    uses: slug,
                    fix:
                      "Replace tag/branch with a 40-char commit SHA in the composite's " <>
                        "`uses:`. Add `# <tag>` as a trailing comment for readability."
                  }
                }
              ]

            true ->
              []
          end
        end)
      else
        []
      end
    end)
  end

  # ─── RE007: workflow-level env: exposes secrets to all steps ─────────

  @doc """
  RE007: Workflow-level (top-level) `env:` block contains a
  `${{ secrets.* }}` reference, exposing the secret to every step
  including untrusted third-party actions. Provenance: Endor Labs
  R-END-01 (least-privilege secrets scope).

  Severity: `:warn`. Action: `:report`.
  """
  def re007_workflow_env_secret_exposure(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      # Detect top-level (column-0) `env:` block. Capture indented body
      # until the next column-0 key.
      case Regex.run(~r/^env:\s*\n((?:[ \t]+[^\n]*\n?)+)/m, content, return: :index) do
        [{full_start, _}, {body_start, body_len}] ->
          body = String.slice(content, body_start, body_len)

          if Regex.match?(~r/\$\{\{\s*secrets\.[A-Za-z_][A-Za-z0-9_]*/, body) do
            line_no = line_number_for_offset(content, full_start)

            [
              %{
                rule: "RE007",
                file: rel,
                severity: :warn,
                reason:
                  "workflow #{rel}:#{line_no} declares a top-level `env:` " <>
                    "block that references `secrets.*` — every step (including " <>
                    "third-party actions) inherits the secret",
                action: :report,
                detail: %{
                  line: line_no,
                  fix:
                    "Move the `secrets.*` references into the specific step's " <>
                      "`env:` block, or into the specific job's `env:`. Reserve the " <>
                      "top-level `env:` for non-secret values."
                }
              }
            ]
          else
            []
          end

        _ ->
          []
      end
    end)
  end

  # ─── RE008: spoofable bot-identity gate ──────────────────────────────

  @doc """
  RE008: A conditional uses `github.actor == 'dependabot[bot]'` (or
  any other bot login) as a trust gate. `github.actor` is the user
  *who triggered the run*, not the PR author — on
  `pull_request_target` from a fork the attacker controls the value.
  Provenance: zizmor `bot-conditions` + Koishybayev et al. (USENIX
  Security 2022).

  Severity: `:critical`. Action: `:report`.
  """
  def re008_spoofable_bot_gate(repo_path) do
    # github.actor compared to any bot-identity string. Catch both
    # `==` and `!=` orientations and either quote style.
    bot_gate_re =
      ~r/github\.actor\s*(?:==|!=)\s*['"]([a-zA-Z0-9_-]+\[bot\])['"]/

    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      Regex.scan(bot_gate_re, content, return: :index)
      |> Enum.map(fn [{idx, _}, {name_start, name_len}] ->
        name = String.slice(content, name_start, name_len)
        line_no = line_number_for_offset(content, idx)

        %{
          rule: "RE008",
          file: rel,
          severity: :critical,
          reason:
            "workflow #{rel}:#{line_no} gates on `github.actor == '#{name}'` — " <>
              "`github.actor` is the run-triggering user, which an attacker controls " <>
              "on `pull_request_target` from a fork",
          action: :report,
          detail: %{
            line: line_no,
            spoofable_login: name,
            fix:
              "Compare against the numeric `github.actor_id`, or against " <>
                "`github.event.pull_request.user.login` (the PR author), or " <>
                "verify via the GitHub App token — never the human-readable login alone."
          }
        }
      end)
    end)
  end

  # ─── RE009: fromJSON(secrets.X) bypasses runner redaction ────────────

  @doc """
  RE009: `${{ fromJSON(secrets.X) }}` — parses a secret as structured
  JSON. Runner-side redaction operates on the *literal* secret text,
  not on the parsed individual fields; once split, the inner values
  leak to logs unredacted. Provenance: zizmor `unredacted-secrets`.

  Severity: `:critical`. Action: `:report`.
  """
  def re009_fromjson_secret(repo_path) do
    re = ~r/\$\{\{\s*fromJSON\(\s*secrets\.[A-Za-z_][A-Za-z0-9_]*\s*\)/

    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      Regex.scan(re, content, return: :index)
      |> Enum.map(fn [{idx, _} | _] ->
        line_no = line_number_for_offset(content, idx)
        slice = String.slice(content, idx, 60)

        %{
          rule: "RE009",
          file: rel,
          severity: :critical,
          reason:
            "workflow #{rel}:#{line_no} parses a secret as JSON (`#{slice}…`) — " <>
              "individual parsed fields are NOT covered by runner-side log redaction",
          action: :report,
          detail: %{
            line: line_no,
            fix:
              "Store each piece of structured data as a separate secret " <>
                "(`AWS_KEY`, `AWS_SECRET`, etc.) rather than a single JSON blob. " <>
                "If you must store JSON, parse it inside a step rather than via fromJSON()."
          }
        }
      end)
    end)
  end

  # ─── RE010: workflow_run consumes artifact without provenance check ──

  @doc """
  RE010: Workflow_run trigger that calls `actions/download-artifact`
  without specifying a `run-id` constrained to the triggering run,
  AND without subsequent provenance verification. The artifact may
  have been written by a malicious PR workflow that ran in unprivileged
  context but whose output is now being trusted by a privileged run.
  Provenance: EuroS&P 2023 (Kermabon-Bobinnec et al.).

  Severity: `:warn`. Action: `:report`.
  """
  def re010_workflow_run_artifact_no_provenance(repo_path) do
    repo_path
    |> workflow_files()
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      workflow_run? =
        Regex.match?(~r/^\s*on:\s*workflow_run\b/m, content) or
          Regex.match?(~r/^\s+workflow_run:/m, content)

      downloads_artifact? =
        Regex.match?(~r/uses:\s*actions\/download-artifact/, content)

      run_id_constrained? =
        Regex.match?(~r/run-id:\s*\$\{\{\s*github\.event\.workflow_run\.id/, content) or
          Regex.match?(~r/run-id:\s*\$\{\{\s*github\.event\.workflow_run\.id\s*\}\}/, content)

      has_provenance_verify? =
        Regex.match?(~r/(?i)\b(cosign|slsa-verifier|gh attestation verify)\b/, content) or
          Regex.match?(~r/(?i)attestations:\s*read/, content)

      if workflow_run? and downloads_artifact? and
           not run_id_constrained? and not has_provenance_verify? do
        [
          %{
            rule: "RE010",
            file: rel,
            severity: :warn,
            reason:
              "workflow #{rel} runs on `workflow_run` and downloads artifacts " <>
                "without constraining `run-id` or verifying provenance — cross-workflow trust crossing",
            action: :report,
            detail: %{
              fix:
                "Either:\n" <>
                  "  1. Set `run-id: ${{ github.event.workflow_run.id }}` and " <>
                  "`github-token: ${{ secrets.GITHUB_TOKEN }}` on the download-artifact step, OR\n" <>
                  "  2. Verify provenance with `gh attestation verify` / `cosign verify` " <>
                  "before consuming the artifact."
            }
          }
        ]
      else
        []
      end
    end)
  end

  # ─── Scan facade ────────────────────────────────────────────────────

  @doc """
  Run every research-extension check on `repo_path` and return the
  standard scan-result map.
  """
  def scan(repo_path, _opts \\ []) do
    findings =
      re001_missing_harden_runner(repo_path) ++
        re002_harden_runner_audit_mode(repo_path) ++
        re003_cache_key_poisoning(repo_path) ++
        re004_container_tag_pin(repo_path) ++
        re005_test_swallows_exit(repo_path) ++
        re006_composite_unpinned_nested(repo_path) ++
        re007_workflow_env_secret_exposure(repo_path) ++
        re008_spoofable_bot_gate(repo_path) ++
        re009_fromjson_secret(repo_path) ++
        re010_workflow_run_artifact_no_provenance(repo_path)

    %{
      findings: findings,
      total: length(findings),
      by_severity: group_by_severity(findings),
      dispatch: dispatch_recommendations(findings)
    }
  end

  # ─── Internals ──────────────────────────────────────────────────────

  # Extract step entries from a workflow's `steps:` blocks. Returns
  # `[{name_or_nil, line_no, body_text}]`. Approximate — a step starts
  # on a `- ` token at the steps-indent and extends until the next
  # `- ` at the same indent or until indent drops below.
  @doc false
  def step_blocks(content) do
    lines =
      content
      |> String.split("\n")
      |> Enum.with_index(1)

    {finished, last} =
      Enum.reduce(lines, {[], nil}, fn {line, no}, {acc, current} ->
        # A new step begins on a `- ` token. We detect both
        # `- name: ...`, `- run: ...`, `- uses: ...` as openers.
        new_step? =
          Regex.match?(~r/^\s+-\s+(?:name|run|uses):/, line)

        cond do
          new_step? ->
            acc2 = flush_step(acc, current)
            indent = indent_of(line)
            name = extract_step_name(line)
            {acc2, {name, no, [line], indent}}

          current != nil ->
            {n, l, body, start_indent} = current

            cond do
              # blank line: keep accumulating, doesn't terminate
              String.trim(line) == "" ->
                {acc, {n, l, [line | body], start_indent}}

              indent_of(line) > start_indent ->
                # Still inside the step body. If we see a name: line
                # before the name was discovered, capture it.
                n2 =
                  if n == nil do
                    case Regex.run(~r/^\s+name:\s*['"]?([^'"\n]+?)['"]?\s*$/, line) do
                      [_, capture] -> String.trim(capture)
                      _ -> n
                    end
                  else
                    n
                  end

                {acc, {n2, l, [line | body], start_indent}}

              true ->
                # Indent dropped — close out the current step.
                acc2 = flush_step(acc, current)
                {acc2, nil}
            end

          true ->
            {acc, nil}
        end
      end)

    finished
    |> flush_step(last)
    |> Enum.reverse()
  end

  defp flush_step(acc, nil), do: acc

  defp flush_step(acc, {name, line_no, body_lines, _indent}) do
    body = body_lines |> Enum.reverse() |> Enum.join("\n")
    [{name, line_no, body} | acc]
  end

  defp extract_step_name(line) do
    case Regex.run(~r/^\s+-\s+name:\s*['"]?([^'"\n]+?)['"]?\s*$/, line) do
      [_, capture] -> String.trim(capture)
      _ -> nil
    end
  end

  defp indent_of(line) do
    case Regex.run(~r/^(\s*)/, line) do
      [_, ws] -> String.length(ws)
      _ -> 0
    end
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
      # Research-extension findings are advisory in nature — fixes
      # vary by defect class (cache-key rewrite vs. step-restructure
      # vs. key-management). Route all to sustainabot.
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
