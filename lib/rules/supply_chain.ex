# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.SupplyChain do
  @moduledoc """
  Supply-chain integrity rules drawn from OSSF Scorecard, SLSA, OWASP
  Top-10 CI/CD, and GitHub Actions hardening guide.

  Rule IDs SC001-SC011.

  These rules concern the **provenance and integrity** of code, actions,
  and release artifacts — distinct from `WorkflowHardening` (content
  defects) and `BaselineHealth` (drift conditions).

  ## Provenance map

  | Rule | Source | Upstream rule / check |
  |---|---|---|
  | SC001 | GitHub hardening | `.github/workflows/` not in CODEOWNERS |
  | SC002 | Scorecard `Dependency-Update-Tool` + GH hardening | Dependabot config missing or doesn't cover `github-actions` ecosystem |
  | SC003 | zizmor `archived-uses` | Action's repo is archived |
  | SC004 | zizmor `typosquat-uses` + Gu et al. 2023 | Action name typosquats a popular action |
  | SC005 | scorecard `Dangerous-Workflow` (subset) | `pull_request_target` trigger present (broad signal — narrow form is WH003) |
  | SC006 | NIST SP 800-218 PS.2.1 + scorecard `SBOM` | Release-publishing workflow without SBOM emission |
  | SC007 | GH hardening + StepSecurity | `runs-on: self-hosted` referenced from a public repo |
  | SC008 | GH hardening + zizmor `use-trusted-publishing` | Publish step uses long-lived secret instead of OIDC |
  | SC009 | scorecard `Security-Policy` | `SECURITY.md` missing |
  | SC010 | scorecard `Webhooks` | Repo webhooks configured without a secret |
  | SC011 | scorecard `Signed-Releases` + Endor `R-END-02` | Release workflow doesn't emit signed artifacts / provenance attestation |

  Module-level conventions match `WorkflowHardening`: pure-local for
  the file-presence rules, GitHub-API for the org/repo-state rules.
  Workflow file discovery uses repo-root `.github/workflows/` (the same
  scope GitHub Actions itself uses).
  """

  require Logger

  @github_api_base "https://api.github.com"

  # Canonical popular actions for SC004 typosquat detection. Action
  # names within edit-distance 2 of any entry, but with a different
  # owner, are flagged. Conservative list — extend deliberately.
  @popular_actions ~w[
    actions/checkout actions/setup-node actions/setup-python
    actions/setup-go actions/setup-java actions/setup-dotnet
    actions/upload-artifact actions/download-artifact
    actions/cache actions/cache/restore actions/cache/save
    actions/github-script actions/labeler actions/stale
    actions/dependency-review-action actions/attest-build-provenance
    docker/setup-buildx-action docker/login-action docker/build-push-action
    docker/setup-qemu-action docker/metadata-action
    goreleaser/goreleaser-action softprops/action-gh-release
    aquasecurity/trivy-action codecov/codecov-action
    github/codeql-action ossf/scorecard-action sigstore/cosign-installer
    step-security/harden-runner peter-evans/create-pull-request
  ]

  # Tools that emit SBOMs. Their presence in a release workflow
  # satisfies SC006.
  @sbom_tools ~w[
    anchore/sbom-action anchore/syft-action
    actions/attest-sbom CycloneDX/gh-node-module-generatebom
    spdx/spdx-sbom-generator microsoft/sbom-tool
  ]

  # Patterns that identify a release-publishing workflow (heuristic).
  @release_tool_patterns [
    ~r/softprops\/action-gh-release@/,
    ~r/goreleaser\/goreleaser-action@/,
    ~r/pypa\/gh-action-pypi-publish@/,
    ~r/rubygems\/release-gem@/,
    ~r/JS-DevTools\/npm-publish@/,
    ~r/cargo-publish/,
    ~r/^\s+publish:\s*$/m,
    ~r/^\s+release:\s*$/m
  ]

  # ─── SC001: .github/workflows/ not in CODEOWNERS ────────────────────

  @doc """
  SC001: `.github/workflows/` (or `*.yml` therein) is not covered by
  `CODEOWNERS`. Without this, anyone with write access can edit the
  workflow files — including bots or automated PRs whose changes
  shouldn't sail through unreviewed.
  """
  def sc001_workflows_not_in_codeowners(repo_path) do
    codeowners = locate_codeowners(repo_path)

    cond do
      codeowners == nil ->
        # No CODEOWNERS file at all — different rule (SC009 covers the
        # SECURITY.md analogue). Emit a finding here because the
        # workflow-edit hardening requires CODEOWNERS to exist.
        [
          %{
            rule: "SC001",
            file: repo_path |> Path.basename(),
            severity: :warn,
            reason:
              "no CODEOWNERS file — workflow edits cannot be gated on " <>
                "designated reviewer approval",
            action: :report,
            detail: %{
              fix:
                "Create `.github/CODEOWNERS` and add a `/.github/workflows/ " <>
                  "@<reviewer>` line."
            }
          }
        ]

      not codeowners_covers_workflows?(codeowners) ->
        [
          %{
            rule: "SC001",
            file: Path.relative_to(codeowners, repo_path),
            severity: :warn,
            reason:
              "CODEOWNERS does not name a reviewer for `.github/workflows/` " <>
                "— workflow edits sail through unreviewed",
            action: :report,
            detail: %{
              fix:
                "Add a line to CODEOWNERS:\n" <>
                  "  /.github/workflows/ @<owner-or-team>"
            }
          }
        ]

      true ->
        []
    end
  end

  # ─── SC002: Dependabot missing or no github-actions ecosystem ────────

  @doc """
  SC002: `.github/dependabot.yml` is missing, OR it exists but doesn't
  include `package-ecosystem: github-actions`. Note from the GHA
  hardening guide: Dependabot alerts only cover semver-tagged actions,
  but updates still help even on SHA-pinned actions when paired with
  release-tag comments.
  """
  def sc002_dependabot_missing(repo_path) do
    path = Path.join([repo_path, ".github", "dependabot.yml"])

    cond do
      not File.exists?(path) ->
        [
          %{
            rule: "SC002",
            file: ".github/dependabot.yml",
            severity: :warn,
            reason:
              "no dependabot.yml — automated dependency updates are not " <>
                "configured for this repository",
            action: :report,
            detail: %{
              fix:
                "Create `.github/dependabot.yml` with at minimum the " <>
                  "`github-actions` ecosystem enabled."
            }
          }
        ]

      true ->
        content = File.read!(path)

        if Regex.match?(~r/package-ecosystem:\s*["']?github-actions["']?\b/, content) do
          []
        else
          [
            %{
              rule: "SC002",
              file: ".github/dependabot.yml",
              severity: :info,
              reason:
                "dependabot.yml exists but does not include the " <>
                  "`github-actions` ecosystem — workflow action pins drift " <>
                  "without notification",
              action: :report,
              detail: %{
                fix:
                  "Add a `- package-ecosystem: \"github-actions\"` block " <>
                    "with `directory: \"/\"` and your preferred schedule."
              }
            }
          ]
        end
    end
  end

  # ─── SC003: Action repository archived ──────────────────────────────

  @doc """
  SC003: A workflow `uses:` references a GitHub repository that has
  been archived (read-only). Archived actions don't receive security
  updates; pinning to one is a forecast of future breakage.

  Requires `GITHUB_TOKEN` to check each `uses:` target's `archived`
  flag. Returns `[]` cleanly without one.
  """
  def sc003_archived_action(repo_path) do
    workflow_files(repo_path)
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      content
      |> extract_uses_slugs()
      |> Enum.uniq()
      |> Enum.flat_map(fn slug ->
        case fetch_repo_archived(slug) do
          {:ok, true} ->
            [
              %{
                rule: "SC003",
                file: rel,
                severity: :warn,
                reason:
                  "workflow #{rel} references `#{slug}`, whose upstream " <>
                    "repository is archived",
                action: :report,
                detail: %{
                  action: slug,
                  fix:
                    "Find an actively-maintained replacement, or vendor " <>
                      "the action into this repo (local `./.github/actions/...`)."
                }
              }
            ]

          _ ->
            []
        end
      end)
    end)
  end

  # ─── SC004: Typosquat action name ───────────────────────────────────

  @doc """
  SC004: A workflow `uses:` references a slug within edit-distance 2
  of a popular action but with a different owner. Catches the
  `actions/checout` and `actins/setup-node` class.
  """
  def sc004_typosquat_action(repo_path) do
    workflow_files(repo_path)
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      content
      |> extract_uses_slugs()
      |> Enum.uniq()
      |> Enum.flat_map(fn slug ->
        case typosquat_candidate(slug) do
          nil ->
            []

          canonical ->
            [
              %{
                rule: "SC004",
                file: rel,
                severity: :critical,
                reason:
                  "workflow #{rel} references `#{slug}` — name is within " <>
                    "edit-distance 2 of canonical `#{canonical}` but owner " <>
                    "differs (typosquat risk)",
                action: :report,
                detail: %{
                  observed: slug,
                  canonical: canonical,
                  fix:
                    "Replace with the canonical owner (`#{canonical}`) " <>
                      "OR confirm this is an intentionally-named fork and " <>
                      "add an inline comment justifying the choice."
                }
              }
            ]
        end
      end)
    end)
  end

  # ─── SC005: pull_request_target present (broad signal) ──────────────

  @doc """
  SC005: A workflow uses `on: pull_request_target` (with or without
  fork checkout). Even without the checkout, this trigger grants the
  workflow a privileged token in a fork-PR context — the existence
  of the trigger is itself a hardening signal that warrants review.

  WH003 catches the high-severity sub-case (trigger + checkout); SC005
  catches the broader use even where no checkout is present.
  """
  def sc005_pull_request_target_present(repo_path) do
    workflow_files(repo_path)
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      if Regex.match?(~r/^\s*-?\s*pull_request_target\b/m, content) or
           Regex.match?(~r/^\s*on:\s*pull_request_target\b/m, content) or
           Regex.match?(~r/^\s*on:\s*\[[^\]]*\bpull_request_target\b/m, content) do
        [
          %{
            rule: "SC005",
            file: rel,
            severity: :high,
            reason:
              "workflow #{rel} uses `pull_request_target` — privileged " <>
                "trigger in fork-PR context. Audit token usage and PR-head " <>
                "checkout pattern.",
            action: :report,
            detail: %{
              fix:
                "If you don't specifically need write access in fork PRs, " <>
                  "switch to `pull_request`. If you do, see WH003 for the " <>
                  "checkout-pattern hardening."
            }
          }
        ]
      else
        []
      end
    end)
  end

  # ─── SC006: Release workflow without SBOM emission ──────────────────

  @doc """
  SC006: A workflow that publishes a release artifact does not also
  call an SBOM-emitting action (anchore/sbom-action, actions/attest-sbom,
  etc.). NIST SSDF PS.2.1 + scorecard `SBOM`.
  """
  def sc006_release_without_sbom(repo_path) do
    workflow_files(repo_path)
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      release? = Enum.any?(@release_tool_patterns, &Regex.match?(&1, content))
      sbom? = Enum.any?(@sbom_tools, &String.contains?(content, &1))

      if release? and not sbom? do
        [
          %{
            rule: "SC006",
            file: rel,
            severity: :warn,
            reason:
              "workflow #{rel} publishes releases but does not emit an " <>
                "SBOM (no anchore/sbom-action, actions/attest-sbom, etc.)",
            action: :report,
            detail: %{
              fix:
                "Add a step calling `anchore/sbom-action@<sha>` or " <>
                  "`actions/attest-sbom@<sha>` before the release-publish step."
            }
          }
        ]
      else
        []
      end
    end)
  end

  # ─── SC007: self-hosted runner in public repo ───────────────────────

  @doc """
  SC007: A workflow uses `runs-on: self-hosted` (or a self-hosted
  label) in a public repository. Any fork can submit a PR that runs
  on your hardware. Per GitHub's own guide: never run self-hosted on
  public repos.

  Requires `GITHUB_TOKEN` to determine repo visibility. Returns `[]`
  without one (rather than warning unconditionally).
  """
  def sc007_self_hosted_on_public(owner, repo, repo_path) do
    with {:ok, %{"private" => false}} <- fetch_repo_basic(owner, repo) do
      workflow_files(repo_path)
      |> Enum.flat_map(fn path ->
        content = File.read!(path)
        rel = Path.relative_to(path, repo_path)

        if Regex.match?(~r/runs-on:\s*\[?\s*self-hosted\b/, content) do
          [
            %{
              rule: "SC007",
              file: rel,
              severity: :critical,
              reason:
                "workflow #{rel} uses self-hosted runner in a public repo " <>
                  "(#{owner}/#{repo}) — fork PRs can run on your hardware",
              action: :report,
              detail: %{
                fix:
                  "Either make the repository private/internal, or migrate " <>
                    "the job to GitHub-hosted runners."
              }
            }
          ]
        else
          []
        end
      end)
    else
      _ -> []
    end
  end

  # ─── SC008: Static-secret publish instead of OIDC ───────────────────

  @doc """
  SC008: A publish step uses a long-lived static secret (e.g. a PyPI
  API token) when an OIDC trusted-publishing alternative exists.
  """
  def sc008_static_secret_publish(repo_path) do
    workflow_files(repo_path)
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      # Heuristic: gh-action-pypi-publish with `password: ${{ secrets.PYPI* }}`
      # rather than OIDC trusted publishing (which uses `id-token: write`).
      pypi_with_secret? =
        Regex.match?(
          ~r/pypa\/gh-action-pypi-publish@.*\n(?:.*\n)*?\s+password:\s*\$\{\{\s*secrets\./m,
          content
        )

      npm_with_secret? =
        Regex.match?(
          ~r/JS-DevTools\/npm-publish@.*\n(?:.*\n)*?\s+token:\s*\$\{\{\s*secrets\./m,
          content
        )

      cases =
        [
          {pypi_with_secret?, "PyPI", "https://docs.pypi.org/trusted-publishers/"},
          {npm_with_secret?, "npm", "https://docs.npmjs.com/generating-provenance-statements"}
        ]
        |> Enum.filter(fn {flag, _, _} -> flag end)

      Enum.map(cases, fn {_, registry, doc} ->
        %{
          rule: "SC008",
          file: rel,
          severity: :warn,
          reason:
            "workflow #{rel} publishes to #{registry} with a long-lived " <>
              "static secret — OIDC trusted publishing is available",
          action: :report,
          detail: %{
            registry: registry,
            fix: "Configure trusted publishing per #{doc}, then drop the static secret."
          }
        }
      end)
    end)
  end

  # ─── SC009: SECURITY.md missing ─────────────────────────────────────

  @doc """
  SC009: No `SECURITY.md` (case-insensitive) at the repo root or in
  `.github/`. Scorecard's `Security-Policy` check.
  """
  def sc009_security_md_missing(repo_path) do
    candidates =
      ["SECURITY.md", "security.md", "Security.md", ".github/SECURITY.md"]
      |> Enum.map(&Path.join(repo_path, &1))

    if Enum.any?(candidates, &File.regular?/1) do
      []
    else
      [
        %{
          rule: "SC009",
          file: repo_path |> Path.basename(),
          severity: :warn,
          reason:
            "no SECURITY.md — vulnerability-disclosure contact is undefined",
          action: :report,
          detail: %{
            fix:
              "Create `SECURITY.md` (or `.github/SECURITY.md`) with a " <>
                "contact email or PSIRT URL and the disclosure SLA."
          }
        }
      ]
    end
  end

  # ─── SC010: Webhooks without secrets ────────────────────────────────

  @doc """
  SC010: Any repo webhook configured without a secret (HMAC origin
  authentication). Scorecard `Webhooks`, critical per their scoring.

  Requires `GITHUB_TOKEN` with `admin:repo_hook` scope on the target.
  Most PATs don't include that; falls back silently.
  """
  def sc010_webhooks_without_secret(owner, repo) do
    case curl_github("repos/#{owner}/#{repo}/hooks") do
      {:ok, hooks} when is_list(hooks) ->
        hooks
        |> Enum.filter(fn hook ->
          config = hook["config"] || %{}
          # GitHub returns secret as a placeholder if set; empty/nil
          # means no secret is configured.
          secret = config["secret"]
          is_nil(secret) or secret == ""
        end)
        |> Enum.map(fn hook ->
          %{
            rule: "SC010",
            file: "#{owner}/#{repo}",
            severity: :critical,
            reason:
              "webhook id=#{hook["id"]} (url=#{hook["config"]["url"]}) " <>
                "has no secret configured — origin cannot be authenticated",
            action: :report,
            detail: %{
              hook_id: hook["id"],
              fix:
                "On https://github.com/#{owner}/#{repo}/settings/hooks, " <>
                  "edit the webhook and add a Secret. Update the receiver " <>
                  "to verify the X-Hub-Signature-256 header."
            }
          }
        end)

      _ ->
        []
    end
  end

  # ─── SC011: Release artifacts not signed / no provenance ────────────

  @doc """
  SC011: A release-publishing workflow does not emit a signed artifact
  or provenance attestation. Scorecard's `Signed-Releases` looks for
  `.sig`, `.asc`, `.minisig`, `.intoto.jsonl` siblings of released
  binaries; this check is a workflow-level pre-flight.
  """
  def sc011_release_without_signing(repo_path) do
    workflow_files(repo_path)
    |> Enum.flat_map(fn path ->
      content = File.read!(path)
      rel = Path.relative_to(path, repo_path)

      release? = Enum.any?(@release_tool_patterns, &Regex.match?(&1, content))

      signed? =
        Regex.match?(~r/sigstore\/cosign-installer@/, content) or
          Regex.match?(~r/cosign\s+sign/, content) or
          Regex.match?(~r/actions\/attest-build-provenance@/, content) or
          Regex.match?(~r/slsa-github-generator\//, content) or
          Regex.match?(~r/gpg\s+--detach-sign/, content)

      if release? and not signed? do
        [
          %{
            rule: "SC011",
            file: rel,
            severity: :high,
            reason:
              "workflow #{rel} publishes releases without emitting a " <>
                "signature or provenance attestation",
            action: :report,
            detail: %{
              fix:
                "Add `sigstore/cosign-installer@<sha>` + a `cosign sign-blob` " <>
                  "step, OR `actions/attest-build-provenance@<sha>` for SLSA " <>
                  "provenance. Both produce sibling .sig/.intoto.jsonl files."
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
  Run every supply-chain check on `repo_path`. `owner_repo` opt enables
  the API-backed checks (SC003, SC007, SC010).
  """
  def scan(repo_path, opts \\ []) do
    {owner, repo} =
      case Keyword.get(opts, :owner_repo) do
        {o, r} when is_binary(o) and is_binary(r) -> {o, r}
        _ -> {nil, nil}
      end

    api_findings =
      cond do
        owner == nil or repo == nil ->
          []

        true ->
          sc007_self_hosted_on_public(owner, repo, repo_path) ++
            sc010_webhooks_without_secret(owner, repo)
      end

    findings =
      sc001_workflows_not_in_codeowners(repo_path) ++
        sc002_dependabot_missing(repo_path) ++
        sc003_archived_action(repo_path) ++
        sc004_typosquat_action(repo_path) ++
        sc005_pull_request_target_present(repo_path) ++
        sc006_release_without_sbom(repo_path) ++
        sc008_static_secret_publish(repo_path) ++
        sc009_security_md_missing(repo_path) ++
        sc011_release_without_signing(repo_path) ++
        api_findings

    %{
      findings: findings,
      total: length(findings),
      by_severity: group_by_severity(findings),
      dispatch: dispatch_recommendations(findings)
    }
  end

  # ─── Internals ──────────────────────────────────────────────────────

  defp workflow_files(repo_path) do
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

  defp locate_codeowners(repo_path) do
    ["CODEOWNERS", ".github/CODEOWNERS", "docs/CODEOWNERS"]
    |> Enum.map(&Path.join(repo_path, &1))
    |> Enum.find(&File.regular?/1)
  end

  defp codeowners_covers_workflows?(codeowners_path) do
    content = File.read!(codeowners_path)
    Regex.match?(~r/(?m)^(\/\.github\/workflows\/|\.github\/workflows\/|\*\s+|\*\.yml)/, content)
  end

  defp extract_uses_slugs(content) do
    Regex.scan(~r/^\s*-?\s*uses:\s*(\S+)/m, content)
    |> Enum.map(fn [_, slug] -> slug end)
    |> Enum.reject(&(String.starts_with?(&1, "./") or String.starts_with?(&1, "docker://")))
    |> Enum.map(fn slug ->
      # Strip @<ref> suffix
      case String.split(slug, "@", parts: 2) do
        [base, _ref] -> base
        [base] -> base
      end
    end)
  end

  defp typosquat_candidate(slug) do
    @popular_actions
    |> Enum.find(fn canonical ->
      # Same path, different owner, and distance ≤ 2.
      slug != canonical and
        not String.starts_with?(slug, "actions/") == false and
        levenshtein(slug, canonical) <= 2 and
        (action_owner(slug) != action_owner(canonical))
    end)
  end

  defp action_owner(slug) do
    case String.split(slug, "/", parts: 2) do
      [owner, _] -> owner
      _ -> nil
    end
  end

  # Iterative O(n*m) Levenshtein. The naïve recursive form was
  # exponential — fine on short test inputs, but ~3^32 on real
  # action-slug pairs which led to scan-time hangs.
  defp levenshtein(a, b) do
    a_list = String.graphemes(a)
    b_list = String.graphemes(b)
    n = length(a_list)
    m = length(b_list)

    cond do
      n == 0 -> m
      m == 0 -> n
      abs(n - m) > 2 -> 3  # short-circuit: caller only cares about <= 2
      true -> dp_levenshtein(a_list, b_list, m)
    end
  end

  defp dp_levenshtein(a_list, b_list, m) do
    init_row = Enum.to_list(0..m)

    Enum.with_index(a_list, 1)
    |> Enum.reduce(init_row, fn {ah, i}, prev_row ->
      [first | _] = prev_row

      {row, _} =
        Enum.reduce(Enum.with_index(b_list, 1), {[i], prev_row}, fn {bh, _j},
                                                                    {acc, [pl, pr | rest]} ->
          cost = if ah == bh, do: 0, else: 1
          [cur | _] = acc

          val =
            Enum.min([
              cur + 1,
              pr + 1,
              pl + cost
            ])

          {[val | acc], [pr | rest]}
        end)

      _ = first
      Enum.reverse(row)
    end)
    |> List.last()
  end

  defp fetch_repo_archived(slug) do
    case curl_github("repos/#{slug}") do
      {:ok, %{"archived" => archived}} -> {:ok, archived}
      {:ok, %{"message" => _}} -> {:error, :not_found}
      other -> other
    end
  end

  defp fetch_repo_basic(owner, repo) do
    case curl_github("repos/#{owner}/#{repo}") do
      {:ok, body} when is_map(body) -> {:ok, body}
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
            {:error, _} -> {:error, "invalid JSON"}
          end

        {error, _} ->
          {:error, "curl failed: #{String.slice(error, 0, 200)}"}
      end
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
      # Mechanically-fixable rules go to rhodibot at higher confidence;
      # owner-judgement rules (security policy, runner posture) go to
      # sustainabot.
      {bot, conf} =
        case finding.rule do
          "SC001" -> {:rhodibot, 0.88}
          "SC002" -> {:rhodibot, 0.90}
          "SC009" -> {:rhodibot, 0.92}
          _ -> {:sustainabot, severity_to_confidence(finding.severity)}
        end

      %{
        bot: bot,
        confidence: conf,
        rule: finding.rule,
        action: finding.action,
        reason: finding.reason
      }
    end)
  end

  defp severity_to_confidence(:critical), do: 0.92
  defp severity_to_confidence(:high), do: 0.85
  defp severity_to_confidence(:warn), do: 0.75
  defp severity_to_confidence(:info), do: 0.60
  defp severity_to_confidence(_), do: 0.50
end
