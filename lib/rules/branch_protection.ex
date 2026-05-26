# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.BranchProtection do
  @moduledoc """
  Branch-protection hygiene rules drawn from the CIS GitHub Benchmark,
  OSSF Scorecard, and NIST SP 800-218 (SSDF) PO/PS/PW practices.

  Rule IDs BP001-BP007.

  These rules concern the **review and integrity controls** on the
  default branch — distinct from `BaselineHealth` (drift conditions),
  `WorkflowHardening` (workflow content) and `SupplyChain` (provenance).

  All rules read from a single GitHub API endpoint:

      gh api repos/{owner}/{repo}/branches/main/protection

  ## Provenance map

  | Rule | Source | Upstream rule / check |
  |---|---|---|
  | BP001 | CIS GH 1.1.x | `required_signatures: true` missing |
  | BP002 | CIS GH 1.3.x | `required_linear_history: true` missing |
  | BP003 | CIS GH 1.4.x | `required_pull_request_reviews.required_approving_review_count < 1` |
  | BP004 | CIS GH 1.5.x | `dismiss_stale_reviews: false` |
  | BP005 | CIS GH 1.6.x + NIST PO.3.2 | `require_code_owner_reviews: true` but CODEOWNERS missing/empty |
  | BP006 | CIS GH | `enforce_admins: false` |
  | BP007 | scorecard `Branch-Protection` tier 1 | default branch allows force-push or deletion |

  ## Dispatch

  All BP rules route to `:sustainabot`. Branch-protection edits require
  admin scope on the repository and are owner-side (not bot-fixable); the
  finding is advisory only.

  Severity-to-confidence mapping: critical → 0.92, high → 0.85,
  warn → 0.75, info → 0.60.

  Each rule returns `[]` cleanly when `GITHUB_TOKEN` /
  `HYPATIA_DISPATCH_PAT` is not set, matching the convention in
  `BaselineHealth` and `SupplyChain`.
  """

  require Logger

  @github_api_base "https://api.github.com"

  # ─── BP001: required_signatures missing ─────────────────────────────

  @doc """
  BP001: `main` branch protection does not require signed commits
  (`required_signatures.enabled != true`). Without this, unsigned
  commits can land on main; the GPG/SSH-signing supply-chain control
  is bypassed.

  Severity: `:high` — provenance gap on the protected branch.

  Requires `GITHUB_TOKEN`. Returns `[]` cleanly without one.
  """
  def bp001_required_signatures_missing(owner, repo) do
    case fetch_branch_protection(owner, repo, "main") do
      {:ok, protection} ->
        if signatures_required?(protection) do
          []
        else
          [
            %{
              rule: "BP001",
              file: "#{owner}/#{repo}",
              severity: :high,
              reason:
                "main branch protection does not require signed commits " <>
                  "(required_signatures != true) — unsigned commits can land",
              action: :report,
              detail: %{
                branch: "main",
                fix:
                  "Enable signed-commit requirement: " <>
                    "`gh api -X POST repos/#{owner}/#{repo}/branches/main/protection/required_signatures`"
              }
            }
          ]
        end

      _ ->
        []
    end
  end

  # ─── BP002: required_linear_history missing ─────────────────────────

  @doc """
  BP002: `main` branch protection does not enforce linear history
  (`required_linear_history != true`). Merge commits and back-merges
  make audit traceability harder and obscure who introduced what.

  Severity: `:warn` — style/audit posture, not a security gap on its
  own. Down-weighted relative to BP001/BP003/BP006/BP007.

  Requires `GITHUB_TOKEN`. Returns `[]` cleanly without one.
  """
  def bp002_required_linear_history_missing(owner, repo) do
    case fetch_branch_protection(owner, repo, "main") do
      {:ok, protection} ->
        if linear_history_required?(protection) do
          []
        else
          [
            %{
              rule: "BP002",
              file: "#{owner}/#{repo}",
              severity: :warn,
              reason:
                "main branch protection does not require linear history " <>
                  "— merge commits can obscure audit trail",
              action: :report,
              detail: %{
                branch: "main",
                fix:
                  "Enable linear-history requirement via " <>
                    "Settings > Branches > main, or " <>
                    "`gh api -X PUT repos/#{owner}/#{repo}/branches/main/protection " <>
                    "-F required_linear_history=true`"
              }
            }
          ]
        end

      _ ->
        []
    end
  end

  # ─── BP003: required_approving_review_count < 1 ─────────────────────

  @doc """
  BP003: `main` branch protection requires fewer than 1 approving
  review (`required_pull_request_reviews.required_approving_review_count`
  is 0, missing, or the whole reviews block is absent). PRs can be
  self-merged with no second pair of eyes.

  Severity: `:critical` — fundamental review gap.

  Requires `GITHUB_TOKEN`. Returns `[]` cleanly without one.
  """
  def bp003_required_approving_reviews_below_one(owner, repo) do
    case fetch_branch_protection(owner, repo, "main") do
      {:ok, protection} ->
        count = approving_review_count(protection)

        if count >= 1 do
          []
        else
          [
            %{
              rule: "BP003",
              file: "#{owner}/#{repo}",
              severity: :critical,
              reason:
                "main branch protection requires #{count} approving review(s) " <>
                  "— PRs can be self-merged with no second pair of eyes",
              action: :report,
              detail: %{
                branch: "main",
                observed_count: count,
                fix:
                  "Set required_approving_review_count >= 1 via " <>
                    "Settings > Branches > main, or " <>
                    "`gh api -X PATCH repos/#{owner}/#{repo}/branches/main/protection " <>
                    "-F required_pull_request_reviews[required_approving_review_count]=1`"
              }
            }
          ]
        end

      _ ->
        []
    end
  end

  # ─── BP004: dismiss_stale_reviews false ─────────────────────────────

  @doc """
  BP004: `main` branch protection does not dismiss stale reviews
  (`required_pull_request_reviews.dismiss_stale_reviews != true`).
  An approval given on commit A persists across subsequent pushes —
  later commits sail through unreviewed under the original approval.

  Severity: `:warn` — bypass of the review-on-current-diff invariant.

  Requires `GITHUB_TOKEN`. Returns `[]` cleanly without one.
  """
  def bp004_dismiss_stale_reviews_off(owner, repo) do
    case fetch_branch_protection(owner, repo, "main") do
      {:ok, protection} ->
        cond do
          # If reviews aren't required at all, BP003 covers the gap;
          # don't double-report.
          not has_review_block?(protection) ->
            []

          dismiss_stale_reviews?(protection) ->
            []

          true ->
            [
              %{
                rule: "BP004",
                file: "#{owner}/#{repo}",
                severity: :warn,
                reason:
                  "main branch protection does not dismiss stale reviews " <>
                    "— approvals carry across subsequent commits unreviewed",
                action: :report,
                detail: %{
                  branch: "main",
                  fix:
                    "Enable dismiss-stale-reviews via " <>
                      "Settings > Branches > main, or " <>
                      "`gh api -X PATCH repos/#{owner}/#{repo}/branches/main/protection " <>
                      "-F required_pull_request_reviews[dismiss_stale_reviews]=true`"
                }
              }
            ]
        end

      _ ->
        []
    end
  end

  # ─── BP005: require_code_owner_reviews but no CODEOWNERS ────────────

  @doc """
  BP005: `main` branch protection requires CODEOWNERS review
  (`required_pull_request_reviews.require_code_owner_reviews == true`)
  but the repository does not contain a CODEOWNERS file, or contains
  one that is empty / comment-only. PRs end up `BLOCKED` because no
  reviewer can ever be matched.

  Severity: `:high` — protection is effectively a denial-of-service
  on the PR queue until CODEOWNERS is restored.

  Requires `GITHUB_TOKEN` (for protection settings) and clones the
  CODEOWNERS file via the contents API. Returns `[]` cleanly without
  a token.
  """
  def bp005_codeowners_required_but_missing(owner, repo) do
    case fetch_branch_protection(owner, repo, "main") do
      {:ok, protection} ->
        cond do
          not code_owner_reviews_required?(protection) ->
            []

          codeowners_present?(owner, repo) ->
            []

          true ->
            [
              %{
                rule: "BP005",
                file: "#{owner}/#{repo}",
                severity: :high,
                reason:
                  "main branch protection requires CODEOWNERS reviews but " <>
                    "no non-empty CODEOWNERS file is present — PRs will " <>
                    "be permanently BLOCKED on missing reviewer match",
                action: :report,
                detail: %{
                  branch: "main",
                  fix:
                    "Either create a non-empty CODEOWNERS (covering at " <>
                      "minimum `*` to a team), or disable " <>
                      "`require_code_owner_reviews` until one is in place."
                }
              }
            ]
        end

      _ ->
        []
    end
  end

  # ─── BP006: enforce_admins false ────────────────────────────────────

  @doc """
  BP006: `main` branch protection does not enforce against admins
  (`enforce_admins.enabled != true`). Admins can bypass every other
  protection — required reviews, status checks, linear history — by
  pushing directly to main. CIS GitHub Benchmark calls this out as a
  baseline expectation.

  Severity: `:high` — every other branch-protection rule is bypassable
  while this is off.

  Requires `GITHUB_TOKEN`. Returns `[]` cleanly without one.
  """
  def bp006_enforce_admins_off(owner, repo) do
    case fetch_branch_protection(owner, repo, "main") do
      {:ok, protection} ->
        if enforce_admins?(protection) do
          []
        else
          [
            %{
              rule: "BP006",
              file: "#{owner}/#{repo}",
              severity: :high,
              reason:
                "main branch protection does not enforce against admins " <>
                  "(enforce_admins != true) — every other protection is bypassable",
              action: :report,
              detail: %{
                branch: "main",
                fix:
                  "Enable enforce_admins via Settings > Branches > main, or " <>
                    "`gh api -X POST repos/#{owner}/#{repo}/branches/main/protection/enforce_admins`"
              }
            }
          ]
        end

      _ ->
        []
    end
  end

  # ─── BP007: force-push or deletion allowed ──────────────────────────

  @doc """
  BP007: `main` branch protection allows force-push or branch deletion
  (`allow_force_pushes.enabled == true` or `allow_deletions.enabled ==
  true`). Either of these defeats history integrity: a force-push can
  rewrite landed commits, a deletion takes the branch out from under
  open PRs.

  Severity: `:critical` — scorecard Branch-Protection treats both as
  tier-1 fail conditions.

  Requires `GITHUB_TOKEN`. Returns `[]` cleanly without one.
  """
  def bp007_force_push_or_delete_allowed(owner, repo) do
    case fetch_branch_protection(owner, repo, "main") do
      {:ok, protection} ->
        flags =
          []
          |> maybe_add(force_pushes_allowed?(protection), "force-push")
          |> maybe_add(deletions_allowed?(protection), "deletion")

        case flags do
          [] ->
            []

          _ ->
            joined = Enum.join(flags, " and ")

            [
              %{
                rule: "BP007",
                file: "#{owner}/#{repo}",
                severity: :critical,
                reason:
                  "main branch protection allows #{joined} — history " <>
                    "integrity on the default branch is not guaranteed",
                action: :report,
                detail: %{
                  branch: "main",
                  allowed: flags,
                  fix:
                    "Disable both `allow_force_pushes` and `allow_deletions` " <>
                      "in Settings > Branches > main, or " <>
                      "`gh api -X PUT repos/#{owner}/#{repo}/branches/main/protection " <>
                      "-F allow_force_pushes=false -F allow_deletions=false`"
                }
              }
            ]
        end

      _ ->
        []
    end
  end

  # ─── scan/2 facade ──────────────────────────────────────────────────

  @doc """
  Run every branch-protection check. Requires `owner_repo: {owner, repo}`
  in `opts`, OR a `repo_path` whose `origin` remote points at GitHub
  (parsed via `git remote get-url`).

  All BP rules are API-backed; with no token, every rule returns `[]`
  and `scan/2` returns the empty-shape result.
  """
  def scan(repo_path, opts \\ []) do
    {owner, repo} =
      case Keyword.get(opts, :owner_repo) do
        {o, r} when is_binary(o) and is_binary(r) -> {o, r}
        _ -> extract_owner_repo(repo_path)
      end

    findings =
      if owner && repo do
        bp001_required_signatures_missing(owner, repo) ++
          bp002_required_linear_history_missing(owner, repo) ++
          bp003_required_approving_reviews_below_one(owner, repo) ++
          bp004_dismiss_stale_reviews_off(owner, repo) ++
          bp005_codeowners_required_but_missing(owner, repo) ++
          bp006_enforce_admins_off(owner, repo) ++
          bp007_force_push_or_delete_allowed(owner, repo)
      else
        []
      end

    %{
      findings: findings,
      total: length(findings),
      by_severity: group_by_severity(findings),
      dispatch: dispatch_recommendations(findings)
    }
  end

  # ─── Dispatch recommendations ───────────────────────────────────────

  @doc """
  Map each finding to a dispatch recommendation. Branch-protection
  edits require admin scope on the repository — every BP rule routes
  to `:sustainabot` (advisory). Confidence is derived from severity.
  """
  def dispatch_recommendations(findings) do
    Enum.map(findings, fn finding ->
      %{
        bot: :sustainabot,
        confidence: severity_to_confidence(finding.severity),
        rule: finding.rule,
        action: finding.action,
        reason: finding.reason
      }
    end)
  end

  # ─── Predicates over the protection payload ─────────────────────────

  defp signatures_required?(%{"required_signatures" => %{"enabled" => true}}), do: true
  defp signatures_required?(_), do: false

  defp linear_history_required?(%{"required_linear_history" => %{"enabled" => true}}), do: true
  defp linear_history_required?(%{"required_linear_history" => true}), do: true
  defp linear_history_required?(_), do: false

  defp approving_review_count(%{
         "required_pull_request_reviews" => %{
           "required_approving_review_count" => n
         }
       })
       when is_integer(n),
       do: n

  defp approving_review_count(_), do: 0

  defp has_review_block?(%{"required_pull_request_reviews" => block}) when is_map(block),
    do: true

  defp has_review_block?(_), do: false

  defp dismiss_stale_reviews?(%{
         "required_pull_request_reviews" => %{"dismiss_stale_reviews" => true}
       }),
       do: true

  defp dismiss_stale_reviews?(_), do: false

  defp code_owner_reviews_required?(%{
         "required_pull_request_reviews" => %{"require_code_owner_reviews" => true}
       }),
       do: true

  defp code_owner_reviews_required?(_), do: false

  defp enforce_admins?(%{"enforce_admins" => %{"enabled" => true}}), do: true
  defp enforce_admins?(%{"enforce_admins" => true}), do: true
  defp enforce_admins?(_), do: false

  defp force_pushes_allowed?(%{"allow_force_pushes" => %{"enabled" => true}}), do: true
  defp force_pushes_allowed?(%{"allow_force_pushes" => true}), do: true
  defp force_pushes_allowed?(_), do: false

  defp deletions_allowed?(%{"allow_deletions" => %{"enabled" => true}}), do: true
  defp deletions_allowed?(%{"allow_deletions" => true}), do: true
  defp deletions_allowed?(_), do: false

  defp maybe_add(list, true, item), do: list ++ [item]
  defp maybe_add(list, _, _item), do: list

  # ─── CODEOWNERS presence ────────────────────────────────────────────

  # BP005 helper. GitHub looks for CODEOWNERS in three locations: root,
  # `.github/`, and `docs/`. A file counts as "present" only if it has
  # at least one non-comment, non-blank line (matching GitHub's parser).
  defp codeowners_present?(owner, repo) do
    ["CODEOWNERS", ".github/CODEOWNERS", "docs/CODEOWNERS"]
    |> Enum.any?(fn path ->
      case curl_github("repos/#{owner}/#{repo}/contents/#{path}") do
        {:ok, %{"content" => b64, "encoding" => "base64"}} ->
          case Base.decode64(b64, ignore: :whitespace) do
            {:ok, content} -> codeowners_non_empty?(content)
            _ -> false
          end

        _ ->
          false
      end
    end)
  end

  defp codeowners_non_empty?(content) do
    content
    |> String.split("\n")
    |> Enum.map(&String.trim/1)
    |> Enum.any?(fn line ->
      line != "" and not String.starts_with?(line, "#")
    end)
  end

  # ─── GitHub API ─────────────────────────────────────────────────────

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

  # ─── Owner/repo extraction ──────────────────────────────────────────

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

  # ─── Bookkeeping ────────────────────────────────────────────────────

  defp group_by_severity(findings) do
    findings
    |> Enum.group_by(& &1.severity)
    |> Enum.map(fn {sev, items} -> {sev, length(items)} end)
    |> Map.new()
  end

  defp severity_to_confidence(:critical), do: 0.92
  defp severity_to_confidence(:high), do: 0.85
  defp severity_to_confidence(:warn), do: 0.75
  defp severity_to_confidence(:info), do: 0.60
  defp severity_to_confidence(_), do: 0.50
end
