# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.BaselineHealth do
  @moduledoc """
  Detects degraded `main`-branch baseline conditions that allow silent rot.

  Three failure modes, each surfaced as a finding:

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

  Dispatches to:
  - sustainabot: BH001/BH003 advisory (owner-level action — branch
    protection / escalation)
  - rhodibot: BH002 follow-up PR to discharge the TODO

  Rule IDs: BH001-BH003

  ## Why this module exists

  Authored 2026-05-25 after a foundational fix to
  `hyperpolymath/reasonably-good-token-vault` (PR #89) where:

  1. PR #82 bumped `ureq` 2.12.1 → 3.3.0 and left a TODO instead of
     migrating. (BH002 detects this class.)
  2. Main went red and stayed red for 3+ days. (BH003 detects this.)
  3. Branch protection had no `required_status_checks`, so PR #82
     merged despite red CI. (BH001 detects this.)

  Each of those is a propagation hazard — every repo in the estate
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

  # ─── scan/2 facade ────────────────────────────────────────────────────

  @doc """
  Run all baseline-health checks. `owner` and `repo` may be `nil` if you
  only want the local (BH002) check; the API-backed checks will return
  empty in that case.
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
          bh003_persistent_red_baseline(owner, repo, opts)
      else
        []
      end

    findings = bh002_migration_todo_in_manifest(repo_path) ++ api_findings

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

      confidence =
        case finding.severity do
          :critical -> 0.95
          :high -> 0.88
          :medium -> 0.80
          :low -> 0.65
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
