# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.GitState do
  @moduledoc """
  Git repository synchronisation state checks.

  Detects repos that are out of sync with their remotes: uncommitted changes,
  unpushed commits, diverged branches, and stale tracking references.
  This is critical for fleet-wide visibility -- if a repo isn't pushed,
  other sessions and CI can't see the changes.

  Dispatches to:
  - sustainabot: advisory for unpushed changes
  - seambot: verify sync after push

  Rule IDs: GS001-GS007
  """

  # ─── GS001: Uncommitted changes ────────────────────────────────────────

  @doc """
  GS001: Detect uncommitted changes (staged or unstaged).
  Severity: medium (work at risk of loss, invisible to fleet).
  Action: commit and push.
  """
  def gs001_uncommitted_changes(repo_path) do
    case System.cmd("git", ["status", "--porcelain"], cd: repo_path, stderr_to_stdout: true) do
      {output, 0} when output != "" ->
        lines = output |> String.split("\n", trim: true)
        staged = Enum.count(lines, fn l -> String.match?(l, ~r/^[MADRC]/) end)
        unstaged = Enum.count(lines, fn l -> String.match?(l, ~r/^.[MDRC]/) end)
        untracked = Enum.count(lines, fn l -> String.starts_with?(l, "??") end)

        findings = []

        findings =
          if staged > 0 do
            [%{
              rule: "GS001",
              file: ".",
              severity: :medium,
              reason: "#{staged} staged but uncommitted change(s) -- commit needed",
              action: :commit,
              detail: %{staged: staged}
            } | findings]
          else
            findings
          end

        findings =
          if unstaged > 0 do
            [%{
              rule: "GS001",
              file: ".",
              severity: :medium,
              reason: "#{unstaged} unstaged modification(s) -- stage and commit needed",
              action: :commit,
              detail: %{unstaged: unstaged}
            } | findings]
          else
            findings
          end

        findings =
          if untracked > 0 do
            [%{
              rule: "GS001",
              file: ".",
              severity: :low,
              reason: "#{untracked} untracked file(s) -- review and add or .gitignore",
              action: :review,
              detail: %{untracked: untracked}
            } | findings]
          else
            findings
          end

        findings

      _ -> []
    end
  end

  # ─── GS002: Unpushed commits ───────────────────────────────────────────

  @doc """
  GS002: Detect commits that exist locally but haven't been pushed.
  Severity: high (other sessions can't see this work).
  Action: push to remote.
  """
  def gs002_unpushed_commits(repo_path) do
    # Get current branch
    branch = get_current_branch(repo_path)

    if branch == nil do
      []
    else
      # Check for upstream tracking
      case System.cmd("git", ["rev-list", "--count", "@{u}..HEAD"],
                      cd: repo_path, stderr_to_stdout: true) do
        {count_str, 0} ->
          count = count_str |> String.trim() |> String.to_integer()

          if count > 0 do
            [%{
              rule: "GS002",
              file: ".",
              severity: :high,
              reason: "#{count} unpushed commit(s) on #{branch} -- push to remote",
              action: :push,
              detail: %{branch: branch, unpushed: count}
            }]
          else
            []
          end

        # No upstream tracking branch
        {output, _} when output != "" ->
          if String.contains?(output, "no upstream") do
            [%{
              rule: "GS002",
              file: ".",
              severity: :high,
              reason: "Branch #{branch} has no upstream tracking -- push with -u",
              action: :push_set_upstream,
              detail: %{branch: branch}
            }]
          else
            []
          end

        _ -> []
      end
    end
  end

  # ─── GS003: Branch divergence ──────────────────────────────────────────

  @doc """
  GS003: Detect branches that have diverged from their upstream.
  Severity: high (merge conflicts likely).
  Action: pull --rebase or merge.
  """
  def gs003_branch_divergence(repo_path) do
    case System.cmd("git", ["rev-list", "--left-right", "--count", "HEAD...@{u}"],
                    cd: repo_path, stderr_to_stdout: true) do
      {output, 0} ->
        parts = output |> String.trim() |> String.split(~r/\s+/)

        case parts do
          [ahead_str, behind_str] ->
            ahead = String.to_integer(ahead_str)
            behind = String.to_integer(behind_str)

            if ahead > 0 and behind > 0 do
              branch = get_current_branch(repo_path)

              [%{
                rule: "GS003",
                file: ".",
                severity: :high,
                reason: "Branch #{branch} diverged: #{ahead} ahead, #{behind} behind upstream -- rebase or merge needed",
                action: :rebase_or_merge,
                detail: %{branch: branch, ahead: ahead, behind: behind}
              }]
            else
              []
            end

          _ -> []
        end

      _ -> []
    end
  end

  # ─── GS004: Stale remote refs ──────────────────────────────────────────

  @doc """
  GS004: Detect stale remote tracking references (remote branches deleted).
  Severity: low (cleanup task).
  Action: git remote prune origin.
  """
  def gs004_stale_remote_refs(repo_path) do
    case System.cmd("git", ["remote", "prune", "origin", "--dry-run"],
                    cd: repo_path, stderr_to_stdout: true) do
      {output, 0} when output != "" ->
        stale_refs =
          output
          |> String.split("\n", trim: true)
          |> Enum.filter(&String.contains?(&1, "[would prune]"))

        if stale_refs != [] do
          [%{
            rule: "GS004",
            file: ".",
            severity: :low,
            reason: "#{length(stale_refs)} stale remote tracking ref(s) -- run git remote prune origin",
            action: :prune_remote,
            detail: %{stale_count: length(stale_refs)}
          }]
        else
          []
        end

      _ -> []
    end
  end

  # ─── GS005: Detached HEAD ──────────────────────────────────────────────

  @doc """
  GS005: Detect detached HEAD state.
  Severity: high (commits will be lost without branch).
  Action: create branch or checkout existing branch.
  """
  def gs005_detached_head(repo_path) do
    # Detached HEAD in a CI runner is the expected state — actions/checkout
    # fetches the PR merge ref and leaves HEAD pointing at it directly,
    # not at a branch. Surfacing a "commits will be lost" warning there
    # is pure noise; the runner is ephemeral and no human is committing
    # into the workspace. Suppress when CI env vars are set.
    if ci_environment?() do
      []
    else
      case System.cmd("git", ["symbolic-ref", "HEAD"],
                      cd: repo_path, stderr_to_stdout: true) do
        {_, 0} -> []  # Not detached
        _ ->
          [%{
            rule: "GS005",
            file: ".",
            severity: :high,
            reason: "HEAD is detached -- commits will be lost. Create a branch or checkout an existing one.",
            action: :create_branch
          }]
      end
    end
  end

  defp ci_environment? do
    # GitHub Actions sets both CI=true and GITHUB_ACTIONS=true.
    # GitLab/CircleCI/Drone/Travis all set CI=true. Keep the predicate
    # generic so the rule self-suppresses on any major CI host.
    System.get_env("CI") in ["true", "1"] or
      System.get_env("GITHUB_ACTIONS") == "true" or
      System.get_env("GITLAB_CI") == "true"
  end

  # ─── GS006: Not on main/master ─────────────────────────────────────────

  @doc """
  GS006: Detect repos on non-default branch (feature branches left checked out).
  Severity: low (informational -- may be intentional).
  Action: review if branch should be merged.
  """
  def gs006_not_on_default_branch(repo_path) do
    branch = get_current_branch(repo_path)

    if branch != nil and branch not in ["main", "master"] do
      [%{
        rule: "GS006",
        file: ".",
        severity: :low,
        reason: "On branch '#{branch}' instead of main/master -- review if merge is needed",
        action: :review,
        detail: %{branch: branch}
      }]
    else
      []
    end
  end

  # ─── GS007: Stale remote branches ───────────────────────────────────────

  @doc """
  GS007: Detect non-main remote branches.
  Policy: single main branch only. Other remote branches are stale and
  should be deleted after merging.
  Severity: medium.
  Action: delete remote branches after merge verification.
  """
  def gs007_stale_remote_branches(repo_path) do
    case System.cmd("git", ["branch", "-r"], cd: repo_path, stderr_to_stdout: true) do
      {output, 0} when output != "" ->
        branches =
          output
          |> String.split("\n", trim: true)
          |> Enum.map(&String.trim/1)
          |> Enum.reject(fn branch ->
            branch == "origin/main" or
            branch == "origin/master" or
            branch == "origin/HEAD" or
            String.contains?(branch, "->")
          end)

        if branches != [] do
          [%{
            rule: "GS007",
            file: ".",
            severity: :medium,
            reason: "Repository has #{length(branches)} non-main remote branch(es). Policy: single main branch only.",
            action: :delete_remote_branches,
            detail: %{stale_branches: branches, count: length(branches)}
          }]
        else
          []
        end

      _ -> []
    end
  end

  # ─── Comprehensive scan ───────────────────────────────────────────────

  @doc """
  Run all git state checks on a repository.
  Returns findings with fleet dispatch recommendations.
  """
  def scan(repo_path) do
    # Verify it's a git repo first
    unless File.dir?(Path.join(repo_path, ".git")) do
      []
    else
      findings =
        gs001_uncommitted_changes(repo_path) ++
        gs002_unpushed_commits(repo_path) ++
        gs003_branch_divergence(repo_path) ++
        gs004_stale_remote_refs(repo_path) ++
        gs005_detached_head(repo_path) ++
        gs006_not_on_default_branch(repo_path) ++
        gs007_stale_remote_branches(repo_path)

      %{
        findings: findings,
        total: length(findings),
        by_severity: group_by_severity(findings),
        dispatch: dispatch_recommendations(findings)
      }
    end
  end

  # ─── Helpers ──────────────────────────────────────────────────────────

  defp get_current_branch(repo_path) do
    case System.cmd("git", ["rev-parse", "--abbrev-ref", "HEAD"],
                    cd: repo_path, stderr_to_stdout: true) do
      {branch, 0} ->
        trimmed = String.trim(branch)
        if trimmed == "HEAD", do: nil, else: trimmed

      _ -> nil
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
      bot = case finding.action do
        :push -> :sustainabot
        :push_set_upstream -> :sustainabot
        :commit -> :sustainabot
        :rebase_or_merge -> :seambot
        :prune_remote -> :rhodibot
        :create_branch -> :seambot
        _ -> :sustainabot
      end

      confidence = case finding.severity do
        :critical -> 0.98
        :high -> 0.90
        :medium -> 0.85
        :low -> 0.70
        _ -> 0.50
      end

      %{bot: bot, confidence: confidence, rule: finding.rule,
        action: finding.action, reason: finding.reason}
    end)
  end
end
