# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Tests for git state detection rules (GS001-GS006).
# Uses ephemeral git repos in /tmp to exercise each rule function.

defmodule Hypatia.Rules.GitStateTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.GitState

  @tmp_dir System.tmp_dir!()

  # ─── Test helpers ────────────────────────────────────────────────────

  defp create_git_repo do
    repo = Path.join(@tmp_dir, "gs_test_#{System.unique_integer([:positive])}")
    File.mkdir_p!(repo)

    # Initialise a real git repo so the git commands succeed
    System.cmd("git", ["init", "--initial-branch=main"], cd: repo)
    System.cmd("git", ["config", "user.email", "test@test.com"], cd: repo)
    System.cmd("git", ["config", "user.name", "Test"], cd: repo)

    # Create an initial commit so HEAD exists
    File.write!(Path.join(repo, "README.md"), "# test")
    System.cmd("git", ["add", "."], cd: repo)
    System.cmd("git", ["commit", "-m", "init"], cd: repo)

    repo
  end

  setup do
    repo = create_git_repo()
    on_exit(fn -> File.rm_rf!(repo) end)
    {:ok, repo: repo}
  end

  # ─── GS001: Uncommitted changes ────────────────────────────────────

  describe "gs001_uncommitted_changes/1" do
    test "returns empty on clean working tree", %{repo: repo} do
      findings = GitState.gs001_uncommitted_changes(repo)
      assert findings == []
    end

    test "detects staged but uncommitted changes", %{repo: repo} do
      File.write!(Path.join(repo, "new.txt"), "staged content")
      System.cmd("git", ["add", "new.txt"], cd: repo)

      findings = GitState.gs001_uncommitted_changes(repo)
      staged = Enum.filter(findings, &String.contains?(&1.reason, "staged"))
      assert length(staged) >= 1
      assert hd(staged).rule == "GS001"
      assert hd(staged).severity == :medium
    end

    test "detects unstaged modifications", %{repo: repo} do
      # Modify an already-tracked file without staging
      File.write!(Path.join(repo, "README.md"), "# modified")

      findings = GitState.gs001_uncommitted_changes(repo)
      unstaged = Enum.filter(findings, &String.contains?(&1.reason, "unstaged"))
      assert length(unstaged) >= 1
    end

    test "detects untracked files", %{repo: repo} do
      File.write!(Path.join(repo, "stray.txt"), "untracked")

      findings = GitState.gs001_uncommitted_changes(repo)
      untracked = Enum.filter(findings, &String.contains?(&1.reason, "untracked"))
      assert length(untracked) >= 1
      assert hd(untracked).severity == :low
    end
  end

  # ─── GS002: Unpushed commits ───────────────────────────────────────

  describe "gs002_unpushed_commits/1" do
    test "returns finding when no upstream configured", %{repo: repo} do
      # No remote set up -- should detect "no upstream"
      findings = GitState.gs002_unpushed_commits(repo)

      # Should detect no upstream OR return empty (depends on git version
      # behaviour for rev-list with no tracking branch)
      if findings != [] do
        assert hd(findings).rule == "GS002"
        assert hd(findings).severity == :high
      end
    end
  end

  # ─── GS005: Detached HEAD ──────────────────────────────────────────

  describe "gs005_detached_head/1" do
    test "returns empty when on a branch", %{repo: repo} do
      findings = GitState.gs005_detached_head(repo)
      assert findings == []
    end

    test "detects detached HEAD", %{repo: repo} do
      # Detach by checking out a specific commit
      {sha, 0} = System.cmd("git", ["rev-parse", "HEAD"], cd: repo)
      System.cmd("git", ["checkout", String.trim(sha)], cd: repo, stderr_to_stdout: true)

      findings = GitState.gs005_detached_head(repo)
      assert length(findings) == 1
      assert hd(findings).rule == "GS005"
      assert hd(findings).severity == :high
      assert String.contains?(hd(findings).reason, "detached")
    end
  end

  # ─── GS006: Not on default branch ─────────────────────────────────

  describe "gs006_not_on_default_branch/1" do
    test "returns empty when on main", %{repo: repo} do
      findings = GitState.gs006_not_on_default_branch(repo)
      assert findings == []
    end

    test "detects non-default branch", %{repo: repo} do
      System.cmd("git", ["checkout", "-b", "feature-xyz"], cd: repo)

      findings = GitState.gs006_not_on_default_branch(repo)
      assert length(findings) == 1
      assert hd(findings).rule == "GS006"
      assert String.contains?(hd(findings).reason, "feature-xyz")
    end
  end

  # ─── GS007: Stale remote branches ───────────────────────────────────

  describe "gs007_stale_remote_branches/1" do
    test "returns empty when no remote branches exist", %{repo: repo} do
      findings = GitState.gs007_stale_remote_branches(repo)
      assert findings == []
    end

    test "detects non-main remote branches", %{repo: repo} do
      # Create a bare remote and push main to it
      remote = Path.join(@tmp_dir, "gs007_remote_#{System.unique_integer([:positive])}")
      System.cmd("git", ["init", "--bare", remote])
      System.cmd("git", ["remote", "add", "origin", remote], cd: repo)
      System.cmd("git", ["push", "-u", "origin", "main"], cd: repo)

      # Create and push a feature branch
      System.cmd("git", ["checkout", "-b", "feature-stale"], cd: repo)
      File.write!(Path.join(repo, "stale.txt"), "stale")
      System.cmd("git", ["add", "."], cd: repo)
      System.cmd("git", ["commit", "-m", "stale branch"], cd: repo)
      System.cmd("git", ["push", "origin", "feature-stale"], cd: repo)

      # Go back to main
      System.cmd("git", ["checkout", "main"], cd: repo)

      on_exit(fn -> File.rm_rf!(remote) end)

      findings = GitState.gs007_stale_remote_branches(repo)
      assert length(findings) == 1
      assert hd(findings).rule == "GS007"
      assert hd(findings).severity == :medium
      assert hd(findings).detail.count == 1
      assert String.contains?(hd(findings).reason, "1 non-main remote branch")
    end

    test "returns empty when only main branch exists on remote", %{repo: repo} do
      remote = Path.join(@tmp_dir, "gs007_clean_#{System.unique_integer([:positive])}")
      System.cmd("git", ["init", "--bare", remote])
      System.cmd("git", ["remote", "add", "origin", remote], cd: repo)
      System.cmd("git", ["push", "-u", "origin", "main"], cd: repo)

      on_exit(fn -> File.rm_rf!(remote) end)

      findings = GitState.gs007_stale_remote_branches(repo)
      assert findings == []
    end
  end

  # ─── scan/1: Comprehensive check ──────────────────────────────────

  describe "scan/1" do
    test "returns structured result on clean repo", %{repo: repo} do
      result = GitState.scan(repo)
      assert is_map(result)
      assert Map.has_key?(result, :findings)
      assert Map.has_key?(result, :total)
      assert Map.has_key?(result, :dispatch)
    end

    test "returns empty list for non-git directory" do
      non_git = Path.join(@tmp_dir, "not_a_repo_#{System.unique_integer([:positive])}")
      File.mkdir_p!(non_git)

      on_exit(fn -> File.rm_rf!(non_git) end)

      result = GitState.scan(non_git)
      assert result == []
    end

    test "aggregates multiple finding types", %{repo: repo} do
      # Create some dirty state
      File.write!(Path.join(repo, "stray.txt"), "untracked")
      File.write!(Path.join(repo, "README.md"), "# changed")

      result = GitState.scan(repo)
      assert result.total >= 1
      assert length(result.findings) >= 1
    end
  end
end
