# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Tests for Dependabot alert querying rules (DA001-DA004).
# These tests exercise the logic without hitting the GitHub API by
# testing the helper functions and scanning behaviour when the token
# is absent.

defmodule Hypatia.Rules.DependabotAlertsTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.DependabotAlerts

  @tmp_dir System.tmp_dir!()

  # ─── DA001: Open alerts logic ──────────────────────────────────────

  describe "da001_open_alerts/2" do
    test "returns empty list when GITHUB_TOKEN is not set" do
      # Temporarily ensure GITHUB_TOKEN is cleared
      old_token = System.get_env("GITHUB_TOKEN")
      System.delete_env("GITHUB_TOKEN")

      on_exit(fn ->
        if old_token, do: System.put_env("GITHUB_TOKEN", old_token)
      end)

      findings = DependabotAlerts.da001_open_alerts("hyperpolymath", "test-nonexistent")
      assert findings == []
    end
  end

  # ─── DA002: Severity summary logic ─────────────────────────────────

  describe "da002_severity_summary/2" do
    test "returns empty list when GITHUB_TOKEN is not set" do
      old_token = System.get_env("GITHUB_TOKEN")
      System.delete_env("GITHUB_TOKEN")

      on_exit(fn ->
        if old_token, do: System.put_env("GITHUB_TOKEN", old_token)
      end)

      findings = DependabotAlerts.da002_severity_summary("hyperpolymath", "test-nonexistent")
      assert findings == []
    end
  end

  # ─── scan/2: Comprehensive check ──────────────────────────────────

  describe "scan/2" do
    test "returns error when GITHUB_TOKEN is missing" do
      old_token = System.get_env("GITHUB_TOKEN")
      System.delete_env("GITHUB_TOKEN")

      on_exit(fn ->
        if old_token, do: System.put_env("GITHUB_TOKEN", old_token)
      end)

      result = DependabotAlerts.scan("hyperpolymath", "test")
      assert {:error, _reason} = result
    end
  end

  # ─── scan_from_path/1: Path-based scanning ────────────────────────

  describe "scan_from_path/1" do
    test "returns error for non-git directory" do
      non_git = Path.join(@tmp_dir, "da_test_#{System.unique_integer([:positive])}")
      File.mkdir_p!(non_git)

      on_exit(fn -> File.rm_rf!(non_git) end)

      result = DependabotAlerts.scan_from_path(non_git)
      assert {:error, _reason} = result
    end

    test "returns error for git repo with no GitHub remote" do
      repo = Path.join(@tmp_dir, "da_local_#{System.unique_integer([:positive])}")
      File.mkdir_p!(repo)
      System.cmd("git", ["init"], cd: repo)
      System.cmd("git", ["config", "user.email", "test@test.com"], cd: repo)
      System.cmd("git", ["config", "user.name", "Test"], cd: repo)
      File.write!(Path.join(repo, "README.md"), "# test")
      System.cmd("git", ["add", "."], cd: repo)
      System.cmd("git", ["commit", "-m", "init"], cd: repo)

      on_exit(fn -> File.rm_rf!(repo) end)

      result = DependabotAlerts.scan_from_path(repo)
      assert {:error, _reason} = result
    end

    test "extracts owner/repo from GitHub remote and attempts scan" do
      repo = Path.join(@tmp_dir, "da_github_#{System.unique_integer([:positive])}")
      File.mkdir_p!(repo)
      System.cmd("git", ["init"], cd: repo)
      System.cmd("git", ["config", "user.email", "test@test.com"], cd: repo)
      System.cmd("git", ["config", "user.name", "Test"], cd: repo)
      File.write!(Path.join(repo, "README.md"), "# test")
      System.cmd("git", ["add", "."], cd: repo)
      System.cmd("git", ["commit", "-m", "init"], cd: repo)
      System.cmd("git", ["remote", "add", "origin", "https://github.com/hyperpolymath/test-repo.git"], cd: repo)

      on_exit(fn -> File.rm_rf!(repo) end)

      # Without GITHUB_TOKEN it should return an error about the token
      old_token = System.get_env("GITHUB_TOKEN")
      System.delete_env("GITHUB_TOKEN")

      on_exit(fn ->
        if old_token, do: System.put_env("GITHUB_TOKEN", old_token)
      end)

      result = DependabotAlerts.scan_from_path(repo)
      assert {:error, msg} = result
      assert String.contains?(msg, "GITHUB_TOKEN")
    end
  end
end
