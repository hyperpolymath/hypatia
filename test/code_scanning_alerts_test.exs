# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Tests for Code Scanning alert querying rules (CSA001-CSA004).
# Exercise logic without hitting the GitHub API.

defmodule Hypatia.Rules.CodeScanningAlertsTest do
  use ExUnit.Case, async: false

  alias Hypatia.Rules.CodeScanningAlerts

  setup do
    old_token = System.get_env("GITHUB_TOKEN")
    System.delete_env("GITHUB_TOKEN")

    on_exit(fn ->
      if old_token, do: System.put_env("GITHUB_TOKEN", old_token)
    end)

    :ok
  end

  describe "csa001_open_alerts/2" do
    test "returns empty list when GITHUB_TOKEN is not set" do
      assert CodeScanningAlerts.csa001_open_alerts("hyperpolymath", "test-nonexistent") == []
    end
  end

  describe "csa002_severity_summary/2" do
    test "returns empty list when GITHUB_TOKEN is not set" do
      assert CodeScanningAlerts.csa002_severity_summary("hyperpolymath", "test-nonexistent") == []
    end
  end

  describe "csa003_stale_alerts/2" do
    test "returns empty list when GITHUB_TOKEN is not set" do
      assert CodeScanningAlerts.csa003_stale_alerts("hyperpolymath", "test-nonexistent") == []
    end
  end

  describe "csa004_dismissed_without_fix/2" do
    test "returns empty list when GITHUB_TOKEN is not set" do
      assert CodeScanningAlerts.csa004_dismissed_without_fix("hyperpolymath", "test-nonexistent") ==
               []
    end
  end

  describe "scan/2" do
    test "returns error tuple when GITHUB_TOKEN is not set" do
      assert {:error, msg} = CodeScanningAlerts.scan("hyperpolymath", "test-nonexistent")
      assert msg =~ "GITHUB_TOKEN not set"
    end
  end

  describe "scan_from_path/1" do
    test "returns error when remote is not a github URL" do
      tmp = Path.join(System.tmp_dir!(), "csa-test-#{System.unique_integer([:positive])}")
      File.mkdir_p!(tmp)
      System.cmd("git", ["init", "-q"], cd: tmp)
      System.cmd("git", ["remote", "add", "origin", "http://gitea.example.com/foo/bar.git"], cd: tmp)

      assert {:error, msg} = CodeScanningAlerts.scan_from_path(tmp)
      assert msg =~ "Remote URL is not a GitHub URL"

      File.rm_rf!(tmp)
    end
  end
end
