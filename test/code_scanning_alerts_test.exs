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

  describe "self_referential_alert?/1" do
    # Regression for the boj-server #149 self-amplifying loop where each
    # Hypatia scan generated new CSA001 alerts about the previous scan's
    # CSA001 alerts (30+ alerts on cartridges/*/ffi/cartridge_shim.zig
    # before the fix).

    test "rejects Hypatia's own CSA001 echo" do
      alert = %{
        "tool" => %{"name" => "Hypatia"},
        "rule" => %{"id" => "hypatia/code_scanning_alerts/CSA001"}
      }

      assert CodeScanningAlerts.self_referential_alert?(alert)
    end

    test "rejects CSA002 / CSA003 / CSA004 echoes too" do
      for rule_id <- ~w(
            hypatia/code_scanning_alerts/CSA002
            hypatia/code_scanning_alerts/CSA003
            hypatia/code_scanning_alerts/CSA004
          ) do
        alert = %{
          "tool" => %{"name" => "Hypatia"},
          "rule" => %{"id" => rule_id}
        }

        assert CodeScanningAlerts.self_referential_alert?(alert),
               "expected #{rule_id} to be flagged self-referential"
      end
    end

    test "keeps CodeQL alerts" do
      alert = %{
        "tool" => %{"name" => "CodeQL"},
        "rule" => %{"id" => "rs/unsafe-cast"}
      }

      refute CodeScanningAlerts.self_referential_alert?(alert)
    end

    test "keeps third-party SARIF alerts" do
      alert = %{
        "tool" => %{"name" => "Snyk"},
        "rule" => %{"id" => "SNYK-RS-12345"}
      }

      refute CodeScanningAlerts.self_referential_alert?(alert)
    end

    test "keeps Hypatia alerts from other rule modules" do
      # Hypatia has many rule modules (code_safety, supply_chain,
      # workflow_hardening, etc.); only the code_scanning_alerts module is
      # a lens over GitHub's API and recursive. Other modules find real
      # things in the code and SHOULD continue to surface even when their
      # findings get re-uploaded as SARIF.
      alert = %{
        "tool" => %{"name" => "Hypatia"},
        "rule" => %{"id" => "hypatia/code_safety/CSA001"}
      }

      refute CodeScanningAlerts.self_referential_alert?(alert)
    end

    test "handles missing tool / rule keys gracefully" do
      refute CodeScanningAlerts.self_referential_alert?(%{})
      refute CodeScanningAlerts.self_referential_alert?(%{"tool" => %{"name" => "Hypatia"}})
      refute CodeScanningAlerts.self_referential_alert?(%{"rule" => %{"id" => "x"}})
    end
  end
end
