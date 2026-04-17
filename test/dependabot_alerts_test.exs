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

  # ─── DA005: classify_alert/1 ──────────────────────────────────────

  describe "classify_alert/1" do
    test "dismissed alert lands in :control tier" do
      alert = %{
        "state" => "dismissed",
        "dismissed_reason" => "tolerable_risk",
        "security_advisory" => %{"severity" => "high"},
        "security_vulnerability" => %{
          "package" => %{"name" => "foo", "ecosystem" => "npm"},
          "first_patched_version" => %{"identifier" => "1.2.5"}
        }
      }

      assert {:control, reason} = DependabotAlerts.classify_alert(alert)
      assert reason =~ "dismissed"
    end

    test "no patched version available lands in :control tier" do
      alert = %{
        "state" => "open",
        "security_advisory" => %{"severity" => "critical"},
        "security_vulnerability" => %{
          "package" => %{"name" => "foo", "ecosystem" => "rust"},
          "vulnerable_version_range" => "< 2.0.0",
          "first_patched_version" => nil
        }
      }

      assert {:control, reason} = DependabotAlerts.classify_alert(alert)
      assert reason =~ "no upstream patch"
    end

    test "major version bump lands in :substitute tier" do
      alert = %{
        "state" => "open",
        "security_advisory" => %{"severity" => "high"},
        "security_vulnerability" => %{
          "package" => %{"name" => "foo", "ecosystem" => "npm"},
          "vulnerable_version_range" => "< 2.0.0",
          "first_patched_version" => %{"identifier" => "3.0.0"}
        }
      }

      assert {:substitute, reason} = DependabotAlerts.classify_alert(alert)
      assert reason =~ "major version"
    end

    test "patch bump on strict-semver ecosystem lands in :eliminate with high confidence" do
      alert = %{
        "state" => "open",
        "security_advisory" => %{"severity" => "high"},
        "security_vulnerability" => %{
          "package" => %{"name" => "serde", "ecosystem" => "rust"},
          "vulnerable_version_range" => "< 1.2.5",
          "first_patched_version" => %{"identifier" => "1.2.5"}
        }
      }

      assert {:eliminate, confidence} = DependabotAlerts.classify_alert(alert)
      assert confidence >= 0.95
    end

    test "critical severity + patch bump on rust hits auto-execute threshold" do
      alert = %{
        "state" => "open",
        "security_advisory" => %{"severity" => "critical"},
        "security_vulnerability" => %{
          "package" => %{"name" => "openssl", "ecosystem" => "rust"},
          "vulnerable_version_range" => "< 0.10.55",
          "first_patched_version" => %{"identifier" => "0.10.55"}
        }
      }

      assert {:eliminate, confidence} = DependabotAlerts.classify_alert(alert)
      assert confidence >= 0.95
    end
  end

  # ─── DA006: to_recipe/2 ───────────────────────────────────────────

  describe "to_recipe/2" do
    test "produces FleetDispatcher-compatible map with tier + confidence" do
      alert = %{
        "number" => 42,
        "state" => "open",
        "html_url" => "https://github.com/foo/bar/security/dependabot/42",
        "security_advisory" => %{"severity" => "high", "cve_id" => "CVE-2025-0001"},
        "security_vulnerability" => %{
          "package" => %{"name" => "serde", "ecosystem" => "rust"},
          "vulnerable_version_range" => "< 1.2.5",
          "first_patched_version" => %{"identifier" => "1.2.5"}
        },
        "dependency" => %{"manifest_path" => "Cargo.toml"}
      }

      recipe = DependabotAlerts.to_recipe(alert, repo: "hyperpolymath/test")

      assert recipe["type"] == "dependabot_fix"
      assert recipe["triangle_tier"] == "eliminate"
      assert recipe["confidence"] >= 0.95
      assert recipe["ecosystem"] == "rust"
      assert recipe["package"] == "serde"
      assert recipe["cve"] == "CVE-2025-0001"
      assert recipe["manifest_path"] == "Cargo.toml"
      assert recipe["alert_number"] == 42
      assert recipe["auto_fixable"] == true
      assert recipe["repo"] == "hyperpolymath/test"
      assert recipe["id"] =~ ~r/^da-[0-9a-f]{12}$/
    end

    test "control tier has requires_human == true and auto_fixable == false" do
      alert = %{
        "number" => 7,
        "state" => "open",
        "security_advisory" => %{"severity" => "medium"},
        "security_vulnerability" => %{
          "package" => %{"name" => "foo", "ecosystem" => "npm"},
          "vulnerable_version_range" => "< 1.0.0",
          "first_patched_version" => nil
        }
      }

      recipe = DependabotAlerts.to_recipe(alert, repo: "foo/bar")
      assert recipe["triangle_tier"] == "control"
      assert recipe["requires_human"] == true
      assert recipe["auto_fixable"] == false
    end
  end

  # ─── DA007: fixes_from_alerts/3 ───────────────────────────────────

  describe "fixes_from_alerts/3" do
    test "yields {:dependabot_fix, recipe, pattern} tuples and skips non-open alerts" do
      alerts = [
        %{
          "number" => 1,
          "state" => "open",
          "security_advisory" => %{"severity" => "high"},
          "security_vulnerability" => %{
            "package" => %{"name" => "a", "ecosystem" => "rust"},
            "vulnerable_version_range" => "< 1.2.5",
            "first_patched_version" => %{"identifier" => "1.2.5"}
          }
        },
        %{
          "number" => 2,
          "state" => "dismissed",
          "security_advisory" => %{"severity" => "high"}
        },
        %{
          "number" => 3,
          "state" => "fixed",
          "security_advisory" => %{"severity" => "critical"}
        }
      ]

      tuples = DependabotAlerts.fixes_from_alerts(alerts, "foo", "bar")
      assert length(tuples) == 1

      [{:dependabot_fix, recipe, pattern}] = tuples
      assert recipe["package"] == "a"
      assert pattern["routed_repo"] == "foo/bar"
      assert pattern["source"] == "dependabot"
      assert pattern["severity_score"] == 0.8
    end
  end

  # ─── summary/1 ────────────────────────────────────────────────────

  describe "summary/1" do
    test "formats a one-line log message" do
      recipe = %{
        "triangle_tier" => "eliminate",
        "package" => "serde",
        "ecosystem" => "rust",
        "confidence" => 0.97
      }

      line = DependabotAlerts.summary(recipe)
      assert line =~ "DependabotFix[eliminate]"
      assert line =~ "rust/serde"
      assert line =~ "conf=0.97"
    end
  end
end
