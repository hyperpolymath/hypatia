# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Tests for baseline-health detection rules (BH001-BH007).
# API-backed rules are tested via their no-token path (returns `[]`
# cleanly); the local manifest and workflow scans are exercised against
# an ephemeral repo in /tmp.

defmodule Hypatia.Rules.BaselineHealthTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.BaselineHealth

  @tmp_dir System.tmp_dir!()

  # ─── Test helpers ────────────────────────────────────────────────────

  defp create_repo do
    repo = Path.join(@tmp_dir, "bh_test_#{System.unique_integer([:positive])}")
    File.mkdir_p!(repo)
    System.cmd("git", ["init", "--initial-branch=main"], cd: repo)
    System.cmd("git", ["config", "user.email", "test@test.com"], cd: repo)
    System.cmd("git", ["config", "user.name", "Test"], cd: repo)
    repo
  end

  setup do
    repo = create_repo()
    on_exit(fn -> File.rm_rf!(repo) end)
    {:ok, repo: repo}
  end

  # ─── BH001: missing required_status_checks ──────────────────────────

  describe "bh001_missing_required_status_checks/2" do
    test "returns [] when GITHUB_TOKEN is not set" do
      old_token = System.get_env("GITHUB_TOKEN")
      old_pat = System.get_env("HYPATIA_DISPATCH_PAT")
      System.delete_env("GITHUB_TOKEN")
      System.delete_env("HYPATIA_DISPATCH_PAT")

      on_exit(fn ->
        if old_token, do: System.put_env("GITHUB_TOKEN", old_token)
        if old_pat, do: System.put_env("HYPATIA_DISPATCH_PAT", old_pat)
      end)

      assert BaselineHealth.bh001_missing_required_status_checks(
               "hyperpolymath",
               "test-nonexistent"
             ) == []
    end
  end

  # ─── BH002: migration TODO in manifest ──────────────────────────────

  describe "bh002_migration_todo_in_manifest/1" do
    test "returns [] on a repo with clean manifests", %{repo: repo} do
      File.write!(Path.join(repo, "Cargo.toml"), """
      [package]
      name = "clean"
      version = "0.1.0"

      [dependencies]
      serde = "1.0"
      """)

      assert BaselineHealth.bh002_migration_todo_in_manifest(repo) == []
    end

    test "detects ureq-style deferred migration TODO", %{repo: repo} do
      File.write!(Path.join(repo, "Cargo.toml"), """
      [dependencies]
      ureq = { version = "3", features = ["json"] }  # TODO: migrate to ureq 3.x API
      serde = "1.0"
      """)

      findings = BaselineHealth.bh002_migration_todo_in_manifest(repo)
      assert length(findings) == 1
      f = hd(findings)
      assert f.rule == "BH002"
      assert f.severity == :high
      assert f.file == "Cargo.toml"
      assert f.action == :open_followup_pr
      assert String.contains?(f.reason, "deferred-migration TODO")
      assert String.contains?(f.detail.text, "ureq")
      assert f.detail.line == 2
    end

    test "detects FIXME-style rename note", %{repo: repo} do
      File.write!(Path.join(repo, "package.json"), """
      {
        "dependencies": {
          "axios": "1.0.0"
        }
      }
      """)

      # FIXME-mentioning-rename gets picked up too
      File.write!(Path.join(repo, "Cargo.toml"), """
      [dependencies]
      reqwest = "0.12"  # FIXME: API rename in 0.12 — caller still uses 0.11 names
      """)

      findings = BaselineHealth.bh002_migration_todo_in_manifest(repo)
      cargo = Enum.find(findings, &(&1.file == "Cargo.toml"))
      assert cargo != nil
      assert cargo.severity == :high
    end

    test "ignores benign TODOs that aren't about migration", %{repo: repo} do
      File.write!(Path.join(repo, "Cargo.toml"), """
      [dependencies]
      tokio = "1.0"  # TODO: switch features once we add the websocket path
      """)

      assert BaselineHealth.bh002_migration_todo_in_manifest(repo) == []
    end

    test "walks subdirectories", %{repo: repo} do
      sub = Path.join(repo, "rgtv-cli")
      File.mkdir_p!(sub)

      File.write!(Path.join(sub, "Cargo.toml"), """
      [dependencies]
      ureq = { version = "3" }  # TODO: migrate to 3.x API
      """)

      findings = BaselineHealth.bh002_migration_todo_in_manifest(repo)
      assert length(findings) == 1
      assert hd(findings).file == "rgtv-cli/Cargo.toml"
    end

    test "skips vendored target/ and node_modules/", %{repo: repo} do
      vend = Path.join([repo, "target", "vendor", "evil"])
      File.mkdir_p!(vend)

      File.write!(Path.join(vend, "Cargo.toml"), """
      ureq = "3"  # TODO: migrate
      """)

      assert BaselineHealth.bh002_migration_todo_in_manifest(repo) == []
    end
  end

  # ─── BH003: persistent red baseline ─────────────────────────────────

  describe "bh003_persistent_red_baseline/3" do
    test "returns [] when GITHUB_TOKEN is not set" do
      old_token = System.get_env("GITHUB_TOKEN")
      old_pat = System.get_env("HYPATIA_DISPATCH_PAT")
      System.delete_env("GITHUB_TOKEN")
      System.delete_env("HYPATIA_DISPATCH_PAT")

      on_exit(fn ->
        if old_token, do: System.put_env("GITHUB_TOKEN", old_token)
        if old_pat, do: System.put_env("HYPATIA_DISPATCH_PAT", old_pat)
      end)

      assert BaselineHealth.bh003_persistent_red_baseline(
               "hyperpolymath",
               "test-nonexistent"
             ) == []
    end

    test "accepts a custom threshold_hours" do
      # Smoke: just that the function arity supports the opts list
      # without raising. With no token, we always get [].
      assert BaselineHealth.bh003_persistent_red_baseline(
               "hyperpolymath",
               "x",
               threshold_hours: 1
             ) == []
    end
  end

  # ─── BH004: dead action SHA pin ─────────────────────────────────────

  describe "bh004_dead_action_sha_pin/1" do
    test "returns [] when no workflow files exist", %{repo: repo} do
      # No .github/workflows/ directory at all.
      assert BaselineHealth.bh004_dead_action_sha_pin(repo) == []
    end

    test "returns [] when no token is set even if SHAs are present", %{repo: repo} do
      old_token = System.get_env("GITHUB_TOKEN")
      old_pat = System.get_env("HYPATIA_DISPATCH_PAT")
      System.delete_env("GITHUB_TOKEN")
      System.delete_env("HYPATIA_DISPATCH_PAT")

      on_exit(fn ->
        if old_token, do: System.put_env("GITHUB_TOKEN", old_token)
        if old_pat, do: System.put_env("HYPATIA_DISPATCH_PAT", old_pat)
      end)

      wf_dir = Path.join([repo, ".github", "workflows"])
      File.mkdir_p!(wf_dir)

      File.write!(Path.join(wf_dir, "test.yml"), """
      name: Test
      on: [push]
      jobs:
        x:
          runs-on: ubuntu-latest
          steps:
            - uses: actions/checkout@ea165f8d65b6e75b540449e92b4886f43607fa02
            - uses: actions/upload-artifact@65c79d7f54e76e4e3c7a8f34db0f4ac8b515c478
      """)

      # No token = we can't verify, so we silently skip rather than
      # emit false positives. The function should not raise.
      assert BaselineHealth.bh004_dead_action_sha_pin(repo) == []
    end

    test "ignores nested .github/workflows in monorepo subdirectories", %{repo: repo} do
      nested_wf_dir = Path.join([repo, "cartridge-a", ".github", "workflows"])
      File.mkdir_p!(nested_wf_dir)

      # Even with the broken SHA, a nested scaffold is not active CI.
      File.write!(Path.join(nested_wf_dir, "scaffold.yml"), """
      name: Scaffold
      on: [push]
      jobs:
        x:
          steps:
            - uses: actions/upload-artifact@65c79d7f54e76e4e3c7a8f34db0f4ac8b515c478
      """)

      assert BaselineHealth.bh004_dead_action_sha_pin(repo) == []
    end
  end

  # ─── BH005: push-only required check ────────────────────────────────

  describe "bh005_push_only_required_check/2" do
    test "returns [] when GITHUB_TOKEN is not set" do
      old_token = System.get_env("GITHUB_TOKEN")
      old_pat = System.get_env("HYPATIA_DISPATCH_PAT")
      System.delete_env("GITHUB_TOKEN")
      System.delete_env("HYPATIA_DISPATCH_PAT")

      on_exit(fn ->
        if old_token, do: System.put_env("GITHUB_TOKEN", old_token)
        if old_pat, do: System.put_env("HYPATIA_DISPATCH_PAT", old_pat)
      end)

      assert BaselineHealth.bh005_push_only_required_check(
               "hyperpolymath",
               "test-nonexistent"
             ) == []
    end
  end

  # ─── BH006: required-check drift ────────────────────────────────────

  describe "bh006_required_check_drift/2" do
    test "returns [] when GITHUB_TOKEN is not set" do
      old_token = System.get_env("GITHUB_TOKEN")
      old_pat = System.get_env("HYPATIA_DISPATCH_PAT")
      System.delete_env("GITHUB_TOKEN")
      System.delete_env("HYPATIA_DISPATCH_PAT")

      on_exit(fn ->
        if old_token, do: System.put_env("GITHUB_TOKEN", old_token)
        if old_pat, do: System.put_env("HYPATIA_DISPATCH_PAT", old_pat)
      end)

      assert BaselineHealth.bh006_required_check_drift(
               "hyperpolymath",
               "test-nonexistent"
             ) == []
    end
  end

  # ─── BH007: signing-key UID gap ─────────────────────────────────────

  describe "bh007_signing_key_uid_gap/2" do
    test "returns [] when GITHUB_TOKEN is not set" do
      old_token = System.get_env("GITHUB_TOKEN")
      old_pat = System.get_env("HYPATIA_DISPATCH_PAT")
      System.delete_env("GITHUB_TOKEN")
      System.delete_env("HYPATIA_DISPATCH_PAT")

      on_exit(fn ->
        if old_token, do: System.put_env("GITHUB_TOKEN", old_token)
        if old_pat, do: System.put_env("HYPATIA_DISPATCH_PAT", old_pat)
      end)

      assert BaselineHealth.bh007_signing_key_uid_gap(
               "hyperpolymath",
               "test-nonexistent"
             ) == []
    end
  end

  # ─── scan/2 facade ──────────────────────────────────────────────────

  describe "scan/2" do
    test "returns the expected shape on a clean repo", %{repo: repo} do
      File.write!(Path.join(repo, "Cargo.toml"), """
      [package]
      name = "x"

      [dependencies]
      serde = "1.0"
      """)

      result = BaselineHealth.scan(repo)
      assert is_map(result)
      assert Map.has_key?(result, :findings)
      assert Map.has_key?(result, :total)
      assert Map.has_key?(result, :by_severity)
      assert Map.has_key?(result, :dispatch)
      assert result.total == 0
    end

    test "surfaces BH002 findings via the facade", %{repo: repo} do
      File.write!(Path.join(repo, "Cargo.toml"), """
      [dependencies]
      ureq = "3"  # TODO: migrate to 3.x API
      """)

      result = BaselineHealth.scan(repo)
      assert result.total == 1
      assert hd(result.findings).rule == "BH002"
      [dispatch] = result.dispatch
      assert dispatch.bot == :rhodibot
      assert dispatch.confidence == 0.88
    end

    test "owner_repo opt overrides remote parsing", %{repo: repo} do
      # No real remote on the ephemeral repo, but the override path
      # still runs without raising.
      result = BaselineHealth.scan(repo, owner_repo: {"hyperpolymath", "test-nonexistent"})
      # Token isn't set in CI usually -> BH001/BH003 return [] -> only BH002 path runs
      assert is_list(result.findings)
    end
  end
end
