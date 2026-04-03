# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.StructuralDriftTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.StructuralDrift

  @tmp_dir System.tmp_dir!()

  setup do
    repo = Path.join(@tmp_dir, "drift_test_#{System.unique_integer([:positive])}")
    File.mkdir_p!(repo)

    on_exit(fn -> File.rm_rf!(repo) end)

    {:ok, repo: repo}
  end

  describe "sd001_legacy_scm/1" do
    test "detects STATE.scm in repo root", %{repo: repo} do
      File.write!(Path.join(repo, "STATE.scm"), "(state)")

      findings = StructuralDrift.sd001_legacy_scm(repo)
      assert length(findings) >= 1
      assert Enum.all?(findings, &(&1.rule == "SD001"))
      assert Enum.all?(findings, &(&1.severity == :critical))
    end

    test "detects META.scm in .machine_readable/", %{repo: repo} do
      dir = Path.join([repo, ".machine_readable"])
      File.mkdir_p!(dir)
      File.write!(Path.join(dir, "META.scm"), "(meta)")

      findings = StructuralDrift.sd001_legacy_scm(repo)
      assert length(findings) >= 1
    end

    test "returns empty when no .scm files exist", %{repo: repo} do
      findings = StructuralDrift.sd001_legacy_scm(repo)
      assert findings == []
    end

    test "detects multiple legacy SCM files", %{repo: repo} do
      File.write!(Path.join(repo, "STATE.scm"), "(state)")
      File.write!(Path.join(repo, "ECOSYSTEM.scm"), "(eco)")
      File.write!(Path.join(repo, "AGENTIC.scm"), "(agentic)")

      findings = StructuralDrift.sd001_legacy_scm(repo)
      assert length(findings) >= 3
    end

    test "flags trigger_intensive on all findings", %{repo: repo} do
      File.write!(Path.join(repo, "STATE.scm"), "(state)")

      findings = StructuralDrift.sd001_legacy_scm(repo)
      assert Enum.all?(findings, & &1.trigger_intensive)
    end
  end

  describe "sd002_trustfile_hs/1" do
    test "detects Trustfile.hs", %{repo: repo} do
      File.write!(Path.join(repo, "Trustfile.hs"), "module Trustfile where")

      findings = StructuralDrift.sd002_trustfile_hs(repo)
      assert length(findings) >= 1
      assert hd(findings).rule == "SD002"
      assert hd(findings).severity == :high
    end

    test "returns empty when no Trustfile.hs exists", %{repo: repo} do
      findings = StructuralDrift.sd002_trustfile_hs(repo)
      assert findings == []
    end
  end

  describe "sd003_ai_djot/1" do
    test "detects AI.djot", %{repo: repo} do
      File.write!(Path.join(repo, "AI.djot"), "# AI Manifest")

      findings = StructuralDrift.sd003_ai_djot(repo)
      assert length(findings) >= 1
      assert hd(findings).rule == "SD003"
    end

    test "returns empty when no AI.djot exists", %{repo: repo} do
      findings = StructuralDrift.sd003_ai_djot(repo)
      assert findings == []
    end
  end

  describe "sd010_tracked_node_modules/1" do
    test "returns empty when no node_modules directory exists", %{repo: repo} do
      # Initialise a git repo so the git commands work
      System.cmd("git", ["init"], cd: repo)
      System.cmd("git", ["config", "user.email", "test@test.com"], cd: repo)
      System.cmd("git", ["config", "user.name", "Test"], cd: repo)
      File.write!(Path.join(repo, "README.md"), "# test")
      System.cmd("git", ["add", "."], cd: repo)
      System.cmd("git", ["commit", "-m", "init"], cd: repo)

      findings = StructuralDrift.sd010_tracked_node_modules(repo)
      assert findings == []
    end

    test "detects tracked node_modules directory", %{repo: repo} do
      System.cmd("git", ["init"], cd: repo)
      System.cmd("git", ["config", "user.email", "test@test.com"], cd: repo)
      System.cmd("git", ["config", "user.name", "Test"], cd: repo)

      nm_dir = Path.join(repo, "node_modules")
      File.mkdir_p!(nm_dir)
      File.write!(Path.join(nm_dir, "package.json"), "{}")
      System.cmd("git", ["add", "."], cd: repo)
      System.cmd("git", ["commit", "-m", "add nm"], cd: repo)

      findings = StructuralDrift.sd010_tracked_node_modules(repo)
      assert length(findings) >= 1
      assert hd(findings).rule == "SD010"
      assert hd(findings).severity == :high
    end
  end

  describe "sd011_missing_gitignore/1" do
    test "returns empty when directory and gitignore entry both exist", %{repo: repo} do
      File.mkdir_p!(Path.join(repo, "target"))
      File.write!(Path.join(repo, ".gitignore"), "target/\n")

      findings = StructuralDrift.sd011_missing_gitignore(repo)
      target_findings = Enum.filter(findings, &String.contains?(&1.reason, "target"))
      assert target_findings == []
    end

    test "detects directory present but missing from gitignore", %{repo: repo} do
      File.mkdir_p!(Path.join(repo, "target"))
      File.write!(Path.join(repo, ".gitignore"), "# empty\n")

      findings = StructuralDrift.sd011_missing_gitignore(repo)
      target_findings = Enum.filter(findings, &String.contains?(&1.reason, "target"))
      assert length(target_findings) == 1
      assert hd(target_findings).rule == "SD011"
      assert hd(target_findings).severity == :medium
    end

    test "returns empty when directory does not exist", %{repo: repo} do
      File.write!(Path.join(repo, ".gitignore"), "# empty\n")

      findings = StructuralDrift.sd011_missing_gitignore(repo)
      assert findings == []
    end
  end

  describe "sd013_path_specific_gitignore/1" do
    test "detects path-specific .zig-cache pattern", %{repo: repo} do
      File.write!(Path.join(repo, ".gitignore"), "src/ffi/.zig-cache/\n")

      findings = StructuralDrift.sd013_path_specific_gitignore(repo)
      assert length(findings) == 1
      assert hd(findings).rule == "SD013"
      assert hd(findings).severity == :low
      assert hd(findings).detail.recommended == ".zig-cache/"
      assert String.contains?(hd(findings).reason, "src/ffi/.zig-cache/")
    end

    test "detects path-specific node_modules pattern", %{repo: repo} do
      File.write!(Path.join(repo, ".gitignore"), "packages/web/node_modules/\n")

      findings = StructuralDrift.sd013_path_specific_gitignore(repo)
      assert length(findings) == 1
      assert hd(findings).detail.recommended == "node_modules/"
    end

    test "detects multiple path-specific patterns", %{repo: repo} do
      content = """
      src/.zig-cache/
      lib/app/_build/
      packages/web/node_modules/
      """
      File.write!(Path.join(repo, ".gitignore"), content)

      findings = StructuralDrift.sd013_path_specific_gitignore(repo)
      assert length(findings) == 3
    end

    test "ignores global patterns (no path prefix)", %{repo: repo} do
      content = """
      .zig-cache/
      zig-out/
      node_modules/
      _build/
      target/
      """
      File.write!(Path.join(repo, ".gitignore"), content)

      findings = StructuralDrift.sd013_path_specific_gitignore(repo)
      assert findings == []
    end

    test "ignores comments", %{repo: repo} do
      File.write!(Path.join(repo, ".gitignore"), "# src/.zig-cache/\n")

      findings = StructuralDrift.sd013_path_specific_gitignore(repo)
      assert findings == []
    end

    test "returns empty when no .gitignore exists", %{repo: repo} do
      findings = StructuralDrift.sd013_path_specific_gitignore(repo)
      assert findings == []
    end
  end

  describe "scan/1" do
    test "returns structured result with all expected keys", %{repo: repo} do
      System.cmd("git", ["init"], cd: repo)
      System.cmd("git", ["config", "user.email", "test@test.com"], cd: repo)
      System.cmd("git", ["config", "user.name", "Test"], cd: repo)
      File.write!(Path.join(repo, "README.md"), "# test")
      System.cmd("git", ["add", "."], cd: repo)
      System.cmd("git", ["commit", "-m", "init"], cd: repo)

      result = StructuralDrift.scan(repo)
      assert Map.has_key?(result, :findings)
      assert Map.has_key?(result, :total)
      assert Map.has_key?(result, :trigger_intensive)
      assert Map.has_key?(result, :dispatch)
    end
  end
end
