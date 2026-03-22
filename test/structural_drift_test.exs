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
end
