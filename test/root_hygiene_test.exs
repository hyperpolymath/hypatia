# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.Rules.RootHygieneTest do
  use ExUnit.Case, async: true
  alias Hypatia.Rules.RootHygiene

  describe "scan_banned/1" do
    test "flags Dockerfile" do
      findings = RootHygiene.scan_banned(["Dockerfile", "README.adoc"])
      assert length(findings) == 1
      assert hd(findings).file == "Dockerfile"
      assert hd(findings).action == :rename
    end

    test "flags SCM files in root" do
      findings = RootHygiene.scan_banned(["STATE.scm", "META.scm"])
      assert length(findings) == 2
      assert Enum.all?(findings, & &1.severity == :critical)
      assert Enum.all?(findings, & &1.action == :move)
    end

    test "flags AI.djot" do
      findings = RootHygiene.scan_banned(["AI.djot"])
      assert length(findings) == 1
      assert hd(findings).action == :delete
    end

    test "flags banned package managers" do
      findings = RootHygiene.scan_banned(["package-lock.json", "yarn.lock", "bun.lockb"])
      assert length(findings) == 3
    end

    test "ignores allowed files" do
      findings = RootHygiene.scan_banned(["README.adoc", "LICENSE", "Justfile"])
      assert findings == []
    end
  end

  describe "scan_stale/1" do
    test "flags plan documents in root" do
      findings = RootHygiene.scan_stale(["PLAN-migration-v2.md"])
      assert length(findings) == 1
      assert hd(findings).action == :move
    end

    test "flags status reports in root" do
      findings = RootHygiene.scan_stale(["PANLL-STATUS-REPORT-2026-03-14.md"])
      assert length(findings) == 1
    end

    test "flags backup files" do
      findings = RootHygiene.scan_stale(["config.bak", "old.tmp"])
      assert length(findings) == 2
    end

    test "ignores normal files" do
      findings = RootHygiene.scan_stale(["README.adoc", "LICENSE"])
      assert findings == []
    end
  end

  describe "scan_required_missing/1" do
    test "flags missing LICENSE" do
      findings = RootHygiene.scan_required_missing(["README.adoc"])
      license_finding = Enum.find(findings, & &1.file == "LICENSE")
      assert license_finding != nil
      assert license_finding.severity == :critical
    end

    test "accepts LICENSE.txt as alternative" do
      findings = RootHygiene.scan_required_missing(["LICENSE.txt", "SECURITY.md",
                                                     ".editorconfig", "0-AI-MANIFEST.a2ml"])
      assert findings == []
    end

    test "flags missing SECURITY.md" do
      findings = RootHygiene.scan_required_missing(["LICENSE"])
      security = Enum.find(findings, & &1.file == "SECURITY.md")
      assert security != nil
    end
  end

  describe "scan/1" do
    test "returns aggregate results" do
      result = RootHygiene.scan(["Dockerfile", "STATE.scm", "PLAN-old.md"])
      assert result.banned_count == 2
      assert result.stale_count == 1
      assert result.missing_count > 0
      assert result.total == result.banned_count + result.stale_count + result.missing_count
    end
  end

  describe "dispatch_recommendations/1" do
    test "maps findings to bots" do
      findings = [
        %{file: "Dockerfile", action: :rename, severity: :high, reason: "test"},
        %{file: "STATE.scm", action: :move, severity: :critical, reason: "test"}
      ]
      recs = RootHygiene.dispatch_recommendations(findings)
      assert length(recs) == 2
      assert Enum.find(recs, & &1.file == "Dockerfile").bot == :rhodibot
      assert Enum.find(recs, & &1.file == "STATE.scm").bot == :finishbot
    end
  end
end
