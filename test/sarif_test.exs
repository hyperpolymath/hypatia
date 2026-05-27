# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.SARIFTest do
  use ExUnit.Case, async: true

  alias Hypatia.SARIF

  describe "from_findings/2 — shape" do
    test "empty findings produce a valid SARIF document with an empty results array" do
      doc = SARIF.from_findings([], "/tmp")

      assert doc["$schema"] =~ "sarif-2.1.0"
      assert doc["version"] == "2.1.0"
      [run] = doc["runs"]
      assert run["tool"]["driver"]["name"] == "Hypatia"
      assert run["results"] == []
      assert run["tool"]["driver"]["rules"] == []
    end

    test "one finding produces one result + one rule" do
      doc =
        SARIF.from_findings(
          [
            %{
              severity: "critical",
              rule_module: "code_safety",
              type: "elixir_system_shell",
              file: "lib/foo.ex",
              reason: "System.shell with interpolation -- shell injection risk"
            }
          ],
          "/tmp/repo"
        )

      [run] = doc["runs"]
      [result] = run["results"]
      [rule] = run["tool"]["driver"]["rules"]

      assert result["ruleId"] == "hypatia/code_safety/elixir_system_shell"
      assert result["level"] == "error"
      assert result["message"]["text"] =~ "shell injection"
      assert result["locations"] |> List.first() |> get_in(["physicalLocation", "artifactLocation", "uri"]) == "lib/foo.ex"
      assert is_binary(result["partialFingerprints"]["hypatiaFindingHash/v1"])

      assert rule["id"] == "hypatia/code_safety/elixir_system_shell"
      assert rule["defaultConfiguration"]["level"] == "error"
    end

    test "dedups rules across multiple results with the same ruleId" do
      findings =
        for n <- 1..5 do
          %{
            severity: "high",
            rule_module: "code_safety",
            type: "unwrap_without_check",
            file: "lib/foo_#{n}.rs",
            reason: "Rust unwrap"
          }
        end

      doc = SARIF.from_findings(findings, "/tmp")
      [run] = doc["runs"]

      assert length(run["results"]) == 5
      assert length(run["tool"]["driver"]["rules"]) == 1
    end

    test "severity → level mapping matches the JS converter" do
      cases = [
        {"critical", "error"},
        {"high", "error"},
        {"medium", "warning"},
        {"low", "note"},
        {"info", "note"},
        {"", "note"}
      ]

      Enum.each(cases, fn {severity, expected_level} ->
        finding = %{
          severity: severity,
          rule_module: "x",
          type: "y",
          file: "a.ex",
          reason: "r"
        }

        [result] = SARIF.from_findings([finding], "/tmp") |> get_in(["runs", Access.at(0), "results"])
        assert result["level"] == expected_level, "severity #{inspect(severity)} → #{expected_level}"
      end)
    end
  end

  describe "URI relativisation" do
    test "absolute paths under the repo root become relative" do
      doc =
        SARIF.from_findings(
          [
            %{severity: "high", rule_module: "x", type: "y", file: "/home/user/hypatia/lib/foo.ex", reason: "r"}
          ],
          "/home/user/hypatia"
        )

      uri = doc |> get_in(["runs", Access.at(0), "results", Access.at(0), "locations", Access.at(0), "physicalLocation", "artifactLocation", "uri"])
      assert uri == "lib/foo.ex"
    end

    test "absolute paths OUTSIDE the repo root degrade to basename" do
      doc =
        SARIF.from_findings(
          [%{severity: "high", rule_module: "x", type: "y", file: "/etc/passwd", reason: "r"}],
          "/home/user/hypatia"
        )

      uri = doc |> get_in(["runs", Access.at(0), "results", Access.at(0), "locations", Access.at(0), "physicalLocation", "artifactLocation", "uri"])
      assert uri == "passwd"
    end

    test "empty file becomes '.' (repo-level finding)" do
      doc =
        SARIF.from_findings(
          [%{severity: "high", rule_module: "x", type: "y", file: "", reason: "r"}],
          "/tmp"
        )

      uri = doc |> get_in(["runs", Access.at(0), "results", Access.at(0), "locations", Access.at(0), "physicalLocation", "artifactLocation", "uri"])
      assert uri == "."
    end
  end

  describe "render/2" do
    test "produces parseable JSON" do
      json =
        SARIF.render(
          [%{severity: "high", rule_module: "x", type: "y", file: "a.ex", reason: "r"}],
          "/tmp"
        )

      assert {:ok, decoded} = Jason.decode(json)
      assert decoded["version"] == "2.1.0"
    end
  end

  describe "meta-rule suppression" do
    test "code_scanning_alerts findings are excluded from SARIF output" do
      findings = [
        %{
          severity: "high",
          rule_module: "code_scanning_alerts",
          type: "CSA001",
          file: ".github/workflows/governance.yml",
          reason: "Code scanning (Hypatia): hypatia/workflow_audit/missing_workflow ..."
        },
        %{
          severity: "high",
          rule_module: "workflow_audit",
          type: "missing_workflow",
          file: ".github/workflows/scorecard.yml",
          reason: "scorecard workflow missing"
        }
      ]

      [run] = SARIF.from_findings(findings, "/tmp") |> Map.fetch!("runs")
      results = run["results"]
      rules = run["tool"]["driver"]["rules"]

      assert length(results) == 1
      assert hd(results)["ruleId"] == "hypatia/workflow_audit/missing_workflow"

      refute Enum.any?(rules, fn r ->
               String.starts_with?(r["id"], "hypatia/code_scanning_alerts/")
             end)
    end

    test "string-keyed rule_module is also recognised as meta" do
      findings = [
        %{
          "severity" => "medium",
          "rule_module" => "code_scanning_alerts",
          "type" => "CSA003",
          "file" => "x.yml",
          "reason" => "stale"
        }
      ]

      [run] = SARIF.from_findings(findings, "/tmp") |> Map.fetch!("runs")
      assert run["results"] == []
    end

    test "atom rule_module is also recognised as meta" do
      findings = [
        %{
          severity: "medium",
          rule_module: :code_scanning_alerts,
          type: "CSA002",
          file: "repo",
          reason: "summary"
        }
      ]

      [run] = SARIF.from_findings(findings, "/tmp") |> Map.fetch!("runs")
      assert run["results"] == []
    end
  end

  describe "fingerprints" do
    test "same {ruleId, uri, type, reason} produces same fingerprint" do
      f = %{severity: "high", rule_module: "x", type: "y", file: "a.ex", reason: "r"}

      [r1] = SARIF.from_findings([f], "/tmp") |> get_in(["runs", Access.at(0), "results"])
      [r2] = SARIF.from_findings([f], "/tmp") |> get_in(["runs", Access.at(0), "results"])

      assert r1["partialFingerprints"]["hypatiaFindingHash/v1"] ==
               r2["partialFingerprints"]["hypatiaFindingHash/v1"]
    end

    test "differing reason produces different fingerprint" do
      f1 = %{severity: "high", rule_module: "x", type: "y", file: "a.ex", reason: "r1"}
      f2 = %{severity: "high", rule_module: "x", type: "y", file: "a.ex", reason: "r2"}

      [r1] = SARIF.from_findings([f1], "/tmp") |> get_in(["runs", Access.at(0), "results"])
      [r2] = SARIF.from_findings([f2], "/tmp") |> get_in(["runs", Access.at(0), "results"])

      refute r1["partialFingerprints"]["hypatiaFindingHash/v1"] ==
               r2["partialFingerprints"]["hypatiaFindingHash/v1"]
    end
  end
end
