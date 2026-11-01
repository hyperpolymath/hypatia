# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.AuditPreventionTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.AuditPrevention

  describe "rules/0 — the transcription ledger as data" do
    test "carries exactly AP001–AP015, in order, with origins" do
      rules = AuditPrevention.rules()
      ids = Enum.map(rules, & &1.id)

      assert ids == Enum.map(1..15, fn n -> "AP" <> String.pad_leading(Integer.to_string(n), 3, "0") end)
      assert Enum.all?(rules, fn r -> String.starts_with?(r.origin, ".audittraining/security-errors/") end)
      assert Enum.all?(rules, fn r -> r.description != "" and r.conditions != [] and r.canonical != "" end)
    end

    test "rule/1 and canonical_equivalent/1 resolve by id and by name" do
      assert AuditPrevention.rule("AP001").rule == "parser_input_limits"
      assert AuditPrevention.rule("workflow_unpinned_action").id == "AP015"
      assert AuditPrevention.rule("nope") == nil
      assert AuditPrevention.canonical_equivalent("AP014") =~ "WorkflowAudit.check_permissions"
      assert AuditPrevention.canonical_equivalent("nope") == nil
    end
  end

  describe "check_parser_input_limits/1 (AP001)" do
    test "flags a parser crate with no stated input limit" do
      contents = %{
        "Cargo.toml" => "[package]\nname = \"x\"\ndescription = \"A config file parser\"\n",
        "src/lib.rs" => "pub fn parse(input: &str) {}\n"
      }

      assert [finding] = AuditPrevention.check_parser_input_limits(contents)
      assert finding.rule == "AP001"
      assert finding.action == "warn_reviewer"
    end

    test "does not flag a parser crate that states a limit" do
      contents = %{
        "Cargo.toml" => "[package]\nname = \"x\"\ndescription = \"parser\"\n",
        "src/lib.rs" => "const MAX_CONFIG_SIZE: usize = 1024;\n"
      }

      assert AuditPrevention.check_parser_input_limits(contents) == []
    end

    test "does not flag a non-parser crate" do
      contents = %{"Cargo.toml" => "[package]\nname = \"x\"\ndescription = \"A web server\"\n"}
      assert AuditPrevention.check_parser_input_limits(contents) == []
    end
  end

  describe "check_wasm_memory_limits/1 (AP002)" do
    test "flags a wasm-featured crate with no memory limit" do
      contents = %{
        "Cargo.toml" => "[package]\nname = \"x\"\n\n[features]\nwasm = []\n",
        "src/lib.rs" => "pub fn run() {}\n"
      }

      assert [finding] = AuditPrevention.check_wasm_memory_limits(contents)
      assert finding.rule == "AP002"
    end

    test "does not flag a wasm crate that states a memory limit" do
      contents = %{
        "Cargo.toml" => "[dependencies]\nwasm-bindgen = \"0.2\"\n",
        "src/lib.rs" => "// memory limit: 256 pages\n"
      }

      assert AuditPrevention.check_wasm_memory_limits(contents) == []
    end
  end

  describe "check_missing_cargo_audit/2 (AP003)" do
    test "flags a Rust repo whose workflows never run cargo audit" do
      files = ["Cargo.toml", ".github/workflows/ci.yml"]
      contents = %{".github/workflows/ci.yml" => "run: cargo test\n"}

      assert [finding] = AuditPrevention.check_missing_cargo_audit(files, contents)
      assert finding.rule == "AP003"
      assert finding.severity == :high
    end

    test "does not flag when a workflow runs cargo audit" do
      files = ["Cargo.toml", ".github/workflows/audit.yml"]
      contents = %{".github/workflows/audit.yml" => "run: cargo audit\n"}

      assert AuditPrevention.check_missing_cargo_audit(files, contents) == []
    end

    test "a cargo audit mention outside workflows is not CI evidence" do
      files = ["Cargo.toml", "README.adoc"]
      contents = %{"README.adoc" => "run cargo audit locally\n"}

      assert [%{rule: "AP003"}] = AuditPrevention.check_missing_cargo_audit(files, contents)
    end

    test "does not flag non-Rust repos" do
      assert AuditPrevention.check_missing_cargo_audit(["mix.exs"], %{}) == []
    end
  end

  describe "check_workspace_vuln/2 (AP004)" do
    test "flags a workspace with a vulnerable member" do
      assert [%{rule: "AP004"}] =
               AuditPrevention.check_workspace_vuln("[workspace]\nmembers = []\n", true)
    end

    test "needs both halves of the conjunction" do
      assert AuditPrevention.check_workspace_vuln("[workspace]\n", false) == []
      assert AuditPrevention.check_workspace_vuln("[package]\n", true) == []
    end
  end

  describe "check_missing_ci_tests/1 (AP005)" do
    test "flags source code with no test workflow" do
      files = [".github/workflows/ci.yml", "src/main.rs"]
      assert [%{rule: "AP005"}] = AuditPrevention.check_missing_ci_tests(files)
    end

    test "accepts .yml and .yaml test workflows" do
      assert AuditPrevention.check_missing_ci_tests([".github/workflows/test.yml", "src/main.rs"]) == []
      assert AuditPrevention.check_missing_ci_tests([".github/workflows/unit-tests.yaml", "lib/x.ex"]) == []
    end

    test "does not flag a repo with no source code" do
      assert AuditPrevention.check_missing_ci_tests(["README.adoc", "docs/x.adoc"]) == []
    end
  end

  describe "check_missing_sast/2 (AP006)" do
    test "flags a multi-language repo with no CodeQL workflow" do
      assert [%{rule: "AP006"}] = AuditPrevention.check_missing_sast(["src/main.rs"], 3)
    end

    test "does not flag single-language repos or CodeQL carriers" do
      assert AuditPrevention.check_missing_sast(["src/main.rs"], 1) == []

      assert AuditPrevention.check_missing_sast(
               [".github/workflows/codeql.yml", "src/main.rs"],
               3
             ) == []

      assert AuditPrevention.check_missing_sast(
               [".github/workflows/codeql.yaml", "src/main.rs"],
               3
             ) == []
    end
  end

  describe "check_property_injection/1 (AP007)" do
    test "flags request-driven dynamic access with no allowlist" do
      content = "const v = obj[userInput]; const w = obj[req.body.key];\n"
      assert [finding] = AuditPrevention.check_property_injection(content)
      assert finding.rule == "AP007"
      assert finding.cwe == "CWE-915"
      assert finding.severity == :high
    end

    test "an allowlist marker in the same file satisfies the negative condition" do
      content = "const ALLOWED = new Set(['a']); const v = obj[userInput]; obj[req.query.k];\n"
      assert AuditPrevention.check_property_injection(content) == []
    end

    test "one access pattern alone is not the conjunction" do
      assert AuditPrevention.check_property_injection("const v = obj[userInput];\n") == []
      assert AuditPrevention.check_property_injection("const v = obj[req.params.id];\n") == []
    end
  end

  describe "crypto checks (AP009, AP010)" do
    test "flags fewer than two required reviewers on a cryptography repo" do
      assert [%{rule: "AP009"}] = AuditPrevention.check_crypto_enhanced_review(["cryptography"], 1)
      assert AuditPrevention.check_crypto_enhanced_review(["cryptography"], 2) == []
      assert AuditPrevention.check_crypto_enhanced_review(["web"], 1) == []
      assert AuditPrevention.check_crypto_enhanced_review(["cryptography"], nil) == []
    end

    test "flags a cryptography repo with no fuzz tree" do
      assert [%{rule: "AP010"}] =
               AuditPrevention.check_crypto_require_fuzzing([:cryptography], ["src/lib.rs"])

      assert AuditPrevention.check_crypto_require_fuzzing(
               ["cryptography"],
               ["fuzz/fuzz_targets/x.rs"]
             ) == []
    end
  end

  describe "check_exemplary_standards/2 (AP013)" do
    test "flags a tooling repo below the scorecard bar" do
      assert [%{rule: "AP013"}] = AuditPrevention.check_exemplary_standards(:tooling, 7.5)
      assert AuditPrevention.check_exemplary_standards("tooling", 8) == []
      assert AuditPrevention.check_exemplary_standards("library", 2) == []
      assert AuditPrevention.check_exemplary_standards(nil, nil) == []
    end
  end

  describe "check_all/1" do
    test "combines implemented checks and never duplicates canonical ones" do
      evidence = %{
        files: ["Cargo.toml", "src/lib.rs"],
        contents: %{
          "Cargo.toml" => "[package]\nname = \"proof-of-work\"\n",
          "src/lib.rs" => "pub fn f() {}\n",
          "web/app.js" => "obj[userInput]; obj[req.body.k];\n"
        },
        repo_tags: ["cryptography"],
        language_count: 1
      }

      ids = evidence |> AuditPrevention.check_all() |> Enum.map(& &1.rule)

      assert "AP003" in ids
      assert "AP005" in ids
      assert "AP007" in ids
      assert "AP010" in ids
      # AP014/AP015/AP011/AP012/AP008 live in their canonical modules.
      refute "AP014" in ids
      refute "AP015" in ids
      refute "AP011" in ids
    end

    test "empty evidence is silent, not an error" do
      assert AuditPrevention.check_all(%{}) == []
    end
  end
end
