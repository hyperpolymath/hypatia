# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.Rules.BuildSystemRulesTest do
  use ExUnit.Case, async: true

  alias Hypatia.Rules.BuildSystemRules

  describe "scan_dune_test_alias_inline_field/1 (Rule A — affinescript#361)" do
    test "flags (test ...) with (alias <name>) child field" do
      content = """
      (test
       (name bench_main)
       (libraries affinescript alcotest unix)
       (modules bench_main bench_fixtures bench_lex)
       (alias bench))
      """

      findings = BuildSystemRules.scan_dune_test_alias_inline_field(content)
      assert length(findings) == 1
      assert hd(findings).rule == "dune_test_alias_inline_field"
      assert hd(findings).severity == :high
      assert hd(findings).description =~ "affinescript#361"
    end

    test "tolerates comments and whitespace between (test and (alias" do
      content = """
      ; comment line
      (test

       (name foo)
       ; another comment
       (libraries bar)
       (alias bench))
      """

      assert [_finding] = BuildSystemRules.scan_dune_test_alias_inline_field(content)
    end

    test "does NOT flag (rule (alias <name>) ...) — the canonical fix" do
      content = """
      (executable
       (name bench_main)
       (libraries affinescript alcotest unix)
       (modules bench_main))

      (rule
       (alias bench)
       (action (run %{exe:bench_main.exe})))
      """

      assert BuildSystemRules.scan_dune_test_alias_inline_field(content) == []
    end

    test "does NOT flag a plain (test ...) with no alias" do
      content = """
      (test
       (name regression)
       (libraries foo)
       (modules t_main))
      """

      assert BuildSystemRules.scan_dune_test_alias_inline_field(content) == []
    end

    test "does NOT flag (alias ...) outside a (test) stanza" do
      content = """
      (executable
       (name some_exe)
       (libraries foo))

      (alias
       (name custom)
       (deps some_exe.exe))
      """

      assert BuildSystemRules.scan_dune_test_alias_inline_field(content) == []
    end
  end

  describe "scan_package_json_org_scoped_pre_publish/1 (Rule B — affinescript#361)" do
    test "flags @hyperpolymath/* in dependencies block" do
      content = """
      {
        "name": "consumer",
        "version": "0.1.0",
        "dependencies": {
          "@hyperpolymath/affine-vscode": "^0.1.0",
          "vscode-languageclient": "^9.0.0"
        }
      }
      """

      findings = BuildSystemRules.scan_package_json_org_scoped_pre_publish(content)
      assert length(findings) == 1
      assert hd(findings).rule == "npm_org_scoped_pre_publish_dependency"
      assert hd(findings).severity == :medium
      assert hd(findings).description =~ "optionalDependencies"
      assert hd(findings).description =~ "affinescript#361"
    end

    test "does NOT flag @hyperpolymath/* in optionalDependencies (the canonical fix)" do
      content = """
      {
        "name": "consumer",
        "version": "0.1.0",
        "dependencies": {
          "vscode-languageclient": "^9.0.0"
        },
        "optionalDependencies": {
          "@hyperpolymath/affine-vscode": "^0.1.0"
        }
      }
      """

      assert BuildSystemRules.scan_package_json_org_scoped_pre_publish(content) == []
    end

    test "does NOT flag @hyperpolymath/* in devDependencies" do
      content = """
      {
        "devDependencies": {
          "@hyperpolymath/test-utils": "^0.1.0"
        },
        "dependencies": {
          "lodash": "^4.0.0"
        }
      }
      """

      assert BuildSystemRules.scan_package_json_org_scoped_pre_publish(content) == []
    end

    test "does NOT flag @hyperpolymath/* in peerDependencies" do
      content = """
      {
        "peerDependencies": {
          "@hyperpolymath/peer": "^0.1.0"
        },
        "dependencies": {
          "react": "^18.0.0"
        }
      }
      """

      assert BuildSystemRules.scan_package_json_org_scoped_pre_publish(content) == []
    end

    test "does NOT flag non-estate org scopes" do
      content = """
      {
        "dependencies": {
          "@types/node": "^20.0.0",
          "@vscode/test-electron": "^2.4.1"
        }
      }
      """

      assert BuildSystemRules.scan_package_json_org_scoped_pre_publish(content) == []
    end

    test "does NOT flag a plain (no @org) dependency" do
      content = ~s({"dependencies": {"react": "^18.0.0"}})
      assert BuildSystemRules.scan_package_json_org_scoped_pre_publish(content) == []
    end
  end

  describe "scan/2 dispatch by filename" do
    test "routes dune files to the dune rule" do
      content = "(test (name x) (modules a) (alias bench))\n"
      findings = BuildSystemRules.scan(content, "bench/dune")
      assert length(findings) == 1
      assert hd(findings).rule == "dune_test_alias_inline_field"
    end

    test "routes package.json files to the npm rule" do
      content = ~s({"dependencies": {"@hyperpolymath/foo": "^0.1.0"}})
      findings = BuildSystemRules.scan(content, "editors/vscode/package.json")
      assert length(findings) == 1
      assert hd(findings).rule == "npm_org_scoped_pre_publish_dependency"
    end

    test "returns [] for unrelated file types" do
      assert BuildSystemRules.scan("anything at all", "src/main.rs") == []
      assert BuildSystemRules.scan("anything at all", "Cargo.toml") == []
      assert BuildSystemRules.scan("anything at all", "dune-project") == []
    end
  end
end
