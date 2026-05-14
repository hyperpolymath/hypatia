# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.ScannerSuppressionTest do
  use ExUnit.Case, async: true

  alias Hypatia.ScannerSuppression

  describe "suppressed?/4 — path-based exemptions" do
    test "exempts secret_detected for .audittraining/ corpora" do
      assert ScannerSuppression.suppressed?(
               ".audittraining/security-errors/echidnabot.md",
               "security_errors",
               "secret_detected"
             )
    end

    test "exempts secret_detected for scanner rule definition files" do
      assert ScannerSuppression.suppressed?(
               "lib/rules/security_errors.ex",
               "security_errors",
               "secret_detected"
             )
    end

    test "exempts secret_detected for fix-scripts/ remediation scripts" do
      assert ScannerSuppression.suppressed?(
               "scripts/fix-scripts/fix-hardcoded-secrets.sh",
               "security_errors",
               "secret_detected"
             )
    end

    test "exempts secret_detected for test fixtures" do
      assert ScannerSuppression.suppressed?(
               "test/code_safety_test.exs",
               "security_errors",
               "secret_detected"
             )

      assert ScannerSuppression.suppressed?(
               "adapters/tests/adapter_tests.rs",
               "security_errors",
               "secret_detected"
             )
    end

    test "does NOT exempt production source files" do
      refute ScannerSuppression.suppressed?(
               "lib/triangle_router.ex",
               "security_errors",
               "secret_detected"
             )
    end

    test "universal excludes apply to any rule" do
      assert ScannerSuppression.suppressed?(
               "node_modules/foo/index.js",
               "security_errors",
               "secret_detected"
             )

      assert ScannerSuppression.suppressed?(
               "target/debug/build/x.rs",
               "code_safety",
               "unwrap_without_check"
             )
    end
  end

  describe "context_safe_line?/2 — line-level exemptions for secret_detected" do
    test "GitHub Actions secrets reference is not a leak" do
      assert ScannerSuppression.context_safe_line?(
               "secret_detected",
               ~s|GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}|
             )
    end

    test "GitHub Actions vars reference is not a leak" do
      assert ScannerSuppression.context_safe_line?(
               "secret_detected",
               ~s|GITEA_HOST: ${{ vars.GITEA_HOST }}|
             )
    end

    test "shell parameter expansion (braced) is not a hardcoded secret" do
      assert ScannerSuppression.context_safe_line?(
               "secret_detected",
               ~s|CICD_CACHE_PASSWORD="${CICD_CACHE_PASSWORD:-}"|
             )
    end

    test "shell parameter expansion (bare) is not a hardcoded secret" do
      assert ScannerSuppression.context_safe_line?(
               "secret_detected",
               ~s|--server.password="$ARANGO_ROOT_PASSWORD"|
             )
    end

    test "command substitution is not a hardcoded secret" do
      assert ScannerSuppression.context_safe_line?(
               "secret_detected",
               ~s|TOKEN="$(vault read -field=token secret/api)"|
             )
    end

    test "Rust test fixture with test- prefix is not a hardcoded secret" do
      assert ScannerSuppression.context_safe_line?(
               "secret_detected",
               ~s|let secret = "test-secret";|
             )
    end

    test "actual hardcoded credential is NOT exempted" do
      refute ScannerSuppression.context_safe_line?(
               "secret_detected",
               ~s|password = "Pa55w0rd!hunter2"|
             )

      refute ScannerSuppression.context_safe_line?(
               "secret_detected",
               ~s|api_key: "ghp_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"|
             )
    end
  end

  describe "inline_allowed?/4 — inline directive suppression" do
    test "matches fully-qualified module/type form" do
      line = "let pw = \"abc\" // hypatia: allow security_errors/secret_detected -- example"

      assert ScannerSuppression.inline_allowed?(
               line,
               nil,
               "security_errors",
               "secret_detected"
             )
    end

    test "matches bare type form" do
      line = "myProof = believe_me  -- hypatia: allow believe_me"

      assert ScannerSuppression.inline_allowed?(
               line,
               nil,
               "code_safety",
               "believe_me"
             )
    end

    test "matches directive on previous line" do
      assert ScannerSuppression.inline_allowed?(
               "let secret = \"sk-foo\"",
               "  # hypatia: allow security_errors/secret_detected",
               "security_errors",
               "secret_detected"
             )
    end

    test "wildcard allows everything" do
      assert ScannerSuppression.inline_allowed?(
               "anything # hypatia: allow */*",
               nil,
               "security_errors",
               "secret_detected"
             )
    end

    test "non-matching directive does not suppress" do
      refute ScannerSuppression.inline_allowed?(
               "let secret = \"x\" # hypatia: allow code_safety/unwrap_without_check",
               nil,
               "security_errors",
               "secret_detected"
             )
    end
  end

  describe "file_allowed?/3 — file-level directive suppression" do
    test "matches directive in file header" do
      content = """
      -- SPDX-License-Identifier: PMPL-1.0-or-later
      -- hypatia: allow code_safety/believe_me -- intentional proof bypass
      --
      module Foo
      myProof = believe_me ()
      """

      assert ScannerSuppression.file_allowed?(content, "code_safety", "believe_me")
    end

    test "does not match directive after header window" do
      header = String.duplicate("-- filler\n", 25)
      content = header <> "-- hypatia: allow code_safety/believe_me\n"

      refute ScannerSuppression.file_allowed?(content, "code_safety", "believe_me")
    end
  end
end
