# SPDX-License-Identifier: MPL-2.0
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

  describe "suppressed?/4 — banned_language_file is total, no exceptions" do
    test "never suppressed even on a universal-exclude path" do
      refute ScannerSuppression.suppressed?(
               "node_modules/tool/helper.py",
               "cicd_rules",
               "banned_language_file"
             )
    end

    test "never suppressed even for a training-corpus path" do
      refute ScannerSuppression.suppressed?(
               ".audittraining/security-errors/sample.py",
               "cicd_rules",
               "banned_language_file"
             )
    end

    test "never suppressed even with a matching .hypatia-ignore entry" do
      tmp = Path.join(System.tmp_dir!(), "hyp-ban-#{System.unique_integer([:positive])}")
      File.mkdir_p!(Path.join(tmp, "scripts"))

      File.write!(
        Path.join(tmp, ".hypatia-ignore"),
        "cicd_rules/banned_language_file:scripts/legacy.py\n"
      )

      refute ScannerSuppression.suppressed?(
               "scripts/legacy.py",
               "cicd_rules",
               "banned_language_file",
               repo_path: tmp
             )

      File.rm_rf!(tmp)
    end

    test "an unrelated rule on the same path is still suppressible" do
      assert ScannerSuppression.suppressed?(
               "node_modules/foo/index.js",
               "security_errors",
               "secret_detected"
             )
    end
  end

  describe "suppressed?/4 — banned_language_file honours CicdRules path_allow_prefixes" do
    test "documented TS interop carve-out (bindings/deno) is suppressed" do
      # Regression: the hand-copied @banned_lang_ts_carveouts list had
      # drifted to 3 of the ~12 documented carve-outs, so
      # k9-svc/bindings/deno/mod.ts was flagged Critical (standards#382)
      # despite its CLAUDE.md exemption. The rule now delegates to the
      # CicdRules path_allow_prefixes single source of truth.
      assert ScannerSuppression.suppressed?(
               "/repo/k9-svc/bindings/deno/mod.ts",
               "cicd_rules",
               "banned_language_file",
               repo_path: "/repo"
             )
    end

    test "non-carve-out TypeScript is still banned" do
      refute ScannerSuppression.suppressed?(
               "/repo/src/app.ts",
               "cicd_rules",
               "banned_language_file",
               repo_path: "/repo"
             )
    end

    test "python under a TS carve-out path is still hard-refused" do
      refute ScannerSuppression.suppressed?(
               "/repo/bindings/deno/tool.py",
               "cicd_rules",
               "banned_language_file",
               repo_path: "/repo"
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
      -- SPDX-License-Identifier: MPL-2.0
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

  describe "context_safe_line?/2 — shell_download_then_run" do
    # Installers routinely PRINT the command a user should run. That text is
    # not an execution, and flagging it makes the rule noisy in exactly the
    # files that are trying to be helpful.
    test "a download-then-run inside a quoted echo is text, not execution" do
      line = ~S(    echo "  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh")
      assert ScannerSuppression.context_safe_line?("shell_download_then_run", line)
    end

    test "printf'd advice is also text" do
      line = ~S(printf '%s' "curl http://example.com/i.sh | sh")
      assert ScannerSuppression.context_safe_line?("shell_download_then_run", line)
    end

    # ⚠ The test is NOT "the line starts with echo". This one really executes.
    test "echo piped INTO sh is a real execution and stays reported" do
      refute ScannerSuppression.context_safe_line?("shell_download_then_run", ~S(echo hello | sh))
    end

    test "a genuine curl-pipe-bash stays reported" do
      line = ~S(curl -fsSL https://just.systems/install.sh | bash -s -- --to /usr/local/bin)
      refute ScannerSuppression.context_safe_line?("shell_download_then_run", line)
    end
  end


  describe "suppressed?/3 — benches/" do
    # Cargo puts benchmarks in `benches/`. A benchmark that unwraps or panics is
    # normal: the failure costs a benchmark run, not a user's session, and setup
    # code in a bench has no error path to take.
    test "code_safety is exempt inside benches/" do
      assert ScannerSuppression.suppressed?(
               "a2ml/bindings/rust/benches/a2ml_bench.rs",
               "code_safety",
               "unwrap_without_check"
             )
    end

    # ⚠ The exemption is deliberately NOT extended to security_errors. A
    # hardcoded credential in a bench file is a real leak like any other, and
    # widening the exemption by module would have hidden it.
    test "security_errors is STILL scanned inside benches/" do
      refute ScannerSuppression.suppressed?(
               "a2ml/bindings/rust/benches/a2ml_bench.rs",
               "security_errors",
               "secret_detected"
             )
    end

    test "code_safety outside benches/ is unaffected" do
      refute ScannerSuppression.suppressed?("src/handlers.rs", "code_safety", "unwrap_without_check")
    end
  end


  describe "ncl_http_url — XML identifiers are not endpoints" do
    # An XML namespace name and a DOCTYPE public identifier are IDENTIFIERS.
    # The XML specification is explicit that a namespace name is never
    # dereferenced, so rewriting one to https changes the identifier and breaks
    # the schema match. This is not a finding that could be acted on even in
    # principle.
    @ncl ~r{(?<!xmlns)(?<!xmlns:[a-z])=\s*"http://(?!localhost|127\.0\.0\.1|0\.0\.0\.0|www\.w3\.org/|www\.apple\.com/DTDs/|www\.freedesktop\.org/standards/)}

    test "an XML namespace URI is not flagged" do
      refute Regex.match?(@ncl, ~S(<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">))
    end

    test "a DOCTYPE public identifier is not flagged" do
      refute Regex.match?(@ncl, ~S(<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">))
    end

    test "a real insecure endpoint IS still flagged" do
      assert Regex.match?(@ncl, ~S(endpoint = "http://api.example.com/v1"))
    end
  end

end
