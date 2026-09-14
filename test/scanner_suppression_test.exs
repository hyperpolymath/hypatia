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

    test "a commented example is text, not execution" do
      line = ~S(# Do not run curl https://example.com/i.sh | sh)
      assert ScannerSuppression.context_safe_line?("shell_download_then_run", line, 2)
    end

    test "a first-line shebang cannot hide download-and-execute" do
      line = ~S(#!/bin/sh curl https://example.com/i.sh | sh)
      refute ScannerSuppression.context_safe_line?("shell_download_then_run", line, 1)
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
      refute ScannerSuppression.suppressed?(
               "src/handlers.rs",
               "code_safety",
               "unwrap_without_check"
             )
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
      refute Regex.match?(
               @ncl,
               ~S(<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">)
             )
    end

    test "a DOCTYPE public identifier is not flagged" do
      refute Regex.match?(
               @ncl,
               ~S(<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">)
             )
    end

    test "a real insecure endpoint IS still flagged" do
      assert Regex.match?(@ncl, ~S(endpoint = "http://api.example.com/v1"))
    end
  end

  describe "strip_lazy_initialisers/2 — a counter, not a regex" do
    alias Hypatia.Rules.CodeSafety

    @pipes "||"

    # A `LazyLock` body runs ONCE, so `.expect(` inside it is not a hot path.
    # The previous implementation was a fixed-depth regex, and regexes cannot
    # count: a pattern one level more nested than it allowed survived elision
    # and failed the gate.
    test "elides a body whose regex literal has NESTED capture groups" do
      src =
        "static E: LazyLock<Regex> = LazyLock::new(" <>
          @pipes <>
          " Regex::new(r\"\\[((?:'[A-Za-z]+)*)\\]\").expect(\"E\"));"

      refute CodeSafety.strip_lazy_initialisers(src, "rust") =~ ".expect("
    end

    # ⚠ Rust's r#".."# form exists so a literal may contain `"`. Regex patterns
    # use it for exactly that. Treating those quotes as delimiters
    # desynchronises the scan.
    test "elides a body using a HASH RAW STRING containing quotes" do
      src =
        "static R: LazyLock<Regex> = LazyLock::new(" <>
          @pipes <>
          " Regex::new(r#\"(a|b)\\s*=\\s*\"([^\"]*)\"\"#).expect(\"R\"));"

      refute CodeSafety.strip_lazy_initialisers(src, "rust") =~ ".expect("
    end

    # The rule must not be blinded — elision is scoped to the initialiser body.
    test "a genuine per-call .expect( is still visible" do
      src =
        "static A: LazyLock<Regex> = LazyLock::new(" <>
          @pipes <>
          " Regex::new(r\"x\").expect(\"A\"));\nfn f() { m.get(\"k\").expect(\"missing\"); }"

      assert CodeSafety.strip_lazy_initialisers(src, "rust") =~ "expect(\"missing\")"
    end

    # ⚠ The dangerous failure mode: a desynchronised counter swallows the rest
    # of the file and silently blinds every later rule.
    test "an unbalanced paren inside a string does not swallow the file" do
      src =
        "static X: LazyLock<Regex> = LazyLock::new(" <>
          @pipes <>
          " Regex::new(\"(\").expect(\"x\"));\nfn after() {}"

      assert CodeSafety.strip_lazy_initialisers(src, "rust") =~ "fn after"
    end
  end

  # ── Comment-masked generic secrets ────────────────────────────────────────
  #
  # The suppression is keyed on the LABEL, never on the comment alone. Of the
  # 18 patterns in @secret_patterns, exactly 3 match on FORM ("this looks like
  # a password assignment") and are the entire false-positive population. The
  # other 15 are structurally unforgeable — a `ghp_` + 36 chars in a comment is
  # still a leaked token, and commenting it out does not revoke it.
  describe "comment_masked_secret_label?/3" do
    test "a commented generic API key is documentation, not a leak" do
      line = ~S(# api_key = "your-api-key-goes-here")
      assert ScannerSuppression.comment_masked_secret_label?("Generic API key", line, 4)
    end

    test "the same line uncommented is still reported" do
      line = ~S(api_key = "your-api-key-goes-here")
      refute ScannerSuppression.comment_masked_secret_label?("Generic API key", line, 4)
    end

    test "// comments count too" do
      line = ~S(// const secret = "placeholder-value-here")
      assert ScannerSuppression.comment_masked_secret_label?("Generic secret", line, 9)
    end

    # ⚠ The five controls below are the point of the whole change. Each is a
    # structurally-unforgeable label: it cannot be produced by accident, so a
    # comment is not evidence of innocence.
    test "a commented ghp_ token is NOT suppressed" do
      line = "# token = ghp_" <> String.duplicate("a", 36)
      refute ScannerSuppression.comment_masked_secret_label?("GitHub PAT", line, 4)
    end

    test "a commented AWS access key is NOT suppressed" do
      line = "# key = AKIA" <> String.duplicate("B", 16)
      refute ScannerSuppression.comment_masked_secret_label?("AWS Access Key", line, 4)
    end

    test "a commented private key header is NOT suppressed" do
      line = "# -----BEGIN RSA PRIVATE KEY-----"
      refute ScannerSuppression.comment_masked_secret_label?("Private Key", line, 4)
    end

    test "a leading -- is a long option, not a comment" do
      line = ~s|  --server.password="$ARANGO_PW" \\|
      refute ScannerSuppression.comment_masked_secret_label?("Password", line, 12)
    end

    test "a shebang cannot mask a secret, at line 1 or anywhere else" do
      line = ~S(#!/bin/sh password="hunter2hunter2")
      refute ScannerSuppression.comment_masked_secret_label?("Password", line, 1)
      refute ScannerSuppression.comment_masked_secret_label?("Password", line, 7)
    end

    test "line 1 never suppresses, even for a plain comment" do
      line = ~S(# password = "example-value-here")
      refute ScannerSuppression.comment_masked_secret_label?("Password", line, 1)
      assert ScannerSuppression.comment_masked_secret_label?("Password", line, 2)
    end

    test "a non-binary label is rejected rather than crashing" do
      refute ScannerSuppression.comment_masked_secret_label?(nil, "# x", 3)
      refute ScannerSuppression.comment_masked_secret_label?(:password, "# x", 3)
    end
  end

  # End-to-end: the labels above are asserted as literals, so they would rot
  # silently if @secret_patterns were renamed. These tests take the label from
  # detect_secrets/1 itself, so a rename breaks the test rather than the rule.
  describe "detect_secrets/1 + rejection, on real labels" do
    defp surviving(line, line_number) do
      line
      |> Hypatia.Rules.SecurityErrors.detect_secrets()
      |> Enum.uniq()
      |> Enum.reject(&ScannerSuppression.comment_masked_secret_label?(&1, line, line_number))
    end

    test "a commented password assignment is fully suppressed" do
      line = ~S(# password = "example-value-here")

      # Arm 4 of the control: this is what the PRE-FIX code did — detection
      # alone, with no rejection step. It reported the comment as a critical.
      assert Hypatia.Rules.SecurityErrors.detect_secrets(line) == ["Password"]

      # ...and this is what it does now.
      assert surviving(line, 6) == []
    end

    test "the same assignment uncommented survives" do
      assert surviving(~S(password = "example-value-here"), 6) == ["Password"]
    end

    test "a commented ghp_ token survives with its real label" do
      line = "# token = ghp_" <> String.duplicate("a", 36)
      assert "GitHub PAT" in surviving(line, 6)
    end

    test "a commented AKIA key survives with its real label" do
      line = "# key = AKIA" <> String.duplicate("B", 16)
      assert "AWS Access Key" in surviving(line, 6)
    end

    test "a commented private key header survives with its real label" do
      assert "Private Key" in surviving("# -----BEGIN RSA PRIVATE KEY-----", 6)
    end

    test "a commented line carrying BOTH a generic and a real secret keeps the real one" do
      line = "# password = \"x-placeholder-y\" ghp_" <> String.duplicate("c", 36)
      survivors = surviving(line, 6)
      assert "GitHub PAT" in survivors
      refute "Password" in survivors
    end
  end

  describe "whole_line_comment?/2" do
    test "indented comments count" do
      assert ScannerSuppression.whole_line_comment?("      # note", 5)
    end

    test "a trailing comment on a code line does not count" do
      refute ScannerSuppression.whole_line_comment?(~S(password = "x"  # note), 5)
    end

    test "a shebang is excluded by FORM, not by position" do
      refute ScannerSuppression.whole_line_comment?("#!/usr/bin/env bash", 40)
    end
  end
end
