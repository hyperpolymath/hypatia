# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.Rules.SecurityErrors do
  @moduledoc """
  Security error database absorbed from Logtalk engine/rules/security_errors.lgt.

  Defines error categories, severity levels, prevention mechanisms,
  SHA pin database, CodeQL language support, and secret detection patterns.
  """

  # ---------------------------------------------------------------------------
  # Error Categories
  # ---------------------------------------------------------------------------

  @error_categories %{
    workflow_permission: "Missing or incorrect workflow permissions",
    unpinned_action: "GitHub Action not SHA-pinned",
    codeql_mismatch: "CodeQL language matrix mismatch",
    missing_spdx: "SPDX license header missing",
    duplicate_workflow: "Duplicate workflow files",
    missing_secret: "Workflow references undefined secret",
    dependabot_vuln: "Dependabot security vulnerability",
    code_scanning: "Code scanning alert",
    branch_protection: "Missing branch protection",
    "glib-variantstriter-unsoundness": "Unsoundness in glib VariantStrIter",
    "protobuf-recursion-crash": "Uncontrolled recursion in protobuf",
    "idna-punycode-mishandling": "Punycode mishandling in idna",
    "jsonwebtoken-type-confusion": "Type confusion in jsonwebtoken",
    "npm-js-yaml-vulnerability": "Vulnerability in js-yaml (prototype pollution)",
    "npm-serialize-javascript-vulnerability": "Vulnerability in serialize-javascript (RCE/XSS)",
    "npm-minimatch-vulnerability": "Vulnerability in minimatch (ReDoS)",
    "npm-glob-vulnerability": "Vulnerability in glob (command injection)",
    "npm-h3-vulnerability": "Vulnerability in h3 (middleware bypass/path traversal)",
    "lru-itermut-stacked-borrows": "Stacked Borrows violation in lru",
    "crossbeam-utils-atomiccell-unsoundness": "Unsoundness in crossbeam-utils AtomicCell",
    "lock-api-data-race": "Data race in lock_api",
    "crossbeam-queue-segqueue-unsoundness": "Unsoundness in crossbeam-queue SegQueue",
    empty_workflow: "Empty or stub workflow file",
    stub_crypto: "Stub/placeholder cryptographic implementation",
    wildcard_cors_web: "Wildcard CORS in web application code",
    elixir_code_injection: "Elixir code injection risk",
    elixir_deserialization: "Unsafe Erlang term deserialization",
    js_eval_usage: "JavaScript eval() usage",
    rust_panic_macro: "Rust panic! causes unrecoverable crash",
    rust_todo_macro: "Rust todo! marks incomplete implementation",
    rust_unimplemented_macro: "Rust unimplemented! marks unfinished code",
    rust_transmute: "Rust mem::transmute bypasses type safety",
    rust_mem_forget: "Rust mem::forget leaks memory",
    rust_manually_drop: "Rust ManuallyDrop risk of resource leaks",
    rust_from_raw: "Rust from_raw unsafe pointer construction",
    rust_as_ptr: "Rust as_ptr raw pointer exposure",
    agda_postulate: "Agda postulate — unproven assumption",
    agda_type_in_type: "Agda --type-in-type unsoundness",
    isabelle_oops: "Isabelle oops — incomplete proof",
    hol4_mk_thm: "HOL4 mk_thm kernel bypass",
    zig_ptr_cast: "Zig @ptrCast unchecked pointer conversion",
    zig_align_cast: "Zig @alignCast unchecked alignment cast",
    zig_int_to_ptr: "Zig @intToPtr unsafe integer-to-pointer",
    zig_bit_cast: "Zig @bitCast unchecked bit reinterpretation",
    zig_ptr_to_int: "Zig @ptrToInt raw pointer address exposure",
    fstar_admit: "F* admit — unproven goal acceptance",
    fstar_assume: "F* assume — unverified assumption",
    ada_pragma_suppress: "Ada pragma Suppress disables runtime checks",
    ada_unchecked_conversion: "Ada Unchecked_Conversion type safety bypass",
    ada_unchecked_deallocation: "Ada Unchecked_Deallocation manual memory management",
    ada_unchecked_access: "Ada Unchecked_Access accessibility bypass"
  }

  def error_categories, do: @error_categories
  def error_category(key), do: Map.get(@error_categories, key)

  # ---------------------------------------------------------------------------
  # Severity Levels
  # ---------------------------------------------------------------------------

  @severity_levels %{
    critical: 1,
    high: 2,
    medium: 3,
    low: 4,
    info: 5
  }

  def severity_levels, do: @severity_levels
  def severity_value(level), do: Map.get(@severity_levels, level, 5)

  # ---------------------------------------------------------------------------
  # Issue Severity Classification
  # ---------------------------------------------------------------------------

  @issue_severity %{
    "hard-coded-cryptographic-value" => :critical,
    "secret_detected" => :critical,
    "sql-injection" => :critical,
    "VulnerabilitiesID" => :critical,
    "remote-property-injection" => :high,
    "TokenPermissionsID" => :high,
    "missing-workflow-permissions" => :high,
    "missing_permissions" => :high,
    "unpinned_action" => :high,
    "upload-pages-artifact-transitive-deps" => :high,
    "PinnedDependenciesID" => :medium,
    "missing_spdx" => :medium,
    "SecurityPolicyID" => :medium,
    "BranchProtectionID" => :medium,
    "CodeReviewID" => :medium,
    "syntax-error" => :medium,
    "glib-variantstriter-unsoundness" => :medium,
    "protobuf-recursion-crash" => :medium,
    "idna-punycode-mishandling" => :medium,
    "jsonwebtoken-type-confusion" => :medium,
    "npm-js-yaml-vulnerability" => :medium,
    "npm-serialize-javascript-vulnerability" => :high,
    "npm-minimatch-vulnerability" => :high,
    "npm-glob-vulnerability" => :high,
    "npm-h3-vulnerability" => :high,
    "lru-itermut-stacked-borrows" => :low,
    "crossbeam-utils-atomiccell-unsoundness" => :high,
    "lock-api-data-race" => :medium,
    "crossbeam-queue-segqueue-unsoundness" => :medium,
    "scorecard-run-step-restriction" => :medium,
    "rust-toolchain-sha-missing-input" => :medium,
    "unused-local-variable" => :low,
    "MaintainedID" => :low,
    "FuzzingID" => :low,
    "CIIBestPracticesID" => :low,
    "deno-lint-include-pattern" => :low,
    "workflow-linter-self-detection" => :low,
    "stub-crypto-function" => :critical,
    "stub-hash-return" => :critical,
    "todo-crypto" => :high,
    "fake-signature" => :critical,
    "js-wildcard-cors" => :high,
    "js-eval" => :critical,
    "js-innerhtml" => :high,
    "js-document-write" => :high,
    "js-hardcoded-secret" => :critical,
    "elixir-system-cmd-interpolation" => :critical,
    "elixir-code-eval" => :critical,
    "elixir-send-unsanitised" => :high,
    "elixir-atom-from-user" => :high,
    "elixir-no-ssl-verify" => :high,
    "elixir-port-open-shell" => :high
  }

  def issue_severity(issue_type), do: Map.get(@issue_severity, issue_type, :info)

  # ---------------------------------------------------------------------------
  # Auto-Fixable Issues
  # ---------------------------------------------------------------------------

  @auto_fixable MapSet.new([
    "TokenPermissionsID",
    "PinnedDependenciesID",
    "missing-workflow-permissions",
    "missing_permissions",
    "unpinned_action",
    "missing_spdx",
    "SecurityPolicyID",
    "BranchProtectionID",
    "unused-local-variable",
    "missing-spdx-header",
    "upload-pages-artifact-transitive-deps",
    "scorecard-run-step-restriction",
    "deno-lint-include-pattern",
    "rust-toolchain-sha-missing-input",
    "workflow-linter-self-detection"
  ])

  def auto_fixable?(issue_type), do: MapSet.member?(@auto_fixable, issue_type)

  # ---------------------------------------------------------------------------
  # Fix Suggestions
  # ---------------------------------------------------------------------------

  @fix_suggestions %{
    "TokenPermissionsID" => "Add \"permissions: read-all\" at workflow level",
    "PinnedDependenciesID" => "Replace @vN with @SHA # vN format",
    "SecurityPolicyID" => "Create SECURITY.md with vulnerability reporting instructions",
    "BranchProtectionID" => "Enable branch protection via Settings > Branches",
    "CodeReviewID" => "Require at least 1 approving review before merge",
    "CIIBestPracticesID" => "Register at bestpractices.coreinfrastructure.org",
    "FuzzingID" => "Add ClusterFuzzLite or cargo-fuzz infrastructure",
    "VulnerabilitiesID" => "Run cargo audit / npm audit and update dependencies",
    "secret_detected" => "Revoke/Rotate the secret immediately and purge git history using git-filter-repo",
    "hard-coded-cryptographic-value" => "Use environment variables or secrets manager",
    "remote-property-injection" => "Add allowlist validation for dynamic property access",
    "unused-local-variable" => "Remove unused code or prefix with underscore",
    "syntax-error" => "Fix syntax error using linter output",
    "glib-variantstriter-unsoundness" => "Update glib to v0.22.3, v0.20.7, v0.19.10 or v0.18.6",
    "protobuf-recursion-crash" => "Update protobuf to >= 3.7.2",
    "idna-punycode-mishandling" => "Update idna to >= 1.0.0",
    "jsonwebtoken-type-confusion" => "Update jsonwebtoken to >= 10.3.0",
    "npm-js-yaml-vulnerability" => "Update js-yaml to >= 3.14.2 or 4.1.1",
    "npm-serialize-javascript-vulnerability" => "Update serialize-javascript to >= 6.0.2 or 7.0.0",
    "npm-minimatch-vulnerability" => "Update minimatch to >= 9.0.5 or 10.0.0",
    "npm-glob-vulnerability" => "Update glob to >= 11.0.0",
    "npm-h3-vulnerability" => "Update h3 to >= 2.0.1-rc.15",
    "lru-itermut-stacked-borrows" => "Update lru to >= 0.16.3",
    "crossbeam-utils-atomiccell-unsoundness" => "Update crossbeam-utils to >= 0.8.7",
    "lock-api-data-race" => "Update lock_api to >= 0.4.2",
    "crossbeam-queue-segqueue-unsoundness" => "Update crossbeam-queue to >= 0.2.3",
    "upload-pages-artifact-transitive-deps" => "Update to v4 SHA 7b1f4a764d45c48632c6b24a0339c27f5614fb0b",
    "scorecard-run-step-restriction" => "Move run steps to separate check-critical job",
    "deno-lint-include-pattern" => "Add explicit include patterns or use deno task lint",
    "rust-toolchain-sha-missing-input" => "Add \"with: toolchain: stable\" to dtolnay step",
    "workflow-linter-self-detection" => "Add grep -v filters for grep/echo/comments",
    "unpinned_action" => "Replace @vN with @SHA # vN format",
    "missing_permissions" => "Add \"permissions: read-all\" at workflow level",
    "missing_spdx" => "Add SPDX-License-Identifier header to file",
    "missing-workflow-permissions" => "Add \"permissions: read-all\" at workflow level",
    "missing-spdx-header" => "Add SPDX-License-Identifier header to file",
    "stub-crypto-function" => "Replace stub with real cryptographic implementation or add compile_error! guard",
    "stub-hash-return" => "Replace format!(\"stub:...\") with real hash computation",
    "todo-crypto" => "Implement the cryptographic function before deployment",
    "fake-signature" => "Replace fake/placeholder crypto value with real implementation",
    "js-wildcard-cors" => "Replace Access-Control-Allow-Origin: * with specific origin or env var",
    "js-eval" => "Remove eval() — use structured alternatives (JSON.parse, Function constructor if needed)",
    "js-innerhtml" => "Use textContent, Trusted Types, or rescript-dom-mounter SafeDOM",
    "js-document-write" => "Use DOM manipulation methods instead of document.write",
    "js-hardcoded-secret" => "Move credentials to environment variables or secrets manager",
    "elixir-system-cmd-interpolation" => "Pass arguments as list to System.cmd, not interpolated string",
    "elixir-code-eval" => "Replace Code.eval_* with structured dispatch or compiled modules",
    "elixir-send-unsanitised" => "Use :erlang.binary_to_term(data, [:safe]) to restrict atom creation",
    "elixir-atom-from-user" => "Use String.to_existing_atom/1 instead — atom table is finite",
    "elixir-no-ssl-verify" => "Set verify: :verify_peer with cacertfile or cacerts option",
    "elixir-port-open-shell" => "Validate and sanitize command before Port.open spawn"
  }

  def fix_suggestion(issue_type), do: Map.get(@fix_suggestions, issue_type)

  # ---------------------------------------------------------------------------
  # Prevention Workflow Mappings
  # ---------------------------------------------------------------------------

  @prevention_workflows %{
    "TokenPermissionsID" => "workflow-linter.yml",
    "unpinned_action" => "workflow-linter.yml",
    "missing_permissions" => "workflow-linter.yml",
    "missing_spdx" => "workflow-linter.yml",
    "PinnedDependenciesID" => "workflow-linter.yml",
    "missing-workflow-permissions" => "workflow-linter.yml",
    "hard-coded-cryptographic-value" => "secret-scanner.yml",
    "VulnerabilitiesID" => "cargo-audit.yml",
    "glib-variantstriter-unsoundness" => "cargo-audit.yml",
    "protobuf-recursion-crash" => "cargo-audit.yml",
    "idna-punycode-mishandling" => "cargo-audit.yml",
    "jsonwebtoken-type-confusion" => "cargo-audit.yml",
    "npm-js-yaml-vulnerability" => "npm-audit.yml",
    "npm-serialize-javascript-vulnerability" => "npm-audit.yml",
    "npm-minimatch-vulnerability" => "npm-audit.yml",
    "npm-glob-vulnerability" => "npm-audit.yml",
    "npm-h3-vulnerability" => "npm-audit.yml",
    "lru-itermut-stacked-borrows" => "cargo-audit.yml",
    "crossbeam-utils-atomiccell-unsoundness" => "cargo-audit.yml",
    "lock-api-data-race" => "cargo-audit.yml",
    "crossbeam-queue-segqueue-unsoundness" => "cargo-audit.yml",
    "SecurityPolicyID" => "scorecard-enforcer.yml",
    "BranchProtectionID" => "scorecard-enforcer.yml",
    "CodeReviewID" => "scorecard-enforcer.yml",
    "upload-pages-artifact-transitive-deps" => "workflow-linter.yml",
    "scorecard-run-step-restriction" => "scorecard-enforcer.yml",
    "rust-toolchain-sha-missing-input" => "rust-ci.yml",
    "workflow-linter-self-detection" => "workflow-linter.yml"
  }

  def prevention_workflow(issue_type), do: Map.get(@prevention_workflows, issue_type)

  # ---------------------------------------------------------------------------
  # GitHub Action SHA Pins (canonical)
  # ---------------------------------------------------------------------------

  @sha_pins %{
    "actions/checkout@v4" => "34e114876b0b11c390a56381ad16ebd13914f8d5",
    "actions/checkout@v5" => "93cb6efe18208431cddfb8368fd83d5badbf9bfd",
    "github/codeql-action@v3" => "6624720a57d4c312633c7b953db2f2da5bcb4c3a",
    "ossf/scorecard-action@v2.4.0" => "62b2cac7ed8198b15735ed49ab1e5cf35480ba46",
    "dtolnay/rust-toolchain@stable" => "4be9e76fd7c4901c61fb841f559994984270fce7",
    "Swatinem/rust-cache@v2" => "779680da715d629ac1d338a641029a2f4372abb5",
    "codecov/codecov-action@v5" => "671740ac38dd9b0130fbe1cec585b89eea48d3de",
    "trufflesecurity/trufflehog@main" => "7ee2e0fdffec27d19ccbb8fb3dcf8a83b9d7f9e8",
    "webfactory/ssh-agent@v0.9.0" => "dc588b651fe13675774614f8e6a936a468676387",
    "ocaml/setup-ocaml@v3" => "dec6499fef64fc5d7ed43d43a87251b7b1c306f5",
    "softprops/action-gh-release@v2" => "a06a81a03ee405af7f2048a818ed3f03bbf83c7b",
    "actions/configure-pages@v5" => "983d7736d9b0ae728b81ab479565c72886d7745b",
    "actions/jekyll-build-pages@v1" => "44a6e6beabd48582f863aeeb6cb2151cc1716697",
    "actions/upload-pages-artifact@v3" => "56afc609e74202658d3ffba0e8f6dda462b719fa",
    "actions/deploy-pages@v4" => "d6db90164ac5ed86f2b6aed7e0febac5b3c0c03e",
    "ruby/setup-ruby@v1" => "09a7688d3b55cf0e976497ff046b70949eeaccfd",
    "editorconfig-checker/action-editorconfig-checker@main" => "4054fa83a075fdf090bd098bdb1c09aaf64a4169",
    "slsa-framework/slsa-github-generator@v2.1.0" => "f7dd8c54c2067bafc12ca7a55595d5ee9b75204a"
  }

  def sha_pins, do: @sha_pins

  def correct_sha(action_ref) do
    Map.get(@sha_pins, action_ref)
  end

  def pin_action(action_ref) do
    case correct_sha(action_ref) do
      nil -> {:error, :unknown_action}
      sha -> {:ok, "#{action_name(action_ref)}@#{sha} # #{action_version(action_ref)}"}
    end
  end

  defp action_name(ref), do: ref |> String.split("@") |> List.first()
  defp action_version(ref), do: ref |> String.split("@") |> List.last()

  # ---------------------------------------------------------------------------
  # CodeQL Language Support
  # ---------------------------------------------------------------------------

  @codeql_supported MapSet.new([
    "javascript-typescript", "python", "go", "java-kotlin",
    "ruby", "csharp", "cpp", "swift"
  ])

  @codeql_unsupported MapSet.new([
    "rust", "ocaml", "haskell", "ada", "rescript", "gleam", "nickel"
  ])

  @file_ext_to_codeql %{
    ".js" => "javascript-typescript",
    ".jsx" => "javascript-typescript",
    ".ts" => "javascript-typescript",
    ".tsx" => "javascript-typescript",
    ".mjs" => "javascript-typescript",
    ".py" => "python",
    ".go" => "go",
    ".java" => "java-kotlin",
    ".kt" => "java-kotlin",
    ".rb" => "ruby",
    ".cs" => "csharp",
    ".cpp" => "cpp",
    ".c" => "cpp",
    ".swift" => "swift"
  }

  def codeql_supported?(lang), do: MapSet.member?(@codeql_supported, lang)
  def codeql_unsupported?(lang), do: MapSet.member?(@codeql_unsupported, lang)
  def codeql_language_for_ext(ext), do: Map.get(@file_ext_to_codeql, ext)

  def validate_codeql_matrix(repo_languages) do
    supported = Enum.filter(repo_languages, &codeql_supported?/1)
    if Enum.empty?(supported), do: ["actions"], else: supported
  end

  # ---------------------------------------------------------------------------
  # Secret Detection Patterns
  # ---------------------------------------------------------------------------

  @secret_patterns [
    {~r/(?i)api[_-]?key\s*[=:]\s*["'][^"']+["']/, "Generic API key"},
    {~r/(?i)secret\s*[=:]\s*["'][^"']+["']/, "Generic secret"},
    {~r/(?i)password\s*[=:]\s*["'][^"']+["']/, "Password"},
    {~r/ghp_[a-zA-Z0-9]{36}/, "GitHub PAT"},
    {~r/github_pat_[a-zA-Z0-9]{22}_[a-zA-Z0-9]{59}/, "Fine-grained GitHub PAT"},
    {~r/glpat-[a-zA-Z0-9\-]{20}/, "GitLab PAT"},
    {~r/AKIA[0-9A-Z]{16}/, "AWS Access Key"},
    {~r/xoxb-[0-9]{10,}-[0-9]{10,}-[a-zA-Z0-9]+/, "Slack Bot Token"},
    {~r/xoxp-[0-9]{10,}-[0-9]{10,}-[a-zA-Z0-9]+/, "Slack User Token"},
    {~r/pypi-AgE[a-zA-Z0-9\-_]{50,}/, "PyPI API Token"},
    {~r/-----BEGIN [A-Z]+ PRIVATE KEY-----/, "Private Key"},
    {~r/sk-[a-zA-Z0-9]{48}/, "OpenAI API Key"},
    {~r/sk-proj-[a-zA-Z0-9]{48}/, "OpenAI Project Key"},
    {~r/sk-ant-api[0-9]{2}-[a-zA-Z0-9\-_]{86}/, "Anthropic API Key"},
    {~r/npm_[a-zA-Z0-9]{36}/, "npm Access Token"},
    {~r/snyk_[a-zA-Z0-9\-]{36}/, "Snyk API Token"},
    {~r/SG\.[a-zA-Z0-9\-_]{22}\.[a-zA-Z0-9\-_]{43}/, "SendGrid API Key"},
    {~r/AIza[0-9A-Za-z\-_]{35}/, "Google API Key"}
  ]

  def detect_secrets(content) do
    Enum.flat_map(@secret_patterns, fn {regex, label} ->
      if Regex.match?(regex, content), do: [label], else: []
    end)
  end

  # ---------------------------------------------------------------------------
  # RSR Language Policy
  # ---------------------------------------------------------------------------

  @allowed_languages MapSet.new([
    "rescript", "rust", "gleam", "julia", "logtalk", "haskell",
    "bash", "nickel", "guile", "ocaml", "ada", "elixir", "zig", "idris2"
  ])

  @banned_languages %{
    "typescript" => "rescript",
    "nodejs" => "deno",
    "golang" => "rust",
    "python" => "julia",
    "java" => "rust",
    "kotlin" => "rust",
    "swift" => "tauri"
  }

  def language_allowed?(lang), do: MapSet.member?(@allowed_languages, String.downcase(lang))
  def language_banned?(lang), do: Map.has_key?(@banned_languages, String.downcase(lang))
  def language_replacement(lang), do: Map.get(@banned_languages, String.downcase(lang))

  # ---------------------------------------------------------------------------
  # CWE Mappings
  # ---------------------------------------------------------------------------

  @cwe_mappings %{
    "env_var_injection" => {"CWE-78", "OS Command Injection"},
    "shell_unquoted" => {"CWE-78", "OS Command Injection"},
    "merkle_verification_weak" => {"CWE-347", "Improper Verification of Cryptographic Signature"},
    "set_verification_missing" => {"CWE-347", "Improper Verification of Cryptographic Signature"},
    "permissive_bypass" => {"CWE-863", "Incorrect Authorization"},
    "wildcard_cors" => {"CWE-942", "Permissive Cross-domain Policy"},
    "unverified_jwt_decode" => {"CWE-347", "Improper Verification of Cryptographic Signature"},
    "default_to_root" => {"CWE-250", "Execution with Unnecessary Privileges"},
    "path_traversal_risk" => {"CWE-22", "Path Traversal"},
    "command_injection_risk" => {"CWE-78", "OS Command Injection"},
    "unwrap_without_check" => {"CWE-754", "Improper Check for Unusual Conditions"},
    "getexn_on_external_data" => {"CWE-754", "Improper Check for Unusual Conditions"},
    "sql_injection" => {"CWE-89", "SQL Injection"},
    "stub_crypto" => {"CWE-327", "Use of Broken or Risky Cryptographic Algorithm"},
    "js_eval" => {"CWE-94", "Improper Control of Code Generation"},
    "elixir_code_eval" => {"CWE-94", "Improper Control of Code Generation"},
    "elixir_deserialization" => {"CWE-502", "Deserialization of Untrusted Data"},
    "elixir_atom_exhaustion" => {"CWE-400", "Uncontrolled Resource Consumption"},
    "ssl_verify_none" => {"CWE-295", "Improper Certificate Validation"},
    "wildcard_cors_web" => {"CWE-942", "Permissive Cross-domain Policy"}
  }

  def cwe_for(issue_type), do: Map.get(@cwe_mappings, issue_type)
end
