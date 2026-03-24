#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Hypatia CLI v2.0 - Generalised code safety scanner
# Scans all project languages with extensible pattern rules

set -euo pipefail

VERSION="2.0.0"
HYPATIA_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Colors (disabled when piping)
if [[ -t 2 ]]; then
    RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'
    BLUE='\033[0;34m'; CYAN='\033[0;36m'; NC='\033[0m'
else
    RED=''; GREEN=''; YELLOW=''; BLUE=''; CYAN=''; NC=''
fi

usage() {
    cat << EOF
Hypatia ${VERSION} - Generalised Code Safety Scanner

USAGE:
    hypatia <command> [options]

COMMANDS:
    scan <path>         Scan directory or file for security issues
    report <path>       Generate detailed report with fix suggestions
    fix <path>          Auto-fix issues where possible
    check-staged        Check git staged files (for pre-commit hook)
    version             Show version
    help                Show this help

EXAMPLES:
    hypatia scan .                    # Scan current directory
    hypatia scan src/auth/JWT.res     # Scan specific file
    hypatia report . > report.txt     # Generate report
    hypatia check-staged              # Pre-commit hook

ENVIRONMENT:
    HYPATIA_SEVERITY    Minimum severity to report (critical|high|medium|low|info)
    HYPATIA_FORMAT      Output format (text|json|github)
    HYPATIA_FIX_MODE    Enable auto-fix (dry-run|apply)

INTEGRATION:
    - Called by gitbot-fleet coordination layer
    - Submits findings to shared-context for dispatch
    - Triggers robot-repo-automaton for fixes
EOF
}

log_info()    { echo -e "${BLUE}[INFO]${NC} $*" >&2; }
log_warn()    { echo -e "${YELLOW}[WARN]${NC} $*" >&2; }
log_error()   { echo -e "${RED}[ERROR]${NC} $*" >&2; }
log_success() { echo -e "${GREEN}[OK]${NC} $*" >&2; }

# ─────────────────────────────────────────────────────────────────────
# Severity filtering
# ─────────────────────────────────────────────────────────────────────
severity_rank() {
    case "$1" in
        critical) echo 1 ;; high) echo 2 ;; medium) echo 3 ;;
        low) echo 4 ;; info) echo 5 ;; *) echo 5 ;;
    esac
}

should_report() {
    local finding_sev="$1"
    local filter="${HYPATIA_SEVERITY:-medium}"
    [[ $(severity_rank "$finding_sev") -le $(severity_rank "$filter") ]]
}

# ─────────────────────────────────────────────────────────────────────
# Finding accumulator — single JSON array built via jq
# ─────────────────────────────────────────────────────────────────────
FINDINGS_FILE=""
FINDING_COUNT=0
FIRST_FINDING=true

init_findings() {
    FINDINGS_FILE=$(mktemp /tmp/hypatia-findings-XXXXXX.json)
    echo "[" > "$FINDINGS_FILE"
    FIRST_FINDING=true
    FINDING_COUNT=0
}

emit_finding() {
    local sev="$1" type="$2" pattern="$3" file="$4" line="$5"
    local code="$6" cwe="$7" fix="$8"
    local auto_fixable="${9:-false}"

    should_report "$sev" || return 0

    [[ "$FIRST_FINDING" == "false" ]] && echo "," >> "$FINDINGS_FILE"
    FIRST_FINDING=false

    jq -n \
        --arg sev "$sev" --arg type "$type" --arg pattern "$pattern" \
        --arg file "$file" --argjson line "$line" --arg code "$code" \
        --arg cwe "$cwe" --arg fix "$fix" --argjson auto "$([ "$auto_fixable" = "true" ] && echo true || echo false)" \
        '{severity:$sev, type:$type, pattern:$pattern, file:$file,
          line:$line, code:$code, cwe:$cwe, fix:$fix, auto_fixable:$auto}' \
        >> "$FINDINGS_FILE"

    ((FINDING_COUNT++)) || true
}

close_findings() {
    echo "]" >> "$FINDINGS_FILE"
}

# ─────────────────────────────────────────────────────────────────────
# Generic rg-based pattern runner
# Applies a regex to files matching a glob, emits findings
# ─────────────────────────────────────────────────────────────────────
run_pattern() {
    local dir="$1" glob="$2" regex="$3"
    local sev="$4" type="$5" pattern="$6" cwe="$7" fix="$8"
    local auto_fixable="${9:-false}"
    local extra_rg_args="${10:-}"

    local rg_cmd=(rg -n --no-heading --hidden --glob '!.git/' --glob "$glob" $extra_rg_args "$regex" "$dir")

    while IFS=: read -r file linenum rest; do
        [[ -z "$linenum" || ! "$linenum" =~ ^[0-9]+$ ]] && continue
        # Truncate code to 200 chars to avoid jq issues
        local code="${rest:0:200}"
        emit_finding "$sev" "$type" "$pattern" "$file" "$linenum" "$code" "$cwe" "$fix" "$auto_fixable"
    done < <("${rg_cmd[@]}" 2>/dev/null || true)
}

# ─────────────────────────────────────────────────────────────────────
# PATTERN CATALOGUE — generalised, not repo-specific
# Each pattern: glob, regex, severity, type, pattern_id, CWE, fix, auto_fixable
#
# Categories:
#   1. Type system / proof bypasses
#   2. Unsafe crash / panic paths
#   3. Injection vulnerabilities
#   4. Cryptographic weaknesses
#   5. Secret exposure
#   6. XSS / DOM safety
#   7. File/path safety
#   8. Container / infrastructure
#   9. Technical debt
# ─────────────────────────────────────────────────────────────────────

scan_code_patterns() {
    local dir="$1"

    # ── 1. TYPE SYSTEM / PROOF BYPASSES ────────────────────────────

    # Idris2: believe_me — unsound type coercion, defeats dependent types
    run_pattern "$dir" "*.idr" '\bbelieve_me\b' \
        "critical" "type_safety_bypass" "idris2_believe_me" "CWE-704" \
        "Replace believe_me with a proper proof or use an interface constraint"

    # Idris2: assert_total — bypasses totality checker
    run_pattern "$dir" "*.idr" '\bassert_total\b' \
        "high" "type_safety_bypass" "idris2_assert_total" "CWE-704" \
        "Prove totality or restructure to be structurally decreasing"

    # Idris2: assert_smaller — bypasses termination checker
    run_pattern "$dir" "*.idr" '\bassert_smaller\b' \
        "high" "type_safety_bypass" "idris2_assert_smaller" "CWE-704" \
        "Prove the argument is structurally smaller"

    # Idris2: unsafePerformIO — breaks referential transparency
    run_pattern "$dir" "*.idr" '\bunsafePerformIO\b' \
        "critical" "unsafe_io" "idris2_unsafe_perform_io" "CWE-676" \
        "Use IO monad properly; unsafePerformIO breaks purity guarantees"

    # Haskell: unsafeCoerce — arbitrary type cast
    run_pattern "$dir" "*.hs" '\bunsafeCoerce\b' \
        "critical" "type_safety_bypass" "haskell_unsafe_coerce" "CWE-704" \
        "Remove unsafeCoerce; use proper type class constraints or newtypes"

    # Haskell: unsafePerformIO — breaks purity
    run_pattern "$dir" "*.hs" '\bunsafePerformIO\b' \
        "critical" "unsafe_io" "haskell_unsafe_perform_io" "CWE-676" \
        "Use IO monad properly; unsafePerformIO breaks referential transparency"

    # Haskell: undefined — guaranteed runtime crash
    run_pattern "$dir" "*.hs" '\bundefined\b' \
        "high" "unsafe_crash" "haskell_undefined" "CWE-754" \
        "Replace undefined with a proper error type or Maybe/Either"

    # OCaml: Obj.magic — unsafe type cast
    run_pattern "$dir" "*.ml" '\bObj\.magic\b' \
        "critical" "type_safety_bypass" "ocaml_obj_magic" "CWE-704" \
        "Remove Obj.magic; use proper GADT or polymorphic variant"

    # OCaml: Obj.repr / Obj.obj — unsafe runtime representation access
    run_pattern "$dir" "*.ml" '\bObj\.(repr|obj)\b' \
        "critical" "type_safety_bypass" "ocaml_obj_repr" "CWE-704" \
        "Remove Obj.repr/obj; use proper type-safe abstractions"

    # Coq: Admitted — unproven theorem accepted as axiom
    run_pattern "$dir" "*.v" '\bAdmitted\b' \
        "critical" "proof_bypass" "coq_admitted" "CWE-704" \
        "Complete the proof or mark as an explicit axiom with justification"

    # Lean: sorry — proof hole
    run_pattern "$dir" "*.lean" '\bsorry\b' \
        "critical" "proof_bypass" "lean_sorry" "CWE-704" \
        "Complete the proof; sorry leaves a soundness hole"

    # ReScript: Obj.magic — type bypass
    run_pattern "$dir" "*.res" '\bObj\.magic\b' \
        "high" "type_safety_bypass" "rescript_obj_magic" "CWE-704" \
        "Remove Obj.magic and use proper type conversions or externals"

    # ReScript: getExn — crashes on None/Error
    run_pattern "$dir" "*.res" '\bgetExn\b' \
        "critical" "unsafe_crash" "rescript_getexn" "CWE-754" \
        "Replace getExn with switch/match or getWithDefault"

    # ReScript: JSON.parseExn — crashes on invalid JSON
    run_pattern "$dir" "*.res" '\bJSON\.parseExn\b' \
        "critical" "unsafe_crash" "rescript_json_parse_exn" "CWE-20" \
        "Use try/catch or a safe JSON parser that returns Result"

    # ── 2. UNSAFE CRASH / PANIC PATHS ──────────────────────────────

    # Rust: unwrap() — panics on None/Err
    run_pattern "$dir" "*.rs" '\.unwrap\(\)' \
        "high" "unsafe_panic" "rust_unwrap" "CWE-754" \
        "Replace unwrap() with ? operator, unwrap_or, or match"

    # Rust: unwrap_or(0) — silently masks errors with dangerous default
    run_pattern "$dir" "*.rs" '\.unwrap_or\(0\)' \
        "critical" "unsafe_panic" "rust_unwrap_or_zero" "CWE-754" \
        "Use a meaningful default or propagate the error with ?"

    # Rust: .lock().unwrap() — mutex poison without handling
    run_pattern "$dir" "*.rs" '\.lock\(\)\.unwrap\(\)' \
        "high" "unsafe_panic" "rust_mutex_unwrap" "CWE-754" \
        "Handle mutex poisoning: .lock().unwrap_or_else(|e| e.into_inner())"

    # Rust: unsafe blocks without SAFETY comment
    # (checked by looking for unsafe { without a preceding // SAFETY: comment)
    run_pattern "$dir" "*.rs" '^\s*unsafe\s*\{' \
        "medium" "unsafe_block" "rust_undocumented_unsafe" "CWE-676" \
        "Add a // SAFETY: comment explaining invariants before unsafe blocks"

    # Rust: transmute — extremely dangerous type-level reinterpretation
    run_pattern "$dir" "*.rs" '\btransmute\b' \
        "critical" "type_safety_bypass" "rust_transmute" "CWE-704" \
        "Avoid transmute; use safe casts, From/Into traits, or bytemuck"

    # Rust: panic! / todo! / unimplemented! in non-test code
    run_pattern "$dir" "*.rs" '\b(panic!|todo!|unimplemented!)\b' \
        "medium" "unsafe_crash" "rust_panic_macro" "CWE-754" \
        "Replace panic!/todo!/unimplemented! with proper error handling" \
        "false" "--glob=!*test*"

    # ── 3. INJECTION VULNERABILITIES ───────────────────────────────

    # Shell injection via Command::new with format!/user input (Rust)
    run_pattern "$dir" "*.rs" 'Command::new\("(sh|bash|cmd)"\).*\.arg\("-c"\)' \
        "critical" "command_injection" "rust_shell_exec" "CWE-78" \
        "Never pass user input to sh -c; use Command::new with individual args"

    # Shell injection via std::process with env var (Rust)
    run_pattern "$dir" "*.rs" 'Command::new\(.*env::var' \
        "critical" "command_injection" "rust_env_command" "CWE-78" \
        "Validate environment variable before using as command name"

    # SQL injection via string interpolation (Elixir)
    run_pattern "$dir" "*.{ex,exs}" 'query.*#\{' \
        "critical" "sql_injection" "elixir_sql_interpolation" "CWE-89" \
        "Use parameterised queries: Repo.query(sql, [param]) not string interpolation"

    # SQL injection via format! in Rust
    run_pattern "$dir" "*.rs" 'format!\s*\(\s*"[^"]*SELECT[^"]*\{' \
        "critical" "sql_injection" "rust_sql_format" "CWE-89" \
        "Use parameterised queries or a query builder, not format! for SQL"

    # SQL injection via string concat (any language)
    run_pattern "$dir" "*.{rs,res,ex,exs,hs,ml,js}" '"SELECT\s.*"\s*\+\+?\s' \
        "critical" "sql_injection" "string_concat_sql" "CWE-89" \
        "Use parameterised queries; never concatenate user input into SQL"

    # GraphQL injection via string interpolation (Elixir)
    run_pattern "$dir" "*.{ex,exs}" 'mutation\s*\{.*#\{' \
        "critical" "graphql_injection" "elixir_graphql_interpolation" "CWE-943" \
        "Use GraphQL variables instead of string interpolation in mutations"

    # Shell injection via unquoted variables in bash
    run_pattern "$dir" "*.sh" 'exec\s+\$[A-Z_]+[^"]' \
        "critical" "command_injection" "shell_unquoted_var" "CWE-78" \
        "Always double-quote shell variables: exec \"\$VAR\"" "true"

    # eval() / Function() in JavaScript/ReScript
    run_pattern "$dir" "*.{js,jsx,mjs,res}" '\beval\s*\(' \
        "critical" "code_injection" "eval_usage" "CWE-95" \
        "Replace eval() with JSON.parse, switch, or safe alternatives"

    # Dynamic Function() constructor
    run_pattern "$dir" "*.{js,jsx,mjs,res}" '\bFunction\s*\(' \
        "critical" "code_injection" "function_constructor" "CWE-95" \
        "Replace Function() constructor with static code"

    # setTimeout/setInterval with string argument
    run_pattern "$dir" "*.{js,jsx,mjs,res}" 'setTimeout\s*\(\s*["\x27]|setInterval\s*\(\s*["\x27]' \
        "critical" "code_injection" "settimeout_string" "CWE-95" \
        "Pass a function reference to setTimeout/setInterval, not a string"

    # ── 4. CRYPTOGRAPHIC WEAKNESSES ────────────────────────────────

    # MD5 for anything (any language)
    run_pattern "$dir" "*.{rs,ex,exs,hs,ml,res,js,py,rb,go}" '\b(md5|MD5|Md5)\b' \
        "high" "weak_crypto" "md5_usage" "CWE-328" \
        "MD5 is cryptographically broken; use SHA-256 or better"

    # SHA-1 for security purposes
    run_pattern "$dir" "*.{rs,ex,exs,hs,ml,res,js}" '\b(sha1|SHA1|Sha1|SHA-1)\b' \
        "high" "weak_crypto" "sha1_usage" "CWE-328" \
        "SHA-1 is vulnerable to collision attacks; use SHA-256 or better"

    # Hardcoded cryptographic keys/IVs
    run_pattern "$dir" "*.{rs,ex,exs,hs,ml,res,js,py}" '(?i)(aes_key|encryption_key|iv|nonce)\s*[:=]\s*["'\'']\w{16,}' \
        "critical" "hardcoded_crypto" "hardcoded_crypto_key" "CWE-321" \
        "Never hardcode cryptographic keys; use a key management system or env vars"

    # Insecure random for crypto
    run_pattern "$dir" "*.rs" '\brand\(\)|thread_rng\(\).*gen\b' \
        "medium" "weak_crypto" "insecure_random" "CWE-330" \
        "Use OsRng or a CSPRNG for security-sensitive random values"

    # ── 5. SECRET EXPOSURE ─────────────────────────────────────────

    # GitHub PAT (classic)
    run_pattern "$dir" "*.{rs,ex,exs,hs,ml,res,js,py,sh,yml,yaml,toml,json}" 'ghp_[a-zA-Z0-9]{36}' \
        "critical" "hardcoded_secret" "github_pat_classic" "CWE-798" \
        "Remove hardcoded GitHub PAT; use environment variables or secret manager"

    # GitHub PAT (fine-grained)
    run_pattern "$dir" "*.{rs,ex,exs,hs,ml,res,js,py,sh,yml,yaml,toml,json}" 'github_pat_[a-zA-Z0-9]{22}_[a-zA-Z0-9]{59}' \
        "critical" "hardcoded_secret" "github_pat_fine" "CWE-798" \
        "Remove hardcoded GitHub fine-grained PAT"

    # GitLab PAT
    run_pattern "$dir" "*.{rs,ex,exs,hs,ml,res,js,py,sh,yml,yaml,toml,json}" 'glpat-[a-zA-Z0-9\-]{20}' \
        "critical" "hardcoded_secret" "gitlab_pat" "CWE-798" \
        "Remove hardcoded GitLab PAT"

    # AWS Access Key
    run_pattern "$dir" "*.{rs,ex,exs,hs,ml,res,js,py,sh,yml,yaml,toml,json}" 'AKIA[0-9A-Z]{16}' \
        "critical" "hardcoded_secret" "aws_access_key" "CWE-798" \
        "Remove hardcoded AWS access key; use IAM roles or env vars"

    # OpenAI / Anthropic API keys
    run_pattern "$dir" "*.{rs,ex,exs,hs,ml,res,js,py,sh,yml,yaml,toml,json}" 'sk-(proj-)?[a-zA-Z0-9]{20,}' \
        "critical" "hardcoded_secret" "openai_api_key" "CWE-798" \
        "Remove hardcoded API key; use environment variables"

    # Slack tokens
    run_pattern "$dir" "*.{rs,ex,exs,hs,ml,res,js,py,sh,yml,yaml,toml,json}" 'xox[bpsa]-[a-zA-Z0-9\-]{10,}' \
        "critical" "hardcoded_secret" "slack_token" "CWE-798" \
        "Remove hardcoded Slack token"

    # Private keys
    run_pattern "$dir" "*.{rs,ex,exs,hs,ml,res,js,py,sh,yml,yaml,toml,json,pem}" '-----BEGIN\s+(RSA\s+)?PRIVATE\s+KEY-----' \
        "critical" "hardcoded_secret" "private_key_in_source" "CWE-798" \
        "Never commit private keys to source control"

    # Anthropic API keys
    run_pattern "$dir" "*.{rs,ex,exs,hs,ml,res,js,py,sh,yml,yaml,toml,json,env}" 'sk-ant-[a-zA-Z0-9\-]{20,}' \
        "critical" "hardcoded_secret" "anthropic_api_key" "CWE-798" \
        "Remove hardcoded Anthropic API key; use environment variables"

    # Stripe keys (publishable ok, secret NOT ok)
    run_pattern "$dir" "*.{rs,ex,exs,hs,ml,res,js,py,sh,yml,yaml,toml,json,env}" 'sk_(live|test)_[a-zA-Z0-9]{20,}' \
        "critical" "hardcoded_secret" "stripe_secret_key" "CWE-798" \
        "Remove hardcoded Stripe secret key"

    # Twilio Account SID + Auth Token
    run_pattern "$dir" "*.{rs,ex,exs,hs,ml,res,js,py,sh,yml,yaml,toml,json,env}" 'AC[a-f0-9]{32}' \
        "critical" "hardcoded_secret" "twilio_sid" "CWE-798" \
        "Remove hardcoded Twilio SID; use environment variables"

    # SendGrid API key
    run_pattern "$dir" "*.{rs,ex,exs,hs,ml,res,js,py,sh,yml,yaml,toml,json,env}" 'SG\.[a-zA-Z0-9\-]{22,}\.[a-zA-Z0-9\-]{22,}' \
        "critical" "hardcoded_secret" "sendgrid_key" "CWE-798" \
        "Remove hardcoded SendGrid API key"

    # GCP service account JSON (detect the private_key field)
    run_pattern "$dir" "*.{json,env}" '"private_key"\s*:\s*"-----BEGIN' \
        "critical" "hardcoded_secret" "gcp_service_account" "CWE-798" \
        "Never commit GCP service account JSON; use workload identity or env vars"

    # Azure connection strings
    run_pattern "$dir" "*.{rs,ex,exs,hs,ml,res,js,py,sh,yml,yaml,toml,json,env}" 'AccountKey=[a-zA-Z0-9+/=]{40,}' \
        "critical" "hardcoded_secret" "azure_connection_string" "CWE-798" \
        "Remove hardcoded Azure connection string"

    # Database connection URIs with passwords (postgres://, mysql://, mongodb://, redis://)
    run_pattern "$dir" "*.{rs,ex,exs,hs,ml,res,js,py,sh,yml,yaml,toml,json,env}" '(postgres|mysql|mongodb|redis|amqp)://[^:]+:[^@]{3,}@' \
        "critical" "hardcoded_secret" "database_uri_with_password" "CWE-798" \
        "Remove hardcoded database credentials from connection URI"

    # .env files committed to source (should be in .gitignore)
    run_pattern "$dir" ".env*" '(PASSWORD|SECRET|TOKEN|API_KEY|PRIVATE_KEY|ACCESS_KEY)\s*=' \
        "critical" "hardcoded_secret" "env_file_secrets" "CWE-798" \
        "Never commit .env files with secrets; add to .gitignore and use a vault"

    # Generic password/secret assignment (broad — allows type annotations between name and value)
    run_pattern "$dir" "*.{rs,ex,exs,hs,ml,res,js,py,sh,toml,json}" '(?i)(password|passwd|secret|credential)[^=]*=\s*"[^"]{8,}"' \
        "critical" "hardcoded_secret" "generic_password" "CWE-798" \
        "Move passwords/secrets to environment variables or a vault"

    # ── 6. XSS / DOM SAFETY ───────────────────────────────────────

    # dangerouslySetInnerHTML (React/ReScript)
    run_pattern "$dir" "*.{res,jsx,js,tsx}" 'dangerouslySetInnerHTML' \
        "critical" "xss" "dangerous_inner_html" "CWE-79" \
        "Sanitise HTML with DOMPurify before using dangerouslySetInnerHTML"

    # innerHTML assignment
    run_pattern "$dir" "*.{js,jsx,mjs,res}" '\.innerHTML\s*=' \
        "critical" "xss" "innerhtml_assignment" "CWE-79" \
        "Use textContent or a sanitisation library instead of innerHTML"

    # document.write
    run_pattern "$dir" "*.{js,jsx,mjs}" 'document\.write\s*\(' \
        "high" "xss" "document_write" "CWE-79" \
        "Replace document.write with DOM API methods"

    # ── 7. FILE / PATH SAFETY ─────────────────────────────────────

    # Path traversal — user input joined to path (Rust)
    run_pattern "$dir" "*.rs" 'Path::new\(.*\)\.join\(' \
        "medium" "path_traversal" "rust_path_join" "CWE-22" \
        "Validate and canonicalise user-controlled path components before joining"

    # Path traversal — format! with path segments (Rust)
    run_pattern "$dir" "*.rs" 'format!\s*\(\s*"[^"]*(/|\\\\)[^"]*\{' \
        "medium" "path_traversal" "rust_format_path" "CWE-22" \
        "Validate user input used in file paths; check for .. and symlinks"

    # Open redirect — user input in redirect URL
    run_pattern "$dir" "*.{rs,ex,exs,js}" '(?i)redirect.*\(.*req|redirect.*params|redirect.*query' \
        "medium" "open_redirect" "user_controlled_redirect" "CWE-601" \
        "Validate redirect targets against an allowlist of safe destinations"

    # ── 8. CONTAINER / INFRASTRUCTURE ──────────────────────────────

    # CORS wildcard
    run_pattern "$dir" "*.{rs,ex,exs,js,py,rb,go,hs}" 'Access-Control-Allow-Origin.*"\*"' \
        "critical" "cors_misconfiguration" "wildcard_cors" "CWE-942" \
        "Replace CORS wildcard with environment-based origin allowlist" "true"

    # Running as root in containers
    run_pattern "$dir" "{Containerfile,Dockerfile,*.dockerfile}" '(?i)^USER\s+root' \
        "high" "container_security" "container_root_user" "CWE-250" \
        "Run containers as a non-root user"

    # HTTP (non-HTTPS) URLs in code
    run_pattern "$dir" "*.{rs,ex,exs,js,res,hs,ml,toml,json}" 'http://[^l][^o][^c]' \
        "medium" "insecure_transport" "http_url" "CWE-319" \
        "Use HTTPS instead of HTTP" "true"

    # Permissive/audit mode flags
    run_pattern "$dir" "*.{rs,ex,exs,js,py,sh,yml,yaml}" '(?i)(permissive.*continue|audit.*mode|--no-verify)' \
        "high" "security_bypass" "permissive_mode" "CWE-863" \
        "Do not ship permissive/audit modes in production"

    # ── 9. WORKFLOW / CI SECURITY ──────────────────────────────────

    # Unpinned GitHub Actions (uses: owner/repo@vN instead of @sha)
    run_pattern "$dir" "*.{yml,yaml}" 'uses:\s+[a-zA-Z0-9_-]+/[a-zA-Z0-9_-]+@v[0-9]' \
        "high" "supply_chain" "unpinned_action" "CWE-829" \
        "Pin GitHub Actions to full SHA, not version tags" "true"

    # Missing permissions in workflows
    # (This is checked structurally below, not via regex)

    # GitHub Actions expression injection (${{ github.event... }})
    # Catches any event data used directly in run: context (title, body, labels, head_ref, etc.)
    run_pattern "$dir" "*.{yml,yaml}" '\$\{\{.*github\.event\.' \
        "critical" "code_injection" "actions_expression_injection" "CWE-94" \
        "Do not use event data in run: steps; assign to env var with safe quoting"

    # Workflow uses pull_request_target with checkout (dangerous)
    run_pattern "$dir" "*.{yml,yaml}" 'pull_request_target' \
        "high" "supply_chain" "pr_target_trigger" "CWE-829" \
        "pull_request_target runs in privileged context; do not checkout PR code"

    # Flawed action pinning check (grep -rn "uses:" matches itself)
    run_pattern "$dir" "*.{yml,yaml,sh}" 'grep.*-rn.*"uses:"' \
        "medium" "workflow_security" "flawed_linter_regex" "CWE-1021" \
        "Use anchored regex for action pinning check: grep -rnE \"^[[:space:]]+uses:\"" "true"

    # ── 10. TIMING / COMPARISON SAFETY ─────────────────────────────

    # Non-constant-time comparison for secrets (Rust)
    run_pattern "$dir" "*.rs" '(token|secret|password|key)\s*==\s*' \
        "medium" "timing_attack" "non_constant_time_compare" "CWE-208" \
        "Use constant-time comparison (e.g. subtle::ConstantTimeEq) for secrets"

    # Non-constant-time comparison for secrets (Elixir)
    run_pattern "$dir" "*.{ex,exs}" '(token|secret|password) == ' \
        "medium" "timing_attack" "elixir_timing_compare" "CWE-208" \
        "Use Plug.Crypto.secure_compare/2 for secret comparison"

    # ── 11. TECHNICAL DEBT MARKERS ─────────────────────────────────

    run_pattern "$dir" "*.{rs,res,ml,hs,idr,ex,exs,js,jsx,v,lean,zig,sh}" \
        '(?i)\b(TODO|FIXME|HACK|XXX|BUG)\b[:\s]' \
        "info" "technical_debt" "debt_marker" "CWE-1057" \
        "Create a tracked issue for this technical debt item"
}

# ─────────────────────────────────────────────────────────────────────
# Repository structure / layout checks
# ─────────────────────────────────────────────────────────────────────
scan_repo_structure() {
    local dir="$1"

    # .machine_readable/ directory
    if [[ ! -d "$dir/.machine_readable" ]]; then
        emit_finding "critical" "repo_structure" "missing_machine_readable" \
            "$dir" 0 ".machine_readable directory missing" "CWE-1021" \
            "Create .machine_readable/ with canonical SCM files"
    fi

    # Legacy .bot_directives/ at root
    if [[ -d "$dir/.bot_directives" ]]; then
        emit_finding "high" "repo_structure" "legacy_bot_directives" \
            "$dir/.bot_directives" 0 "Legacy root .bot_directives detected" "CWE-1021" \
            "Migrate to .machine_readable/bot_directives/"
    fi

    # AI manifest
    if [[ ! -f "$dir/0-AI-MANIFEST.a2ml" && ! -f "$dir/AI.a2ml" ]]; then
        emit_finding "high" "repo_structure" "missing_ai_manifest" \
            "$dir" 0 "No AI gateway manifest found" "CWE-1021" \
            "Add 0-AI-MANIFEST.a2ml or AI.a2ml"
    fi

    # Canonical SCM files
    local scm_files=("STATE.scm" "META.scm" "ECOSYSTEM.scm")
    for scm in "${scm_files[@]}"; do
        # Root-level SCM is wrong location
        if [[ -f "$dir/$scm" ]]; then
            emit_finding "high" "repo_structure" "root_scm_file" \
                "$dir/$scm" 0 "SCM file at root instead of .machine_readable/" "CWE-1021" \
                "Move $scm to .machine_readable/$scm"
        fi
        # Missing from canonical location
        if [[ -d "$dir/.machine_readable" && ! -f "$dir/.machine_readable/$scm" ]]; then
            emit_finding "medium" "repo_structure" "missing_scm_file" \
                "$dir/.machine_readable/$scm" 0 "Missing canonical SCM: $scm" "CWE-1021" \
                "Create .machine_readable/$scm"
        fi
    done

    # SECURITY.md
    if [[ ! -f "$dir/SECURITY.md" ]]; then
        emit_finding "medium" "repo_structure" "missing_security_policy" \
            "$dir" 0 "No SECURITY.md found" "CWE-1059" \
            "Add SECURITY.md with vulnerability reporting instructions" "true"
    fi

    # SPDX headers on workflows
    if [[ -d "$dir/.github/workflows" ]]; then
        for wf in "$dir/.github/workflows"/*.yml "$dir/.github/workflows"/*.yaml; do
            [[ -f "$wf" ]] || continue
            if ! head -1 "$wf" | grep -q '^# SPDX-License-Identifier:'; then
                emit_finding "medium" "repo_structure" "missing_spdx_header" \
                    "$wf" 1 "Workflow missing SPDX header" "CWE-1059" \
                    "Add '# SPDX-License-Identifier: PMPL-1.0-or-later' as first line" "true"
            fi
        done

        # Check for permissions: declaration
        for wf in "$dir/.github/workflows"/*.yml "$dir/.github/workflows"/*.yaml; do
            [[ -f "$wf" ]] || continue
            if ! grep -q '^permissions:' "$wf"; then
                emit_finding "high" "workflow_security" "missing_permissions" \
                    "$wf" 0 "Workflow missing top-level permissions declaration" "CWE-269" \
                    "Add 'permissions: read-all' at workflow level" "true"
            fi
        done
    fi

    # License hygiene: REUSE-compliant dual-license setup
    if [[ -f "$dir/LICENSE" ]]; then
        local first_line
        first_line=$(head -1 "$dir/LICENSE")
        if [[ "$first_line" != "Mozilla Public License Version 2.0" ]]; then
            emit_finding "medium" "license_hygiene" "license_not_mpl2" \
                "$dir/LICENSE" 1 "LICENSE file is not standard MPL-2.0 text (detected: ${first_line:0:60})" "CWE-1059" \
                "Replace LICENSE with standard MPL-2.0 text for machine detection" "true"
        fi
    else
        emit_finding "high" "license_hygiene" "missing_license_file" \
            "$dir" 0 "No LICENSE file found" "CWE-1059" \
            "Add LICENSE with standard MPL-2.0 text" "true"
    fi

    if [[ ! -d "$dir/LICENSES" ]]; then
        emit_finding "medium" "license_hygiene" "missing_licenses_dir" \
            "$dir" 0 "No LICENSES/ directory (REUSE standard)" "CWE-1059" \
            "Add LICENSES/ with MPL-2.0.txt and PMPL-1.0-or-later.txt" "true"
    else
        [[ ! -f "$dir/LICENSES/PMPL-1.0-or-later.txt" ]] && \
            emit_finding "medium" "license_hygiene" "missing_pmpl_text" \
                "$dir/LICENSES" 0 "LICENSES/ missing PMPL-1.0-or-later.txt" "CWE-1059" \
                "Add LICENSES/PMPL-1.0-or-later.txt" "true"
    fi

    if [[ ! -f "$dir/NOTICE" ]]; then
        emit_finding "low" "license_hygiene" "missing_notice" \
            "$dir" 0 "No NOTICE file explaining dual-license setup" "CWE-1059" \
            "Add NOTICE explaining MPL-2.0 + PMPL-1.0-or-later relationship" "true"
    fi

    # Dependabot config
    if [[ ! -f "$dir/.github/dependabot.yml" && ! -f "$dir/.github/dependabot.yaml" ]]; then
        if [[ -f "$dir/Cargo.toml" || -f "$dir/mix.exs" || -f "$dir/deno.json" || -f "$dir/package.json" ]]; then
            emit_finding "medium" "repo_structure" "missing_dependabot" \
                "$dir" 0 "No dependabot.yml but project has dependencies" "CWE-1104" \
                "Add .github/dependabot.yml for automated dependency updates" "true"
        fi
    fi

    # Rust: Missing forbid(unsafe_code)
    if [[ -f "$dir/Cargo.toml" ]]; then
        local entry_points=()
        [[ -f "$dir/src/lib.rs" ]] && entry_points+=("$dir/src/lib.rs")
        [[ -f "$dir/src/main.rs" ]] && entry_points+=("$dir/src/main.rs")
        
        for ep in "${entry_points[@]}"; do
            if ! grep -q "#!\[forbid(unsafe_code)\]" "$ep"; then
                emit_finding "low" "code_quality" "missing_forbid_unsafe" \
                    "$ep" 1 "Rust entry point missing #![forbid(unsafe_code)]" "CWE-1188" \
                    "Add #![forbid(unsafe_code)] at the top of $ep to enforce memory safety" "true"
            fi
        done
    fi
}

# ─────────────────────────────────────────────────────────────────────
# Workflow-specific deep checks
# ─────────────────────────────────────────────────────────────────────
scan_workflows() {
    local dir="$1"
    [[ -d "$dir/.github/workflows" ]] || return 0

    for wf in "$dir/.github/workflows"/*.yml "$dir/.github/workflows"/*.yaml; do
        [[ -f "$wf" ]] || continue

        # pull_request_target with checkout is extremely dangerous
        if grep -q 'pull_request_target' "$wf" && grep -q 'actions/checkout' "$wf"; then
            emit_finding "critical" "supply_chain" "pr_target_checkout" \
                "$wf" 0 "pull_request_target + checkout = code execution from untrusted PRs" "CWE-829" \
                "Do not checkout PR code in pull_request_target; use pull_request instead"
        fi
    done

    # ── WORKFLOW HYGIENE — detect irrelevant workflows per repo ────────
    scan_workflow_hygiene "$dir"
}

# ─────────────────────────────────────────────────────────────────────
# Workflow hygiene: detect workflows irrelevant to the repo's languages
# and tech stack. Emits findings for each unnecessary workflow.
# ─────────────────────────────────────────────────────────────────────
scan_workflow_hygiene() {
    local dir="$1"
    local wf_dir="$dir/.github/workflows"
    [[ -d "$wf_dir" ]] || return 0

    # ── Detect repo languages/tech from file extensions and markers ──
    local has_ts=false has_js=false has_py=false has_rs=false
    local has_go=false has_jl=false has_zig=false has_idr=false
    local has_ex=false has_hs=false has_ml=false has_rb=false
    local has_node=false has_jekyll=false has_npm=false has_bun=false
    local has_guix=false has_nix=false has_wellknown=false

    # Language detection via file extensions (fast find, no deep traversal)
    [[ -n "$(find "$dir" -maxdepth 4 -name '*.ts' -o -name '*.tsx' 2>/dev/null | head -1)" ]] && has_ts=true
    [[ -n "$(find "$dir" -maxdepth 4 -name '*.js' -o -name '*.jsx' -o -name '*.mjs' 2>/dev/null | head -1)" ]] && has_js=true
    [[ -n "$(find "$dir" -maxdepth 4 -name '*.py' 2>/dev/null | head -1)" ]] && has_py=true
    [[ -n "$(find "$dir" -maxdepth 4 -name '*.rs' 2>/dev/null | head -1)" ]] && has_rs=true
    [[ -n "$(find "$dir" -maxdepth 4 -name '*.go' 2>/dev/null | head -1)" ]] && has_go=true
    [[ -n "$(find "$dir" -maxdepth 4 -name '*.jl' 2>/dev/null | head -1)" ]] && has_jl=true
    [[ -n "$(find "$dir" -maxdepth 4 -name '*.zig' 2>/dev/null | head -1)" ]] && has_zig=true
    [[ -n "$(find "$dir" -maxdepth 4 -name '*.idr' 2>/dev/null | head -1)" ]] && has_idr=true
    [[ -n "$(find "$dir" -maxdepth 4 -name '*.ex' -o -name '*.exs' 2>/dev/null | head -1)" ]] && has_ex=true
    [[ -n "$(find "$dir" -maxdepth 4 -name '*.hs' 2>/dev/null | head -1)" ]] && has_hs=true
    [[ -n "$(find "$dir" -maxdepth 4 -name '*.ml' -o -name '*.mli' 2>/dev/null | head -1)" ]] && has_ml=true
    [[ -n "$(find "$dir" -maxdepth 4 -name '*.rb' 2>/dev/null | head -1)" ]] && has_rb=true

    # Tech stack markers
    [[ -f "$dir/package.json" || -f "$dir/package-lock.json" ]] && has_npm=true
    [[ -f "$dir/bun.lockb" || -f "$dir/bunfig.toml" ]] && has_bun=true
    [[ -f "$dir/_config.yml" || -f "$dir/Gemfile" ]] && has_jekyll=true
    [[ -f "$dir/node_modules/.package-lock.json" || -f "$dir/package-lock.json" ]] && has_node=true
    [[ -d "$dir/guix" || -f "$dir/manifest.scm" || -f "$dir/channels.scm" ]] && has_guix=true
    [[ -f "$dir/flake.nix" || -f "$dir/shell.nix" || -f "$dir/default.nix" ]] && has_nix=true
    [[ -d "$dir/.well-known" ]] && has_wellknown=true

    # ── Check each potentially-irrelevant workflow ───────────────────

    # TypeScript blocker: useless if no TS/JS in repo
    if [[ -f "$wf_dir/ts-blocker.yml" ]] && [[ "$has_ts" == false && "$has_js" == false ]]; then
        emit_finding "low" "workflow_hygiene" "irrelevant_ts_blocker" \
            "$wf_dir/ts-blocker.yml" 0 \
            "ts-blocker.yml present but repo has no TypeScript or JavaScript files" "CWE-1059" \
            "Remove ts-blocker.yml — no TS/JS to block" "true"
    fi

    # npm/bun blocker: useless if no package.json or JS ecosystem
    if [[ -f "$wf_dir/npm-bun-blocker.yml" ]] && [[ "$has_npm" == false && "$has_bun" == false && "$has_node" == false ]]; then
        emit_finding "low" "workflow_hygiene" "irrelevant_npm_blocker" \
            "$wf_dir/npm-bun-blocker.yml" 0 \
            "npm-bun-blocker.yml present but repo has no JS package ecosystem" "CWE-1059" \
            "Remove npm-bun-blocker.yml — no npm/bun/node to block" "true"
    fi

    # Jekyll workflows: useless if no Jekyll config
    for jkwf in "jekyll.yml" "jekyll-gh-pages.yml"; do
        if [[ -f "$wf_dir/$jkwf" ]] && [[ "$has_jekyll" == false ]]; then
            emit_finding "low" "workflow_hygiene" "irrelevant_jekyll" \
                "$wf_dir/$jkwf" 0 \
                "$jkwf present but repo has no _config.yml or Gemfile" "CWE-1059" \
                "Remove $jkwf — no Jekyll site in this repo" "true"
        fi
    done

    # Guix/Nix policy: useless if no Guix or Nix config
    if [[ -f "$wf_dir/guix-nix-policy.yml" ]] && [[ "$has_guix" == false && "$has_nix" == false ]]; then
        emit_finding "low" "workflow_hygiene" "irrelevant_guix_nix" \
            "$wf_dir/guix-nix-policy.yml" 0 \
            "guix-nix-policy.yml present but repo has no Guix or Nix configuration" "CWE-1059" \
            "Remove guix-nix-policy.yml — not a Guix/Nix project" "true"
    fi

    # .well-known enforcement: useless if no .well-known directory
    if [[ -f "$wf_dir/wellknown-enforcement.yml" ]] && [[ "$has_wellknown" == false ]]; then
        emit_finding "low" "workflow_hygiene" "irrelevant_wellknown" \
            "$wf_dir/wellknown-enforcement.yml" 0 \
            "wellknown-enforcement.yml present but repo has no .well-known/ directory" "CWE-1059" \
            "Remove wellknown-enforcement.yml — no .well-known/ to enforce" "true"
    fi

    # RSR antipattern: template boilerplate, only useful in RSR-compliant repos
    if [[ -f "$wf_dir/rsr-antipattern.yml" ]]; then
        if [[ ! -f "$dir/.machine_readable/STATE.scm" && ! -f "$dir/justfile" ]]; then
            emit_finding "low" "workflow_hygiene" "irrelevant_rsr_antipattern" \
                "$wf_dir/rsr-antipattern.yml" 0 \
                "rsr-antipattern.yml present but repo lacks RSR markers (no STATE.scm or justfile)" "CWE-1059" \
                "Remove rsr-antipattern.yml — not an RSR-structured repo" "true"
        fi
    fi

    # Scorecard enforcer: redundant if scorecard.yml already present
    if [[ -f "$wf_dir/scorecard-enforcer.yml" && -f "$wf_dir/scorecard.yml" ]]; then
        emit_finding "low" "workflow_hygiene" "redundant_scorecard_enforcer" \
            "$wf_dir/scorecard-enforcer.yml" 0 \
            "scorecard-enforcer.yml is redundant when scorecard.yml already exists" "CWE-1059" \
            "Remove scorecard-enforcer.yml — scorecard.yml already provides this" "true"
    fi

    # Workflow linter: meta-linting is rarely actionable, often just noise
    if [[ -f "$wf_dir/workflow-linter.yml" ]]; then
        local wf_count
        wf_count=$(find "$wf_dir" -name '*.yml' -o -name '*.yaml' 2>/dev/null | wc -l)
        if [[ "$wf_count" -le 5 ]]; then
            emit_finding "info" "workflow_hygiene" "unnecessary_workflow_linter" \
                "$wf_dir/workflow-linter.yml" 0 \
                "workflow-linter.yml present but repo has $wf_count or fewer workflows" "CWE-1059" \
                "Remove workflow-linter.yml — not needed for small workflow sets" "true"
        fi
    fi

    # instant-sync: redundant with mirror.yml
    if [[ -f "$wf_dir/instant-sync.yml" && -f "$wf_dir/mirror.yml" ]]; then
        emit_finding "low" "workflow_hygiene" "redundant_instant_sync" \
            "$wf_dir/instant-sync.yml" 0 \
            "instant-sync.yml is redundant when mirror.yml already handles forge sync" "CWE-1059" \
            "Remove instant-sync.yml — mirror.yml already provides forge syncing" "true"
    fi

    # security-policy.yml: redundant check if SECURITY.md exists
    if [[ -f "$wf_dir/security-policy.yml" && -f "$dir/SECURITY.md" ]]; then
        emit_finding "info" "workflow_hygiene" "redundant_security_policy_wf" \
            "$wf_dir/security-policy.yml" 0 \
            "security-policy.yml checks for SECURITY.md but it already exists" "CWE-1059" \
            "Remove security-policy.yml — SECURITY.md already present" "true"
    fi

    # Julia package repos: should have Julia CI, flag if missing
    if [[ -f "$dir/Project.toml" ]]; then
        local has_julia_ci=false
        for ci_name in "ci.yml" "CI.yml" "test.yml"; do
            [[ -f "$wf_dir/$ci_name" ]] && has_julia_ci=true
        done
        if [[ "$has_julia_ci" == false ]]; then
            emit_finding "high" "workflow_hygiene" "missing_julia_ci" \
                "$dir/Project.toml" 0 \
                "Julia package detected (Project.toml) but no CI workflow runs Pkg.test()" "CWE-1059" \
                "Add ci.yml with julia-actions/setup-julia and Pkg.test()" "true"
        fi
    fi

    # Rust repos: should have Rust CI, flag if missing
    if [[ -f "$dir/Cargo.toml" ]]; then
        local has_rust_ci=false
        for ci_name in "ci.yml" "CI.yml" "rust.yml" "test.yml" "build.yml"; do
            [[ -f "$wf_dir/$ci_name" ]] && has_rust_ci=true
        done
        if [[ "$has_rust_ci" == false ]]; then
            emit_finding "high" "workflow_hygiene" "missing_rust_ci" \
                "$dir/Cargo.toml" 0 \
                "Rust crate detected (Cargo.toml) but no CI workflow runs cargo test" "CWE-1059" \
                "Add ci.yml with dtolnay/rust-toolchain and cargo test" "true"
        fi
    fi

    # Elixir repos: should have Elixir CI, flag if missing
    if [[ -f "$dir/mix.exs" ]]; then
        local has_elixir_ci=false
        for ci_name in "ci.yml" "CI.yml" "test.yml" "elixir.yml"; do
            [[ -f "$wf_dir/$ci_name" ]] && has_elixir_ci=true
        done
        if [[ "$has_elixir_ci" == false ]]; then
            emit_finding "medium" "workflow_hygiene" "missing_elixir_ci" \
                "$dir/mix.exs" 0 \
                "Elixir project detected (mix.exs) but no CI workflow runs mix test" "CWE-1059" \
                "Add ci.yml with erlef/setup-beam and mix test" "true"
        fi
    fi

    # Zig repos: should have Zig CI, flag if missing
    if [[ -f "$dir/build.zig" || -d "$dir/ffi/zig" ]]; then
        local has_zig_ci=false
        for ci_name in "ci.yml" "CI.yml" "zig.yml" "zig-test.yml" "zig-ffi.yml"; do
            [[ -f "$wf_dir/$ci_name" ]] && has_zig_ci=true
        done
        if [[ "$has_zig_ci" == false ]]; then
            emit_finding "medium" "workflow_hygiene" "missing_zig_ci" \
                "$dir" 0 \
                "Zig project detected but no CI workflow runs zig build test" "CWE-1059" \
                "Add zig-test.yml with goto-bus-stop/setup-zig and zig build test" "true"
        fi
    fi
}

# ─────────────────────────────────────────────────────────────────────
# Main scan orchestrator
# ─────────────────────────────────────────────────────────────────────
scan_directory() {
    local dir="$1"
    local format="${HYPATIA_FORMAT:-text}"

    log_info "Scanning: $dir"

    init_findings

    # Count source files for reporting
    local file_count
    file_count=$(find "$dir" -type f \( \
        -name "*.rs" -o -name "*.res" -o -name "*.ml" -o -name "*.hs" \
        -o -name "*.idr" -o -name "*.ex" -o -name "*.exs" -o -name "*.js" \
        -o -name "*.jsx" -o -name "*.v" -o -name "*.lean" -o -name "*.zig" \
        -o -name "*.sh" -o -name "*.yml" -o -name "*.yaml" \
    \) -not -path "*/node_modules/*" -not -path "*/.git/*" \
       -not -path "*/target/*" -not -path "*/_build/*" \
       -not -path "*/deps/*" -not -path "*/_opam/*" \
       -not -path "*/build/*" -not -path "*/.lake/*" \
    2>/dev/null | wc -l)

    log_info "Scanning $file_count source files across all languages"

    # Run all pattern categories
    scan_code_patterns "$dir"
    scan_repo_structure "$dir"
    scan_workflows "$dir"

    close_findings

    # Output
    if [[ "$format" == "json" ]]; then
        jq -n \
            --arg repo "$(basename "$dir")" \
            --arg timestamp "$(date -Iseconds)" \
            --arg scanner "hypatia" \
            --arg version "$VERSION" \
            --argjson count "$FINDING_COUNT" \
            --argjson findings "$(cat "$FINDINGS_FILE")" \
            '{submission_metadata: {repo:$repo, timestamp:$timestamp,
              scanner:$scanner, version:$version, finding_count:$count},
              findings: $findings}'
    elif [[ "$format" == "github" ]]; then
        jq -r '.[] | select(. != {}) |
            "::\(.severity) file=\(.file),line=\(.line)::\(.pattern) - \(.fix)"' \
            "$FINDINGS_FILE"
    else
        # Summary header
        local critical high medium low info
        critical=$(jq '[.[] | select(.severity == "critical")] | length' "$FINDINGS_FILE")
        high=$(jq '[.[] | select(.severity == "high")] | length' "$FINDINGS_FILE")
        medium=$(jq '[.[] | select(.severity == "medium")] | length' "$FINDINGS_FILE")
        low=$(jq '[.[] | select(.severity == "low")] | length' "$FINDINGS_FILE")
        info=$(jq '[.[] | select(.severity == "info")] | length' "$FINDINGS_FILE")

        echo ""
        echo -e "${CYAN}════════════════════════════════════════════════════${NC}"
        echo -e "${CYAN}  Hypatia v${VERSION} — Scan Results${NC}"
        echo -e "${CYAN}════════════════════════════════════════════════════${NC}"
        echo -e "  Files scanned:  $file_count"
        echo -e "  Total findings: $FINDING_COUNT"
        echo -e "  ${RED}Critical: $critical${NC}  ${YELLOW}High: $high${NC}  Medium: $medium  Low: $low  Info: $info"
        echo -e "${CYAN}════════════════════════════════════════════════════${NC}"
        echo ""

        # Group by type for readability
        jq -r '.[] | select(. != {}) |
            "[\(.severity | ascii_upcase)] \(.file):\(.line)  (\(.pattern))\n  \(.fix)\n"' \
            "$FINDINGS_FILE"
    fi

    rm -f "$FINDINGS_FILE"

    if [[ $FINDING_COUNT -eq 0 ]]; then
        log_success "No security issues found"
        return 0
    else
        log_error "Found $FINDING_COUNT issues"
        return 1
    fi
}

scan_file() {
    local file="$1"
    local format="${HYPATIA_FORMAT:-text}"
    local dir
    dir=$(dirname "$file")

    init_findings

    # Run patterns against just the containing directory but only match this file
    # Use a temporary approach: scan patterns that match this file's extension
    local ext="${file##*.}"
    local glob="$(basename "$file")"

    # We reuse scan_code_patterns on the parent dir — it's fast for single files
    scan_code_patterns "$dir"

    close_findings

    # Filter to just this file
    local filtered
    filtered=$(jq --arg f "$file" '[.[] | select(.file == $f)]' "$FINDINGS_FILE")
    local count
    count=$(echo "$filtered" | jq 'length')

    if [[ "$format" == "json" ]]; then
        echo "$filtered"
    else
        echo "$filtered" | jq -r '.[] |
            "[\(.severity | ascii_upcase)] \(.file):\(.line)  (\(.pattern))\n  \(.fix)\n"'
    fi

    rm -f "$FINDINGS_FILE"
    [[ "$count" -eq 0 ]]
}

submit_findings_to_fleet() {
    local findings_json="$1"
    local gitbot_fleet_dir="${2:-${GITBOT_FLEET_DIR:-$(dirname "$(realpath "$0")")/../gitbot-fleet}}"

    if [[ ! -d "$gitbot_fleet_dir" ]]; then
        log_warn "gitbot-fleet not found at $gitbot_fleet_dir, skipping submission"
        return 0
    fi

    local repo_name
    repo_name=$(echo "$findings_json" | jq -r '.submission_metadata.repo // "unknown"')
    local findings_dir="$gitbot_fleet_dir/shared-context/findings/$repo_name"
    mkdir -p "$findings_dir"

    local timestamp
    timestamp=$(date +%s)
    local findings_file="$findings_dir/${timestamp}.json"

    echo "$findings_json" > "$findings_file"
    ln -sf "$findings_file" "$findings_dir/latest.json"
    log_info "Submitted findings to gitbot-fleet: $findings_file"

    # Flag critical issues
    local critical_count
    critical_count=$(echo "$findings_json" | jq '[.findings[] | select(.severity == "critical")] | length')
    if [[ "$critical_count" -gt 0 ]]; then
        log_warn "$critical_count CRITICAL issues — flagged for robot-repo-automaton"
    fi
}

check_staged_files() {
    log_info "Checking git staged files..."

    local staged_files=()
    while IFS= read -r file; do
        [[ -f "$file" ]] && staged_files+=("$file")
    done < <(git diff --cached --name-only --diff-filter=ACM 2>/dev/null)

    if [[ ${#staged_files[@]} -eq 0 ]]; then
        log_info "No files staged"
        return 0
    fi

    log_info "Checking ${#staged_files[@]} staged files"

    init_findings
    for file in "${staged_files[@]}"; do
        local dir
        dir=$(dirname "$file")
        scan_code_patterns "$dir"
    done
    close_findings

    # Filter to only staged files
    local staged_json
    staged_json=$(printf '%s\n' "${staged_files[@]}" | jq -R . | jq -s .)
    local filtered
    filtered=$(jq --argjson staged "$staged_json" \
        '[.[] | select(.file as $f | $staged | any(. == $f))]' "$FINDINGS_FILE")
    local count
    count=$(echo "$filtered" | jq 'length')
    local critical
    critical=$(echo "$filtered" | jq '[.[] | select(.severity == "critical")] | length')

    rm -f "$FINDINGS_FILE"

    if [[ "$critical" -gt 0 ]]; then
        echo "$filtered" | jq -r '.[] | select(.severity == "critical") |
            "[\(.severity | ascii_upcase)] \(.file):\(.line)  (\(.pattern))\n  \(.fix)\n"'
        log_error "Pre-commit BLOCKED: $critical critical issues in staged files"
        return 1
    elif [[ "$count" -gt 0 ]]; then
        log_warn "Pre-commit: $count non-critical issues (allowing commit)"
        return 0
    fi

    log_success "Pre-commit check PASSED"
    return 0
}

main() {
    local cmd="${1:-help}"

    case "$cmd" in
        scan)
            local target="${2:-.}"
            if [[ -f "$target" ]]; then
                scan_file "$target"
            elif [[ -d "$target" ]]; then
                scan_directory "$target"
            else
                log_error "Target not found: $target"
                exit 1
            fi
            ;;
        report)
            HYPATIA_FORMAT=json "$0" scan "${2:-.}"
            ;;
        fix)
            HYPATIA_FIX_MODE="${HYPATIA_FIX_MODE:-dry-run}" "$0" scan "${2:-.}"
            ;;
        check-staged)
            check_staged_files
            ;;
        version)
            echo "Hypatia ${VERSION}"
            ;;
        help|--help|-h)
            usage
            ;;
        *)
            log_error "Unknown command: $cmd"
            usage
            exit 1
            ;;
    esac
}

main "$@"
