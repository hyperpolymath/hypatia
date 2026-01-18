#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
# cicd-hyper-a Pre-Commit Hook Template
# Runs preventive rules before commits are created
#
# This hook checks:
#   - Language policy (RSR compliance)
#   - Secret detection
#   - Formatting validation
#   - SPDX license headers
#   - Workflow security

set -e

# ==============================================================================
# INITIALIZATION
# ==============================================================================

# Find script directory (resolves symlinks)
HOOK_DIR="$(cd "$(dirname "$0")" && pwd)"

# Source shared libraries
if [ -f "$HOOK_DIR/lib/common.sh" ]; then
    . "$HOOK_DIR/lib/common.sh"
else
    echo "[ERROR] Cannot find lib/common.sh - hook not properly installed"
    exit 1
fi

# Optional: source cache and API libraries
[ -f "$HOOK_DIR/lib/cache.sh" ] && . "$HOOK_DIR/lib/cache.sh"
[ -f "$HOOK_DIR/lib/api.sh" ] && . "$HOOK_DIR/lib/api.sh"

# Initialize hook
init_hook "pre-commit"

# ==============================================================================
# GET STAGED FILES
# ==============================================================================

STAGED_FILES=$(get_staged_files)

if [ -z "$STAGED_FILES" ]; then
    log_info "No staged files to check"
    exit 0
fi

log_info "Checking $(echo "$STAGED_FILES" | wc -l | tr -d ' ') staged file(s)"

# ==============================================================================
# RULE: RSR LANGUAGE POLICY
# ==============================================================================

check_language_policy() {
    log_info "Checking language policy..."

    # TypeScript check (banned - use ReScript)
    TS_FILES=$(echo "$STAGED_FILES" | grep -E '\.(ts|tsx)$' || true)
    if [ -n "$TS_FILES" ]; then
        add_error "TypeScript files not allowed per RSR policy"
        log_error "Use ReScript instead. Found files:"
        echo "$TS_FILES" | while read -r f; do
            log_error "  - $f"
        done
    fi

    # Go check (banned - use Rust)
    GO_FILES=$(echo "$STAGED_FILES" | grep -E '\.go$' || true)
    if [ -n "$GO_FILES" ]; then
        add_error "Go files not allowed per RSR policy"
        log_error "Use Rust instead. Found files:"
        echo "$GO_FILES" | while read -r f; do
            log_error "  - $f"
        done
    fi

    # Python check (banned except SaltStack - use Julia/Rust/ReScript)
    PY_FILES=$(echo "$STAGED_FILES" | grep -E '\.py$' | grep -v '^salt/' || true)
    if [ -n "$PY_FILES" ]; then
        add_error "Python files not allowed per RSR policy (except SaltStack)"
        log_error "Use Julia, Rust, or ReScript instead. Found files:"
        echo "$PY_FILES" | while read -r f; do
            log_error "  - $f"
        done
    fi

    # Node.js artifacts check (banned - use Deno)
    NODE_FILES=$(echo "$STAGED_FILES" | grep -E '(package-lock\.json|yarn\.lock|pnpm-lock\.yaml|node_modules/)' || true)
    if [ -n "$NODE_FILES" ]; then
        add_error "Node.js artifacts not allowed per RSR policy"
        log_error "Use Deno instead. Found files:"
        echo "$NODE_FILES" | while read -r f; do
            log_error "  - $f"
        done
    fi

    # Makefile check (banned - use Just/Guix/Nix)
    MAKE_FILES=$(echo "$STAGED_FILES" | grep -E '^Makefile$' || true)
    if [ -n "$MAKE_FILES" ]; then
        add_error "Makefile not allowed per RSR policy"
        log_error "Use Just, Guix, or Nix instead"
    fi
}

# ==============================================================================
# RULE: SECRET DETECTION
# ==============================================================================

check_secrets() {
    log_info "Scanning for secrets..."

    echo "$STAGED_FILES" | while read -r file; do
        [ -z "$file" ] && continue
        [ -f "$file" ] || continue
        is_binary "$file" && continue

        # GitHub Personal Access Token
        if grep -qE 'ghp_[a-zA-Z0-9]{36}' "$file" 2>/dev/null; then
            add_error "GitHub PAT detected in $file"
        fi

        # GitHub OAuth Token
        if grep -qE 'gho_[a-zA-Z0-9]{36}' "$file" 2>/dev/null; then
            add_error "GitHub OAuth token detected in $file"
        fi

        # GitHub Fine-grained PAT
        if grep -qE 'github_pat_[a-zA-Z0-9]{22}_[a-zA-Z0-9]{59}' "$file" 2>/dev/null; then
            add_error "GitHub fine-grained PAT detected in $file"
        fi

        # AWS Access Key
        if grep -qE 'AKIA[0-9A-Z]{16}' "$file" 2>/dev/null; then
            add_error "AWS Access Key detected in $file"
        fi

        # OpenAI API Key
        if grep -qE 'sk-[a-zA-Z0-9]{48}' "$file" 2>/dev/null; then
            add_error "OpenAI API key detected in $file"
        fi

        # Slack Token
        if grep -qE 'xox[baprs]-[a-zA-Z0-9-]{10,}' "$file" 2>/dev/null; then
            add_error "Slack token detected in $file"
        fi

        # Private Key
        if grep -qE '-----BEGIN[[:space:]]+(RSA|DSA|EC|OPENSSH|PGP)?[[:space:]]*PRIVATE[[:space:]]+KEY-----' "$file" 2>/dev/null; then
            add_error "Private key detected in $file"
        fi

        # Generic secret patterns
        if grep -qE '(api[_-]?key|secret|password|token|credential)[[:space:]]*[=:][[:space:]]*["'"'"'][A-Za-z0-9+/=_-]{20,}["'"'"']' "$file" 2>/dev/null; then
            add_warning "Possible hardcoded secret in $file"
        fi
    done
}

# ==============================================================================
# RULE: SPDX LICENSE HEADERS
# ==============================================================================

check_spdx_headers() {
    log_info "Checking SPDX headers..."

    echo "$STAGED_FILES" | while read -r file; do
        [ -z "$file" ] && continue
        [ -f "$file" ] || continue

        # Skip files that don't support comments
        case "$file" in
            *.json|*.png|*.jpg|*.jpeg|*.gif|*.ico|*.woff*|*.ttf|*.eot|*.pdf|*.lock|*.svg)
                continue
                ;;
        esac

        # Skip binary files
        is_binary "$file" && continue

        # Check for SPDX header
        if ! has_spdx_header "$file"; then
            add_warning "Missing SPDX header in $file"
        fi
    done
}

# ==============================================================================
# RULE: WORKFLOW SECURITY
# ==============================================================================

check_workflow_security() {
    WORKFLOW_FILES=$(echo "$STAGED_FILES" | grep -E '\.github/workflows/.*\.ya?ml$' || true)
    [ -z "$WORKFLOW_FILES" ] && return 0

    log_info "Checking workflow security..."

    echo "$WORKFLOW_FILES" | while read -r wf; do
        [ -z "$wf" ] && continue
        [ -f "$wf" ] || continue

        # Check for unpinned actions
        if has_unpinned_actions "$wf"; then
            add_error "Unpinned GitHub Action in $wf"
            log_error "Use SHA-pinned actions: uses: action/name@SHA # version"
            grep -n 'uses:' "$wf" | grep -E '@(v[0-9]+|main|master|latest)[[:space:]]*$' | while read -r line; do
                log_error "  $line"
            done
        fi

        # Check for missing permissions
        if ! has_permissions_declaration "$wf"; then
            add_warning "Missing permissions declaration in $wf"
            log_warn "Add 'permissions: read-all' at workflow level"
        fi

        # Check for missing SPDX header
        if ! has_spdx_header "$wf"; then
            add_warning "Missing SPDX header in $wf"
        fi

        # Validate YAML syntax
        if ! validate_yaml "$wf"; then
            add_error "Invalid YAML syntax in $wf"
        fi
    done
}

# ==============================================================================
# RULE: FORMATTING VALIDATION
# ==============================================================================

check_formatting() {
    log_info "Checking formatting..."

    # Check for trailing whitespace in text files
    echo "$STAGED_FILES" | while read -r file; do
        [ -z "$file" ] && continue
        [ -f "$file" ] || continue
        is_binary "$file" && continue

        # Check for trailing whitespace
        if grep -qE '[[:space:]]$' "$file" 2>/dev/null; then
            add_warning "Trailing whitespace in $file"
        fi

        # Check for tabs in specific file types
        case "$file" in
            *.py|*.yaml|*.yml)
                if grep -q '	' "$file" 2>/dev/null; then
                    add_warning "Tab characters in $file (use spaces)"
                fi
                ;;
        esac

        # Check for DOS line endings
        if file "$file" 2>/dev/null | grep -q 'CRLF'; then
            add_warning "Windows line endings (CRLF) in $file"
        fi
    done
}

# ==============================================================================
# RULE: LARGE FILE CHECK
# ==============================================================================

check_large_files() {
    MAX_SIZE="${CICD_MAX_FILE_SIZE:-1048576}"  # 1MB default

    log_info "Checking file sizes..."

    echo "$STAGED_FILES" | while read -r file; do
        [ -z "$file" ] && continue
        [ -f "$file" ] || continue

        size=$(wc -c < "$file" 2>/dev/null || echo 0)
        if [ "$size" -gt "$MAX_SIZE" ]; then
            _size_mb=$((size / 1024 / 1024))
            add_warning "Large file: $file (${_size_mb}MB)"
            log_warn "Consider using Git LFS for large files"
        fi
    done
}

# ==============================================================================
# RULE: CREDENTIAL FILE CHECK
# ==============================================================================

check_credential_files() {
    log_info "Checking for credential files..."

    CRED_FILES=$(echo "$STAGED_FILES" | grep -E '\.(env|pem|key|p12|pfx|jks|keystore)$|\.netrc$|credentials\.json$|secrets\.ya?ml$|\.htpasswd$' || true)

    if [ -n "$CRED_FILES" ]; then
        echo "$CRED_FILES" | while read -r f; do
            add_error "Credential file staged: $f"
            log_error "Add to .gitignore instead"
        done
    fi
}

# ==============================================================================
# RUN ALL CHECKS
# ==============================================================================

run_all_checks() {
    check_language_policy
    check_secrets
    check_spdx_headers
    check_workflow_security
    check_formatting
    check_large_files
    check_credential_files
}

# ==============================================================================
# MAIN
# ==============================================================================

run_all_checks

# Report to API if available
if [ "$(get_error_count)" -gt 0 ]; then
    report_hook_execution "pre-commit" "failed" "$(get_error_count) errors" 2>/dev/null || true
else
    report_hook_execution "pre-commit" "success" "" 2>/dev/null || true
fi

check_errors_and_exit "pre-commit"
