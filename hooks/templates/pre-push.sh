#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
# cicd-hyper-a Pre-Push Hook Template
# Validates commits before pushing to remote
#
# This hook checks:
#   - Force push protection for protected branches
#   - Commit signature verification
#   - Large file detection
#   - Deep credential scan
#   - CI simulation (lightweight)

set -e

# ==============================================================================
# INITIALIZATION
# ==============================================================================

HOOK_DIR="$(cd "$(dirname "$0")" && pwd)"

# Source shared libraries
if [ -f "$HOOK_DIR/lib/common.sh" ]; then
    . "$HOOK_DIR/lib/common.sh"
else
    echo "[ERROR] Cannot find lib/common.sh - hook not properly installed"
    exit 1
fi

[ -f "$HOOK_DIR/lib/cache.sh" ] && . "$HOOK_DIR/lib/cache.sh"
[ -f "$HOOK_DIR/lib/api.sh" ] && . "$HOOK_DIR/lib/api.sh"

init_hook "pre-push"

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Maximum file size (10MB default)
MAX_FILE_SIZE="${CICD_MAX_FILE_SIZE:-10485760}"

# Protected branch patterns
PROTECTED_BRANCHES="${CICD_PROTECTED_BRANCHES:-main|master|production|prod|release/.*}"

# Require signed commits
REQUIRE_SIGNED="${CICD_REQUIRE_SIGNED:-false}"

# ==============================================================================
# READ PUSH REFS
# ==============================================================================

# pre-push receives: <local ref> <local sha> <remote ref> <remote sha>
# on stdin for each ref being pushed

TOTAL_COMMITS=0
PUSH_REFS=""

while read -r local_ref local_sha remote_ref remote_sha; do
    # Skip deleted branches
    if [ "$local_sha" = "0000000000000000000000000000000000000000" ]; then
        log_info "Skipping deleted branch: $local_ref"
        continue
    fi

    PUSH_REFS="$PUSH_REFS$local_ref $local_sha $remote_ref $remote_sha\n"

    # Count commits being pushed
    if [ "$remote_sha" = "0000000000000000000000000000000000000000" ]; then
        # New branch - all commits are new
        _count=$(git rev-list --count "$local_sha" 2>/dev/null || echo 0)
    else
        _count=$(git rev-list --count "$remote_sha..$local_sha" 2>/dev/null || echo 0)
    fi
    TOTAL_COMMITS=$((TOTAL_COMMITS + _count))
done

if [ -z "$PUSH_REFS" ]; then
    log_info "No refs to push"
    exit 0
fi

log_info "Pushing $TOTAL_COMMITS commit(s)"

# ==============================================================================
# RULE: FORCE PUSH PROTECTION
# ==============================================================================

check_force_push() {
    log_info "Checking force push protection..."

    echo "$PUSH_REFS" | while read -r local_ref local_sha remote_ref remote_sha; do
        [ -z "$local_ref" ] && continue

        # Extract branch name from ref
        branch=$(echo "$remote_ref" | sed 's|refs/heads/||')

        # Check if protected branch
        if echo "$branch" | grep -qE "^($PROTECTED_BRANCHES)$"; then
            # Check if this is a force push
            if [ "$remote_sha" != "0000000000000000000000000000000000000000" ]; then
                if ! git merge-base --is-ancestor "$remote_sha" "$local_sha" 2>/dev/null; then
                    add_error "Force push to protected branch '$branch' is not allowed"
                    log_error "Use a regular merge or rebase workflow instead"
                fi
            fi
        fi
    done
}

# ==============================================================================
# RULE: SIGNED COMMITS
# ==============================================================================

check_signed_commits() {
    [ "$REQUIRE_SIGNED" = "true" ] || return 0

    log_info "Verifying commit signatures..."

    echo "$PUSH_REFS" | while read -r local_ref local_sha remote_ref remote_sha; do
        [ -z "$local_ref" ] && continue

        # Get commit range
        if [ "$remote_sha" = "0000000000000000000000000000000000000000" ]; then
            _range="$local_sha"
        else
            _range="$remote_sha..$local_sha"
        fi

        # Check each commit
        git rev-list "$_range" 2>/dev/null | while read -r commit; do
            # Check for GPG signature
            _sig=$(git log -1 --format='%G?' "$commit" 2>/dev/null)
            case "$_sig" in
                G|U)
                    # Good or untrusted but valid signature
                    ;;
                N)
                    add_error "Commit $commit is not signed"
                    log_error "Sign commits with: git commit -S"
                    ;;
                B|E|X|Y|R)
                    add_warning "Commit $commit has invalid signature (status: $_sig)"
                    ;;
            esac
        done
    done
}

# ==============================================================================
# RULE: LARGE FILE DETECTION
# ==============================================================================

check_large_files() {
    log_info "Checking for large files..."

    echo "$PUSH_REFS" | while read -r local_ref local_sha remote_ref remote_sha; do
        [ -z "$local_ref" ] && continue

        # Get files changed in push
        if [ "$remote_sha" = "0000000000000000000000000000000000000000" ]; then
            _files=$(git ls-tree -r --name-only "$local_sha" 2>/dev/null)
        else
            _files=$(git diff --name-only "$remote_sha" "$local_sha" 2>/dev/null)
        fi

        echo "$_files" | while read -r file; do
            [ -z "$file" ] && continue
            [ -f "$file" ] || continue

            size=$(wc -c < "$file" 2>/dev/null || echo 0)
            if [ "$size" -gt "$MAX_FILE_SIZE" ]; then
                _size_mb=$((size / 1024 / 1024))
                add_error "File exceeds size limit: $file (${_size_mb}MB > $((MAX_FILE_SIZE / 1024 / 1024))MB)"
                log_error "Use Git LFS for large files: git lfs track '$file'"
            fi
        done
    done
}

# ==============================================================================
# RULE: DEEP CREDENTIAL SCAN
# ==============================================================================

check_credentials() {
    log_info "Performing deep credential scan..."

    echo "$PUSH_REFS" | while read -r local_ref local_sha remote_ref remote_sha; do
        [ -z "$local_ref" ] && continue

        # Get files changed in push
        if [ "$remote_sha" = "0000000000000000000000000000000000000000" ]; then
            _files=$(git ls-tree -r --name-only "$local_sha" 2>/dev/null)
        else
            _files=$(git diff --name-only "$remote_sha" "$local_sha" 2>/dev/null)
        fi

        echo "$_files" | while read -r file; do
            [ -z "$file" ] && continue
            [ -f "$file" ] || continue
            is_binary "$file" && continue

            # Check for private keys
            if grep -qE '-----BEGIN[[:space:]]+(RSA|DSA|EC|OPENSSH|PGP)?[[:space:]]*PRIVATE[[:space:]]+KEY-----' "$file" 2>/dev/null; then
                add_error "Private key detected in $file"
            fi

            # Check for credential files by name
            case "$file" in
                *.env|.env.*|*.pem|*.key|*.p12|*.pfx|.netrc|.htpasswd)
                    add_error "Credential file detected: $file"
                    log_error "Add to .gitignore instead"
                    ;;
                credentials*.json|*secrets.yaml|*secrets.yml)
                    add_error "Secrets file detected: $file"
                    log_error "Add to .gitignore instead"
                    ;;
            esac
        done
    done
}

# ==============================================================================
# RULE: WORKFLOW SECURITY (DEEP CHECK)
# ==============================================================================

check_workflow_security() {
    log_info "Checking workflow security..."

    echo "$PUSH_REFS" | while read -r local_ref local_sha remote_ref remote_sha; do
        [ -z "$local_ref" ] && continue

        # Get workflow files changed
        if [ "$remote_sha" = "0000000000000000000000000000000000000000" ]; then
            _files=$(git ls-tree -r --name-only "$local_sha" 2>/dev/null | grep -E '\.github/workflows/.*\.ya?ml$' || true)
        else
            _files=$(git diff --name-only "$remote_sha" "$local_sha" 2>/dev/null | grep -E '\.github/workflows/.*\.ya?ml$' || true)
        fi

        echo "$_files" | while read -r wf; do
            [ -z "$wf" ] && continue
            [ -f "$wf" ] || continue

            # Check for dangerous pull_request_target patterns
            if grep -qE 'pull_request_target:' "$wf" 2>/dev/null; then
                if grep -qE 'checkout.*\$\{\{[[:space:]]*github\.event\.pull_request' "$wf" 2>/dev/null; then
                    add_warning "Dangerous pull_request_target pattern in $wf"
                    log_warn "This pattern can allow arbitrary code execution from PRs"
                fi
            fi

            # Check for workflow_dispatch with unvalidated inputs
            if grep -qE 'workflow_dispatch:' "$wf" 2>/dev/null; then
                if grep -qE 'run:.*\$\{\{[[:space:]]*github\.event\.inputs\.' "$wf" 2>/dev/null; then
                    add_warning "Unvalidated workflow_dispatch input used in run: in $wf"
                fi
            fi

            # Check for dangerous issue_comment triggers
            if grep -qE 'issue_comment:' "$wf" 2>/dev/null; then
                if grep -qE 'run:.*\$\{\{[[:space:]]*github\.event\.comment\.body' "$wf" 2>/dev/null; then
                    add_error "Dangerous issue_comment body injection in $wf"
                fi
            fi
        done
    done
}

# ==============================================================================
# RULE: CI SIMULATION
# ==============================================================================

simulate_ci() {
    # Only run if explicitly enabled
    [ "${CICD_SIMULATE_CI:-false}" = "true" ] || return 0

    log_info "Running CI simulation..."

    # Check for Cargo.toml (Rust project)
    if [ -f "Cargo.toml" ]; then
        log_info "  Running cargo check..."
        if ! cargo check --quiet 2>/dev/null; then
            add_error "Rust compilation failed"
        fi
    fi

    # Check for deno.json (Deno project)
    if [ -f "deno.json" ] || [ -f "deno.jsonc" ]; then
        log_info "  Running deno check..."
        if command -v deno >/dev/null 2>&1; then
            if ! deno check . 2>/dev/null; then
                add_warning "Deno type check failed"
            fi
        fi
    fi

    # Check for rescript.json (ReScript project)
    if [ -f "rescript.json" ]; then
        log_info "  Running rescript build check..."
        if command -v rescript >/dev/null 2>&1; then
            if ! rescript build 2>/dev/null; then
                add_warning "ReScript build failed"
            fi
        fi
    fi

    # Check for cabal file (Haskell project)
    if [ -f "*.cabal" ] 2>/dev/null; then
        log_info "  Running cabal check..."
        if command -v cabal >/dev/null 2>&1; then
            if ! cabal check 2>/dev/null; then
                add_warning "Cabal check failed"
            fi
        fi
    fi
}

# ==============================================================================
# RULE: BRANCH NAME VALIDATION
# ==============================================================================

check_branch_names() {
    log_info "Validating branch names..."

    echo "$PUSH_REFS" | while read -r local_ref local_sha remote_ref remote_sha; do
        [ -z "$local_ref" ] && continue

        branch=$(echo "$local_ref" | sed 's|refs/heads/||')

        # Check for spaces in branch name
        if echo "$branch" | grep -q ' '; then
            add_error "Branch name contains spaces: $branch"
        fi

        # Check for special characters
        if echo "$branch" | grep -qE '[^a-zA-Z0-9/_.-]'; then
            add_warning "Branch name contains special characters: $branch"
        fi

        # Check branch naming conventions
        case "$branch" in
            feature/*|fix/*|bugfix/*|hotfix/*|release/*|chore/*|docs/*|test/*|refactor/*)
                # Good naming convention
                ;;
            main|master|develop|staging)
                # Standard branches
                ;;
            *)
                add_warning "Consider using conventional branch naming: feature/*, fix/*, etc."
                ;;
        esac
    done
}

# ==============================================================================
# RUN ALL CHECKS
# ==============================================================================

run_all_checks() {
    check_force_push
    check_signed_commits
    check_large_files
    check_credentials
    check_workflow_security
    check_branch_names
    simulate_ci
}

# ==============================================================================
# MAIN
# ==============================================================================

# Parse push refs from stdin and run checks
echo "$PUSH_REFS" > /dev/null  # Ensure we have refs

run_all_checks

# Report to API if available
if [ "$(get_error_count)" -gt 0 ]; then
    report_hook_execution "pre-push" "failed" "$(get_error_count) errors" 2>/dev/null || true
else
    report_hook_execution "pre-push" "success" "" 2>/dev/null || true
fi

check_errors_and_exit "pre-push"
