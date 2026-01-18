#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
# cicd-hyper-a Post-Receive Hook Template
# Server-side hook for triggering curative actions after push
#
# This hook:
#   - Notifies cicd-hyper-a engine of pushes
#   - Triggers appropriate curative rules
#   - Logs push events for analytics
#   - Sends webhooks to external services

set -e

# ==============================================================================
# INITIALIZATION
# ==============================================================================

HOOK_DIR="$(cd "$(dirname "$0")" && pwd)"

# Source shared libraries
if [ -f "$HOOK_DIR/lib/common.sh" ]; then
    . "$HOOK_DIR/lib/common.sh"
else
    echo "[ERROR] Cannot find lib/common.sh"
    # Don't exit on post-receive - just log error
fi

[ -f "$HOOK_DIR/lib/cache.sh" ] && . "$HOOK_DIR/lib/cache.sh"
[ -f "$HOOK_DIR/lib/api.sh" ] && . "$HOOK_DIR/lib/api.sh"

# Post-receive should not block, so just print header
color_print "bold" "cicd-hyper-a post-receive"

# ==============================================================================
# CONFIGURATION
# ==============================================================================

CICD_API_URL="${CICD_HYPER_A_API:-http://localhost:8080}"
CICD_API_TOKEN="${CICD_HYPER_A_TOKEN:-}"
CICD_WEBHOOKS="${CICD_WEBHOOKS:-}"

# ==============================================================================
# PROCESS REFS
# ==============================================================================

process_push() {
    local oldrev="$1"
    local newrev="$2"
    local refname="$3"

    # Skip if branch was deleted
    if [ "$newrev" = "0000000000000000000000000000000000000000" ]; then
        log_info "Branch deleted: $refname"
        return 0
    fi

    # Extract branch name
    branch=$(echo "$refname" | sed 's|refs/heads/||')
    repo_path=$(pwd)
    repo_name=$(basename "$repo_path" .git)

    log_info "Processing push to $branch in $repo_name"

    # Get list of changed files
    if [ "$oldrev" = "0000000000000000000000000000000000000000" ]; then
        files=$(git ls-tree -r --name-only "$newrev" 2>/dev/null)
    else
        files=$(git diff --name-only "$oldrev" "$newrev" 2>/dev/null)
    fi

    # ==================================================================
    # DETECT CHANGE TYPES
    # ==================================================================

    workflow_changed="false"
    deps_changed="false"
    security_changed="false"
    docs_changed="false"
    source_changed="false"

    # Workflow changes
    if echo "$files" | grep -qE '\.github/workflows/'; then
        workflow_changed="true"
        log_info "  - Workflow files changed"
    fi

    # Dependency changes
    if echo "$files" | grep -qE '(Cargo\.toml|Cargo\.lock|deno\.json|package\.json|rescript\.json|cabal|mix\.exs|gleam\.toml)'; then
        deps_changed="true"
        log_info "  - Dependencies changed"
    fi

    # Security policy changes
    if echo "$files" | grep -qE '(SECURITY\.md|\.github/SECURITY\.md|\.github/dependabot\.yml)'; then
        security_changed="true"
        log_info "  - Security policy changed"
    fi

    # Documentation changes
    if echo "$files" | grep -qE '\.(md|adoc|rst|txt)$|^docs/|^README'; then
        docs_changed="true"
        log_info "  - Documentation changed"
    fi

    # Source code changes
    if echo "$files" | grep -qE '\.(rs|res|js|ts|hs|ml|ex|gleam|jl|sh)$|^src/'; then
        source_changed="true"
        log_info "  - Source code changed"
    fi

    # ==================================================================
    # NOTIFY CICD-HYPER-A ENGINE
    # ==================================================================

    if [ -n "$CICD_API_TOKEN" ] && [ -n "$CICD_API_URL" ]; then
        # Build file list JSON
        files_json=$(echo "$files" | head -100 | while read -r f; do
            [ -n "$f" ] && printf '"%s",' "$f"
        done | sed 's/,$//')

        payload=$(cat <<EOF
{
    "event": "push",
    "repository": "$repo_name",
    "branch": "$branch",
    "old_commit": "$oldrev",
    "new_commit": "$newrev",
    "ref": "$refname",
    "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
    "changes": {
        "workflow": $workflow_changed,
        "dependencies": $deps_changed,
        "security": $security_changed,
        "documentation": $docs_changed,
        "source": $source_changed
    },
    "files": [$files_json]
}
EOF
)

        api_post "/hooks/post-receive" "$payload" >/dev/null 2>&1 || \
            log_warn "Failed to notify cicd-hyper-a engine"
    fi

    # ==================================================================
    # TRIGGER CURATIVE ACTIONS
    # ==================================================================

    if [ "$workflow_changed" = "true" ]; then
        trigger_workflow_analysis "$newrev" "$files"
    fi

    if [ "$deps_changed" = "true" ]; then
        trigger_dependency_scan "$newrev"
    fi

    if [ "$security_changed" = "true" ]; then
        trigger_security_review "$newrev"
    fi

    # ==================================================================
    # SEND WEBHOOKS
    # ==================================================================

    if [ -n "$CICD_WEBHOOKS" ]; then
        send_webhooks "$payload"
    fi
}

# ==============================================================================
# CURATIVE ACTION TRIGGERS
# ==============================================================================

trigger_workflow_analysis() {
    local commit="$1"
    local files="$2"

    log_info "Triggering workflow analysis..."

    # Check for unpinned actions
    workflow_files=$(echo "$files" | grep -E '\.github/workflows/.*\.ya?ml$' || true)

    for wf in $workflow_files; do
        if [ -f "$wf" ]; then
            unpinned=$(git show "$commit:$wf" 2>/dev/null | grep -E 'uses:\s+[^@]+@(v[0-9]+|main|master|latest)\s*$' || true)
            if [ -n "$unpinned" ]; then
                log_warn "Found unpinned actions in $wf"
                log_info "Run 'cicd-hyper-a fix unpinned-actions --file $wf' to auto-pin"
            fi
        fi
    done

    # Queue workflow linting job
    if [ -n "$CICD_API_TOKEN" ]; then
        api_post "/jobs/queue" '{"type": "workflow-lint", "commit": "'"$commit"'"}' >/dev/null 2>&1 || true
    fi
}

trigger_dependency_scan() {
    local commit="$1"

    log_info "Triggering dependency scan..."

    # Queue dependency audit job
    if [ -n "$CICD_API_TOKEN" ]; then
        api_post "/jobs/queue" '{"type": "dependency-audit", "commit": "'"$commit"'"}' >/dev/null 2>&1 || true
    fi

    # Local quick check if cargo is available
    if [ -f "Cargo.toml" ] && command -v cargo >/dev/null 2>&1; then
        if cargo audit --quiet 2>/dev/null | grep -q "Vulnerability"; then
            log_warn "Vulnerabilities detected in dependencies"
            log_info "Run 'cargo audit' for details"
        fi
    fi
}

trigger_security_review() {
    local commit="$1"

    log_info "Triggering security review..."

    # Queue security review job
    if [ -n "$CICD_API_TOKEN" ]; then
        api_post "/jobs/queue" '{"type": "security-review", "commit": "'"$commit"'"}' >/dev/null 2>&1 || true
    fi
}

# ==============================================================================
# WEBHOOK DISTRIBUTION
# ==============================================================================

send_webhooks() {
    local payload="$1"

    # Parse webhook URLs (comma-separated)
    echo "$CICD_WEBHOOKS" | tr ',' '\n' | while read -r url; do
        [ -z "$url" ] && continue
        send_webhook "$url" "$payload" || log_warn "Webhook failed: $url"
    done
}

# ==============================================================================
# LOGGING
# ==============================================================================

log_push_event() {
    local oldrev="$1"
    local newrev="$2"
    local refname="$3"

    # Log to file if directory exists
    LOG_DIR="${CICD_LOG_DIR:-/var/log/cicd-hyper-a}"
    if [ -d "$LOG_DIR" ]; then
        echo "$(date -u +%Y-%m-%dT%H:%M:%SZ) push $refname $oldrev $newrev" >> "$LOG_DIR/post-receive.log"
    fi
}

# ==============================================================================
# MAIN
# ==============================================================================

# Read refs from stdin
while read -r oldrev newrev refname; do
    [ -z "$refname" ] && continue

    log_push_event "$oldrev" "$newrev" "$refname"
    process_push "$oldrev" "$newrev" "$refname"
done

log_success "Post-receive processing complete"
exit 0
