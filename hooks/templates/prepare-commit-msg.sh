#!/bin/sh
# SPDX-License-Identifier: PMPL-1.0-or-later
# cicd-hyper-a Prepare Commit Message Hook Template
# Auto-adds metadata to commit messages
#
# This hook adds:
#   - Branch-based prefix (issue/ticket number)
#   - Template for conventional commits
#   - Co-author information
#   - Metadata footer

set -e

# ==============================================================================
# INITIALIZATION
# ==============================================================================

HOOK_DIR="$(cd "$(dirname "$0")" && pwd)"

# Source shared libraries (optional for this hook)
if [ -f "$HOOK_DIR/lib/common.sh" ]; then
    . "$HOOK_DIR/lib/common.sh"
fi

# Arguments
COMMIT_MSG_FILE="$1"
COMMIT_SOURCE="${2:-}"  # message, template, merge, squash, commit
COMMIT_SHA="${3:-}"

# Exit early for merge commits and amends unless configured otherwise
case "$COMMIT_SOURCE" in
    merge)
        [ "${CICD_MODIFY_MERGE:-false}" = "true" ] || exit 0
        ;;
    squash)
        [ "${CICD_MODIFY_SQUASH:-false}" = "true" ] || exit 0
        ;;
    commit)
        # Amend - usually don't modify
        [ "${CICD_MODIFY_AMEND:-false}" = "true" ] || exit 0
        ;;
esac

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Add conventional commit template
ADD_TEMPLATE="${CICD_ADD_TEMPLATE:-true}"

# Add issue number from branch name
ADD_ISSUE_FROM_BRANCH="${CICD_ADD_ISSUE_FROM_BRANCH:-true}"

# Add co-author from environment
ADD_COAUTHOR="${CICD_ADD_COAUTHOR:-false}"
COAUTHOR_NAME="${CICD_COAUTHOR_NAME:-}"
COAUTHOR_EMAIL="${CICD_COAUTHOR_EMAIL:-}"

# Add metadata footer
ADD_METADATA="${CICD_ADD_METADATA:-false}"

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

get_branch_name() {
    git symbolic-ref --short HEAD 2>/dev/null || echo ""
}

get_issue_from_branch() {
    local branch=$(get_branch_name)
    local issue=""

    # Pattern: feature/JIRA-123-description
    issue=$(echo "$branch" | grep -oE '[A-Z]+-[0-9]+' | head -1)
    [ -n "$issue" ] && echo "$issue" && return

    # Pattern: feature/123-description
    issue=$(echo "$branch" | grep -oE '/[0-9]+' | head -1 | tr -d '/')
    [ -n "$issue" ] && echo "#$issue" && return

    # Pattern: issue-123
    issue=$(echo "$branch" | grep -oE 'issue-[0-9]+' | head -1 | sed 's/issue-/#/')
    [ -n "$issue" ] && echo "$issue" && return

    echo ""
}

get_type_from_branch() {
    local branch=$(get_branch_name)

    case "$branch" in
        feature/*|feat/*)  echo "feat" ;;
        fix/*|bugfix/*|hotfix/*) echo "fix" ;;
        docs/*|doc/*) echo "docs" ;;
        style/*) echo "style" ;;
        refactor/*) echo "refactor" ;;
        perf/*|performance/*) echo "perf" ;;
        test/*|tests/*) echo "test" ;;
        build/*) echo "build" ;;
        ci/*|cicd/*) echo "ci" ;;
        chore/*) echo "chore" ;;
        revert/*) echo "revert" ;;
        *) echo "" ;;
    esac
}

get_scope_from_branch() {
    local branch=$(get_branch_name)

    # Pattern: type/scope/description or type/scope-description
    echo "$branch" | sed -E 's|^[^/]+/([^/]+)/.*|\1|; s|^[^/]+/([a-z]+)-.*|\1|' | grep -v '/' || echo ""
}

# ==============================================================================
# TEMPLATE GENERATION
# ==============================================================================

add_template() {
    [ "$ADD_TEMPLATE" = "true" ] || return 0

    local current_msg=$(cat "$COMMIT_MSG_FILE")

    # Don't add template if message already exists (except for comments)
    local content=$(echo "$current_msg" | grep -v '^#' | sed '/^$/d')
    if [ -n "$content" ]; then
        return 0
    fi

    local type=$(get_type_from_branch)
    local scope=$(get_scope_from_branch)
    local issue=$(get_issue_from_branch)

    # Build prefix
    local prefix=""
    if [ -n "$type" ]; then
        if [ -n "$scope" ]; then
            prefix="$type($scope): "
        else
            prefix="$type: "
        fi
    fi

    # Build template
    cat > "$COMMIT_MSG_FILE" << EOF
$prefix
# Write a short description above (max 72 chars)
#
# Body: Explain what and why (wrap at 100 chars)
#
EOF

    if [ -n "$issue" ]; then
        echo "# Issue: $issue" >> "$COMMIT_MSG_FILE"
    fi

    cat >> "$COMMIT_MSG_FILE" << EOF
#
# Conventional commit types:
#   feat:     New feature
#   fix:      Bug fix
#   docs:     Documentation only
#   style:    Formatting, no code change
#   refactor: Code restructuring
#   perf:     Performance improvement
#   test:     Adding/updating tests
#   build:    Build system changes
#   ci:       CI/CD changes
#   chore:    Maintenance tasks
#   revert:   Reverting changes
#
# Add ! after type for breaking changes: feat!: breaking change
EOF
}

# ==============================================================================
# ISSUE INJECTION
# ==============================================================================

add_issue_reference() {
    [ "$ADD_ISSUE_FROM_BRANCH" = "true" ] || return 0

    local issue=$(get_issue_from_branch)
    [ -z "$issue" ] && return 0

    local current_msg=$(cat "$COMMIT_MSG_FILE")

    # Check if issue is already referenced
    if echo "$current_msg" | grep -qE "(^|[[:space:]])$issue([[:space:]]|$)"; then
        return 0
    fi

    # Add issue reference to subject line or as footer
    if echo "$current_msg" | head -1 | grep -qE '^[a-z]+(\([a-z]+\))?:'; then
        # Conventional commit - add to end of subject if short enough
        local subject=$(echo "$current_msg" | head -1)
        local new_subject="$subject ($issue)"
        if [ ${#new_subject} -le 72 ]; then
            sed -i "1s/$/ ($issue)/" "$COMMIT_MSG_FILE"
        else
            # Add as footer instead
            echo "" >> "$COMMIT_MSG_FILE"
            echo "Refs: $issue" >> "$COMMIT_MSG_FILE"
        fi
    else
        # Add as footer
        echo "" >> "$COMMIT_MSG_FILE"
        echo "Refs: $issue" >> "$COMMIT_MSG_FILE"
    fi
}

# ==============================================================================
# CO-AUTHOR INJECTION
# ==============================================================================

add_coauthor() {
    [ "$ADD_COAUTHOR" = "true" ] || return 0
    [ -n "$COAUTHOR_NAME" ] || return 0
    [ -n "$COAUTHOR_EMAIL" ] || return 0

    local current_msg=$(cat "$COMMIT_MSG_FILE")

    # Check if co-author already present
    if echo "$current_msg" | grep -qi "Co-Authored-By:.*$COAUTHOR_NAME"; then
        return 0
    fi

    # Add co-author footer
    echo "" >> "$COMMIT_MSG_FILE"
    echo "Co-Authored-By: $COAUTHOR_NAME <$COAUTHOR_EMAIL>" >> "$COMMIT_MSG_FILE"
}

# ==============================================================================
# METADATA INJECTION
# ==============================================================================

add_metadata() {
    [ "$ADD_METADATA" = "true" ] || return 0

    local branch=$(get_branch_name)
    local timestamp=$(date -u +%Y-%m-%dT%H:%M:%SZ)

    # Add metadata as git notes or trailer
    echo "" >> "$COMMIT_MSG_FILE"
    echo "---" >> "$COMMIT_MSG_FILE"
    echo "Branch: $branch" >> "$COMMIT_MSG_FILE"
    echo "Timestamp: $timestamp" >> "$COMMIT_MSG_FILE"

    # Add CI context if available
    [ -n "${CI:-}" ] && echo "CI: true" >> "$COMMIT_MSG_FILE"
    [ -n "${GITHUB_ACTIONS:-}" ] && echo "Runner: github-actions" >> "$COMMIT_MSG_FILE"
}

# ==============================================================================
# MAIN
# ==============================================================================

# Only run for new commits (not message-only edits)
if [ "$COMMIT_SOURCE" = "message" ]; then
    exit 0
fi

# Generate template if needed
add_template

# Add issue reference from branch
add_issue_reference

# Add co-author if configured
add_coauthor

# Add metadata footer if configured
add_metadata

exit 0
