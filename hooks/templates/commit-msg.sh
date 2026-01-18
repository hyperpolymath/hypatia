#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
# cicd-hyper-a Commit Message Hook Template
# Validates commit message format
#
# This hook validates:
#   - Conventional commit format
#   - Subject line length
#   - Body formatting
#   - Required metadata
#   - Issue references

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
    exit 1
fi

# Commit message file is passed as first argument
COMMIT_MSG_FILE="$1"

if [ ! -f "$COMMIT_MSG_FILE" ]; then
    log_error "Commit message file not found: $COMMIT_MSG_FILE"
    exit 1
fi

init_error_count
print_header "commit-msg"

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Maximum subject line length
MAX_SUBJECT_LENGTH="${CICD_MAX_SUBJECT_LENGTH:-72}"

# Maximum body line length
MAX_BODY_LENGTH="${CICD_MAX_BODY_LENGTH:-100}"

# Require conventional commits format
REQUIRE_CONVENTIONAL="${CICD_REQUIRE_CONVENTIONAL:-true}"

# Require issue reference
REQUIRE_ISSUE="${CICD_REQUIRE_ISSUE:-false}"

# Conventional commit types
CONVENTIONAL_TYPES="feat|fix|docs|style|refactor|perf|test|build|ci|chore|revert"

# ==============================================================================
# READ COMMIT MESSAGE
# ==============================================================================

# Read commit message, ignoring comments
COMMIT_MSG=$(grep -v '^#' "$COMMIT_MSG_FILE" | sed '/^$/d')
SUBJECT=$(echo "$COMMIT_MSG" | head -1)
BODY=$(echo "$COMMIT_MSG" | tail -n +2 | sed '/^$/d')

if [ -z "$SUBJECT" ]; then
    add_error "Commit message is empty"
    check_errors_and_exit "commit-msg"
fi

log_info "Validating commit message..."
log_debug "Subject: $SUBJECT"

# ==============================================================================
# RULE: SUBJECT LINE LENGTH
# ==============================================================================

check_subject_length() {
    local len=${#SUBJECT}

    if [ "$len" -gt "$MAX_SUBJECT_LENGTH" ]; then
        add_error "Subject line too long: $len chars (max $MAX_SUBJECT_LENGTH)"
        log_error "Subject: $SUBJECT"
    fi

    if [ "$len" -lt 10 ]; then
        add_warning "Subject line too short: $len chars (min 10 recommended)"
    fi
}

# ==============================================================================
# RULE: CONVENTIONAL COMMITS
# ==============================================================================

check_conventional_format() {
    [ "$REQUIRE_CONVENTIONAL" = "true" ] || return 0

    # Pattern: type(scope): description
    # or: type: description
    # or: type!: description (breaking change)
    if ! echo "$SUBJECT" | grep -qE "^($CONVENTIONAL_TYPES)(\([a-z0-9_-]+\))?!?:[[:space:]].+"; then
        add_error "Commit message does not follow conventional format"
        log_error "Expected: <type>[(scope)][!]: <description>"
        log_error "Types: $CONVENTIONAL_TYPES"
        log_error "Examples:"
        log_error "  feat: add new feature"
        log_error "  fix(auth): resolve login issue"
        log_error "  chore!: breaking change"
    fi
}

# ==============================================================================
# RULE: SUBJECT LINE FORMAT
# ==============================================================================

check_subject_format() {
    # Should not end with period
    if echo "$SUBJECT" | grep -qE '\.$'; then
        add_warning "Subject line should not end with a period"
    fi

    # Should not be all uppercase
    if echo "$SUBJECT" | grep -qE '^[A-Z0-9[:space:][:punct:]]+$' && [ ${#SUBJECT} -gt 15 ]; then
        add_warning "Subject line should not be all uppercase"
    fi

    # Should start with lowercase after type prefix (for conventional commits)
    if [ "$REQUIRE_CONVENTIONAL" = "true" ]; then
        local desc=$(echo "$SUBJECT" | sed 's/^[^:]*:[[:space:]]*//')
        local first_char=$(echo "$desc" | cut -c1)
        if echo "$first_char" | grep -qE '^[A-Z]$'; then
            add_warning "Description should start with lowercase letter"
        fi
    fi

    # Check for common bad patterns
    if echo "$SUBJECT" | grep -qiE '^(wip|fixup|squash|temp|test|asdf|xxx|todo)$'; then
        add_warning "Subject appears to be a work-in-progress commit"
    fi
}

# ==============================================================================
# RULE: BODY FORMAT
# ==============================================================================

check_body_format() {
    [ -n "$BODY" ] || return 0

    # Check for blank line between subject and body
    local second_line=$(grep -v '^#' "$COMMIT_MSG_FILE" | sed -n '2p')
    if [ -n "$second_line" ]; then
        add_error "Missing blank line between subject and body"
    fi

    # Check body line lengths
    echo "$BODY" | while read -r line; do
        local len=${#line}
        if [ "$len" -gt "$MAX_BODY_LENGTH" ]; then
            add_warning "Body line exceeds $MAX_BODY_LENGTH chars: $len"
        fi
    done
}

# ==============================================================================
# RULE: ISSUE REFERENCE
# ==============================================================================

check_issue_reference() {
    [ "$REQUIRE_ISSUE" = "true" ] || return 0

    # Look for issue references in subject or body
    # Patterns: #123, GH-123, JIRA-123, Fixes #123, Closes #123
    if ! echo "$COMMIT_MSG" | grep -qE '(#[0-9]+|[A-Z]+-[0-9]+|Fixes[[:space:]]+#[0-9]+|Closes[[:space:]]+#[0-9]+|Resolves[[:space:]]+#[0-9]+)'; then
        add_warning "No issue reference found in commit message"
        log_warn "Consider adding: #123, JIRA-123, or 'Fixes #123'"
    fi
}

# ==============================================================================
# RULE: SIGN-OFF
# ==============================================================================

check_signoff() {
    # Check if sign-off is required
    [ "${CICD_REQUIRE_SIGNOFF:-false}" = "true" ] || return 0

    if ! echo "$COMMIT_MSG" | grep -qE '^Signed-off-by:[[:space:]]+.+[[:space:]]+<.+@.+>'; then
        add_error "Commit message missing required sign-off"
        log_error "Add sign-off with: git commit -s"
    fi
}

# ==============================================================================
# RULE: CO-AUTHOR
# ==============================================================================

check_coauthor() {
    # Validate co-author format if present
    if echo "$COMMIT_MSG" | grep -qE '^Co-[Aa]uthored-[Bb]y:'; then
        if ! echo "$COMMIT_MSG" | grep -qE '^Co-[Aa]uthored-[Bb]y:[[:space:]]+.+[[:space:]]+<.+@.+>'; then
            add_warning "Invalid Co-Authored-By format"
            log_warn "Expected: Co-Authored-By: Name <email@example.com>"
        fi
    fi
}

# ==============================================================================
# RULE: PROHIBITED CONTENT
# ==============================================================================

check_prohibited_content() {
    # Check for secrets in commit message
    if echo "$COMMIT_MSG" | grep -qE 'password[[:space:]]*[:=][[:space:]]*[A-Za-z0-9]'; then
        add_warning "Possible password in commit message"
    fi

    # Check for profanity (basic list)
    if echo "$COMMIT_MSG" | grep -qiE '\b(damn|shit|fuck|crap|hell)\b'; then
        add_warning "Commit message contains potentially inappropriate language"
    fi

    # Check for TODO/FIXME markers (may indicate incomplete work)
    if echo "$COMMIT_MSG" | grep -qiE '\b(TODO|FIXME|XXX|HACK)\b'; then
        add_warning "Commit message contains TODO/FIXME marker"
    fi
}

# ==============================================================================
# RUN ALL CHECKS
# ==============================================================================

run_all_checks() {
    check_subject_length
    check_conventional_format
    check_subject_format
    check_body_format
    check_issue_reference
    check_signoff
    check_coauthor
    check_prohibited_content
}

# ==============================================================================
# MAIN
# ==============================================================================

run_all_checks

check_errors_and_exit "commit-msg"
