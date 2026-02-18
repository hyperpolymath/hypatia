#!/bin/sh
# SPDX-License-Identifier: PMPL-1.0-or-later
# cicd-hyper-a Git Hooks - Common Functions
# Shared utilities for all hook templates
#
# This file is designed to be sourced by hook scripts:
#   . "$(dirname "$0")/lib/common.sh"

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Respect NO_COLOR environment variable
# https://no-color.org/
if [ -n "${NO_COLOR:-}" ] || [ "${TERM:-}" = "dumb" ]; then
    CICD_USE_COLOR=0
else
    CICD_USE_COLOR=1
fi

# Default configuration paths
CICD_CONFIG_FILE="${CICD_CONFIG_FILE:-${GIT_DIR:-$(git rev-parse --git-dir 2>/dev/null)}/hooks/config.yml}"
CICD_CACHE_DIR="${CICD_CACHE_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/cicd-hyper-a}"
CICD_API_URL="${CICD_HYPER_A_API:-http://localhost:8080}"
CICD_API_TOKEN="${CICD_HYPER_A_TOKEN:-}"

# Exit codes
EXIT_SUCCESS=0
EXIT_FAILURE=1
EXIT_SKIP=2

# ==============================================================================
# COLOR OUTPUT
# ==============================================================================

# Print colored output if colors are enabled
# Usage: color_print "red" "Error message"
color_print() {
    _color="$1"
    shift
    _msg="$*"

    if [ "$CICD_USE_COLOR" = "1" ]; then
        case "$_color" in
            red)     printf '\033[0;31m%s\033[0m\n' "$_msg" ;;
            green)   printf '\033[0;32m%s\033[0m\n' "$_msg" ;;
            yellow)  printf '\033[1;33m%s\033[0m\n' "$_msg" ;;
            blue)    printf '\033[0;34m%s\033[0m\n' "$_msg" ;;
            magenta) printf '\033[0;35m%s\033[0m\n' "$_msg" ;;
            cyan)    printf '\033[0;36m%s\033[0m\n' "$_msg" ;;
            bold)    printf '\033[1m%s\033[0m\n' "$_msg" ;;
            *)       printf '%s\n' "$_msg" ;;
        esac
    else
        printf '%s\n' "$_msg"
    fi
}

# Convenience functions
log_info() { color_print "cyan" "[INFO] $*"; }
log_warn() { color_print "yellow" "[WARN] $*"; }
log_error() { color_print "red" "[ERROR] $*"; }
log_success() { color_print "green" "[OK] $*"; }
log_debug() { [ "${CICD_DEBUG:-0}" = "1" ] && color_print "magenta" "[DEBUG] $*" || true; }

# Print a header for hook output
print_header() {
    _title="$1"
    color_print "bold" "===================================="
    color_print "bold" "cicd-hyper-a: $_title"
    color_print "bold" "===================================="
}

# ==============================================================================
# FILE UTILITIES
# ==============================================================================

# Get list of staged files
# Usage: staged_files=$(get_staged_files)
get_staged_files() {
    git diff --cached --name-only --diff-filter=ACM 2>/dev/null
}

# Get list of files changed between commits
# Usage: changed_files=$(get_changed_files "old_sha" "new_sha")
get_changed_files() {
    _old="$1"
    _new="$2"

    if [ "$_old" = "0000000000000000000000000000000000000000" ]; then
        git ls-tree -r --name-only "$_new" 2>/dev/null
    else
        git diff --name-only "$_old" "$_new" 2>/dev/null
    fi
}

# Check if file is binary
# Usage: if is_binary "file.bin"; then ...
is_binary() {
    _file="$1"
    [ -f "$_file" ] || return 1

    # Use file command if available
    if command -v file >/dev/null 2>&1; then
        file -b "$_file" | grep -qi 'binary\|executable\|data' && return 0
    fi

    # Fallback: check for null bytes in first 8KB
    head -c 8192 "$_file" 2>/dev/null | grep -q '[^[:print:][:space:]]' && return 0

    return 1
}

# Check if file matches extension pattern
# Usage: if has_extension "file.js" "js|ts|jsx|tsx"; then ...
has_extension() {
    _file="$1"
    _pattern="$2"
    echo "$_file" | grep -qE "\.($_pattern)$"
}

# Get file extension
# Usage: ext=$(get_extension "file.js")  # Returns "js"
get_extension() {
    _file="$1"
    echo "$_file" | sed 's/.*\.//'
}

# Check if path is in directory
# Usage: if is_in_dir "src/lib/foo.js" "src/"; then ...
is_in_dir() {
    _path="$1"
    _dir="$2"
    echo "$_path" | grep -q "^$_dir"
}

# ==============================================================================
# LANGUAGE DETECTION
# ==============================================================================

# Detect languages present in file list
# Usage: langs=$(detect_languages "$file_list")
detect_languages() {
    _files="$1"
    _langs=""

    echo "$_files" | grep -qE '\.(js|jsx|mjs|cjs)$' && _langs="$_langs javascript"
    echo "$_files" | grep -qE '\.(ts|tsx)$' && _langs="$_langs typescript"
    echo "$_files" | grep -qE '\.res$' && _langs="$_langs rescript"
    echo "$_files" | grep -qE '\.rs$' && _langs="$_langs rust"
    echo "$_files" | grep -qE '\.go$' && _langs="$_langs go"
    echo "$_files" | grep -qE '\.py$' && _langs="$_langs python"
    echo "$_files" | grep -qE '\.(ml|mli)$' && _langs="$_langs ocaml"
    echo "$_files" | grep -qE '\.gleam$' && _langs="$_langs gleam"
    echo "$_files" | grep -qE '\.(ex|exs)$' && _langs="$_langs elixir"
    echo "$_files" | grep -qE '\.(hs|lhs)$' && _langs="$_langs haskell"
    echo "$_files" | grep -qE '\.(jl)$' && _langs="$_langs julia"
    echo "$_files" | grep -qE '\.sh$' && _langs="$_langs shell"
    echo "$_files" | grep -qE '\.(yaml|yml)$' && _langs="$_langs yaml"
    echo "$_files" | grep -qE '\.json$' && _langs="$_langs json"
    echo "$_files" | grep -qE '\.scm$' && _langs="$_langs scheme"
    echo "$_files" | grep -qE '\.ncl$' && _langs="$_langs nickel"
    echo "$_files" | grep -qE '\.(lgt|logtalk)$' && _langs="$_langs logtalk"
    echo "$_files" | grep -qE '\.adoc$' && _langs="$_langs asciidoc"
    echo "$_files" | grep -qE '\.md$' && _langs="$_langs markdown"

    echo "$_langs" | tr ' ' '\n' | grep -v '^$' | sort -u | tr '\n' ' ' | sed 's/ $//'
}

# Check if language is banned per RSR policy
# Usage: if is_banned_language "typescript"; then ...
is_banned_language() {
    case "$1" in
        typescript|go|python|java|kotlin) return 0 ;;
        *) return 1 ;;
    esac
}

# Get replacement for banned language
# Usage: alt=$(get_language_alternative "typescript")
get_language_alternative() {
    case "$1" in
        typescript) echo "ReScript" ;;
        go) echo "Rust" ;;
        python) echo "Julia/Rust/ReScript" ;;
        java|kotlin) echo "Rust" ;;
        *) echo "allowed languages" ;;
    esac
}

# ==============================================================================
# CONFIGURATION PARSING
# ==============================================================================

# Read YAML value using grep/sed (no dependencies)
# Usage: value=$(yaml_get "config.yml" "hooks.pre-commit.enabled")
yaml_get() {
    _file="$1"
    _key="$2"

    [ -f "$_file" ] || return 1

    # Simple key-value extraction (flat config only)
    # For nested config, use yq if available
    if command -v yq >/dev/null 2>&1; then
        yq eval ".$_key // \"\"" "$_file" 2>/dev/null
    else
        # Fallback: basic grep for flat keys
        _simple_key=$(echo "$_key" | sed 's/.*\.//')
        grep "^[[:space:]]*$_simple_key:" "$_file" 2>/dev/null | \
            sed 's/^[^:]*:[[:space:]]*//' | \
            sed 's/^["\x27]//' | sed 's/["\x27]$//'
    fi
}

# Check if hook is enabled in config
# Usage: if is_hook_enabled "pre-commit"; then ...
is_hook_enabled() {
    _hook="$1"
    _enabled=$(yaml_get "$CICD_CONFIG_FILE" "hooks.$_hook.enabled")
    [ "$_enabled" != "false" ]
}

# Get hook-specific configuration
# Usage: threshold=$(get_hook_config "pre-push" "max_file_size")
get_hook_config() {
    _hook="$1"
    _key="$2"
    yaml_get "$CICD_CONFIG_FILE" "hooks.$_hook.$_key"
}

# ==============================================================================
# VALIDATION UTILITIES
# ==============================================================================

# Check for SPDX header in file
# Usage: if has_spdx_header "file.sh"; then ...
has_spdx_header() {
    _file="$1"
    [ -f "$_file" ] || return 1
    head -10 "$_file" 2>/dev/null | grep -qi "SPDX-License-Identifier"
}

# Check for unpinned GitHub Actions
# Usage: if has_unpinned_actions "workflow.yml"; then ...
has_unpinned_actions() {
    _file="$1"
    [ -f "$_file" ] || return 1
    grep -E 'uses:\s+[^@]+@(v[0-9]+|main|master|latest)\s*$' "$_file" >/dev/null 2>&1
}

# Check for permissions declaration in workflow
# Usage: if has_permissions_declaration "workflow.yml"; then ...
has_permissions_declaration() {
    _file="$1"
    [ -f "$_file" ] || return 1
    grep -q 'permissions:' "$_file" 2>/dev/null
}

# Validate YAML syntax
# Usage: if validate_yaml "file.yml"; then ...
validate_yaml() {
    _file="$1"
    [ -f "$_file" ] || return 1

    if command -v yq >/dev/null 2>&1; then
        yq eval '.' "$_file" >/dev/null 2>&1
    elif command -v python3 >/dev/null 2>&1; then
        python3 -c "import yaml; yaml.safe_load(open('$_file'))" 2>/dev/null
    else
        # No YAML validator available - assume valid
        return 0
    fi
}

# ==============================================================================
# SECRET DETECTION PATTERNS
# ==============================================================================

# Common secret patterns (POSIX-compatible regex)
SECRET_PATTERNS='
(api[_-]?key|secret|password|token|credential)[[:space:]]*[=:][[:space:]]*["\x27][A-Za-z0-9+/=_-]{16,}["\x27]
ghp_[a-zA-Z0-9]{36}
gho_[a-zA-Z0-9]{36}
github_pat_[a-zA-Z0-9]{22}_[a-zA-Z0-9]{59}
AKIA[0-9A-Z]{16}
sk-[a-zA-Z0-9]{48}
xox[baprs]-[a-zA-Z0-9-]{10,}
-----BEGIN[[:space:]]+(RSA|DSA|EC|OPENSSH|PGP)[[:space:]]+PRIVATE[[:space:]]+KEY-----
'

# Check file for secrets
# Usage: if has_secrets "file.py"; then ...
has_secrets() {
    _file="$1"
    [ -f "$_file" ] || return 1
    is_binary "$_file" && return 1

    echo "$SECRET_PATTERNS" | while IFS= read -r pattern; do
        [ -z "$pattern" ] && continue
        grep -qE "$pattern" "$_file" 2>/dev/null && return 0
    done
    return 1
}

# Get list of detected secrets with context
# Usage: secrets=$(detect_secrets "file.py")
detect_secrets() {
    _file="$1"
    [ -f "$_file" ] || return 1
    is_binary "$_file" && return 1

    _result=""
    echo "$SECRET_PATTERNS" | while IFS= read -r pattern; do
        [ -z "$pattern" ] && continue
        _matches=$(grep -n "$pattern" "$_file" 2>/dev/null || true)
        [ -n "$_matches" ] && _result="$_result$_matches\n"
    done
    echo "$_result"
}

# ==============================================================================
# RESULT HANDLING
# ==============================================================================

# Initialize error counter
init_error_count() {
    CICD_ERROR_COUNT=0
    CICD_WARN_COUNT=0
}

# Increment error count
add_error() {
    CICD_ERROR_COUNT=$((CICD_ERROR_COUNT + 1))
    [ -n "$1" ] && log_error "$*"
}

# Increment warning count
add_warning() {
    CICD_WARN_COUNT=$((CICD_WARN_COUNT + 1))
    [ -n "$1" ] && log_warn "$*"
}

# Get current error count
get_error_count() {
    echo "${CICD_ERROR_COUNT:-0}"
}

# Get current warning count
get_warning_count() {
    echo "${CICD_WARN_COUNT:-0}"
}

# Check if any errors occurred and exit appropriately
# Usage: check_errors_and_exit "hook-name"
check_errors_and_exit() {
    _hook_name="${1:-hook}"
    _errors="${CICD_ERROR_COUNT:-0}"
    _warns="${CICD_WARN_COUNT:-0}"

    if [ "$_errors" -gt 0 ]; then
        log_error "$_hook_name failed with $_errors error(s), $_warns warning(s)"
        log_info "Fix the issues above or use 'git ... --no-verify' to bypass (not recommended)"
        exit "$EXIT_FAILURE"
    fi

    if [ "$_warns" -gt 0 ]; then
        log_warn "$_hook_name completed with $_warns warning(s)"
    else
        log_success "$_hook_name passed"
    fi

    exit "$EXIT_SUCCESS"
}

# ==============================================================================
# REPOSITORY UTILITIES
# ==============================================================================

# Get repository name
# Usage: repo=$(get_repo_name)
get_repo_name() {
    basename "$(git rev-parse --show-toplevel 2>/dev/null)" 2>/dev/null || \
        basename "$(pwd)"
}

# Get current branch name
# Usage: branch=$(get_current_branch)
get_current_branch() {
    git symbolic-ref --short HEAD 2>/dev/null || \
        git rev-parse --short HEAD 2>/dev/null || \
        echo "unknown"
}

# Check if we're in a git repository
# Usage: if is_git_repo; then ...
is_git_repo() {
    git rev-parse --git-dir >/dev/null 2>&1
}

# Check if branch is protected
# Usage: if is_protected_branch "main"; then ...
is_protected_branch() {
    _branch="$1"
    case "$_branch" in
        main|master|production|prod|release/*) return 0 ;;
        *) return 1 ;;
    esac
}

# ==============================================================================
# INITIALIZATION
# ==============================================================================

# Initialize hook environment
# Usage: init_hook "pre-commit"
init_hook() {
    _hook_name="${1:-hook}"

    # Ensure we're in a git repository
    if ! is_git_repo; then
        log_error "Not in a git repository"
        exit "$EXIT_FAILURE"
    fi

    # Check if hook is enabled
    if [ -f "$CICD_CONFIG_FILE" ] && ! is_hook_enabled "$_hook_name"; then
        log_debug "$_hook_name is disabled in config"
        exit "$EXIT_SUCCESS"
    fi

    # Initialize error counting
    init_error_count

    # Print header
    print_header "$_hook_name"

    log_debug "Config file: $CICD_CONFIG_FILE"
    log_debug "Cache dir: $CICD_CACHE_DIR"
}

# Ensure cache directory exists
ensure_cache_dir() {
    [ -d "$CICD_CACHE_DIR" ] || mkdir -p "$CICD_CACHE_DIR"
}
