#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
# cicd-hyper-a Git Hooks Installer
#
# Installs cicd-hyper-a git hooks into a repository.
#
# Usage:
#   ./installer.sh [options] [target-repo]
#
# Options:
#   -h, --help          Show this help message
#   -f, --force         Overwrite existing hooks without backup
#   -n, --dry-run       Show what would be done without making changes
#   -q, --quiet         Suppress output
#   -v, --verbose       Enable verbose output
#   --hooks=LIST        Comma-separated list of hooks to install
#   --config=FILE       Use custom config file
#   --uninstall         Remove installed hooks
#   --update            Update existing hooks to latest version
#
# Examples:
#   ./installer.sh                           # Install to current repo
#   ./installer.sh /path/to/repo             # Install to specific repo
#   ./installer.sh --hooks=pre-commit,pre-push  # Install specific hooks
#   ./installer.sh --uninstall               # Remove hooks

set -e

# ==============================================================================
# CONFIGURATION
# ==============================================================================

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
VERSION="1.0.0"

# Default values
DRY_RUN=false
FORCE=false
QUIET=false
VERBOSE=false
UNINSTALL=false
UPDATE=false
HOOKS="pre-commit,pre-push,commit-msg,prepare-commit-msg"
CONFIG_FILE=""
TARGET_REPO=""

# Colors
if [ -t 1 ] && [ "${NO_COLOR:-}" = "" ]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[1;33m'
    BLUE='\033[0;34m'
    BOLD='\033[1m'
    NC='\033[0m'
else
    RED=''
    GREEN=''
    YELLOW=''
    BLUE=''
    BOLD=''
    NC=''
fi

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

log() {
    [ "$QUIET" = true ] && return
    printf "%s\n" "$*"
}

log_info() {
    [ "$QUIET" = true ] && return
    printf "${BLUE}[INFO]${NC} %s\n" "$*"
}

log_success() {
    [ "$QUIET" = true ] && return
    printf "${GREEN}[OK]${NC} %s\n" "$*"
}

log_warn() {
    printf "${YELLOW}[WARN]${NC} %s\n" "$*" >&2
}

log_error() {
    printf "${RED}[ERROR]${NC} %s\n" "$*" >&2
}

log_debug() {
    [ "$VERBOSE" = true ] || return
    printf "${BLUE}[DEBUG]${NC} %s\n" "$*"
}

die() {
    log_error "$*"
    exit 1
}

usage() {
    cat << EOF
${BOLD}cicd-hyper-a Git Hooks Installer v${VERSION}${NC}

${BOLD}Usage:${NC}
    $(basename "$0") [options] [target-repo]

${BOLD}Options:${NC}
    -h, --help          Show this help message
    -f, --force         Overwrite existing hooks without backup
    -n, --dry-run       Show what would be done without making changes
    -q, --quiet         Suppress output
    -v, --verbose       Enable verbose output
    --hooks=LIST        Comma-separated list of hooks to install
                        Available: pre-commit, pre-push, post-receive,
                                   commit-msg, prepare-commit-msg
    --config=FILE       Use custom config file
    --uninstall         Remove installed hooks
    --update            Update existing hooks to latest version

${BOLD}Examples:${NC}
    $(basename "$0")                              # Install to current repo
    $(basename "$0") /path/to/repo                # Install to specific repo
    $(basename "$0") --hooks=pre-commit,pre-push  # Install specific hooks
    $(basename "$0") --uninstall                  # Remove hooks

${BOLD}Environment Variables:${NC}
    CICD_HYPER_A_API    API endpoint for cicd-hyper-a
    CICD_HYPER_A_TOKEN  API authentication token
    CICD_CACHE_HOST     Redis/Dragonfly host
    CICD_CACHE_PORT     Redis/Dragonfly port

EOF
}

# ==============================================================================
# ARGUMENT PARSING
# ==============================================================================

parse_args() {
    while [ $# -gt 0 ]; do
        case "$1" in
            -h|--help)
                usage
                exit 0
                ;;
            -f|--force)
                FORCE=true
                ;;
            -n|--dry-run)
                DRY_RUN=true
                ;;
            -q|--quiet)
                QUIET=true
                ;;
            -v|--verbose)
                VERBOSE=true
                ;;
            --hooks=*)
                HOOKS="${1#*=}"
                ;;
            --config=*)
                CONFIG_FILE="${1#*=}"
                ;;
            --uninstall)
                UNINSTALL=true
                ;;
            --update)
                UPDATE=true
                ;;
            -*)
                die "Unknown option: $1"
                ;;
            *)
                TARGET_REPO="$1"
                ;;
        esac
        shift
    done
}

# ==============================================================================
# VALIDATION
# ==============================================================================

validate_repo() {
    local repo="$1"

    if [ ! -d "$repo" ]; then
        die "Directory does not exist: $repo"
    fi

    if [ ! -d "$repo/.git" ]; then
        die "Not a git repository: $repo"
    fi

    log_debug "Validated repository: $repo"
}

validate_hooks() {
    local hooks="$1"
    local valid_hooks="pre-commit pre-push post-receive commit-msg prepare-commit-msg"

    echo "$hooks" | tr ',' '\n' | while read -r hook; do
        [ -z "$hook" ] && continue
        if ! echo "$valid_hooks" | grep -qw "$hook"; then
            die "Invalid hook: $hook"
        fi
    done

    log_debug "Validated hooks: $hooks"
}

validate_templates() {
    local templates_dir="$SCRIPT_DIR/templates"

    if [ ! -d "$templates_dir" ]; then
        die "Templates directory not found: $templates_dir"
    fi

    log_debug "Templates directory: $templates_dir"
}

# ==============================================================================
# INSTALLATION
# ==============================================================================

backup_hook() {
    local hook_path="$1"
    local backup_path="${hook_path}.backup.$(date +%Y%m%d%H%M%S)"

    if [ "$DRY_RUN" = true ]; then
        log_info "[DRY-RUN] Would backup: $hook_path -> $backup_path"
        return 0
    fi

    cp "$hook_path" "$backup_path"
    log_info "Backed up: $hook_path -> $backup_path"
}

install_hook() {
    local hook_name="$1"
    local repo="$2"
    local template="$SCRIPT_DIR/templates/${hook_name}.sh"
    local target="$repo/.git/hooks/$hook_name"

    log_debug "Installing hook: $hook_name"

    # Check if template exists
    if [ ! -f "$template" ]; then
        log_warn "Template not found: $template"
        return 1
    fi

    # Check for existing hook
    if [ -f "$target" ]; then
        if [ "$FORCE" = true ]; then
            log_debug "Force mode - overwriting existing hook"
        else
            # Check if it's our hook
            if grep -q "cicd-hyper-a" "$target" 2>/dev/null; then
                if [ "$UPDATE" = true ]; then
                    backup_hook "$target"
                else
                    log_info "Hook already installed: $hook_name (use --update to refresh)"
                    return 0
                fi
            else
                backup_hook "$target"
            fi
        fi
    fi

    if [ "$DRY_RUN" = true ]; then
        log_info "[DRY-RUN] Would install: $template -> $target"
        return 0
    fi

    # Copy template
    cp "$template" "$target"
    chmod +x "$target"

    log_success "Installed: $hook_name"
}

install_lib() {
    local repo="$1"
    local lib_dir="$repo/.git/hooks/lib"

    log_debug "Installing lib directory"

    if [ "$DRY_RUN" = true ]; then
        log_info "[DRY-RUN] Would create: $lib_dir"
        return 0
    fi

    # Create lib directory
    mkdir -p "$lib_dir"

    # Copy lib files
    for lib_file in "$SCRIPT_DIR/lib"/*.sh; do
        if [ -f "$lib_file" ]; then
            local filename=$(basename "$lib_file")
            cp "$lib_file" "$lib_dir/$filename"
            chmod +x "$lib_dir/$filename"
            log_debug "Installed lib: $filename"
        fi
    done

    log_success "Installed lib directory"
}

install_config() {
    local repo="$1"
    local config_source="$SCRIPT_DIR/config.yml"
    local config_target="$repo/.git/hooks/config.yml"

    # Use custom config if specified
    if [ -n "$CONFIG_FILE" ]; then
        if [ -f "$CONFIG_FILE" ]; then
            config_source="$CONFIG_FILE"
        else
            die "Config file not found: $CONFIG_FILE"
        fi
    fi

    # Don't overwrite existing config unless forced
    if [ -f "$config_target" ] && [ "$FORCE" != true ] && [ "$UPDATE" != true ]; then
        log_info "Config already exists: $config_target (use --force to overwrite)"
        return 0
    fi

    if [ "$DRY_RUN" = true ]; then
        log_info "[DRY-RUN] Would copy: $config_source -> $config_target"
        return 0
    fi

    cp "$config_source" "$config_target"
    log_success "Installed config: $config_target"
}

install_all() {
    local repo="$1"

    log_info "Installing cicd-hyper-a hooks to: $repo"

    # Install lib directory first
    install_lib "$repo"

    # Install config
    install_config "$repo"

    # Install each hook
    echo "$HOOKS" | tr ',' '\n' | while read -r hook; do
        [ -z "$hook" ] && continue
        install_hook "$hook" "$repo"
    done

    log
    log_success "Installation complete!"
    log
    log "To configure hooks, edit: $repo/.git/hooks/config.yml"
    log "To bypass hooks: git commit --no-verify"
}

# ==============================================================================
# UNINSTALLATION
# ==============================================================================

uninstall_hook() {
    local hook_name="$1"
    local repo="$2"
    local target="$repo/.git/hooks/$hook_name"

    if [ ! -f "$target" ]; then
        log_debug "Hook not installed: $hook_name"
        return 0
    fi

    # Only remove our hooks
    if ! grep -q "cicd-hyper-a" "$target" 2>/dev/null; then
        log_warn "Not a cicd-hyper-a hook, skipping: $hook_name"
        return 0
    fi

    if [ "$DRY_RUN" = true ]; then
        log_info "[DRY-RUN] Would remove: $target"
        return 0
    fi

    rm "$target"
    log_success "Removed: $hook_name"
}

uninstall_all() {
    local repo="$1"

    log_info "Uninstalling cicd-hyper-a hooks from: $repo"

    # Uninstall each hook
    for hook in pre-commit pre-push post-receive commit-msg prepare-commit-msg; do
        uninstall_hook "$hook" "$repo"
    done

    # Remove lib directory
    local lib_dir="$repo/.git/hooks/lib"
    if [ -d "$lib_dir" ]; then
        if [ "$DRY_RUN" = true ]; then
            log_info "[DRY-RUN] Would remove: $lib_dir"
        else
            rm -rf "$lib_dir"
            log_success "Removed: lib directory"
        fi
    fi

    # Remove config (optionally)
    local config_file="$repo/.git/hooks/config.yml"
    if [ -f "$config_file" ]; then
        if [ "$FORCE" = true ]; then
            if [ "$DRY_RUN" = true ]; then
                log_info "[DRY-RUN] Would remove: $config_file"
            else
                rm "$config_file"
                log_success "Removed: config.yml"
            fi
        else
            log_info "Config preserved: $config_file (use --force to remove)"
        fi
    fi

    log
    log_success "Uninstallation complete!"
}

# ==============================================================================
# MAIN
# ==============================================================================

main() {
    parse_args "$@"

    # Set target repo to current directory if not specified
    if [ -z "$TARGET_REPO" ]; then
        TARGET_REPO=$(pwd)
    fi

    # Convert to absolute path
    TARGET_REPO=$(cd "$TARGET_REPO" && pwd)

    # Validate
    validate_repo "$TARGET_REPO"
    validate_hooks "$HOOKS"
    validate_templates

    # Show dry-run notice
    if [ "$DRY_RUN" = true ]; then
        log
        log "${YELLOW}=== DRY RUN MODE ===${NC}"
        log "No changes will be made"
        log
    fi

    # Execute action
    if [ "$UNINSTALL" = true ]; then
        uninstall_all "$TARGET_REPO"
    else
        install_all "$TARGET_REPO"
    fi
}

main "$@"
