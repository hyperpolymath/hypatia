#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# scm-to-a2ml.sh — Convert SCM checkpoint files to A2ML format
#
# Scans a repo's .machine_readable/ directory for checkpoint .scm files
# (STATE, META, ECOSYSTEM, AGENTIC, NEUROSYM, PLAYBOOK, LANGUAGES)
# and creates corresponding .a2ml versions using TOML-like A2ML format.
#
# Does NOT delete original .scm files — user reviews conversions first.
#
# Usage:
#   scm-to-a2ml.sh <repo-path>
#   scm-to-a2ml.sh --scan <repos-root>   # scan mode: report only, no conversion
#
# Author: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)

set -euo pipefail

# --- Constants ---
readonly CHECKPOINT_NAMES="STATE META ECOSYSTEM AGENTIC NEUROSYM PLAYBOOK LANGUAGES"
readonly TODAY="$(date +%Y-%m-%d)"
readonly SCRIPT_NAME="$(basename "$0")"

# --- Color output (if terminal) ---
if [[ -t 1 ]]; then
    readonly C_GREEN='\033[0;32m'
    readonly C_YELLOW='\033[0;33m'
    readonly C_RED='\033[0;31m'
    readonly C_CYAN='\033[0;36m'
    readonly C_RESET='\033[0m'
else
    readonly C_GREEN='' C_YELLOW='' C_RED='' C_CYAN='' C_RESET=''
fi

# --- Utility functions ---

info()  { printf "${C_CYAN}[INFO]${C_RESET}  %s\n" "$*"; }
ok()    { printf "${C_GREEN}[OK]${C_RESET}    %s\n" "$*"; }
warn()  { printf "${C_YELLOW}[WARN]${C_RESET}  %s\n" "$*"; }
error() { printf "${C_RED}[ERROR]${C_RESET} %s\n" "$*" >&2; }

usage() {
    cat <<EOF
Usage:
  $SCRIPT_NAME <repo-path>            Convert .scm checkpoint files in a single repo
  $SCRIPT_NAME --scan <repos-root>    Scan repos and report migration status (no changes)
  $SCRIPT_NAME --help                 Show this help

Options:
  --dry-run     Show what would be converted without writing files
  --force       Overwrite existing .a2ml files (default: skip if exists)

Examples:
  $SCRIPT_NAME ~/Documents/hyperpolymath-repos/my-repo
  $SCRIPT_NAME --scan ~/Documents/hyperpolymath-repos/
  $SCRIPT_NAME --dry-run ~/Documents/hyperpolymath-repos/my-repo
EOF
    exit 0
}

# --- SCM to A2ML conversion ---
# Parses S-expression checkpoint files and emits TOML-like A2ML.
#
# Conversion rules:
#   - ;; comments       -> # comments
#   - (section ...)     -> [section]
#   - (key "value")     -> key = "value"
#   - (key value)       -> key = "value"  (unquoted atoms get quoted)
#   - (key (items ...)) -> key = ["item1", "item2", ...]
#   - Nested (sub ...)  -> [section.sub]
#   - Empty lists ()    -> omitted or key = []

convert_scm_to_a2ml() {
    local scm_file="$1"
    local a2ml_file="$2"
    local basename_noext="$3"
    local project_name="$4"

    # Extract the file purpose from the basename
    local file_desc=""
    case "$basename_noext" in
        STATE)     file_desc="Project state checkpoint" ;;
        META)      file_desc="Project meta-information" ;;
        ECOSYSTEM) file_desc="Ecosystem position and relationships" ;;
        AGENTIC)   file_desc="Agentic contract and automation rules" ;;
        NEUROSYM)  file_desc="Neurosymbolic integration manifest" ;;
        PLAYBOOK)  file_desc="Operational playbook" ;;
        LANGUAGES) file_desc="Language configuration" ;;
        *)         file_desc="Checkpoint file" ;;
    esac

    # Write A2ML header
    {
        echo "# SPDX-License-Identifier: PMPL-1.0-or-later"
        echo "# ${basename_noext}.a2ml — ${file_desc}"
        echo "# Converted from ${basename_noext}.scm by scm-to-a2ml.sh on ${TODAY}"
        echo ""
    } > "$a2ml_file"

    # Parse the SCM file and convert to A2ML
    # Strategy: line-by-line parsing with state tracking
    local current_section=""
    local in_list=0
    local list_items=""

    while IFS= read -r line; do
        # Skip empty lines
        if [[ -z "${line// /}" ]]; then
            echo "" >> "$a2ml_file"
            continue
        fi

        # Convert comments: ;; -> #
        if [[ "$line" =~ ^[[:space:]]*\;\; ]]; then
            local comment_text
            comment_text="${line#*;;}"
            # Skip SPDX and Media-Type lines (we already wrote our header)
            if [[ "$comment_text" =~ SPDX-License-Identifier ]] || \
               [[ "$comment_text" =~ Media-Type ]]; then
                continue
            fi
            echo "#${comment_text}" >> "$a2ml_file"
            continue
        fi

        # Strip leading/trailing whitespace for parsing
        local trimmed
        trimmed="$(echo "$line" | sed 's/^[[:space:]]*//' | sed 's/[[:space:]]*$//')"

        # Top-level form opener: (state, (meta, (ecosystem, etc.
        # These are the root wrappers — skip them
        if [[ "$trimmed" =~ ^\(([a-z_-]+)[[:space:]]*$ ]]; then
            local form_name="${BASH_REMATCH[1]}"
            # Root forms like (state, (meta, (ecosystem — skip
            local lc_basename
            lc_basename="$(echo "$basename_noext" | tr '[:upper:]' '[:lower:]')"
            if [[ "$form_name" == "$lc_basename" ]]; then
                continue
            fi
            # Section opener
            current_section="$form_name"
            echo "[$form_name]" >> "$a2ml_file"
            continue
        fi

        # Section with content on same line: (section-name
        if [[ "$trimmed" =~ ^\(([a-z_-]+)$ ]]; then
            current_section="${BASH_REMATCH[1]}"
            echo "[${current_section}]" >> "$a2ml_file"
            continue
        fi

        # Key-value: (key "value")
        if [[ "$trimmed" =~ ^\(([a-z_-]+)[[:space:]]+\"([^\"]*)\"\)$ ]]; then
            local key="${BASH_REMATCH[1]}"
            local val="${BASH_REMATCH[2]}"
            echo "${key} = \"${val}\"" >> "$a2ml_file"
            continue
        fi

        # Key-value with unquoted atom: (key value)
        if [[ "$trimmed" =~ ^\(([a-z_-]+)[[:space:]]+([^()\"[:space:]]+)\)$ ]]; then
            local key="${BASH_REMATCH[1]}"
            local val="${BASH_REMATCH[2]}"
            # Keep numbers unquoted
            if [[ "$val" =~ ^[0-9]+(\.[0-9]+)?$ ]]; then
                echo "${key} = ${val}" >> "$a2ml_file"
            else
                echo "${key} = \"${val}\"" >> "$a2ml_file"
            fi
            continue
        fi

        # Key with empty list: (key ())
        if [[ "$trimmed" =~ ^\(([a-z_-]+)[[:space:]]+\(\)\)$ ]]; then
            local key="${BASH_REMATCH[1]}"
            echo "${key} = []" >> "$a2ml_file"
            continue
        fi

        # Bare section-like key with no value, just a label: (key)
        if [[ "$trimmed" =~ ^\(([a-z_-]+)\)$ ]]; then
            local key="${BASH_REMATCH[1]}"
            echo "${key} = []" >> "$a2ml_file"
            continue
        fi

        # Quoted string item in a list: ("some string")
        if [[ "$trimmed" =~ ^\(\"([^\"]*)\"\)$ ]]; then
            echo "  - \"${BASH_REMATCH[1]}\"" >> "$a2ml_file"
            continue
        fi

        # Bare quoted string in a list context: "some string"
        if [[ "$trimmed" =~ ^\"([^\"]*)\"$ ]]; then
            echo "  - \"${BASH_REMATCH[1]}\"" >> "$a2ml_file"
            continue
        fi

        # Closing parens — skip
        if [[ "$trimmed" =~ ^\)+$ ]]; then
            continue
        fi

        # Key-value where value has parens/complex content: (key "value with (parens)")
        # Fallback: emit as comment for manual review
        if [[ "$trimmed" =~ ^\( ]]; then
            local cleaned
            cleaned="$(echo "$trimmed" | sed 's/^(//;s/)$//')"
            echo "# [REVIEW] ${cleaned}" >> "$a2ml_file"
            continue
        fi

        # Anything else: preserve as comment for review
        echo "# [REVIEW] ${trimmed}" >> "$a2ml_file"

    done < "$scm_file"

    # Clean up multiple blank lines
    sed -i '/^$/N;/^\n$/d' "$a2ml_file" 2>/dev/null || true

    return 0
}

# --- Derive project name from repo path ---
get_project_name() {
    local repo_path="$1"
    basename "$repo_path"
}

# --- Convert a single repo ---
convert_repo() {
    local repo_path="$1"
    local dry_run="${2:-false}"
    local force="${3:-false}"

    local mr_dir="${repo_path}/.machine_readable"

    if [[ ! -d "$mr_dir" ]]; then
        warn "No .machine_readable/ directory in: ${repo_path}"
        return 1
    fi

    local project_name
    project_name="$(get_project_name "$repo_path")"
    local converted=0
    local skipped=0
    local failed=0

    for name in $CHECKPOINT_NAMES; do
        local scm_file="${mr_dir}/${name}.scm"
        local a2ml_file="${mr_dir}/${name}.a2ml"

        # Skip if no .scm file
        if [[ ! -f "$scm_file" ]]; then
            continue
        fi

        # Skip if .a2ml already exists (unless --force)
        if [[ -f "$a2ml_file" ]] && [[ "$force" != "true" ]]; then
            info "Skipping ${name}.scm (${name}.a2ml already exists) in ${project_name}"
            skipped=$((skipped + 1))
            continue
        fi

        if [[ "$dry_run" == "true" ]]; then
            info "[DRY RUN] Would convert: ${scm_file} -> ${a2ml_file}"
            converted=$((converted + 1))
            continue
        fi

        # Perform conversion
        if convert_scm_to_a2ml "$scm_file" "$a2ml_file" "$name" "$project_name"; then
            ok "Converted: ${name}.scm -> ${name}.a2ml in ${project_name}"
            converted=$((converted + 1))
        else
            error "Failed to convert: ${scm_file}"
            failed=$((failed + 1))
        fi
    done

    # Also check 6scm/ subdirectory
    local scm6_dir="${mr_dir}/6scm"
    if [[ -d "$scm6_dir" ]]; then
        info "Found 6scm/ archive directory in ${project_name} — skipping (archived originals)"
    fi

    echo ""
    info "Results for ${project_name}:"
    info "  Converted: ${converted}"
    info "  Skipped:   ${skipped} (already have .a2ml)"
    info "  Failed:    ${failed}"

    return 0
}

# --- Scan mode ---
scan_repos() {
    local repos_root="$1"

    info "Scanning repos under: ${repos_root}"
    echo ""

    local total_repos=0
    local repos_with_scm=0
    local repos_with_a2ml=0
    local repos_with_both=0
    local repos_scm_only=0
    local total_scm_files=0
    local total_a2ml_files=0

    # Find all .machine_readable dirs
    while IFS= read -r mr_dir; do
        local repo_dir
        repo_dir="$(dirname "$mr_dir")"
        total_repos=$((total_repos + 1))

        local has_scm=false
        local has_a2ml=false
        local scm_count=0
        local a2ml_count=0

        for name in $CHECKPOINT_NAMES; do
            [[ -f "${mr_dir}/${name}.scm" ]] && { has_scm=true; scm_count=$((scm_count + 1)); }
            [[ -f "${mr_dir}/${name}.a2ml" ]] && { has_a2ml=true; a2ml_count=$((a2ml_count + 1)); }
        done

        total_scm_files=$((total_scm_files + scm_count))
        total_a2ml_files=$((total_a2ml_files + a2ml_count))

        if $has_scm; then repos_with_scm=$((repos_with_scm + 1)); fi
        if $has_a2ml; then repos_with_a2ml=$((repos_with_a2ml + 1)); fi
        if $has_scm && $has_a2ml; then repos_with_both=$((repos_with_both + 1)); fi
        if $has_scm && ! $has_a2ml; then repos_scm_only=$((repos_scm_only + 1)); fi

    done < <(find "$repos_root" -type d -name '.machine_readable' 2>/dev/null | sort)

    echo "============================================"
    echo "  SCM -> A2ML Migration Report"
    echo "============================================"
    echo ""
    echo "  Directories with .machine_readable/:  ${total_repos}"
    echo "  Repos with .scm checkpoint files:     ${repos_with_scm}"
    echo "  Repos with .a2ml checkpoint files:     ${repos_with_a2ml}"
    echo "  Repos with BOTH (partially migrated):  ${repos_with_both}"
    echo "  Repos with SCM only (need migration):  ${repos_scm_only}"
    echo ""
    echo "  Total .scm checkpoint files:           ${total_scm_files}"
    echo "  Total .a2ml checkpoint files:           ${total_a2ml_files}"
    echo "  Files still needing conversion:         $((total_scm_files - total_a2ml_files))"
    echo ""
    echo "============================================"

    return 0
}

# --- Main ---
main() {
    local mode="convert"
    local dry_run="false"
    local force="false"
    local target_path=""

    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --help|-h)
                usage
                ;;
            --scan)
                mode="scan"
                shift
                ;;
            --dry-run)
                dry_run="true"
                shift
                ;;
            --force)
                force="true"
                shift
                ;;
            -*)
                error "Unknown option: $1"
                usage
                ;;
            *)
                target_path="$1"
                shift
                ;;
        esac
    done

    if [[ -z "$target_path" ]]; then
        error "No path specified."
        usage
    fi

    # Resolve to absolute path
    target_path="$(cd "$target_path" 2>/dev/null && pwd)" || {
        error "Path does not exist: $target_path"
        exit 1
    }

    case "$mode" in
        scan)
            scan_repos "$target_path"
            ;;
        convert)
            info "Converting SCM checkpoint files in: ${target_path}"
            [[ "$dry_run" == "true" ]] && info "DRY RUN mode — no files will be written"
            [[ "$force" == "true" ]] && warn "FORCE mode — existing .a2ml files will be overwritten"
            echo ""
            convert_repo "$target_path" "$dry_run" "$force"
            ;;
    esac
}

main "$@"
