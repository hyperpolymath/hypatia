#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Hypatia RAM Disk Orchestrator
#
# Creates a tmpfs workspace, clones repos, runs the full hypatia scanner,
# applies fixes via robot-repo-automaton, commits and pushes results,
# then cleans up. Designed to be resource-aware and safe.
#
# Usage:
#   ramdisk-orchestrator.sh [options] [repo1 repo2 ...]
#
#   Options:
#     --size SIZE         RAM disk size (default: 2G, auto-shrinks if RAM is low)
#     --org ORG           GitHub org to scan (default: hyperpolymath)
#     --all               Scan all repos in the org
#     --farm              Scan repos registered in .git-private-farm
#     --severity SEV      Minimum severity to report (default: high)
#     --fix               Apply auto-fixable fixes and commit
#     --push              Push committed fixes to remotes
#     --dry-run           Show what would be done without doing it
#     --keep              Do not unmount RAM disk on completion
#     --mount PATH        Custom mount point (default: /tmp/hypatia-workspace)
#     --max-repos N       Limit number of repos to process
#     --fallback-disk     Fall back to Eclipse drive if RAM is insufficient
#     --quiet             Suppress non-essential output
#     --help              Show this help
#
# Environment:
#     GITHUB_TOKEN        Required for private repos and API calls
#     HYPATIA_DIR         Path to hypatia repo (auto-detected)
#     FLEET_DIR           Path to gitbot-fleet repo (auto-detected)

set -euo pipefail

# ─────────────────────────────────────────────────────────────────────
# Configuration
# ─────────────────────────────────────────────────────────────────────
VERSION="1.0.0"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HYPATIA_DIR="${HYPATIA_DIR:-$(dirname "$SCRIPT_DIR")}"
FLEET_DIR="${FLEET_DIR:-${HYPATIA_DIR}/../gitbot-fleet}"
FARM_DIR="${FARM_DIR:-${HYPATIA_DIR}/../.git-private-farm}"
ECLIPSE_REPOS="${HYPATIA_REPOS_DIR:-$(dirname "$HYPATIA_DIR")}"

# Defaults
MOUNT_POINT="/tmp/hypatia-workspace"
RAMDISK_SIZE="2G"
ORG="hyperpolymath"
SEVERITY="high"
DO_FIX=false
DO_PUSH=false
DRY_RUN=false
KEEP_MOUNT=false
SCAN_ALL=false
SCAN_FARM=false
FALLBACK_DISK=false
QUIET=false
MAX_REPOS=0
REPOS=()

# Colors
if [[ -t 2 ]]; then
    RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'
    BLUE='\033[0;34m'; CYAN='\033[0;36m'; BOLD='\033[1m'; NC='\033[0m'
else
    RED=''; GREEN=''; YELLOW=''; BLUE=''; CYAN=''; BOLD=''; NC=''
fi

log()      { [[ "$QUIET" == "true" ]] || echo -e "${BLUE}[hypatia-orch]${NC} $*" >&2; }
log_warn() { echo -e "${YELLOW}[hypatia-orch]${NC} $*" >&2; }
log_err()  { echo -e "${RED}[hypatia-orch]${NC} $*" >&2; }
log_ok()   { echo -e "${GREEN}[hypatia-orch]${NC} $*" >&2; }

usage() {
    sed -n '/^# Usage:/,/^[^#]/{ /^#/s/^# \?//p }' "$0"
    exit 0
}

# ─────────────────────────────────────────────────────────────────────
# Argument parsing
# ─────────────────────────────────────────────────────────────────────
parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --size)       RAMDISK_SIZE="$2"; shift 2 ;;
            --org)        ORG="$2"; shift 2 ;;
            --all)        SCAN_ALL=true; shift ;;
            --farm)       SCAN_FARM=true; shift ;;
            --severity)   SEVERITY="$2"; shift 2 ;;
            --fix)        DO_FIX=true; shift ;;
            --push)       DO_PUSH=true; shift ;;
            --dry-run)    DRY_RUN=true; shift ;;
            --keep)       KEEP_MOUNT=true; shift ;;
            --mount)      MOUNT_POINT="$2"; shift 2 ;;
            --max-repos)  MAX_REPOS="$2"; shift 2 ;;
            --fallback-disk) FALLBACK_DISK=true; shift ;;
            --quiet)      QUIET=true; shift ;;
            --help|-h)    usage ;;
            -*)           log_err "Unknown option: $1"; exit 1 ;;
            *)            REPOS+=("$1"); shift ;;
        esac
    done
}

# ─────────────────────────────────────────────────────────────────────
# Resource checks
# ─────────────────────────────────────────────────────────────────────
get_available_ram_gb() {
    awk '/MemAvailable/ { printf "%.1f", $2/1048576 }' /proc/meminfo
}

get_ramdisk_size_bytes() {
    local size="$1"
    local num="${size%[GgMm]}"
    local unit="${size: -1}"
    case "$unit" in
        G|g) echo $((num * 1073741824)) ;;
        M|m) echo $((num * 1048576)) ;;
        *)   echo "$num" ;;
    esac
}

check_resources() {
    local avail_gb
    avail_gb=$(get_available_ram_gb)
    local requested_bytes
    requested_bytes=$(get_ramdisk_size_bytes "$RAMDISK_SIZE")
    local requested_gb
    requested_gb=$(echo "$requested_bytes" | awk '{printf "%.1f", $1/1073741824}')

    log "Available RAM: ${avail_gb}GB, requested: ${requested_gb}GB"

    # Need at least 1GB headroom beyond the ramdisk
    local min_avail
    min_avail=$(echo "$requested_gb" | awk '{printf "%.1f", $1 + 1.0}')

    if awk "BEGIN { exit ($avail_gb < $min_avail) ? 0 : 1 }"; then
        log_warn "Insufficient RAM (${avail_gb}GB available, need ${min_avail}GB)"

        if [[ "$FALLBACK_DISK" == "true" ]]; then
            log_warn "Falling back to Eclipse drive at $ECLIPSE_REPOS"
            MOUNT_POINT="$ECLIPSE_REPOS/.hypatia-workspace"
            USE_TMPFS=false
            return 0
        fi

        # Try a smaller ramdisk
        local half_avail
        half_avail=$(echo "$avail_gb" | awk '{printf "%dM", ($1 * 512)}')
        if awk "BEGIN { exit ($avail_gb > 1.5) ? 0 : 1 }"; then
            log_warn "Reducing ramdisk to ${half_avail}"
            RAMDISK_SIZE="$half_avail"
            USE_TMPFS=true
            return 0
        fi

        log_err "Not enough RAM for even a minimal ramdisk. Use --fallback-disk."
        return 1
    fi

    USE_TMPFS=true
}

# ─────────────────────────────────────────────────────────────────────
# Mount / unmount
# ─────────────────────────────────────────────────────────────────────
mount_workspace() {
    if [[ "$DRY_RUN" == "true" ]]; then
        log "[DRY-RUN] Would mount tmpfs (${RAMDISK_SIZE}) at $MOUNT_POINT"
        mkdir -p "$MOUNT_POINT"
        return 0
    fi

    if [[ "$USE_TMPFS" == "true" ]]; then
        # Check if we can use sudo before attempting mount
        if ! sudo -n true 2>/dev/null; then
            log_warn "No passwordless sudo — falling back to disk workspace"
            USE_TMPFS=false
            MOUNT_POINT="/tmp/hypatia-workspace-$$"
        fi
    fi

    if [[ "$USE_TMPFS" == "true" ]]; then
        mkdir -p "$MOUNT_POINT"
        if mountpoint -q "$MOUNT_POINT" 2>/dev/null; then
            # Verify we own it
            if [[ -w "$MOUNT_POINT" ]]; then
                log "Workspace already mounted at $MOUNT_POINT"
            else
                log_warn "Stale root-owned mount at $MOUNT_POINT — remounting"
                sudo umount "$MOUNT_POINT" 2>/dev/null || true
                sudo mount -t tmpfs -o size="$RAMDISK_SIZE",mode=0700,uid="$(id -u)",gid="$(id -g)" tmpfs "$MOUNT_POINT"
            fi
        else
            log "Mounting tmpfs (${RAMDISK_SIZE}) at $MOUNT_POINT"
            sudo mount -t tmpfs -o size="$RAMDISK_SIZE",mode=0700,uid="$(id -u)",gid="$(id -g)" tmpfs "$MOUNT_POINT"
        fi
    else
        mkdir -p "$MOUNT_POINT"
        log "Using disk-backed workspace at $MOUNT_POINT"
    fi

    # Create subdirectories
    mkdir -p "$MOUNT_POINT"/{repos,findings,fixes,logs}
}

unmount_workspace() {
    if [[ "$KEEP_MOUNT" == "true" ]]; then
        log "Keeping workspace at $MOUNT_POINT (--keep)"
        return 0
    fi

    if [[ "$USE_TMPFS" == "true" ]] && mountpoint -q "$MOUNT_POINT" 2>/dev/null; then
        log "Unmounting workspace"
        sudo umount "$MOUNT_POINT"
        rmdir "$MOUNT_POINT" 2>/dev/null || true
    elif [[ "$USE_TMPFS" != "true" && -d "$MOUNT_POINT" ]]; then
        log "Cleaning disk-backed workspace"
        rm -rf "$MOUNT_POINT"
    fi
}

# ─────────────────────────────────────────────────────────────────────
# Repo discovery
# ─────────────────────────────────────────────────────────────────────
discover_repos() {
    if [[ ${#REPOS[@]} -gt 0 ]]; then
        log "Using ${#REPOS[@]} specified repos"
        return 0
    fi

    if [[ "$SCAN_FARM" == "true" && -f "$FARM_DIR/farm-manifest.json" ]]; then
        log "Loading repos from farm manifest"
        while IFS= read -r repo; do
            REPOS+=("$repo")
        done < <(jq -r '.repos[].name' "$FARM_DIR/farm-manifest.json" 2>/dev/null)
        log "Found ${#REPOS[@]} repos in farm manifest"
        return 0
    fi

    if [[ "$SCAN_ALL" == "true" ]]; then
        log "Discovering repos from GitHub org: $ORG"
        while IFS= read -r repo; do
            REPOS+=("$repo")
        done < <(gh api --paginate "/users/$ORG/repos?per_page=100&type=owner" \
            --jq '.[].name' 2>/dev/null || true)
        log "Found ${#REPOS[@]} repos on GitHub"
        return 0
    fi

    # Default: scan local Eclipse repos that have .machine_readable/
    log "Discovering local repos with .machine_readable/"
    for dir in "$ECLIPSE_REPOS"/*/; do
        [[ -d "$dir/.machine_readable" || -d "$dir/.git" ]] || continue
        local name
        name=$(basename "$dir")
        # Skip known non-repos
        [[ "$name" == ".git-private-farm" ]] && continue
        [[ "$name" == "hyperpolymath-archive" ]] && continue
        [[ "$name" == ".hypatia-workspace" ]] && continue
        REPOS+=("$name")
    done
    log "Found ${#REPOS[@]} local repos"
}

# ─────────────────────────────────────────────────────────────────────
# Per-repo processing
# ─────────────────────────────────────────────────────────────────────
process_repo() {
    local repo_name="$1"
    local repo_num="$2"
    local total="$3"
    local work_dir="$MOUNT_POINT/repos/$repo_name"
    local findings_file="$MOUNT_POINT/findings/${repo_name}.json"
    local log_file="$MOUNT_POINT/logs/${repo_name}.log"

    log "${CYAN}[${repo_num}/${total}]${NC} Processing: $repo_name"

    # Prefer local clone on Eclipse drive (avoids network)
    local source_dir="$ECLIPSE_REPOS/$repo_name"
    if [[ -d "$source_dir/.git" ]]; then
        # Local repo — clone from filesystem (fast, no network)
        if [[ "$DRY_RUN" == "true" ]]; then
            log "  [DRY-RUN] Would clone from $source_dir"
            return 0
        fi
        git clone --quiet --depth 1 --single-branch "file://$source_dir" "$work_dir" 2>>"$log_file" || {
            log_warn "  Failed to clone $repo_name locally, trying GitHub"
            git clone --quiet --depth 1 "https://github.com/$ORG/$repo_name.git" "$work_dir" 2>>"$log_file" || {
                log_err "  Failed to clone $repo_name"
                return 1
            }
        }
    else
        # Not on disk — clone from GitHub
        if [[ "$DRY_RUN" == "true" ]]; then
            log "  [DRY-RUN] Would clone from GitHub: $ORG/$repo_name"
            return 0
        fi
        git clone --quiet --depth 1 "https://github.com/$ORG/$repo_name.git" "$work_dir" 2>>"$log_file" || {
            log_err "  Failed to clone $repo_name from GitHub"
            return 1
        }
    fi

    # Run hypatia scan
    local scan_exit=0
    HYPATIA_SEVERITY="$SEVERITY" HYPATIA_FORMAT=json \
        bash "$HYPATIA_DIR/hypatia-cli.sh" scan "$work_dir" \
        > "$findings_file" 2>>"$log_file" || scan_exit=$?

    local finding_count
    finding_count=$(jq '.submission_metadata.finding_count // 0' "$findings_file" 2>/dev/null || echo 0)
    local critical_count
    critical_count=$(jq '[.findings[] | select(.severity == "critical")] | length' "$findings_file" 2>/dev/null || echo 0)

    if [[ "$finding_count" -eq 0 ]]; then
        log_ok "  Clean: 0 findings"
    else
        log_warn "  Found $finding_count issues ($critical_count critical)"
    fi

    # Submit findings to fleet
    if [[ -d "$FLEET_DIR/shared-context" && "$finding_count" -gt 0 ]]; then
        local fleet_findings_dir="$FLEET_DIR/shared-context/findings/$repo_name"
        mkdir -p "$fleet_findings_dir"
        cp "$findings_file" "$fleet_findings_dir/$(date +%s).json"
        ln -sf "$fleet_findings_dir/$(date +%s).json" "$fleet_findings_dir/latest.json" 2>/dev/null || true
    fi

    # Apply fixes if requested
    if [[ "$DO_FIX" == "true" && "$finding_count" -gt 0 ]]; then
        apply_fixes "$repo_name" "$work_dir" "$findings_file" "$log_file"
    fi

    # Clean up clone to free RAM for next repo
    rm -rf "$work_dir"
}

apply_fixes() {
    local repo_name="$1"
    local work_dir="$2"
    local findings_file="$3"
    local log_file="$4"

    # Extract auto-fixable findings
    local fixable_count
    fixable_count=$(jq '[.findings[] | select(.auto_fixable == true)] | length' "$findings_file" 2>/dev/null || echo 0)

    if [[ "$fixable_count" -eq 0 ]]; then
        return 0
    fi

    log "  Applying $fixable_count auto-fixes..."

    local fix_applied=0

    # Fix: unpinned GitHub Actions
    if jq -e '.findings[] | select(.pattern == "unpinned_action")' "$findings_file" >/dev/null 2>&1; then
        if [[ -x "$FLEET_DIR/scripts/fix-unpinned-actions.sh" ]]; then
            bash "$FLEET_DIR/scripts/fix-unpinned-actions.sh" "$work_dir" >>"$log_file" 2>&1 && ((fix_applied++)) || true
        fi
    fi

    # Fix: missing SPDX headers
    if jq -e '.findings[] | select(.pattern == "missing_spdx_header")' "$findings_file" >/dev/null 2>&1; then
        if [[ -x "$FLEET_DIR/scripts/fix-missing-spdx.sh" ]]; then
            bash "$FLEET_DIR/scripts/fix-missing-spdx.sh" "$work_dir" >>"$log_file" 2>&1 && ((fix_applied++)) || true
        fi
    fi

    # Fix: missing workflow permissions
    if jq -e '.findings[] | select(.pattern == "missing_permissions")' "$findings_file" >/dev/null 2>&1; then
        if [[ -x "$FLEET_DIR/scripts/fix-workflow-permissions.sh" ]]; then
            bash "$FLEET_DIR/scripts/fix-workflow-permissions.sh" "$work_dir" >>"$log_file" 2>&1 && ((fix_applied++)) || true
        fi
    fi

    # Fix: CORS wildcard
    if jq -e '.findings[] | select(.pattern == "wildcard_cors")' "$findings_file" >/dev/null 2>&1; then
        if [[ -x "$FLEET_DIR/scripts/fix-cors-wildcard.sh" ]]; then
            bash "$FLEET_DIR/scripts/fix-cors-wildcard.sh" "$work_dir" >>"$log_file" 2>&1 && ((fix_applied++)) || true
        fi
    fi

    # Fix: HTTP URLs
    if jq -e '.findings[] | select(.pattern == "http_url")' "$findings_file" >/dev/null 2>&1; then
        if [[ -x "$FLEET_DIR/scripts/fix-http-to-https.sh" ]]; then
            bash "$FLEET_DIR/scripts/fix-http-to-https.sh" "$work_dir" >>"$log_file" 2>&1 && ((fix_applied++)) || true
        fi
    fi

    # Fix: shell quoting
    if jq -e '.findings[] | select(.pattern == "shell_unquoted_var")' "$findings_file" >/dev/null 2>&1; then
        if [[ -x "$FLEET_DIR/scripts/fix-shell-quoting.sh" ]]; then
            bash "$FLEET_DIR/scripts/fix-shell-quoting.sh" "$work_dir" >>"$log_file" 2>&1 && ((fix_applied++)) || true
        fi
    fi

    if [[ "$fix_applied" -gt 0 ]]; then
        log_ok "  Applied $fix_applied fix scripts"

        # Commit fixes
        if git -C "$work_dir" diff --quiet 2>/dev/null; then
            log "  No changes after fixes (already clean)"
        else
            git -C "$work_dir" add -A 2>>"$log_file"
            git -C "$work_dir" commit -m "$(cat <<EOF
fix: auto-remediate security findings (hypatia v${VERSION})

Applied $fix_applied automated fixes via hypatia ramdisk orchestrator.
Findings file: $findings_file

Co-Authored-By: Hypatia Scanner <hypatia@hyperpolymath.dev>
EOF
)" >>"$log_file" 2>&1 || true

            # Push if requested
            if [[ "$DO_PUSH" == "true" ]]; then
                # Push back to the source repo on Eclipse drive
                local source_dir="$ECLIPSE_REPOS/$repo_name"
                if [[ -d "$source_dir/.git" ]]; then
                    git -C "$work_dir" push origin HEAD 2>>"$log_file" && {
                        log_ok "  Pushed fixes to $source_dir"
                        # Also push to GitHub
                        git -C "$source_dir" push origin HEAD 2>>"$log_file" || true
                    } || log_warn "  Push failed for $repo_name"
                fi
            fi
        fi

        # Record fix outcome for learning pipeline
        local outcome_file="$MOUNT_POINT/fixes/${repo_name}-$(date +%s).json"
        jq -n \
            --arg repo "$repo_name" \
            --arg timestamp "$(date -Iseconds)" \
            --argjson fixes "$fix_applied" \
            --arg scanner "hypatia-ramdisk-orch" \
            '{repo:$repo, timestamp:$timestamp, fixes_applied:$fixes,
              scanner:$scanner, outcome:"success"}' \
            > "$outcome_file"
    fi
}

# ─────────────────────────────────────────────────────────────────────
# Summary report
# ─────────────────────────────────────────────────────────────────────
generate_summary() {
    local total_repos="$1"
    local processed="$2"
    local clean="$3"
    local with_issues="$4"
    local failed="$5"

    echo ""
    echo -e "${CYAN}════════════════════════════════════════════════════════${NC}"
    echo -e "${CYAN}  Hypatia RAM Disk Orchestrator — Summary${NC}"
    echo -e "${CYAN}════════════════════════════════════════════════════════${NC}"
    echo -e "  Version:     ${VERSION}"
    local ws_type="disk"
    [[ "$USE_TMPFS" == "true" ]] && ws_type="tmpfs ${RAMDISK_SIZE}"
    echo -e "  Workspace:   ${MOUNT_POINT} (${ws_type})"
    echo -e "  Repos:       ${processed}/${total_repos} processed"
    echo -e "  ${GREEN}Clean:       ${clean}${NC}"
    echo -e "  ${YELLOW}With issues: ${with_issues}${NC}"
    echo -e "  ${RED}Failed:      ${failed}${NC}"
    echo -e "  Fixes:       $(ls "$MOUNT_POINT/fixes/" 2>/dev/null | wc -l) fix outcomes"
    echo -e "  Findings:    $MOUNT_POINT/findings/"
    echo -e "  Logs:        $MOUNT_POINT/logs/"
    echo -e "${CYAN}════════════════════════════════════════════════════════${NC}"

    # Aggregate finding counts across all repos
    if [[ -d "$MOUNT_POINT/findings" ]]; then
        local total_findings critical_total
        total_findings=$(jq -s '[.[].submission_metadata.finding_count // 0] | add' \
            "$MOUNT_POINT"/findings/*.json 2>/dev/null || echo 0)
        critical_total=$(jq -s '[.[].findings[] | select(.severity == "critical")] | length' \
            "$MOUNT_POINT"/findings/*.json 2>/dev/null || echo 0)

        echo ""
        echo -e "  Total findings across all repos: ${BOLD}$total_findings${NC}"
        echo -e "  Critical findings: ${RED}$critical_total${NC}"

        # Top patterns
        echo ""
        echo -e "  ${BOLD}Top patterns:${NC}"
        jq -s '[.[].findings[]] | group_by(.pattern) | map({pattern: .[0].pattern, count: length, severity: .[0].severity}) | sort_by(-.count) | .[:10][] | "    \(.count)\t\(.severity)\t\(.pattern)"' \
            "$MOUNT_POINT"/findings/*.json 2>/dev/null | head -10 || true
    fi

    echo ""
}

# ─────────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────────
main() {
    parse_args "$@"

    log "${BOLD}Hypatia RAM Disk Orchestrator v${VERSION}${NC}"

    # Verify hypatia scanner exists
    if [[ ! -f "$HYPATIA_DIR/hypatia-cli.sh" ]]; then
        log_err "Hypatia scanner not found at $HYPATIA_DIR/hypatia-cli.sh"
        exit 1
    fi

    # Check resources
    check_resources || exit 1

    # Discover repos
    discover_repos

    if [[ ${#REPOS[@]} -eq 0 ]]; then
        log_err "No repos to scan"
        exit 1
    fi

    # Apply max-repos limit
    if [[ "$MAX_REPOS" -gt 0 && ${#REPOS[@]} -gt "$MAX_REPOS" ]]; then
        log "Limiting to $MAX_REPOS repos (of ${#REPOS[@]})"
        REPOS=("${REPOS[@]:0:$MAX_REPOS}")
    fi

    local total=${#REPOS[@]}
    log "Processing $total repos (severity: $SEVERITY, fix: $DO_FIX, push: $DO_PUSH)"

    # Mount workspace
    mount_workspace
    trap 'unmount_workspace' EXIT

    # Process each repo sequentially (resource-aware)
    local processed=0 clean=0 with_issues=0 failed=0

    for i in "${!REPOS[@]}"; do
        local repo="${REPOS[$i]}"
        local num=$((i + 1))

        if process_repo "$repo" "$num" "$total"; then
            ((processed++)) || true
            local findings_file="$MOUNT_POINT/findings/${repo}.json"
            if [[ -f "$findings_file" ]]; then
                local count
                count=$(jq '.submission_metadata.finding_count // 0' "$findings_file" 2>/dev/null || echo 0)
                if [[ "$count" -eq 0 ]]; then
                    ((clean++)) || true
                else
                    ((with_issues++)) || true
                fi
            else
                ((clean++)) || true
            fi
        else
            ((failed++)) || true
            ((processed++)) || true
        fi
    done

    # Generate summary
    generate_summary "$total" "$processed" "$clean" "$with_issues" "$failed"

    # Return non-zero if any critical findings
    if [[ -d "$MOUNT_POINT/findings" ]]; then
        local critical
        critical=$(jq -s '[.[].findings[] | select(.severity == "critical")] | length' \
            "$MOUNT_POINT"/findings/*.json 2>/dev/null || echo 0)
        [[ "$critical" -eq 0 ]]
    fi
}

main "$@"
