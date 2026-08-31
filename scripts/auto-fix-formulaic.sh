#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# auto-fix-formulaic.sh — Legacy formulaic diagnostics compatibility command
#
# Mutation is intentionally disabled. The former implementation bypassed the
# Hypatia policy pipeline, carried a stale SHA database alongside actions.lock,
# inserted permissions: read-all contrary to current least-privilege policy,
# and could stage unrelated work with git add -A.
#
# GitHub Action resolution belongs to the authoritative `gh actions-lock`
# recipe. Workflow permissions require workflow-specific policy evaluation.
# Until those transactional recipes are wired, this command is read-only.
#
# Usage: ./scripts/auto-fix-formulaic.sh <repo-path>
# Examples:
#   ./scripts/auto-fix-formulaic.sh /home/hyperpolymath/developer/hyper-repos/aerie

set -euo pipefail

FIXES_APPLIED=0
REPOS_SCANNED=0
WARNINGS=0

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

log() { echo "[hypatia-autofix] $*"; }
warn() {
  echo "[hypatia-autofix] WARNING: $*" >&2
  WARNINGS=$((WARNINGS + 1))
}
fix() {
  echo "[hypatia-autofix] FIX: $*"
  FIXES_APPLIED=$((FIXES_APPLIED + 1))
}

# Record activity in the repo's .hypatia/ log
record() {
  local repo="$1" action="$2" details="${3:-}"
  bash "${SCRIPT_DIR}/bot-accountability.sh" record "$repo" "hypatia-autofix" "$action" "$details" 2>/dev/null || true
}

# ---------------------------------------------------------------------------
# Diagnostic 1: Detect binary artifacts tracked by git
# ---------------------------------------------------------------------------
fix_tracked_binaries() {
  local repo="$1"
  cd "$repo" || return 0

  # Common binary patterns that should never be tracked
  local -a binary_patterns=(
    "*.exe" "*.dll" "*.so" "*.dylib" "*.a" "*.o"
    "*.pyc" "*.pyo" "*.class"
    "erl_crash.dump"
    "*.beam"  # if outside _build/
  )

  # Check for tracked binaries using git
  for pattern in "${binary_patterns[@]}"; do
    local tracked
    tracked=$(git ls-files "$pattern" 2>/dev/null || true)
    if [ -n "$tracked" ]; then
      warn "Binary tracked in git: $tracked (in $(basename "$repo"))"
    fi
  done

  # Check for large tracked files (>1MB)
  git ls-files -z 2>/dev/null | while IFS= read -r -d '' f; do
    if [ -f "$f" ]; then
      local size
      size=$(stat -c%s "$f" 2>/dev/null || echo 0)
      if [ "$size" -gt 1048576 ]; then
        warn "Large file tracked ($(( size / 1024 ))KB): $f (in $(basename "$repo"))"
      fi
    fi
  done
}

# ---------------------------------------------------------------------------
# Diagnostic 2: Check for AGPL references (should be PMPL)
# ---------------------------------------------------------------------------
fix_agpl_references() {
  local repo="$1"

  # Skip test fixtures and mock data
  local agpl_files
  agpl_files=$(grep -rl "AGPL-3.0" "$repo" \
    --include='*.ex' --include='*.exs' --include='*.rs' \
    --include='*.idr' --include='*.zig' --include='*.yml' \
    --include='*.yaml' --include='*.json' --include='*.md' \
    --include='*.adoc' --include='*.toml' \
    --exclude-dir='.git' --exclude-dir='node_modules' \
    --exclude-dir='_build' --exclude-dir='deps' \
    --exclude-dir='target' \
    2>/dev/null || true)

  for f in $agpl_files; do
    # Skip mock/fixture files that simulate external API responses
    if echo "$f" | grep -qE 'mock|fixture.*github'; then
      continue
    fi
    warn "AGPL-3.0 reference in: $f (should be MPL-2.0)"
  done
}

# ---------------------------------------------------------------------------
# Diagnostic 3: Check for missing SECURITY.md in public repos
# ---------------------------------------------------------------------------
check_security_md() {
  local repo="$1"
  if [ ! -f "${repo}/SECURITY.md" ]; then
    warn "Missing SECURITY.md in $(basename "$repo")"
  fi
}

# ---------------------------------------------------------------------------
# Diagnostic 4: Check for missing .editorconfig
# ---------------------------------------------------------------------------
check_editorconfig() {
  local repo="$1"
  if [ ! -f "${repo}/.editorconfig" ]; then
    warn "Missing .editorconfig in $(basename "$repo")"
  fi
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
scan_repo() {
  local repo="$1"
  [ -d "${repo}/.git" ] || return 0

  log "Scanning $(basename "$repo")..."
  REPOS_SCANNED=$((REPOS_SCANNED + 1))

  local before=$FIXES_APPLIED
  fix_tracked_binaries "$repo"
  fix_agpl_references "$repo"
  check_security_md "$repo"
  check_editorconfig "$repo"
  local after=$FIXES_APPLIED

  local fix_count=$((after - before))

  # Record visit with fix count
  record "$repo" "diagnostic_scan" "fixes=${fix_count};warnings=${WARNINGS}"
}

# Reject legacy mutation and incomplete-estate modes explicitly.
for arg in "$@"; do
  case "$arg" in
    --push)
      echo "ERROR: --push is disabled; this compatibility command is diagnostic-only." >&2
      exit 2
      ;;
  esac
done

if [ "${1:-}" = "all" ]; then
  echo "ERROR: incomplete 'all' traversal is disabled; use the authoritative estate manifest." >&2
  exit 2
elif [ -n "${1:-}" ]; then
  scan_repo "$1"
else
  echo "Usage: $0 <repo-path>"
  echo "  $0 /home/hyperpolymath/developer/hyper-repos/aerie"
  echo ""
  echo "Mutation, --push, and incomplete 'all' traversal are disabled."
  exit 1
fi

log "Done. Diagnostic-only scan: repos=${REPOS_SCANNED}, fixes=${FIXES_APPLIED}, warnings=${WARNINGS}."

# --- Kin Protocol: write heartbeat ---
KIN_DIR="${HOME}/.hypatia/kin"
mkdir -p "$KIN_DIR"
cat > "${KIN_DIR}/auto-fix.heartbeat.json" <<HEARTBEAT
{
  "kin_id": "auto-fix",
  "role": "fixer",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "status": "diagnostic_only",
  "version": "1.1.0",
  "last_run": {
    "repos_scanned": ${REPOS_SCANNED},
    "fixes_applied": ${FIXES_APPLIED},
    "warnings": ${WARNINGS},
    "mutation_enabled": false
  },
  "errors": [],
  "capabilities": ["tracked_binary_audit", "license_reference_audit", "security_file_check", "editorconfig_check"]
}
HEARTBEAT
