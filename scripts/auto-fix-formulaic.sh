#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# auto-fix-formulaic.sh — Direct auto-fix for predictable, formulaic issues
#
# This bypasses the full Hypatia pipeline and directly fixes issues that
# are so predictable they should never appear in any scan. These are the
# issues that the sophisticated pipeline SHOULD catch but currently doesn't
# because the operational deployment is missing.
#
# Usage: ./scripts/auto-fix-formulaic.sh [repo-path|all]
# Examples:
#   ./scripts/auto-fix-formulaic.sh ~/Documents/hyperpolymath-repos/aerie
#   ./scripts/auto-fix-formulaic.sh all  # scan all repos

set -euo pipefail

REPOS_DIR="${HOME}/Documents/hyperpolymath-repos"
FIXES_APPLIED=0
REPOS_SCANNED=0

# Canonical SHA pins — from Hypatia.Rules.SecurityErrors
declare -A SHA_PINS=(
  ["actions/checkout@v4"]="34e114876b0b11c390a56381ad16ebd13914f8d5"
  ["actions/checkout@v5"]="93cb6efe18208431cddfb8368fd83d5badbf9bfd"
  ["github/codeql-action@v3"]="6624720a57d4c312633c7b953db2f2da5bcb4c3a"
  ["ossf/scorecard-action@v2.4.0"]="62b2cac7ed8198b15735ed49ab1e5cf35480ba46"
  ["dtolnay/rust-toolchain@stable"]="4be9e76fd7c4901c61fb841f559994984270fce7"
  ["Swatinem/rust-cache@v2"]="779680da715d629ac1d338a641029a2f4372abb5"
  ["codecov/codecov-action@v5"]="671740ac38dd9b0130fbe1cec585b89eea48d3de"
  ["trufflesecurity/trufflehog@main"]="7ee2e0fdffec27d19ccbb8fb3dcf8a83b9d7f9e8"
  ["webfactory/ssh-agent@v0.9.0"]="dc588b651fe13675774614f8e6a936a468676387"
  ["ocaml/setup-ocaml@v3"]="dec6499fef64fc5d7ed43d43a87251b7b1c306f5"
  ["softprops/action-gh-release@v2"]="a06a81a03ee405af7f2048a818ed3f03bbf83c7b"
  ["actions/configure-pages@v5"]="983d7736d9b0ae728b81ab479565c72886d7745b"
  ["actions/jekyll-build-pages@v1"]="44a6e6beabd48582f863aeeb6cb2151cc1716697"
  ["actions/upload-pages-artifact@v3"]="56afc609e74202658d3ffba0e8f6dda462b719fa"
  ["actions/deploy-pages@v4"]="d6db90164ac5ed86f2b6aed7e0febac5b3c0c03e"
  ["ruby/setup-ruby@v1"]="09a7688d3b55cf0e976497ff046b70949eeaccfd"
  ["editorconfig-checker/action-editorconfig-checker@main"]="4054fa83a075fdf090bd098bdb1c09aaf64a4169"
  ["slsa-framework/slsa-github-generator@v2.1.0"]="f7dd8c54c2067bafc12ca7a55595d5ee9b75204a"
)

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

log() { echo "[hypatia-autofix] $*"; }
warn() { echo "[hypatia-autofix] WARNING: $*" >&2; }
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
# Fix 1: Pin unpinned GitHub Actions to SHA
# ---------------------------------------------------------------------------
fix_unpinned_actions() {
  local repo="$1"
  local workflows_dir="${repo}/.github/workflows"
  [ -d "$workflows_dir" ] || return 0

  for wf in "${workflows_dir}"/*.yml; do
    [ -f "$wf" ] || continue

    # Find lines with uses: action@vN (not already SHA-pinned)
    while IFS= read -r line; do
      # Extract action@version
      action_ref=$(echo "$line" | grep -oP 'uses:\s*\K[^@]+@v[0-9][^\s]*' || true)
      [ -z "$action_ref" ] && continue

      # Check if already SHA-pinned (40-char hex after @)
      if echo "$line" | grep -qP '@[0-9a-f]{40}'; then
        continue
      fi

      # Look up canonical SHA
      local sha="${SHA_PINS[$action_ref]:-}"
      if [ -n "$sha" ]; then
        local action_name="${action_ref%%@*}"
        local version="${action_ref##*@}"
        local old_pattern="${action_ref}"
        local new_pattern="${action_name}@${sha} # ${version}"

        sed -i "s|${old_pattern}|${new_pattern}|g" "$wf"
        fix "Pinned ${action_ref} → ${sha} in $(basename "$wf")"
      else
        warn "Unknown action ${action_ref} in $(basename "$wf") — add to SHA_PINS"
      fi
    done < <(grep -n 'uses:.*@v[0-9]' "$wf" 2>/dev/null || true)
  done
}

# ---------------------------------------------------------------------------
# Fix 2: Add missing permissions: read-all to workflows
# ---------------------------------------------------------------------------
fix_missing_permissions() {
  local repo="$1"
  local workflows_dir="${repo}/.github/workflows"
  [ -d "$workflows_dir" ] || return 0

  for wf in "${workflows_dir}"/*.yml; do
    [ -f "$wf" ] || continue

    if ! grep -q '^permissions:' "$wf" 2>/dev/null; then
      # Insert permissions after the 'on:' block (after first blank line after on:)
      # Simple approach: insert after SPDX header line or at line 2
      if grep -q 'SPDX-License-Identifier' "$wf"; then
        sed -i '/^name:/i permissions: read-all\n' "$wf"
      else
        sed -i '1a\permissions: read-all' "$wf"
      fi
      fix "Added permissions: read-all to $(basename "$wf")"
    fi
  done
}

# ---------------------------------------------------------------------------
# Fix 3: Detect binary artifacts tracked by git
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
# Fix 4: Check for AGPL references (should be PMPL)
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
    warn "AGPL-3.0 reference in: $f (should be PMPL-1.0-or-later)"
  done
}

# ---------------------------------------------------------------------------
# Fix 5: Check for missing SECURITY.md in public repos
# ---------------------------------------------------------------------------
check_security_md() {
  local repo="$1"
  if [ ! -f "${repo}/SECURITY.md" ]; then
    warn "Missing SECURITY.md in $(basename "$repo")"
  fi
}

# ---------------------------------------------------------------------------
# Fix 6: Check for missing .editorconfig
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
  fix_unpinned_actions "$repo"
  fix_missing_permissions "$repo"
  fix_tracked_binaries "$repo"
  fix_agpl_references "$repo"
  check_security_md "$repo"
  check_editorconfig "$repo"
  local after=$FIXES_APPLIED

  # Record visit with fix count
  record "$repo" "scan" "fixes=$((after - before))"
}

if [ "${1:-}" = "all" ]; then
  log "Scanning all repos in ${REPOS_DIR}..."
  for repo_dir in "${REPOS_DIR}"/*/; do
    [ -d "${repo_dir}/.git" ] || continue
    scan_repo "$repo_dir"
  done
elif [ -n "${1:-}" ]; then
  scan_repo "$1"
else
  echo "Usage: $0 [repo-path|all]"
  echo "  $0 ~/Documents/hyperpolymath-repos/aerie"
  echo "  $0 all"
  exit 1
fi

log "Done. Scanned ${REPOS_SCANNED} repos, applied ${FIXES_APPLIED} fixes."
