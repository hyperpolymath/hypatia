#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# fix-pinned-dependencies.sh — Resolve GitHub Actions tags to full SHA pins
#
# Unlike fix-unpinned-actions.sh (which uses a hardcoded lookup table of known
# SHAs), this script resolves ANY action tag to its SHA via the GitHub API.
# This covers third-party and custom actions not in the canonical table.
#
# Requirements:
#   - gh CLI authenticated (GITHUB_TOKEN or gh auth login)
#   - jq (for JSON parsing)
#
# Usage: fix-pinned-dependencies.sh <repo-path>
#
# Behaviour:
#   - Finds all .github/workflows/*.yml files
#   - For each `uses: owner/repo@vN` or `uses: owner/repo@tag`, resolves to SHA
#   - Replaces @tag with @SHA # tag (preserving the tag as a comment)
#   - Handles @main and @master branch refs
#   - Skips actions already pinned to a 40-char hex SHA
#   - Skips local actions (./path) and Docker actions (docker://)
#   - Caches resolved SHAs to avoid redundant API calls within one run

set -euo pipefail

# --- Dependency checks ---

if ! command -v gh &>/dev/null; then
  echo "[fix-pinned-deps] ERROR: gh CLI not found. Install: https://cli.github.com/" >&2
  exit 1
fi

if ! command -v jq &>/dev/null; then
  echo "[fix-pinned-deps] ERROR: jq not found. Install: sudo dnf install jq" >&2
  exit 1
fi

if ! gh auth status &>/dev/null 2>&1; then
  echo "[fix-pinned-deps] ERROR: gh not authenticated. Run: gh auth login" >&2
  exit 1
fi

# --- Arguments ---

REPO="${1:?Usage: fix-pinned-dependencies.sh <repo-path>}"
WORKFLOWS_DIR="${REPO}/.github/workflows"
FIXES=0
ERRORS=0
SKIPPED=0

if [[ ! -d "$WORKFLOWS_DIR" ]]; then
  echo "[fix-pinned-deps] No .github/workflows directory found — nothing to do"
  exit 0
fi

# --- SHA resolution cache (associative array) ---
# Key: "owner/repo@ref" -> Value: resolved SHA
declare -A SHA_CACHE

# resolve_ref_to_sha <owner/repo> <ref>
# Outputs the SHA on stdout, returns 0 on success, 1 on failure.
resolve_ref_to_sha() {
  local action_slug="$1"
  local ref="$2"
  local cache_key="${action_slug}@${ref}"

  # Check cache first
  if [[ -n "${SHA_CACHE[$cache_key]+x}" ]]; then
    echo "${SHA_CACHE[$cache_key]}"
    return 0
  fi

  local owner repo
  owner="${action_slug%%/*}"
  repo="${action_slug#*/}"

  # Strip any sub-path (e.g., github/codeql-action/analyze -> github/codeql-action)
  repo="${repo%%/*}"

  local full_repo="${owner}/${repo}"
  local sha=""

  # Strategy 1: Try as a tag ref
  sha=$(gh api "repos/${full_repo}/git/ref/tags/${ref}" \
    --jq '.object.sha // empty' 2>/dev/null || true)

  # Tags can be annotated (object type "tag") — dereference to commit
  if [[ -n "$sha" ]]; then
    local obj_type
    obj_type=$(gh api "repos/${full_repo}/git/tags/${sha}" \
      --jq '.object.type // empty' 2>/dev/null || true)
    if [[ "$obj_type" == "commit" ]]; then
      sha=$(gh api "repos/${full_repo}/git/tags/${sha}" \
        --jq '.object.sha' 2>/dev/null || true)
    fi
  fi

  # Strategy 2: Try as a branch ref
  if [[ -z "$sha" ]]; then
    sha=$(gh api "repos/${full_repo}/git/ref/heads/${ref}" \
      --jq '.object.sha // empty' 2>/dev/null || true)
  fi

  # Strategy 3: Try matching commit directly (for cases like @v1 -> release branch)
  if [[ -z "$sha" ]]; then
    sha=$(gh api "repos/${full_repo}/commits/${ref}" \
      --jq '.sha // empty' 2>/dev/null || true)
  fi

  if [[ -n "$sha" && ${#sha} -eq 40 ]]; then
    SHA_CACHE["$cache_key"]="$sha"
    echo "$sha"
    return 0
  fi

  return 1
}

# is_sha <string> — returns 0 if the string is a 40-char hex SHA
is_sha() {
  [[ "$1" =~ ^[0-9a-f]{40}$ ]]
}

# --- Main processing loop ---

for wf in "${WORKFLOWS_DIR}"/*.yml "${WORKFLOWS_DIR}"/*.yaml; do
  [[ -f "$wf" ]] || continue

  wf_basename="$(basename "$wf")"
  wf_modified=false

  # Read the file content for in-memory processing
  wf_content="$(cat "$wf")"

  # Extract all `uses:` lines with their action references
  # Match pattern: uses: owner/repo@ref or uses: owner/repo/sub@ref
  # Skip local actions (./) and Docker actions (docker://)
  while IFS= read -r match_line; do
    [[ -z "$match_line" ]] && continue

    # Extract the full action reference (e.g., "actions/checkout@v4")
    action_ref="$(echo "$match_line" | grep -oP 'uses:\s*\K[^\s#]+' 2>/dev/null || true)"
    [[ -z "$action_ref" ]] && continue

    # Skip local actions and Docker references
    if [[ "$action_ref" == ./* ]] || [[ "$action_ref" == docker://* ]]; then
      continue
    fi

    # Split into slug and ref
    if [[ "$action_ref" != *@* ]]; then
      continue
    fi

    action_slug="${action_ref%%@*}"
    action_tag="${action_ref##*@}"

    # Skip if already SHA-pinned
    if is_sha "$action_tag"; then
      SKIPPED=$((SKIPPED + 1))
      continue
    fi

    # Skip empty refs
    if [[ -z "$action_tag" ]]; then
      continue
    fi

    # Resolve the tag/branch to a SHA
    if resolved_sha="$(resolve_ref_to_sha "$action_slug" "$action_tag")"; then
      # Build the replacement string: action@SHA # tag
      old_ref="${action_slug}@${action_tag}"
      new_ref="${action_slug}@${resolved_sha} # ${action_tag}"

      # Only replace if old_ref appears without an existing SHA comment
      # Use sed to perform the replacement in the file
      # Escape slashes in paths for sed
      old_ref_escaped="$(printf '%s' "$old_ref" | sed 's/[&/\]/\\&/g')"
      new_ref_escaped="$(printf '%s' "$new_ref" | sed 's/[&/\]/\\&/g')"

      if grep -qF "$old_ref" "$wf"; then
        sed -i "s|${old_ref}|${new_ref}|g" "$wf"
        echo "[fix-pinned-deps] ${wf_basename}: ${action_slug}@${action_tag} -> ${resolved_sha:0:12}..."
        FIXES=$((FIXES + 1))
        wf_modified=true
      fi
    else
      echo "[fix-pinned-deps] WARNING: Could not resolve ${action_slug}@${action_tag} in ${wf_basename}" >&2
      ERRORS=$((ERRORS + 1))
    fi

  done < <(grep -n 'uses:' "$wf" 2>/dev/null || true)

done

# --- Summary ---

echo ""
echo "[fix-pinned-deps] Summary:"
echo "  Pinned:  ${FIXES}"
echo "  Skipped: ${SKIPPED} (already SHA-pinned)"
echo "  Errors:  ${ERRORS} (could not resolve)"

if [[ $ERRORS -gt 0 ]]; then
  exit 2
fi
