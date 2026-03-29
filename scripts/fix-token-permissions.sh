#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# fix-token-permissions.sh — Add `permissions: read-all` to GitHub Actions workflows
#
# Checks every .github/workflows/*.yml file for a top-level `permissions:` block.
# If missing, inserts `permissions: read-all` between the `on:` trigger block and
# the `jobs:` block. This enforces the principle of least privilege for GITHUB_TOKEN.
#
# Idempotent: skips files that already declare top-level permissions.
#
# Usage: fix-token-permissions.sh <repo-path>

set -euo pipefail

# --- Arguments ---

REPO="${1:?Usage: fix-token-permissions.sh <repo-path>}"
WORKFLOWS_DIR="${REPO}/.github/workflows"
FIXES=0
SKIPPED=0

if [[ ! -d "$WORKFLOWS_DIR" ]]; then
  echo "[fix-token-perms] No .github/workflows directory found — nothing to do"
  exit 0
fi

# has_toplevel_permissions <file>
# Returns 0 if the file has a top-level `permissions:` key (not indented).
has_toplevel_permissions() {
  local file="$1"
  # Match `permissions:` at the start of a line (no leading whitespace).
  # This distinguishes top-level from job-level permissions blocks.
  grep -qE '^permissions:' "$file" 2>/dev/null
}

# insert_permissions_before_jobs <file>
# Inserts `permissions: read-all` on its own line immediately before `jobs:`.
# Falls back to appending after `on:` block if `jobs:` is missing.
insert_permissions_before_jobs() {
  local file="$1"
  local tmpfile
  tmpfile="$(mktemp)"

  local inserted=false
  local in_on_block=false
  local found_jobs=false

  while IFS= read -r line || [[ -n "$line" ]]; do
    # Detect the `jobs:` top-level key
    if [[ "$line" =~ ^jobs: ]] && [[ "$inserted" == false ]]; then
      # Insert permissions block before jobs:
      echo ""
      echo "permissions: read-all"
      echo ""
      echo "$line"
      inserted=true
      found_jobs=true
      continue
    fi

    echo "$line"
  done < "$file" > "$tmpfile"

  # If we never found `jobs:`, try inserting after the `on:` block.
  # The `on:` block ends at the next top-level key (non-indented, non-comment line).
  if [[ "$inserted" == false ]]; then
    local tmpfile2
    tmpfile2="$(mktemp)"
    local past_on=false
    local on_found=false
    inserted=false

    while IFS= read -r line || [[ -n "$line" ]]; do
      # Detect start of `on:` block
      if [[ "$line" =~ ^on: ]] || [[ "$line" =~ ^\"on\": ]] || [[ "$line" =~ ^\'on\': ]]; then
        on_found=true
        echo "$line"
        continue
      fi

      # If we're past the `on:` block and hit a non-indented non-comment line
      if [[ "$on_found" == true ]] && [[ "$past_on" == false ]]; then
        if [[ "$line" =~ ^[a-zA-Z] ]] && [[ ! "$line" =~ ^# ]]; then
          past_on=true
          if [[ "$inserted" == false ]]; then
            echo ""
            echo "permissions: read-all"
            echo ""
            inserted=true
          fi
        fi
      fi

      echo "$line"
    done < "$file" > "$tmpfile2"

    if [[ "$inserted" == true ]]; then
      mv "$tmpfile2" "$tmpfile"
    else
      rm -f "$tmpfile2"
    fi
  fi

  if [[ "$inserted" == true ]]; then
    mv "$tmpfile" "$file"
    return 0
  else
    rm -f "$tmpfile"
    return 1
  fi
}

# --- Main processing loop ---

for wf in "${WORKFLOWS_DIR}"/*.yml "${WORKFLOWS_DIR}"/*.yaml; do
  [[ -f "$wf" ]] || continue

  wf_basename="$(basename "$wf")"

  # Skip files that already have top-level permissions
  if has_toplevel_permissions "$wf"; then
    SKIPPED=$((SKIPPED + 1))
    continue
  fi

  # Insert permissions: read-all
  if insert_permissions_before_jobs "$wf"; then
    echo "[fix-token-perms] Added permissions: read-all to ${wf_basename}"
    FIXES=$((FIXES + 1))
  else
    echo "[fix-token-perms] WARNING: Could not find insertion point in ${wf_basename}" >&2
  fi

done

# --- Summary ---

echo ""
echo "[fix-token-perms] Summary:"
echo "  Fixed:   ${FIXES}"
echo "  Skipped: ${SKIPPED} (already have permissions)"
