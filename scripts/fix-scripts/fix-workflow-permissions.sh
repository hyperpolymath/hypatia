#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-workflow-permissions.sh — Add permissions: read-all to GitHub Actions workflows
# Recipe: recipe-fix-workflow-permissions (confidence: 0.95, auto_fixable: true)
#
# Usage: fix-workflow-permissions.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-workflow-permissions.sh <repo-path>}"
WORKFLOWS_DIR="${REPO}/.github/workflows"
FIXES=0

[[ -d "$WORKFLOWS_DIR" ]] || { echo "[fix-workflow-permissions] No workflows dir"; exit 0; }

for wf in "${WORKFLOWS_DIR}"/*.yml "${WORKFLOWS_DIR}"/*.yaml; do
  [[ -f "$wf" ]] || continue

  # Skip if already has top-level permissions
  if grep -q '^permissions:' "$wf" 2>/dev/null; then
    continue
  fi

  # Insert permissions: read-all after the 'on:' block
  # Strategy: find the 'on:' line, then insert before the next top-level key
  # Simpler: insert right before the first 'jobs:' line
  if grep -q '^jobs:' "$wf"; then
    sed -i '/^jobs:/i\permissions: read-all\n' "$wf"
    echo "[fix-workflow-permissions] Added permissions: read-all to $(basename "$wf")"
    FIXES=$((FIXES + 1))
  fi
done

echo "[fix-workflow-permissions] Total: ${FIXES} workflow(s) fixed"
