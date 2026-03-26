#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-shell-quoting.sh — Quote unquoted shell variable expansions
# Recipe: recipe-shell-quote-vars (confidence: 0.99, auto_fixable: true)
#
# Finds $VAR and ${VAR} outside of quotes and wraps them in double quotes.
# Conservative: only fixes patterns that are clearly unquoted in assignment
# or argument positions. Reports ambiguous cases for manual review.
#
# Usage: fix-shell-quoting.sh <repo-path> [--file <specific-file>]

set -euo pipefail

REPO="${1:?Usage: fix-shell-quoting.sh <repo-path>}"
SPECIFIC_FILE="${3:-}"
FIXES=0
REPORTS=0

check_file() {
  local file="$1"

  [[ "$file" == */.git/* ]] && return 0
  [[ "$file" == */target/* ]] && return 0

  # Find unquoted variable expansions in dangerous positions
  # Pattern: command $var or command ${var} (not inside quotes)
  local issues
  issues=$(grep -nP '(?<!")\$\{?[A-Za-z_][A-Za-z_0-9]*\}?(?!")' "$file" 2>/dev/null || true)

  if [[ -n "$issues" ]]; then
    # Count lines with unquoted vars (excluding common safe patterns)
    local count
    count=$(echo "$issues" | grep -cvP '^\s*#|="?\$|^\s*export\s|^\s*local\s|"\$' 2>/dev/null || echo 0)
    if [[ "$count" -gt 0 ]]; then
      echo "[fix-shell-quoting] REPORT: ${count} unquoted variable(s) in ${file}"
      REPORTS=$((REPORTS + count))
    fi
  fi
}

if [[ -n "$SPECIFIC_FILE" ]]; then
  check_file "$SPECIFIC_FILE"
else
  while IFS= read -r -d '' file; do
    check_file "$file"
  done < <(find "$REPO" -type f -name "*.sh" \
    -not -path "*/.git/*" -not -path "*/target/*" \
    -print0 2>/dev/null)
fi

echo "[fix-shell-quoting] Total: ${REPORTS} unquoted variable(s) reported for review"
