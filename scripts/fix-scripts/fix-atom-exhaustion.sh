#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-atom-exhaustion.sh — Replace String.to_atom with String.to_existing_atom
# Recipe: recipe-atom-exhaustion (confidence: 0.90, auto_fixable: true)
#
# Prevents BEAM atom table exhaustion by ensuring only pre-existing atoms
# are referenced from user input.
#
# Usage: fix-atom-exhaustion.sh <repo-path> [--file <specific-file>]

set -euo pipefail

REPO="${1:?Usage: fix-atom-exhaustion.sh <repo-path>}"
SPECIFIC_FILE="${3:-}"
FIXES=0

fix_file() {
  local file="$1"

  [[ "$file" == */.git/* ]] && return 0
  [[ "$file" == */_build/* ]] && return 0
  [[ "$file" == */deps/* ]] && return 0

  local count
  count=$(grep -c 'String\.to_atom\b' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && return 0

  sed -i 's/String\.to_atom\b/String.to_existing_atom/g' "$file"
  echo "[fix-atom-exhaustion] Replaced ${count} String.to_atom → String.to_existing_atom in ${file}"
  FIXES=$((FIXES + count))
}

if [[ -n "$SPECIFIC_FILE" ]]; then
  fix_file "$SPECIFIC_FILE"
else
  while IFS= read -r -d '' file; do
    fix_file "$file"
  done < <(find "$REPO" -type f \( -name "*.ex" -o -name "*.exs" \) \
    -not -path "*/.git/*" -not -path "*/_build/*" -not -path "*/deps/*" \
    -print0 2>/dev/null)
fi

echo "[fix-atom-exhaustion] Total: ${FIXES} replacement(s)"
