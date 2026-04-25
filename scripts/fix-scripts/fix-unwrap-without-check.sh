#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-unwrap-without-check.sh — Report .unwrap() calls without a prior .is_ok()/.is_some() guard
# Recipe: recipe-unwrap-to-match (confidence: 0.75, auto_fixable: false)
#
# Calling .unwrap() on a Result or Option without first checking .is_ok()/.is_some()
# can panic at runtime. Use match, if let, or ? instead.
#
# Usage: fix-unwrap-without-check.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-unwrap-without-check.sh <repo-path>}"
REPORTS=0

while IFS= read -r -d '' file; do
  [[ "$file" == */target/* ]] && continue
  [[ "$file" == */.git/* ]] && continue

  # Count all .unwrap() calls first
  unwrap_count=$(grep -cP '\.unwrap\s*\(\s*\)' "$file" 2>/dev/null || echo 0)
  [[ "$unwrap_count" -eq 0 ]] && continue

  # Count .is_ok() / .is_some() / .is_err() guards in the same file
  guard_count=$(grep -cP '\.(is_ok|is_some|is_err|is_none)\s*\(\s*\)' "$file" 2>/dev/null || echo 0)

  # Flag if unwraps outnumber guards — heuristic: not proof of missing check
  if [[ "$unwrap_count" -gt "$guard_count" ]]; then
    unguarded=$((unwrap_count - guard_count))
    echo "[fix-unwrap-without-check] ${file}: ${unwrap_count} unwrap(), ${guard_count} guard(s) — ~${unguarded} possibly unguarded"
    grep -nP '\.unwrap\s*\(\s*\)' "$file" 2>/dev/null | head -3
    REPORTS=$((REPORTS + unguarded))
  fi
done < <(find "$REPO" -type f -name "*.rs" \
  -not -path "*/.git/*" -not -path "*/target/*" -print0 2>/dev/null)

echo "[fix-unwrap-without-check] Total: ~${REPORTS} potentially unguarded unwrap(s) reported"
