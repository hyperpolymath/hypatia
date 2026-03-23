#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-unwrap-to-match.sh — Report .unwrap() calls for conversion to match/Result
# Recipe: recipe-unwrap-to-match + recipe-panic-to-result (confidence: 0.75)
#
# Reports Rust .unwrap() and panic!/todo!/unimplemented! usage for manual review.
# Does not auto-fix because replacements are context-dependent.
#
# Usage: fix-unwrap-to-match.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-unwrap-to-match.sh <repo-path>}"
REPORTS=0

while IFS= read -r -d '' file; do
  [[ "$file" == */target/* ]] && continue

  # Check for unwrap()
  unwrap_count=$(grep -c '\.unwrap()' "$file" 2>/dev/null || echo 0)
  # Check for panic macros
  panic_count=$(grep -cP '\b(panic!|todo!|unimplemented!|unreachable!)' "$file" 2>/dev/null || echo 0)

  total=$((unwrap_count + panic_count))
  [[ "$total" -eq 0 ]] && continue

  echo "[fix-unwrap-to-match] ${file}: ${unwrap_count} unwrap(), ${panic_count} panic macros"
  REPORTS=$((REPORTS + total))
done < <(find "$REPO" -type f -name "*.rs" \
  -not -path "*/.git/*" -not -path "*/target/*" \
  -print0 2>/dev/null)

echo "[fix-unwrap-to-match] Total: ${REPORTS} instance(s) reported for review"
