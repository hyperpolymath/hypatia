#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-panic-path.sh — Comprehensive PanicPath reporter (all PA005 sub-patterns)
# Recipe: recipe-panic-to-result + recipe-unwrap-to-match (confidence: 0.75)
#
# Covers: .unwrap() on user input, lock results, SystemTime, unguarded Options;
# panic!/todo!/unimplemented! macros; .expect() with unhelpful messages.
#
# Usage: fix-panic-path.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-panic-path.sh <repo-path>}"
REPORTS=0

while IFS= read -r -d '' file; do
  [[ "$file" == */target/* ]] && continue
  [[ "$file" == */.git/* ]] && continue

  # All panic-adjacent patterns in one pass
  unwrap_count=$(grep -cP '\.unwrap\s*\(\s*\)' "$file" 2>/dev/null || echo 0)
  expect_count=$(grep -cP '\.expect\s*\(' "$file" 2>/dev/null || echo 0)
  macro_count=$(grep -cP '\b(panic!|todo!|unimplemented!|unreachable!)' "$file" 2>/dev/null || echo 0)

  total=$((unwrap_count + expect_count + macro_count))
  [[ "$total" -eq 0 ]] && continue

  # Classify which subtypes are present
  subtypes=""
  [[ "$unwrap_count" -gt 0 ]] && subtypes="${subtypes} unwrap(${unwrap_count})"
  [[ "$expect_count" -gt 0 ]] && subtypes="${subtypes} expect(${expect_count})"
  [[ "$macro_count" -gt 0 ]] && subtypes="${subtypes} macros(${macro_count})"

  echo "[fix-panic-path] ${file}:${subtypes}"

  # Show specific lines for unwrap on lock/systemtime (higher risk)
  grep -nP '\.(lock|read|write)\s*\(\s*\)\s*\.\s*unwrap\s*\(' "$file" 2>/dev/null \
    | sed 's/^/  [lock-unwrap] /' | head -2
  grep -nP '(duration_since|elapsed)\s*\([^)]*\)\s*\.\s*unwrap\s*\(' "$file" 2>/dev/null \
    | sed 's/^/  [time-unwrap] /' | head -2

  REPORTS=$((REPORTS + total))
done < <(find "$REPO" -type f -name "*.rs" \
  -not -path "*/.git/*" -not -path "*/target/*" -print0 2>/dev/null)

echo "[fix-panic-path] Total: ${REPORTS} PanicPath pattern(s) reported for review"
