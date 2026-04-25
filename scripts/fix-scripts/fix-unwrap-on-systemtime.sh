#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-unwrap-on-systemtime.sh — Report .unwrap() on SystemTime duration_since results
# Recipe: recipe-unwrap-to-match (confidence: 0.75, auto_fixable: false)
#
# SystemTime::now().duration_since() can return Err if the system clock goes backward.
# Unwrapping panics; callers should handle SystemTimeError with match or unwrap_or.
#
# Usage: fix-unwrap-on-systemtime.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-unwrap-on-systemtime.sh <repo-path>}"
REPORTS=0

while IFS= read -r -d '' file; do
  [[ "$file" == */target/* ]] && continue
  [[ "$file" == */.git/* ]] && continue

  # Match duration_since(...).unwrap() or elapsed().unwrap()
  count=$(grep -cP '(duration_since|elapsed)\s*\([^)]*\)\s*\.\s*unwrap\s*\(' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && continue

  echo "[fix-unwrap-on-systemtime] ${file}: ${count} SystemTime unwrap(s)"
  grep -nP '(duration_since|elapsed)\s*\([^)]*\)\s*\.\s*unwrap\s*\(' "$file" 2>/dev/null | head -3
  REPORTS=$((REPORTS + count))
done < <(find "$REPO" -type f -name "*.rs" \
  -not -path "*/.git/*" -not -path "*/target/*" -print0 2>/dev/null)

echo "[fix-unwrap-on-systemtime] Total: ${REPORTS} SystemTime unwrap(s) reported for review"
