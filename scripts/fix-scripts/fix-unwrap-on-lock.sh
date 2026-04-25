#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-unwrap-on-lock.sh — Report .unwrap() on Mutex/RwLock lock results
# Recipe: recipe-unwrap-to-match (confidence: 0.75, auto_fixable: false)
#
# Mutex::lock() returns Err on poisoned locks. Unwrapping panics the thread;
# callers should handle PoisonError or call .lock().unwrap_or_else(|e| e.into_inner()).
#
# Usage: fix-unwrap-on-lock.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-unwrap-on-lock.sh <repo-path>}"
REPORTS=0

while IFS= read -r -d '' file; do
  [[ "$file" == */target/* ]] && continue
  [[ "$file" == */.git/* ]] && continue

  # Match .lock().unwrap() or .read().unwrap() or .write().unwrap()
  count=$(grep -cP '\.(lock|read|write)\s*\(\s*\)\s*\.\s*unwrap\s*\(' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && continue

  echo "[fix-unwrap-on-lock] ${file}: ${count} mutex/rwlock unwrap(s)"
  grep -nP '\.(lock|read|write)\s*\(\s*\)\s*\.\s*unwrap\s*\(' "$file" 2>/dev/null | head -3
  REPORTS=$((REPORTS + count))
done < <(find "$REPO" -type f -name "*.rs" \
  -not -path "*/.git/*" -not -path "*/target/*" -print0 2>/dev/null)

echo "[fix-unwrap-on-lock] Total: ${REPORTS} lock unwrap(s) reported for review"
