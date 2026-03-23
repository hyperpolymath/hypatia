#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-cors-wildcard.sh — Report CORS wildcard (*) usage
# Recipe: recipe-fix-cors-wildcard (confidence: 0.85, auto_fixable: false)
#
# Reports Access-Control-Allow-Origin: * patterns for manual tightening.
#
# Usage: fix-cors-wildcard.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-cors-wildcard.sh <repo-path>}"
REPORTS=0

while IFS= read -r -d '' file; do
  [[ "$file" == */.git/* ]] && continue
  [[ "$file" == */target/* ]] && continue

  count=$(grep -ciP '(access-control-allow-origin|cors).*\*' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && continue

  echo "[fix-cors-wildcard] REPORT: ${file} — ${count} CORS wildcard pattern(s)"
  grep -niP '(access-control-allow-origin|cors).*\*' "$file" 2>/dev/null | head -3
  REPORTS=$((REPORTS + count))
done < <(find "$REPO" -type f \
  \( -name "*.ex" -o -name "*.exs" -o -name "*.rs" -o -name "*.js" \
     -o -name "*.json" -o -name "*.yml" -o -name "*.yaml" -o -name "*.toml" \) \
  -not -path "*/.git/*" -not -path "*/target/*" \
  -print0 2>/dev/null)

echo "[fix-cors-wildcard] Total: ${REPORTS} CORS wildcard(s) reported"
