#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-innerhtml.sh — Report innerHTML usage for XSS prevention
# Recipe: recipe-fix-innerHTML (confidence: 0.85, auto_fixable: false)
#
# Reports innerHTML/outerHTML assignments that could enable XSS.
# Replacement with textContent or DOM APIs is context-dependent.
#
# Usage: fix-innerhtml.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-innerhtml.sh <repo-path>}"
REPORTS=0

while IFS= read -r -d '' file; do
  [[ "$file" == */.git/* ]] && continue
  [[ "$file" == */node_modules/* ]] && continue

  count=$(grep -cP '\b(innerHTML|outerHTML)\s*=' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && continue

  echo "[fix-innerhtml] REPORT: ${file} — ${count} innerHTML/outerHTML assignment(s)"
  grep -nP '\b(innerHTML|outerHTML)\s*=' "$file" 2>/dev/null | head -5
  REPORTS=$((REPORTS + count))
done < <(find "$REPO" -type f \
  \( -name "*.js" -o -name "*.mjs" -o -name "*.res" -o -name "*.ts" \) \
  -not -path "*/.git/*" -not -path "*/node_modules/*" \
  -print0 2>/dev/null)

echo "[fix-innerhtml] Total: ${REPORTS} innerHTML/outerHTML assignment(s) reported"
