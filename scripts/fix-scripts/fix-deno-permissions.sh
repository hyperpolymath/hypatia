#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-deno-permissions.sh — Report Deno -A (all permissions) usage
# Recipe: recipe-deno-least-privilege (confidence: 0.75, auto_fixable: false)
#
# Detects `deno run -A` or `--allow-all` and reports for least-privilege review.
#
# NOTE FOR SECURITY SCANNERS: this script DETECTS `deno -A` usage in
# other files; it does not INVOKE Deno with broad permissions itself.
# The `-A` and `--allow-all` strings below are grep patterns, not
# command invocations. (panic-attack false positive — flagged because
# of the literal `-A` in the regex.)
#
# Usage: fix-deno-permissions.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-deno-permissions.sh <repo-path>}"
REPORTS=0

while IFS= read -r -d '' file; do
  [[ "$file" == */.git/* ]] && continue

  count=$(grep -cP 'deno\s+(run|task).*(-A\b|--allow-all)' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && continue

  echo "[fix-deno-permissions] REPORT: ${file} — ${count} deno -A usage(s)"
  grep -nP 'deno\s+(run|task).*(-A\b|--allow-all)' "$file" 2>/dev/null | head -5
  REPORTS=$((REPORTS + count))
done < <(find "$REPO" -type f \
  \( -name "*.sh" -o -name "*.yml" -o -name "*.yaml" -o -name "deno.json" \
     -o -name "deno.jsonc" -o -name "Justfile" -o -name "justfile" \
     -o -name "Makefile" \) \
  -not -path "*/.git/*" \
  -print0 2>/dev/null)

echo "[fix-deno-permissions] Total: ${REPORTS} deno -A usage(s) reported"
