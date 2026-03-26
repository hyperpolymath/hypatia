#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-eval-to-safe.sh — Report eval/exec usage
# Recipe: recipe-eval-to-safe-exec (confidence: 0.60, auto_fixable: false)
#
# Usage: fix-eval-to-safe.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-eval-to-safe.sh <repo-path>}"
REPORTS=0

PATTERNS=(
  'Code\.eval_string'    # Elixir
  'Code\.eval_quoted'    # Elixir
  '\beval\s*\('          # JavaScript
  '\bexec\s*\('          # Various
  'Function\s*\('        # JavaScript Function constructor
  'new\s+Function'       # JavaScript
)

while IFS= read -r -d '' file; do
  [[ "$file" == */.git/* ]] && continue
  [[ "$file" == */target/* ]] && continue
  [[ "$file" == */_build/* ]] && continue
  [[ "$file" == */node_modules/* ]] && continue

  for pattern in "${PATTERNS[@]}"; do
    matches=$(grep -cnP "$pattern" "$file" 2>/dev/null || echo 0)
    if [[ "$matches" -gt 0 ]]; then
      echo "[fix-eval-to-safe] REPORT: ${file} — ${matches} eval/exec call(s)"
      REPORTS=$((REPORTS + matches))
      break
    fi
  done
done < <(find "$REPO" -type f \
  \( -name "*.ex" -o -name "*.exs" -o -name "*.js" -o -name "*.mjs" -o -name "*.res" \) \
  -not -path "*/.git/*" -not -path "*/target/*" -not -path "*/_build/*" \
  -print0 2>/dev/null)

echo "[fix-eval-to-safe] Total: ${REPORTS} eval/exec call(s) reported"
