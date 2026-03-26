#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-sql-parameterize.sh — Report string-concatenated SQL for parameterization
# Recipe: recipe-fix-sql-injection (confidence: 0.90, auto_fixable: false)
#
# Detects SQL queries built by string concatenation/interpolation.
# Reports for manual review — SQL context is too varied for safe auto-fix.
#
# Usage: fix-sql-parameterize.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-sql-parameterize.sh <repo-path>}"
REPORTS=0

# Patterns suggesting SQL string concatenation
SQL_PATTERNS=(
  '"SELECT.*\+.*"'       # Java/JS style
  '"INSERT.*\+.*"'
  '"UPDATE.*\+.*"'
  '"DELETE.*\+.*"'
  '"SELECT.*#\{'         # Ruby/Elixir interpolation
  '"INSERT.*#\{'
  'query.*\$\{'          # JS template literal
  'execute.*\$\{'
  'f"SELECT'             # Python f-string
  'f"INSERT'
  '"SELECT.*\.\.'        # Rust format
)

while IFS= read -r -d '' file; do
  [[ "$file" == */.git/* ]] && continue
  [[ "$file" == */target/* ]] && continue
  [[ "$file" == */_build/* ]] && continue
  [[ "$file" == */node_modules/* ]] && continue

  for pattern in "${SQL_PATTERNS[@]}"; do
    matches=$(grep -cnP "$pattern" "$file" 2>/dev/null || echo 0)
    if [[ "$matches" -gt 0 ]]; then
      echo "[fix-sql-parameterize] REPORT: ${file} — ${matches} potential SQL injection pattern(s)"
      grep -nP "$pattern" "$file" 2>/dev/null | head -3
      REPORTS=$((REPORTS + matches))
      break  # Don't double-count same file
    fi
  done
done < <(find "$REPO" -type f \
  \( -name "*.ex" -o -name "*.exs" -o -name "*.rs" -o -name "*.js" \
     -o -name "*.res" -o -name "*.gleam" -o -name "*.rb" \) \
  -not -path "*/.git/*" -not -path "*/target/*" -not -path "*/_build/*" \
  -print0 2>/dev/null)

echo "[fix-sql-parameterize] Total: ${REPORTS} potential SQL injection(s) reported"
