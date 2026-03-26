#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-secret-to-env.sh — Report hardcoded secrets that should be env vars
# Recipe: recipe-secret-to-env (confidence: 0.55, auto_fixable: false)
#
# Detects hardcoded credentials that should use environment variables.
# Lower confidence — requires manual verification of each finding.
#
# Usage: fix-secret-to-env.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-secret-to-env.sh <repo-path>}"
REPORTS=0

# Look for hardcoded API keys/tokens in assignments
PATTERNS=(
  '(api_key|apikey|api_token|auth_token)\s*[:=]\s*"[^"]{10,}'
  '(database_url|db_url|db_password)\s*[:=]\s*"[^"]{5,}'
  '(smtp_password|mail_password)\s*[:=]\s*"[^"]{5,}'
)

while IFS= read -r -d '' file; do
  [[ "$file" == */.git/* ]] && continue
  [[ "$file" == */.env.example ]] && continue
  [[ "$file" == *test* ]] && continue  # Skip test files

  for pattern in "${PATTERNS[@]}"; do
    matches=$(grep -ciP "$pattern" "$file" 2>/dev/null || echo 0)
    if [[ "$matches" -gt 0 ]]; then
      echo "[fix-secret-to-env] REPORT: ${file} — ${matches} hardcoded credential(s)"
      REPORTS=$((REPORTS + matches))
      break
    fi
  done
done < <(find "$REPO" -type f \
  \( -name "*.ex" -o -name "*.exs" -o -name "*.rs" -o -name "*.toml" \
     -o -name "*.json" -o -name "*.yml" -o -name "*.yaml" \) \
  -not -path "*/.git/*" -not -path "*/target/*" -not -path "*/_build/*" \
  -print0 2>/dev/null)

echo "[fix-secret-to-env] Total: ${REPORTS} hardcoded credential(s) reported"
