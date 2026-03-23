#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-heredoc-install.sh — Report curl|bash install patterns
# Recipe: recipe-fix-heredoc-install (confidence: 0.90, auto_fixable: false)
#
# Detects dangerous "curl URL | bash" and "wget URL | sh" patterns.
# Reports for manual review — replacement requires verified download + checksum.
#
# Usage: fix-heredoc-install.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-heredoc-install.sh <repo-path>}"
REPORTS=0

PATTERNS=(
  'curl.*\|\s*(ba)?sh'
  'wget.*\|\s*(ba)?sh'
  'curl.*\|\s*sudo\s+(ba)?sh'
  'wget.*\|\s*sudo\s+(ba)?sh'
)

while IFS= read -r -d '' file; do
  [[ "$file" == */.git/* ]] && continue

  for pattern in "${PATTERNS[@]}"; do
    matches=$(grep -cnP "$pattern" "$file" 2>/dev/null || echo 0)
    if [[ "$matches" -gt 0 ]]; then
      echo "[fix-heredoc-install] REPORT: ${file} — ${matches} curl|bash pattern(s)"
      grep -nP "$pattern" "$file" 2>/dev/null | head -5
      REPORTS=$((REPORTS + matches))
      break
    fi
  done
done < <(find "$REPO" -type f \
  \( -name "*.sh" -o -name "*.yml" -o -name "*.yaml" -o -name "*.md" \
     -o -name "*.adoc" -o -name "Containerfile" -o -name "Dockerfile" \) \
  -not -path "*/.git/*" \
  -print0 2>/dev/null)

echo "[fix-heredoc-install] Total: ${REPORTS} curl|bash pattern(s) reported"
