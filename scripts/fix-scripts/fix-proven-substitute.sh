#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-proven-substitute.sh — Report code that could use proven/ module equivalents
# Recipe: recipe-fix-proven-substitute (confidence: 0.85, auto_fixable: false)
#
# Cross-references unsafe patterns against the proven/ ecosystem of formally
# verified modules. Reports where a proven alternative exists.
#
# Usage: fix-proven-substitute.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-proven-substitute.sh <repo-path>}"
REPORTS=0

# Patterns with proven alternatives
declare -A PROVEN_ALTERNATIVES=(
  ["\.unwrap()"]="proven-result (safe unwrap with proof)"
  ["panic!"]="proven-result (type-safe error handling)"
  ["unsafe {"]="proven-ffi (verified FFI wrappers)"
  ["believe_me"]="proven-types (dependently typed alternatives)"
  ["String\.to_atom"]="proven-atom (bounded atom creation)"
  ["\beval\b"]="proven-dispatch (static dispatch maps)"
)

while IFS= read -r -d '' file; do
  [[ "$file" == */.git/* ]] && continue
  [[ "$file" == */target/* ]] && continue
  [[ "$file" == */_build/* ]] && continue
  [[ "$file" == */proven/* ]] && continue  # Don't report on proven itself

  for pattern in "${!PROVEN_ALTERNATIVES[@]}"; do
    alternative="${PROVEN_ALTERNATIVES[$pattern]}"
    matches=$(grep -cP "$pattern" "$file" 2>/dev/null || echo 0)
    if [[ "$matches" -gt 0 ]]; then
      echo "[fix-proven-substitute] ${file}: ${matches}x ${pattern} → consider ${alternative}"
      REPORTS=$((REPORTS + matches))
    fi
  done
done < <(find "$REPO" -type f \
  \( -name "*.rs" -o -name "*.ex" -o -name "*.exs" -o -name "*.idr" \) \
  -not -path "*/.git/*" -not -path "*/target/*" -not -path "*/_build/*" \
  -not -path "*/proven/*" \
  -print0 2>/dev/null)

echo "[fix-proven-substitute] Total: ${REPORTS} substitution opportunity(ies) reported"
