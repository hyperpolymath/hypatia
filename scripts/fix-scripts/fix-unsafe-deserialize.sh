#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-unsafe-deserialize.sh — Report unsafe deserialization patterns
# Recipe: recipe-safe-deserialize (confidence: 0.70, auto_fixable: false)
#
# Usage: fix-unsafe-deserialize.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-unsafe-deserialize.sh <repo-path>}"
REPORTS=0

PATTERNS=(
  'JSON\.parseExn'                  # ReScript unsafe JSON parse
  'Poison\.decode!'                 # Elixir unsafe decode
  ':erlang\.binary_to_term'         # Erlang unsafe deserialization
  'pickle\.loads?\b'               # Python pickle (if found)
  'yaml\.load\b(?!.*Loader)'      # Python yaml.load without Loader
  'serde_json::from_str.*unwrap'   # Rust unwrap on deser
)

while IFS= read -r -d '' file; do
  [[ "$file" == */.git/* ]] && continue
  [[ "$file" == */target/* ]] && continue
  [[ "$file" == */_build/* ]] && continue

  for pattern in "${PATTERNS[@]}"; do
    matches=$(grep -cnP "$pattern" "$file" 2>/dev/null || echo 0)
    if [[ "$matches" -gt 0 ]]; then
      echo "[fix-unsafe-deserialize] REPORT: ${file} — ${matches} unsafe deserialization(s)"
      REPORTS=$((REPORTS + matches))
      break
    fi
  done
done < <(find "$REPO" -type f \
  \( -name "*.res" -o -name "*.resi" -o -name "*.ex" -o -name "*.exs" \
     -o -name "*.rs" -o -name "*.gleam" \) \
  -not -path "*/.git/*" -not -path "*/target/*" -not -path "*/_build/*" \
  -print0 2>/dev/null)

echo "[fix-unsafe-deserialize] Total: ${REPORTS} unsafe deserialization(s) reported"
