#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-unsafe-type-coercion.sh — Report unsafe type coercion patterns
# Recipe: recipe-unsafe-type-coercion (confidence: 0.80, auto_fixable: false)
#
# Detects: Obj.magic (OCaml), unsafeCoerce (Haskell), believe_me (Idris2),
# transmute (Rust), cast Refl (Idris2).
#
# Usage: fix-unsafe-type-coercion.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-unsafe-type-coercion.sh <repo-path>}"
REPORTS=0

declare -A LANG_PATTERNS=(
  ["*.ml"]="Obj\.magic"
  ["*.hs"]="unsafeCoerce|unsafePerformIO"
  ["*.idr"]="believe_me|assert_total|cast\s+Refl"
  ["*.rs"]="transmute\b|transmute_copy"
  ["*.v"]="Admitted\."
  ["*.lean"]="sorry\b"
)

for glob in "${!LANG_PATTERNS[@]}"; do
  pattern="${LANG_PATTERNS[$glob]}"
  while IFS= read -r -d '' file; do
    [[ "$file" == */.git/* ]] && continue
    [[ "$file" == */target/* ]] && continue
    [[ "$file" == */_build/* ]] && continue
    [[ "$file" == */.build/* ]] && continue
    [[ "$file" == */.lake/* ]] && continue
    [[ "$file" == */external_corpora/* ]] && continue

    count=$(grep -cP "$pattern" "$file" 2>/dev/null || echo 0)
    if [[ "$count" -gt 0 ]]; then
      echo "[fix-unsafe-type-coercion] ${file}: ${count} unsafe coercion(s)"
      grep -nP "$pattern" "$file" 2>/dev/null | head -3
      REPORTS=$((REPORTS + count))
    fi
  done < <(find "$REPO" -type f -name "$glob" \
    -not -path "*/.git/*" -not -path "*/target/*" -not -path "*/_build/*" \
    -not -path "*/.build/*" -not -path "*/.lake/*" -not -path "*/external_corpora/*" \
    -print0 2>/dev/null)
done

echo "[fix-unsafe-type-coercion] Total: ${REPORTS} unsafe coercion(s) reported"
