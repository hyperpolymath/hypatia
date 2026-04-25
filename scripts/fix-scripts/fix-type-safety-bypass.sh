#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-type-safety-bypass.sh — Report generic type-system bypass patterns
# Recipe: recipe-unsafe-type-coercion (confidence: 0.70, auto_fixable: false)
#
# Catches unknown type-safety bypasses across languages: Obj.magic, unsafeCoerce,
# transmute, believe_me, cast Refl, Admitted. Complements fix-unsafe-type-coercion.sh
# with a broader sweep for unknown/uncategorised patterns.
#
# Usage: fix-type-safety-bypass.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-type-safety-bypass.sh <repo-path>}"
REPORTS=0

declare -A PATTERNS=(
  ["*.ml"]="Obj\.magic|Obj\.repr|Obj\.obj\b"
  ["*.hs"]="unsafeCoerce|unsafePerformIO|unsafeDupablePerformIO"
  ["*.idr"]="believe_me|assert_total|cast\s+Refl"
  ["*.rs"]="std::mem::transmute\b|core::mem::transmute\b"
  ["*.v"]="Admitted\."
  ["*.lean"]="sorry\b"
  ["*.agda"]="postulate\b"
)

for glob in "${!PATTERNS[@]}"; do
  pattern="${PATTERNS[$glob]}"
  while IFS= read -r -d '' file; do
    [[ "$file" == */.git/* ]] && continue
    [[ "$file" == */target/* ]] && continue
    [[ "$file" == */_build/* ]] && continue
    [[ "$file" == */.build/* ]] && continue
    [[ "$file" == */.lake/* ]] && continue

    count=$(grep -cP "$pattern" "$file" 2>/dev/null || echo 0)
    [[ "$count" -eq 0 ]] && continue

    echo "[fix-type-safety-bypass] ${file}: ${count} type-bypass pattern(s)"
    grep -nP "$pattern" "$file" 2>/dev/null | head -3
    REPORTS=$((REPORTS + count))
  done < <(find "$REPO" -type f -name "$glob" \
    -not -path "*/.git/*" -not -path "*/target/*" -not -path "*/_build/*" \
    -not -path "*/.build/*" -not -path "*/.lake/*" \
    -print0 2>/dev/null)
done

echo "[fix-type-safety-bypass] Total: ${REPORTS} type-safety bypass(es) reported"
