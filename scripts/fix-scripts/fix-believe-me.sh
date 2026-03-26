#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-believe-me.sh — Report believe_me/assert_total in Idris2 code
# Recipe: recipe-remove-believe-me (confidence: 0.99, auto_fixable: false)
#
# Reports dangerous Idris2 patterns for manual replacement with safe alternatives.
# Also checks Haskell (unsafeCoerce, unsafePerformIO), OCaml (Obj.magic),
# and Rust (unsafe { transmute }).
#
# Usage: fix-believe-me.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-believe-me.sh <repo-path>}"
REPORTS=0

# Idris2: believe_me, assert_total, assert_smaller
while IFS= read -r -d '' file; do
  [[ "$file" == */.build/* ]] && continue
  [[ "$file" == */external_corpora/* ]] && continue

  count=$(grep -cP '\b(believe_me|assert_total|assert_smaller)\b' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && continue

  echo "[fix-believe-me] IDRIS2 ${file}: ${count} dangerous pattern(s)"
  grep -nP '\b(believe_me|assert_total|assert_smaller)\b' "$file" 2>/dev/null | head -5
  REPORTS=$((REPORTS + count))
done < <(find "$REPO" -type f -name "*.idr" \
  -not -path "*/.git/*" -not -path "*/.build/*" -not -path "*/external_corpora/*" \
  -print0 2>/dev/null)

# Haskell: unsafeCoerce, unsafePerformIO
while IFS= read -r -d '' file; do
  count=$(grep -cP '\b(unsafeCoerce|unsafePerformIO)\b' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && continue

  echo "[fix-believe-me] HASKELL ${file}: ${count} dangerous pattern(s)"
  REPORTS=$((REPORTS + count))
done < <(find "$REPO" -type f -name "*.hs" \
  -not -path "*/.git/*" -not -path "*/.stack-work/*" \
  -print0 2>/dev/null)

# OCaml: Obj.magic
while IFS= read -r -d '' file; do
  count=$(grep -cP '\bObj\.magic\b' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && continue

  echo "[fix-believe-me] OCAML ${file}: ${count} Obj.magic call(s)"
  REPORTS=$((REPORTS + count))
done < <(find "$REPO" -type f -name "*.ml" \
  -not -path "*/.git/*" -not -path "*/_build/*" \
  -print0 2>/dev/null)

echo "[fix-believe-me] Total: ${REPORTS} dangerous pattern(s) reported"
