#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-obj-magic-bypass.sh — Report Obj.magic type-system bypass in OCaml/ReScript
# Recipe: recipe-unsafe-type-coercion (confidence: 0.75, auto_fixable: false)
#
# Obj.magic casts any value to any type, defeating the type checker entirely.
# Each call should be replaced with a safe coercion, phantom type, or GADT approach.
#
# Usage: fix-obj-magic-bypass.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-obj-magic-bypass.sh <repo-path>}"
REPORTS=0

while IFS= read -r -d '' file; do
  [[ "$file" == */_build/* ]] && continue
  [[ "$file" == */.git/* ]] && continue

  count=$(grep -cP '\bObj\.magic\b' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && continue

  echo "[fix-obj-magic-bypass] ${file}: ${count} Obj.magic call(s)"
  grep -nP '\bObj\.magic\b' "$file" 2>/dev/null | head -3
  REPORTS=$((REPORTS + count))
done < <(find "$REPO" -type f \( -name "*.ml" -o -name "*.mli" -o -name "*.res" -o -name "*.resi" \) \
  -not -path "*/.git/*" -not -path "*/_build/*" -print0 2>/dev/null)

echo "[fix-obj-magic-bypass] Total: ${REPORTS} Obj.magic bypass(es) reported for review"
