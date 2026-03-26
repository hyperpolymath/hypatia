#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-sorry-lean.sh — Report sorry/Admitted proofs in Lean4/Coq code
# Recipe: recipe-fix-sorry-lean (confidence: 0.80, auto_fixable: false)
#
# Scans for incomplete proofs and classifies them by type.
# Cannot auto-fix — proofs require mathematical reasoning.
#
# Usage: fix-sorry-lean.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-sorry-lean.sh <repo-path>}"
LEAN_SORRY=0
COQ_ADMITTED=0

echo "[fix-sorry-lean] Scanning for incomplete proofs..."

# Lean4: sorry
while IFS= read -r -d '' file; do
  [[ "$file" == */.lake/* ]] && continue
  [[ "$file" == */lake-packages/* ]] && continue

  count=$(grep -c '\bsorry\b' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && continue

  echo "[fix-sorry-lean] LEAN4 ${file}: ${count} sorry"
  grep -n '\bsorry\b' "$file" 2>/dev/null | head -10
  LEAN_SORRY=$((LEAN_SORRY + count))
done < <(find "$REPO" -type f -name "*.lean" \
  -not -path "*/.git/*" -not -path "*/.lake/*" -not -path "*/lake-packages/*" \
  -print0 2>/dev/null)

# Coq: Admitted
while IFS= read -r -d '' file; do
  count=$(grep -c '\bAdmitted\.' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && continue

  echo "[fix-sorry-lean] COQ ${file}: ${count} Admitted"
  grep -n '\bAdmitted\.' "$file" 2>/dev/null | head -10
  COQ_ADMITTED=$((COQ_ADMITTED + count))
done < <(find "$REPO" -type f -name "*.v" \
  -not -path "*/.git/*" \
  -print0 2>/dev/null)

TOTAL=$((LEAN_SORRY + COQ_ADMITTED))
echo "[fix-sorry-lean] Summary: ${LEAN_SORRY} Lean4 sorry, ${COQ_ADMITTED} Coq Admitted (${TOTAL} total)"
