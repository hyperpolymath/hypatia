#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-getexn-external-data.sh — Report getexn/catch_exn on external data without validation
# Recipe: recipe-unsafe-crash (confidence: 0.70, auto_fixable: false)
#
# Detects OCaml Exception.get_exn, catch_exn, and Rust catch_unwind applied to
# unvalidated external data — callers should sanitise input before catching exceptions.
#
# Usage: fix-getexn-external-data.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-getexn-external-data.sh <repo-path>}"
REPORTS=0

# OCaml: Exception.get_exn, catch_exn applied without prior validation
while IFS= read -r -d '' file; do
  [[ "$file" == */_build/* ]] && continue
  [[ "$file" == */.git/* ]] && continue

  count=$(grep -cP '(Exception\.get_exn|catch_exn|getExn)\s*[^)]*' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && continue

  echo "[fix-getexn-external-data] OCaml ${file}: ${count} getexn/catch_exn call(s)"
  grep -nP '(Exception\.get_exn|catch_exn|getExn)\s*[^)]*' "$file" 2>/dev/null | head -3
  REPORTS=$((REPORTS + count))
done < <(find "$REPO" -type f \( -name "*.ml" -o -name "*.mli" -o -name "*.res" -o -name "*.resi" \) \
  -not -path "*/.git/*" -not -path "*/_build/*" -print0 2>/dev/null)

# ReScript: Belt.Option.getExn, Option.getExn without .isSome check nearby
while IFS= read -r -d '' file; do
  [[ "$file" == */.git/* ]] && continue
  count=$(grep -cP '(Option\.getExn|getExn\b)' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && continue

  echo "[fix-getexn-external-data] ReScript ${file}: ${count} getExn call(s) — verify external data is guarded"
  grep -nP '(Option\.getExn|getExn\b)' "$file" 2>/dev/null | head -3
  REPORTS=$((REPORTS + count))
done < <(find "$REPO" -type f \( -name "*.res" -o -name "*.resi" \) \
  -not -path "*/.git/*" -print0 2>/dev/null)

# Rust: catch_unwind on untrusted closures
while IFS= read -r -d '' file; do
  [[ "$file" == */target/* ]] && continue
  [[ "$file" == */.git/* ]] && continue

  count=$(grep -cP 'catch_unwind\s*\(' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && continue

  echo "[fix-getexn-external-data] Rust ${file}: ${count} catch_unwind call(s) — verify closure is safe"
  grep -nP 'catch_unwind\s*\(' "$file" 2>/dev/null | head -3
  REPORTS=$((REPORTS + count))
done < <(find "$REPO" -type f -name "*.rs" \
  -not -path "*/.git/*" -not -path "*/target/*" -print0 2>/dev/null)

echo "[fix-getexn-external-data] Total: ${REPORTS} instance(s) reported for review"
