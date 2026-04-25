#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-unsafe-panic.sh — Report generic unsafe panic patterns (unsafe_panic-unknown)
# Recipe: recipe-panic-to-result (confidence: 0.70, auto_fixable: false)
#
# Broad sweep: panic!, todo!, unimplemented!, unreachable!, assert!, expect()
# across Rust, Elixir (raise/throw), OCaml (failwith/invalid_arg).
#
# Usage: fix-unsafe-panic.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-unsafe-panic.sh <repo-path>}"
REPORTS=0

# Rust: all panic-inducing macros and expect()
while IFS= read -r -d '' file; do
  [[ "$file" == */target/* ]] && continue
  [[ "$file" == */.git/* ]] && continue

  count=$(grep -cP '\b(panic!|todo!|unimplemented!|unreachable!|\.expect\s*\()' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && continue

  echo "[fix-unsafe-panic] Rust ${file}: ${count} panic-inducing call(s)"
  grep -nP '\b(panic!|todo!|unimplemented!|unreachable!|\.expect\s*\()' "$file" 2>/dev/null | head -3
  REPORTS=$((REPORTS + count))
done < <(find "$REPO" -type f -name "*.rs" \
  -not -path "*/.git/*" -not -path "*/target/*" -print0 2>/dev/null)

# OCaml: failwith / invalid_arg / raise
while IFS= read -r -d '' file; do
  [[ "$file" == */_build/* ]] && continue
  [[ "$file" == */.git/* ]] && continue

  count=$(grep -cP '\b(failwith|invalid_arg|raise\s+\()' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && continue

  echo "[fix-unsafe-panic] OCaml ${file}: ${count} exception-raising call(s)"
  grep -nP '\b(failwith|invalid_arg|raise\s+\()' "$file" 2>/dev/null | head -3
  REPORTS=$((REPORTS + count))
done < <(find "$REPO" -type f \( -name "*.ml" -o -name "*.mli" \) \
  -not -path "*/.git/*" -not -path "*/_build/*" -print0 2>/dev/null)

# Elixir: raise / throw
while IFS= read -r -d '' file; do
  [[ "$file" == */_build/* ]] && continue
  [[ "$file" == */.git/* ]] && continue

  count=$(grep -cP '^\s*(raise|throw)\s+' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && continue

  echo "[fix-unsafe-panic] Elixir ${file}: ${count} raise/throw call(s)"
  grep -nP '^\s*(raise|throw)\s+' "$file" 2>/dev/null | head -3
  REPORTS=$((REPORTS + count))
done < <(find "$REPO" -type f \( -name "*.ex" -o -name "*.exs" \) \
  -not -path "*/.git/*" -not -path "*/_build/*" -print0 2>/dev/null)

echo "[fix-unsafe-panic] Total: ${REPORTS} unsafe panic pattern(s) reported"
