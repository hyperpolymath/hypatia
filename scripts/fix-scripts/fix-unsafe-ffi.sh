#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-unsafe-ffi.sh — Report raw FFI calls without safety wrappers
# Recipe: recipe-unsafe-ffi-wrapper (confidence: 0.70, auto_fixable: false)
#
# Usage: fix-unsafe-ffi.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-unsafe-ffi.sh <repo-path>}"
REPORTS=0

# Rust: unsafe { ... } blocks (report for review)
while IFS= read -r -d '' file; do
  [[ "$file" == */target/* ]] && continue
  count=$(grep -cP '\bunsafe\s*\{' "$file" 2>/dev/null || echo 0)
  if [[ "$count" -gt 0 ]]; then
    echo "[fix-unsafe-ffi] RUST ${file}: ${count} unsafe block(s)"
    REPORTS=$((REPORTS + count))
  fi
done < <(find "$REPO" -type f -name "*.rs" -not -path "*/.git/*" -not -path "*/target/*" -print0 2>/dev/null)

# Elixir: Port.open / NIF calls without error handling
while IFS= read -r -d '' file; do
  [[ "$file" == */_build/* ]] && continue
  count=$(grep -cP '(Port\.open|:erlang\.nif_error|NIF)' "$file" 2>/dev/null || echo 0)
  if [[ "$count" -gt 0 ]]; then
    echo "[fix-unsafe-ffi] ELIXIR ${file}: ${count} NIF/Port call(s)"
    REPORTS=$((REPORTS + count))
  fi
done < <(find "$REPO" -type f \( -name "*.ex" -o -name "*.exs" \) -not -path "*/.git/*" -not -path "*/_build/*" -print0 2>/dev/null)

echo "[fix-unsafe-ffi] Total: ${REPORTS} unsafe FFI pattern(s) reported"
