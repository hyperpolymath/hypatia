#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-unchecked-error.sh — Report unchecked error handling
# Recipe: recipe-unchecked-error (confidence: 0.60, auto_fixable: false)
#
# Usage: fix-unchecked-error.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-unchecked-error.sh <repo-path>}"
REPORTS=0

# Elixir: _ = expression (ignoring return value)
while IFS= read -r -d '' file; do
  [[ "$file" == */_build/* ]] && continue
  count=$(grep -cP '^\s*_\s*=' "$file" 2>/dev/null || echo 0)
  if [[ "$count" -gt 0 ]]; then
    echo "[fix-unchecked-error] ELIXIR ${file}: ${count} ignored return value(s)"
    REPORTS=$((REPORTS + count))
  fi
done < <(find "$REPO" -type f \( -name "*.ex" -o -name "*.exs" \) -not -path "*/.git/*" -not -path "*/_build/*" -print0 2>/dev/null)

# Rust: let _ = expr (ignoring Result)
while IFS= read -r -d '' file; do
  [[ "$file" == */target/* ]] && continue
  count=$(grep -cP 'let\s+_\s*=' "$file" 2>/dev/null || echo 0)
  if [[ "$count" -gt 0 ]]; then
    echo "[fix-unchecked-error] RUST ${file}: ${count} let _ = (ignored Result?)"
    REPORTS=$((REPORTS + count))
  fi
done < <(find "$REPO" -type f -name "*.rs" -not -path "*/.git/*" -not -path "*/target/*" -print0 2>/dev/null)

echo "[fix-unchecked-error] Total: ${REPORTS} unchecked error(s) reported"
