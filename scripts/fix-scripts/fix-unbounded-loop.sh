#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-unbounded-loop.sh — Report unbounded loops
# Recipe: recipe-unbounded-loop (confidence: 0.55, auto_fixable: false)
#
# Usage: fix-unbounded-loop.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-unbounded-loop.sh <repo-path>}"
REPORTS=0

# Rust: loop {} without break/return
while IFS= read -r -d '' file; do
  [[ "$file" == */target/* ]] && continue
  count=$(grep -cP '^\s*loop\s*\{' "$file" 2>/dev/null || echo 0)
  if [[ "$count" -gt 0 ]]; then
    echo "[fix-unbounded-loop] RUST ${file}: ${count} unbounded loop(s)"
    REPORTS=$((REPORTS + count))
  fi
done < <(find "$REPO" -type f -name "*.rs" -not -path "*/.git/*" -not -path "*/target/*" -print0 2>/dev/null)

# Shell: while true without timeout/break
while IFS= read -r -d '' file; do
  count=$(grep -cP 'while\s+(true|:)\s*;?\s*do' "$file" 2>/dev/null || echo 0)
  if [[ "$count" -gt 0 ]]; then
    echo "[fix-unbounded-loop] SHELL ${file}: ${count} while-true loop(s)"
    REPORTS=$((REPORTS + count))
  fi
done < <(find "$REPO" -type f -name "*.sh" -not -path "*/.git/*" -print0 2>/dev/null)

echo "[fix-unbounded-loop] Total: ${REPORTS} unbounded loop(s) reported"
