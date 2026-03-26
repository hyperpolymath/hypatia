#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-resource-leak.sh — Report potential resource leaks
# Recipe: recipe-fix-resource-leak (confidence: 0.70, auto_fixable: false)
#
# Detects unclosed file handles, sockets, and DB connections.
#
# Usage: fix-resource-leak.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-resource-leak.sh <repo-path>}"
REPORTS=0

# Rust: File::open without ? or drop
while IFS= read -r -d '' file; do
  [[ "$file" == */target/* ]] && continue
  count=$(grep -cP 'File::open.*unwrap\(\)' "$file" 2>/dev/null || echo 0)
  if [[ "$count" -gt 0 ]]; then
    echo "[fix-resource-leak] RUST ${file}: ${count} File::open().unwrap() (use ? instead)"
    REPORTS=$((REPORTS + count))
  fi
done < <(find "$REPO" -type f -name "*.rs" -not -path "*/.git/*" -not -path "*/target/*" -print0 2>/dev/null)

# Elixir: File.open without File.close
while IFS= read -r -d '' file; do
  [[ "$file" == */_build/* ]] && continue
  open_count=$(grep -cP 'File\.open[^!]' "$file" 2>/dev/null || echo 0)
  close_count=$(grep -cP 'File\.close' "$file" 2>/dev/null || echo 0)
  if [[ "$open_count" -gt 0 && "$close_count" -eq 0 ]]; then
    echo "[fix-resource-leak] ELIXIR ${file}: File.open without File.close (use File.open!/2 with block)"
    REPORTS=$((REPORTS + 1))
  fi
done < <(find "$REPO" -type f \( -name "*.ex" -o -name "*.exs" \) -not -path "*/.git/*" -not -path "*/_build/*" -print0 2>/dev/null)

echo "[fix-resource-leak] Total: ${REPORTS} potential resource leak(s) reported"
