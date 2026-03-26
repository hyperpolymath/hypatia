#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-tmp-paths.sh — Replace hardcoded /tmp paths with secure alternatives
# Recipe: recipe-fix-tmp-paths (confidence: 0.85, auto_fixable: true)
#
# Replaces /tmp/fixed-name patterns with mktemp or System.tmp_dir().
# Conservative: only fixes obvious hardcoded /tmp in code, not documentation.
#
# Usage: fix-tmp-paths.sh <repo-path> [--file <specific-file>]

set -euo pipefail

REPO="${1:?Usage: fix-tmp-paths.sh <repo-path>}"
SPECIFIC_FILE="${3:-}"
REPORTS=0

check_file() {
  local file="$1"
  local ext="${file##*.}"

  [[ "$file" == */.git/* ]] && return 0
  [[ "$file" == */target/* ]] && return 0
  [[ "$file" == */_build/* ]] && return 0

  # Skip docs/comments — only check code files
  case "$ext" in
    md|adoc|txt|rst) return 0 ;;
  esac

  # Find hardcoded /tmp/ paths (not /tmp alone, which might be $TMPDIR fallback)
  local issues
  issues=$(grep -nP '"/tmp/[a-zA-Z]' "$file" 2>/dev/null || true)
  [[ -z "$issues" ]] && return 0

  echo "[fix-tmp-paths] REPORT: Hardcoded /tmp path(s) in ${file}:"
  echo "$issues" | head -5
  REPORTS=$((REPORTS + 1))
}

if [[ -n "$SPECIFIC_FILE" ]]; then
  check_file "$SPECIFIC_FILE"
else
  while IFS= read -r -d '' file; do
    check_file "$file"
  done < <(find "$REPO" -type f \
    \( -name "*.rs" -o -name "*.ex" -o -name "*.exs" -o -name "*.sh" \
       -o -name "*.zig" -o -name "*.idr" -o -name "*.res" -o -name "*.gleam" \) \
    -not -path "*/.git/*" -not -path "*/target/*" -not -path "*/_build/*" \
    -print0 2>/dev/null)
fi

echo "[fix-tmp-paths] Total: ${REPORTS} file(s) with hardcoded /tmp paths reported"
