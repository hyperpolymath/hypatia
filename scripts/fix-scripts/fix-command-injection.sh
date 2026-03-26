#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-command-injection.sh — Report shell command construction patterns
# Recipe: recipe-fix-command-injection (confidence: 0.85, auto_fixable: false)
#
# Detects dangerous shell command construction (string concatenation into
# system/exec/popen calls). Reports for manual review.
#
# Usage: fix-command-injection.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-command-injection.sh <repo-path>}"
REPORTS=0

# Patterns per language
check_elixir() {
  local file="$1"
  local count
  count=$(grep -cP '(System\.cmd|:os\.cmd|Port\.open.*:spawn).*#\{' "$file" 2>/dev/null || echo 0)
  count=$((count + $(grep -cP 'System\.cmd\(.*<>' "$file" 2>/dev/null || echo 0)))
  echo "$count"
}

check_rust() {
  local file="$1"
  local count
  count=$(grep -cP 'Command::new\(.*format!' "$file" 2>/dev/null || echo 0)
  count=$((count + $(grep -cP '\.arg\(.*format!' "$file" 2>/dev/null || echo 0)))
  echo "$count"
}

check_shell() {
  local file="$1"
  # Unquoted variables in command position
  local count
  count=$(grep -cP '(eval|exec)\s+[^"]*\$' "$file" 2>/dev/null || echo 0)
  echo "$count"
}

while IFS= read -r -d '' file; do
  [[ "$file" == */.git/* ]] && continue
  [[ "$file" == */target/* ]] && continue
  [[ "$file" == */_build/* ]] && continue

  local ext="${file##*.}"
  local count=0

  case "$ext" in
    ex|exs) count=$(check_elixir "$file") ;;
    rs) count=$(check_rust "$file") ;;
    sh) count=$(check_shell "$file") ;;
  esac

  if [[ "$count" -gt 0 ]]; then
    echo "[fix-command-injection] REPORT: ${file} — ${count} pattern(s)"
    REPORTS=$((REPORTS + count))
  fi
done < <(find "$REPO" -type f \
  \( -name "*.ex" -o -name "*.exs" -o -name "*.rs" -o -name "*.sh" \) \
  -not -path "*/.git/*" -not -path "*/target/*" -not -path "*/_build/*" \
  -print0 2>/dev/null)

echo "[fix-command-injection] Total: ${REPORTS} potential command injection(s) reported"
