#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-todo-markers.sh — Report TODO/FIXME/HACK/XXX markers
# Recipe: recipe-fix-todo-markers (confidence: 0.50, auto_fixable: false)
#
# Scans for TODO markers and generates a summary report.
# Low confidence — these are informational, not security issues.
#
# Usage: fix-todo-markers.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-todo-markers.sh <repo-path>}"

echo "[fix-todo-markers] Scanning $(basename "$REPO") for TODO markers..."

TODOS=$(grep -rnP '\b(TODO|FIXME|HACK|XXX)\b' "$REPO" \
  --include="*.rs" --include="*.ex" --include="*.exs" --include="*.idr" \
  --include="*.zig" --include="*.sh" --include="*.res" --include="*.resi" \
  --include="*.gleam" --include="*.lean" --include="*.hs" --include="*.v" \
  --exclude-dir=".git" --exclude-dir="target" --exclude-dir="_build" \
  --exclude-dir=".lake" --exclude-dir="deps" \
  2>/dev/null || true)

if [[ -z "$TODOS" ]]; then
  echo "[fix-todo-markers] No TODO markers found"
  exit 0
fi

# Count by type
TODO_COUNT=$(echo "$TODOS" | grep -cP '\bTODO\b' || echo 0)
FIXME_COUNT=$(echo "$TODOS" | grep -cP '\bFIXME\b' || echo 0)
HACK_COUNT=$(echo "$TODOS" | grep -cP '\bHACK\b' || echo 0)
XXX_COUNT=$(echo "$TODOS" | grep -cP '\bXXX\b' || echo 0)

echo "[fix-todo-markers] Summary:"
echo "  TODO:  ${TODO_COUNT}"
echo "  FIXME: ${FIXME_COUNT}"
echo "  HACK:  ${HACK_COUNT}"
echo "  XXX:   ${XXX_COUNT}"
echo "  Total: $((TODO_COUNT + FIXME_COUNT + HACK_COUNT + XXX_COUNT))"
