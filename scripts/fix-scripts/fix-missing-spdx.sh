#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# fix-missing-spdx.sh — Add or fix SPDX license headers
# Recipe: recipe-fix-spdx-license (confidence: 0.99, auto_fixable: true)
#
# Replaces AGPL-3.0 references with MPL-2.0.
# Adds SPDX header to files missing one.
#
# Usage: fix-missing-spdx.sh <repo-path> [--file <specific-file>]

set -euo pipefail

REPO="${1:?Usage: fix-missing-spdx.sh <repo-path>}"
SPECIFIC_FILE="${3:-}"
FIXES=0

# File extension to comment style mapping
declare -A COMMENT_STYLE=(
  [rs]="// SPDX-License-Identifier: MPL-2.0"
  [ex]="# SPDX-License-Identifier: MPL-2.0"
  [exs]="# SPDX-License-Identifier: MPL-2.0"
  [idr]="-- SPDX-License-Identifier: MPL-2.0"
  [zig]="// SPDX-License-Identifier: MPL-2.0"
  [sh]="# SPDX-License-Identifier: MPL-2.0"
  [res]="// SPDX-License-Identifier: MPL-2.0"
  [resi]="// SPDX-License-Identifier: MPL-2.0"
  [gleam]="// SPDX-License-Identifier: MPL-2.0"
  [yml]="# SPDX-License-Identifier: MPL-2.0"
  [yaml]="# SPDX-License-Identifier: MPL-2.0"
  [toml]="# SPDX-License-Identifier: MPL-2.0"
  [lean]="-- SPDX-License-Identifier: MPL-2.0"
  [v]="// SPDX-License-Identifier: MPL-2.0"
  [hs]="-- SPDX-License-Identifier: MPL-2.0"
  [ml]="(* SPDX-License-Identifier: MPL-2.0 *)"
  [ads]="-- SPDX-License-Identifier: MPL-2.0"
  [adb]="-- SPDX-License-Identifier: MPL-2.0"
  [jl]="# SPDX-License-Identifier: MPL-2.0"
  [ncl]="# SPDX-License-Identifier: MPL-2.0"
)

fix_file() {
  local file="$1"
  local ext="${file##*.}"

  # Skip files we don't handle
  [[ -z "${COMMENT_STYLE[$ext]:-}" ]] && return 0
  [[ "$file" == */.git/* ]] && return 0
  [[ "$file" == */target/* ]] && return 0
  [[ "$file" == */_build/* ]] && return 0
  [[ "$file" == */deps/* ]] && return 0
  [[ "$file" == */.lake/* ]] && return 0

  # Fix 1: Replace AGPL-3.0 with MPL-2.0
  if grep -q "AGPL-3.0" "$file" 2>/dev/null; then
    # Don't fix files in repos that legitimately use AGPL-3.0-or-later
    if [[ "$file" != */idaptik/* ]] \
    && [[ "$file" != */game-server-admin/* ]] \
    && [[ "$file" != */airborne-submarine-squadron/* ]]; then
      sed -i 's/AGPL-3.0-or-later/MPL-2.0/g; s/AGPL-3.0/MPL-2.0/g' "$file"
      echo "[fix-missing-spdx] Replaced AGPL-3.0 → MPL-2.0 in ${file}"
      FIXES=$((FIXES + 1))
    fi
  fi

  # Fix 2: Add missing SPDX header
  if ! grep -q "SPDX-License-Identifier" "$file" 2>/dev/null; then
    local header="${COMMENT_STYLE[$ext]}"

    # Handle shebang lines — insert after shebang
    if head -1 "$file" | grep -q "^#!"; then
      sed -i "1a\\${header}" "$file"
    else
      sed -i "1i\\${header}" "$file"
    fi
    echo "[fix-missing-spdx] Added SPDX header to ${file}"
    FIXES=$((FIXES + 1))
  fi
}

if [[ -n "$SPECIFIC_FILE" ]]; then
  fix_file "$SPECIFIC_FILE"
else
  while IFS= read -r -d '' file; do
    fix_file "$file"
  done < <(find "$REPO" -type f \
    \( -name "*.rs" -o -name "*.ex" -o -name "*.exs" -o -name "*.idr" \
       -o -name "*.zig" -o -name "*.sh" -o -name "*.res" -o -name "*.resi" \
       -o -name "*.gleam" -o -name "*.yml" -o -name "*.yaml" -o -name "*.toml" \
       -o -name "*.lean" -o -name "*.v" -o -name "*.hs" -o -name "*.ml" \
       -o -name "*.ads" -o -name "*.adb" -o -name "*.jl" -o -name "*.ncl" \) \
    -not -path "*/.git/*" -not -path "*/target/*" -not -path "*/_build/*" \
    -not -path "*/deps/*" -not -path "*/.lake/*" \
    -print0 2>/dev/null)
fi

echo "[fix-missing-spdx] Total: ${FIXES} fix(es) applied"
