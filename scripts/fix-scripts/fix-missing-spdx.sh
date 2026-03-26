#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-missing-spdx.sh — Add or fix SPDX license headers
# Recipe: recipe-fix-spdx-license (confidence: 0.99, auto_fixable: true)
#
# Replaces AGPL-3.0 references with PMPL-1.0-or-later.
# Adds SPDX header to files missing one.
#
# Usage: fix-missing-spdx.sh <repo-path> [--file <specific-file>]

set -euo pipefail

REPO="${1:?Usage: fix-missing-spdx.sh <repo-path>}"
SPECIFIC_FILE="${3:-}"
FIXES=0

# File extension to comment style mapping
declare -A COMMENT_STYLE=(
  [rs]="// SPDX-License-Identifier: PMPL-1.0-or-later"
  [ex]="# SPDX-License-Identifier: PMPL-1.0-or-later"
  [exs]="# SPDX-License-Identifier: PMPL-1.0-or-later"
  [idr]="-- SPDX-License-Identifier: PMPL-1.0-or-later"
  [zig]="// SPDX-License-Identifier: PMPL-1.0-or-later"
  [sh]="# SPDX-License-Identifier: PMPL-1.0-or-later"
  [res]="// SPDX-License-Identifier: PMPL-1.0-or-later"
  [resi]="// SPDX-License-Identifier: PMPL-1.0-or-later"
  [gleam]="// SPDX-License-Identifier: PMPL-1.0-or-later"
  [yml]="# SPDX-License-Identifier: PMPL-1.0-or-later"
  [yaml]="# SPDX-License-Identifier: PMPL-1.0-or-later"
  [toml]="# SPDX-License-Identifier: PMPL-1.0-or-later"
  [lean]="-- SPDX-License-Identifier: PMPL-1.0-or-later"
  [v]="// SPDX-License-Identifier: PMPL-1.0-or-later"
  [hs]="-- SPDX-License-Identifier: PMPL-1.0-or-later"
  [ml]="(* SPDX-License-Identifier: PMPL-1.0-or-later *)"
  [ads]="-- SPDX-License-Identifier: PMPL-1.0-or-later"
  [adb]="-- SPDX-License-Identifier: PMPL-1.0-or-later"
  [jl]="# SPDX-License-Identifier: PMPL-1.0-or-later"
  [ncl]="# SPDX-License-Identifier: PMPL-1.0-or-later"
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

  # Fix 1: Replace AGPL-3.0 with PMPL-1.0-or-later
  if grep -q "AGPL-3.0" "$file" 2>/dev/null; then
    # Don't fix files in IDApTIK (legitimately AGPL)
    if [[ "$file" != */idaptik/* ]]; then
      sed -i 's/AGPL-3.0-or-later/PMPL-1.0-or-later/g; s/AGPL-3.0/PMPL-1.0-or-later/g' "$file"
      echo "[fix-missing-spdx] Replaced AGPL-3.0 → PMPL-1.0-or-later in ${file}"
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
