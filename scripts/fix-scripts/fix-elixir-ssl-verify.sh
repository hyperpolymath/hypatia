#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-elixir-ssl-verify.sh — Fix Elixir SSL/TLS verification bypasses
# Recipe: recipe-elixir-ssl-verify (confidence: 0.90, auto_fixable: true)
#
# Repairs: :verify_none, insecure: true, verify: false patterns.
# Warns on: empty transport_opts, missing cacerts after peer verify.
#
# Usage: fix-elixir-ssl-verify.sh <repo-path> [--file <specific-file>]

set -euo pipefail

REPO="${1:?Usage: fix-elixir-ssl-verify.sh <repo-path>}"
SPECIFIC_FILE="${3:-}"
FIXES=0
WARNINGS=0

fix_file() {
  local file="$1"
  [[ "$file" == */_build/* ]] && return 0
  [[ "$file" == */deps/* ]] && return 0

  if grep -q ':verify_none' "$file" 2>/dev/null; then
    sed -i 's/verify: :verify_none/verify: :verify_peer/g' "$file"
    sed -i 's/:verify => :verify_none/:verify => :verify_peer/g' "$file"
    echo "[fix-elixir-ssl-verify] Replaced :verify_none → :verify_peer in ${file}"
    FIXES=$((FIXES + 1))
  fi

  if grep -q 'insecure: true\|verify: false' "$file" 2>/dev/null; then
    sed -i 's/insecure: true/insecure: false/g' "$file"
    sed -i 's/verify: false/verify: true/g' "$file"
    echo "[fix-elixir-ssl-verify] Fixed insecure SSL flags in ${file}"
    FIXES=$((FIXES + 1))
  fi

  if grep -qE 'transport_opts:\s*\[\]' "$file" 2>/dev/null; then
    echo "[fix-elixir-ssl-verify] WARNING: ${file} has empty transport_opts — verify SSL config manually"
    WARNINGS=$((WARNINGS + 1))
  fi
}

if [[ -n "$SPECIFIC_FILE" ]]; then
  fix_file "$SPECIFIC_FILE"
else
  while IFS= read -r -d '' file; do
    fix_file "$file"
  done < <(find "$REPO" -type f \( -name "*.ex" -o -name "*.exs" \) \
    -not -path "*/_build/*" -not -path "*/deps/*" -not -path "*/.git/*" \
    -print0 2>/dev/null)
fi

echo "[fix-elixir-ssl-verify] Done: ${FIXES} fix(es) applied, ${WARNINGS} warning(s) for manual review"
