#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-http-to-https.sh — Replace http:// URLs with https://
# Recipe: recipe-fix-http-to-https (confidence: 0.95, auto_fixable: true)
#
# Usage: fix-http-to-https.sh <repo-path> [--file <specific-file>]

set -euo pipefail

REPO="${1:?Usage: fix-http-to-https.sh <repo-path>}"
SPECIFIC_FILE="${3:-}"
FIXES=0

# Domains known to support HTTPS
SAFE_DOMAINS=(
  "github.com" "gitlab.com" "bitbucket.org"
  "crates.io" "hex.pm" "npmjs.com"
  "docs.rs" "doc.rust-lang.org"
  "elixir-lang.org" "erlang.org"
  "wikipedia.org" "wikimedia.org"
  "example.com"  # RFC 2606, both work
)

# Skip patterns (these legitimately use http://)
SKIP_PATTERNS=(
  "http://localhost"
  "http://127.0.0.1"
  "http://0.0.0.0"
  "http://[::1]"
  "http://example.com"  # Test fixtures
  "http://schemas.android.com"  # Android XML namespace
  "http://www.w3.org"  # XML namespace URIs (not actual URLs)
  "http://purl.org"  # Persistent URL standard
)

fix_file() {
  local file="$1"

  # Skip binary files, git internals, build dirs
  [[ "$file" == */.git/* ]] && return 0
  [[ "$file" == */node_modules/* ]] && return 0
  [[ "$file" == */target/* ]] && return 0
  [[ "$file" == */_build/* ]] && return 0
  [[ "$file" == */.zig-cache/* ]] && return 0

  # Only process text files
  file -b --mime-type "$file" 2>/dev/null | grep -q "text/" || return 0

  # Build sed exclusion pattern
  local sed_cmd="s|http://|https://|g"
  for skip in "${SKIP_PATTERNS[@]}"; do
    # Don't replace these patterns — restore them after global replace
    local https_version="${skip/http:\/\//https:\/\/}"
    sed_cmd="${sed_cmd}; s|${https_version}|${skip}|g"
  done

  local before after
  before=$(grep -c "http://" "$file" 2>/dev/null || echo 0)
  [[ "$before" -eq 0 ]] && return 0

  sed -i "${sed_cmd}" "$file"

  after=$(grep -c "http://" "$file" 2>/dev/null || echo 0)
  local fixed=$((before - after))

  if [[ "$fixed" -gt 0 ]]; then
    echo "[fix-http-to-https] Fixed ${fixed} URL(s) in ${file}"
    FIXES=$((FIXES + fixed))
  fi
}

if [[ -n "$SPECIFIC_FILE" ]]; then
  fix_file "$SPECIFIC_FILE"
else
  # Process source files in the repo
  while IFS= read -r -d '' file; do
    fix_file "$file"
  done < <(find "$REPO" -type f \
    \( -name "*.yml" -o -name "*.yaml" -o -name "*.md" -o -name "*.adoc" \
       -o -name "*.rs" -o -name "*.ex" -o -name "*.exs" -o -name "*.idr" \
       -o -name "*.zig" -o -name "*.sh" -o -name "*.toml" -o -name "*.json" \
       -o -name "*.res" -o -name "*.resi" -o -name "*.gleam" \) \
    -not -path "*/.git/*" -not -path "*/target/*" -not -path "*/_build/*" \
    -print0 2>/dev/null)
fi

echo "[fix-http-to-https] Total: ${FIXES} URL(s) fixed"
