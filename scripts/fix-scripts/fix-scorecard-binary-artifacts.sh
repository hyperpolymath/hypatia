#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-scorecard-binary-artifacts.sh — Report binary artifacts in repository
# Recipe: recipe-scorecard-binary-artifacts (SC-001, auto_fixable: false)
#
# Scans for binary files committed to the repository and produces a
# human-readable report. Binary removal requires human judgement.
#
# Usage: fix-scorecard-binary-artifacts.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-scorecard-binary-artifacts.sh <repo-path>}"
REPORT="${REPO}/.hypatia-binary-artifacts-report.txt"

count=0
{
  echo "# Binary Artifacts Report — $(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "# Repository: ${REPO}"
  echo "#"
  echo "# ACTION REQUIRED: Review and remove unnecessary binary files."
  echo "# Add legitimate binaries to .gitattributes with -diff or to .gitignore."
  echo ""

  while IFS= read -r -d '' file; do
    if file "$file" | grep -qE 'ELF|PE32|Mach-O|compiled|bytecode|binary|data'; then
      echo "BINARY: ${file#"${REPO}/"}"
      count=$((count + 1))
    fi
  done < <(find "$REPO" -type f \
    -not -path "*/.git/*" -not -path "*/target/*" -not -path "*/_build/*" \
    -not -path "*/node_modules/*" -not -path "*/deps/*" \
    \( -name "*.so" -o -name "*.dylib" -o -name "*.dll" -o -name "*.exe" \
       -o -name "*.a" -o -name "*.lib" -o -name "*.o" -o -name "*.class" \
       -o -name "*.jar" -o -name "*.beam" -o -name "*.pyc" \) \
    -print0 2>/dev/null)

  echo ""
  echo "Total binary files found: ${count}"
} | tee "$REPORT"

echo "[fix-scorecard-binary-artifacts] Report written to ${REPORT}"
echo "[fix-scorecard-binary-artifacts] Found ${count} binary artifact(s) — manual review required"
