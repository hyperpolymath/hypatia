#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-scorecard-contributors.sh — Report contributor health (SC-011)
# Recipe: recipe-scorecard-contributors (auto_fixable: false)
#
# Generates a CONTRIBUTORS summary and checks for healthy contributor diversity.
# The OpenSSF check looks at commit history; cannot be auto-fixed.
#
# Usage: fix-scorecard-contributors.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-scorecard-contributors.sh <repo-path>}"
REPORT="${REPO}/.hypatia-contributors-report.txt"

{
  echo "# Contributors Report — $(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "# Repository: ${REPO}"
  echo ""

  echo "## Commit authors (last 90 days)"
  git -C "$REPO" log --since="90 days ago" --format="%ae" 2>/dev/null \
    | sort | uniq -c | sort -rn | head -20 || echo "  (git log unavailable)"

  echo ""
  echo "## All-time contributor count"
  count=$(git -C "$REPO" log --format="%ae" 2>/dev/null | sort -u | wc -l || echo 0)
  echo "  ${count} unique contributor email(s)"

  echo ""
  echo "## OpenSSF recommendation"
  echo "  Projects with fewer than 2 contributors in 90 days score poorly."
  echo "  Encourage code review from multiple identities."
  echo "  See: https://github.com/ossf/scorecard/blob/main/docs/checks.md#contributors"
} | tee "$REPORT"

echo "[fix-scorecard-contributors] Report written to ${REPORT}"
