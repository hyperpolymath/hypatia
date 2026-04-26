#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-scorecard-maintained.sh — Report project maintenance health (SC-016)
# Recipe: recipe-scorecard-maintained (auto_fixable: false)
#
# Reports last commit date, open issues, and open PRs to help assess
# maintenance health. The OpenSSF check looks at commit recency; fixing
# this score requires actual development activity.
#
# Usage: fix-scorecard-maintained.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-scorecard-maintained.sh <repo-path>}"
REPORT="${REPO}/.hypatia-maintained-report.txt"

{
  echo "# Maintenance Health Report — $(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "# Repository: ${REPO}"
  echo ""

  echo "## Last commit"
  git -C "$REPO" log -1 --format="%cd %s" --date=short 2>/dev/null || echo "  (unavailable)"

  echo ""
  echo "## Commits in last 90 days"
  count=$(git -C "$REPO" log --since="90 days ago" --oneline 2>/dev/null | wc -l || echo 0)
  echo "  ${count} commit(s)"

  echo ""
  echo "## OpenSSF guidance"
  echo "  A maintained project should have commits within the last 90 days."
  echo "  Archived repositories automatically fail this check."
  echo "  See: https://github.com/ossf/scorecard/blob/main/docs/checks.md#maintained"
} | tee "$REPORT"

echo "[fix-scorecard-maintained] Report written to ${REPORT}"
