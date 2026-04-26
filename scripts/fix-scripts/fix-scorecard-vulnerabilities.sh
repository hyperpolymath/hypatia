#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-scorecard-vulnerabilities.sh — Enable vulnerability disclosure workflow (SC-023)
# Recipe: recipe-scorecard-vulnerabilities (confidence: 0.92, auto_fixable: true)
#
# Enables automated dependency vulnerability detection by delegating to
# fix-dependabot.sh. Also verifies SECURITY.md is present.
#
# Usage: fix-scorecard-vulnerabilities.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-scorecard-vulnerabilities.sh <repo-path>}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Ensure dependabot is configured
"${SCRIPT_DIR}/fix-dependabot.sh" "$REPO"

# Ensure SECURITY.md exists
if [[ ! -f "${REPO}/SECURITY.md" ]]; then
  "${SCRIPT_DIR}/fix-security-policy.sh" "$REPO" 2>/dev/null || true
fi

echo "[fix-scorecard-vulnerabilities] Vulnerability detection configured via Dependabot"
