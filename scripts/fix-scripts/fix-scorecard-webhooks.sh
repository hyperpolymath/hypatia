#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# fix-scorecard-webhooks.sh — Report webhook security posture (SC-024)
# Recipe: recipe-scorecard-webhooks (auto_fixable: false)
#
# Lists all webhooks for the repository and checks for:
#   - Webhooks without a secret
#   - Webhooks using http:// instead of https://
#   - Webhook SSL verification disabled
#
# Requires GITHUB_TOKEN / GH_TOKEN in environment.
#
# Usage: fix-scorecard-webhooks.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-scorecard-webhooks.sh <repo-path>}"
REPORT="${REPO}/.hypatia-webhooks-report.txt"

REMOTE_URL=$(git -C "$REPO" remote get-url origin 2>/dev/null || true)
if [[ "$REMOTE_URL" =~ github\.com[:/]([^/]+)/([^/.]+)(\.git)?$ ]]; then
  OWNER="${BASH_REMATCH[1]}"
  REPO_NAME="${BASH_REMATCH[2]}"
else
  echo "[fix-scorecard-webhooks] Not a GitHub remote — cannot list webhooks"
  exit 1
fi

{
  echo "# Webhook Security Report — $(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "# Repository: ${OWNER}/${REPO_NAME}"
  echo ""

  gh api "/repos/${OWNER}/${REPO_NAME}/hooks" 2>/dev/null | \
    jq -r '
      if length == 0 then
        "No webhooks configured."
      else
        .[] |
        .config.url as $url |
        .config.secret as $sec |
        .config.insecure_ssl as $ssl |
        (if ($sec == null or $sec == "") then ["NO SECRET"] else [] end) as $e1 |
        (if ($url | startswith("https://") | not) then ["HTTP NOT HTTPS"] else [] end) as $e2 |
        (if ($ssl != "0") then ["SSL DISABLED"] else [] end) as $e3 |
        ($e1 + $e2 + $e3) as $issues |
        (if ($issues | length) == 0 then "OK" else "ISSUE: " + ($issues | join(", ")) end) as $status |
        "\($status)\t\($url)"
      end
    ' | awk -F'\t' '{ if (NF==2) printf "%-20s  %s\n", $1, $2; else print $0 }' || echo "  (gh CLI unavailable or insufficient permissions)"

  echo ""
  echo "## Remediation"
  echo "  Set a webhook secret via GitHub Settings > Webhooks > Edit > Secret"
  echo "  Ensure all webhook URLs use https://"
} | tee "$REPORT"

echo "[fix-scorecard-webhooks] Report written to ${REPORT}"
