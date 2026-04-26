#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
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
    python3 -c "
import sys, json
hooks = json.load(sys.stdin)
if not hooks:
    print('No webhooks configured.')
for h in hooks:
    url = h.get('config', {}).get('url', '?')
    has_secret = bool(h.get('config', {}).get('secret'))
    ssl_ok = h.get('config', {}).get('insecure_ssl') == '0'
    issues = []
    if not has_secret:
        issues.append('NO SECRET')
    if not url.startswith('https://'):
        issues.append('HTTP NOT HTTPS')
    if not ssl_ok:
        issues.append('SSL DISABLED')
    status = 'OK' if not issues else 'ISSUE: ' + ', '.join(issues)
    print(f'{status:20s}  {url}')
" || echo "  (gh CLI unavailable or insufficient permissions)"

  echo ""
  echo "## Remediation"
  echo "  Set a webhook secret via GitHub Settings > Webhooks > Edit > Secret"
  echo "  Ensure all webhook URLs use https://"
} | tee "$REPORT"

echo "[fix-scorecard-webhooks] Report written to ${REPORT}"
