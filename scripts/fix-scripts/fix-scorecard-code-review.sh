#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-scorecard-code-review.sh — Enable branch protection for code review (SC-010)
# Recipe: recipe-scorecard-code-review (confidence: 0.88, auto_fixable: true)
#
# Enables branch protection on the default branch requiring at least one review.
# Requires GITHUB_TOKEN with admin:repo scope or GH_TOKEN in environment.
#
# Usage: fix-scorecard-code-review.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-scorecard-code-review.sh <repo-path>}"

# Detect owner/repo from git remote
REMOTE_URL=$(git -C "$REPO" remote get-url origin 2>/dev/null || true)
if [[ -z "$REMOTE_URL" ]]; then
  echo "[fix-scorecard-code-review] Cannot determine remote — skipping"
  exit 1
fi

# Parse owner/repo from GitHub URL (https or ssh)
if [[ "$REMOTE_URL" =~ github\.com[:/]([^/]+)/([^/.]+)(\.git)?$ ]]; then
  OWNER="${BASH_REMATCH[1]}"
  REPO_NAME="${BASH_REMATCH[2]}"
else
  echo "[fix-scorecard-code-review] Not a GitHub remote: ${REMOTE_URL}"
  exit 1
fi

DEFAULT_BRANCH=$(git -C "$REPO" remote show origin 2>/dev/null | grep 'HEAD branch' | awk '{print $NF}' || echo "main")

echo "[fix-scorecard-code-review] Enabling branch protection on ${OWNER}/${REPO_NAME}:${DEFAULT_BRANCH}"

gh api \
  --method PUT \
  "/repos/${OWNER}/${REPO_NAME}/branches/${DEFAULT_BRANCH}/protection" \
  --field required_status_checks=null \
  --field enforce_admins=false \
  --field 'required_pull_request_reviews[required_approving_review_count]=1' \
  --field 'required_pull_request_reviews[dismiss_stale_reviews]=true' \
  --field restrictions=null \
  --field allow_force_pushes=false \
  --field allow_deletions=false \
  --silent \
  && echo "[fix-scorecard-code-review] Branch protection enabled on ${DEFAULT_BRANCH}" \
  || echo "[fix-scorecard-code-review] Failed — check GH_TOKEN has admin:repo scope"
