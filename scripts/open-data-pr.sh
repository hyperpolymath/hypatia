#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# open-data-pr.sh — land machine-written data files on main through a PR
#
# Usage: open-data-pr.sh <branch-suffix> <title> <path>...
#
# Commits <path>... on a new branch `inbox-steward/<run_id>-<suffix>`, pushes
# it, opens a PR against main and queues a squash auto-merge. Used by
# inbox-steward-intake.yml instead of `git push origin HEAD` to main, which
# bypassed review and signing and was masked so a refusal stayed green.
#
# Needs: GH_TOKEN (a credential with Contents and Pull requests write on this
# repository), GITHUB_RUN_ID, GITHUB_REPOSITORY, and a checkout whose `origin`
# authenticates with the same credential. Every failure exits non-zero.
set -euo pipefail

if [ "$#" -lt 3 ]; then
  echo "usage: $0 <branch-suffix> <title> <path>..." >&2
  exit 2
fi

suffix="$1"
title="$2"
shift 2

git add -- "$@"
if git diff --cached --quiet; then
  echo "No changes under $* — nothing to publish."
  exit 0
fi

branch="inbox-steward/${GITHUB_RUN_ID}-${suffix}"
git switch -c "$branch"
git commit -m "$title"
git push origin "HEAD:refs/heads/${branch}"

pr_url="$(gh pr create --repo "$GITHUB_REPOSITORY" --base main --head "$branch" \
  --title "$title" \
  --body "Machine-written by inbox-steward-intake.yml, run ${GITHUB_RUN_ID}. Data files only.")"
echo "opened ${pr_url}"

# --auto queues the merge until the base-branch policy allows it; a bare
# merge right after creation is refused before the policy settles.
gh pr merge "${pr_url##*/}" --repo "$GITHUB_REPOSITORY" --squash --auto --delete-branch
echo "auto-merge queued for ${pr_url}"
