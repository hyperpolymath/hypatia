#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
# Owner: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# sweep.sh — estate driver: detect + auto-remediate (B,D) + report (A).
# Enumerates the owner's own (non-fork, non-archived) repos, classifies each
# with detect.sh, applies remediate.sh for the safe classes (unless dry-run),
# and upserts a single rolling tracking issue with the findings.
#
# Env: OWNER (default hyperpolymath), DRY_RUN (true|false), MAX_BURN_PRS
#      (default 15), ISSUE_REPO (where the tracking issue lives, default hypatia),
#      CI_HEALTH_DENYLIST (passed through to remediate.sh).
set -euo pipefail
O="${OWNER:-hyperpolymath}"; DRY="${DRY_RUN:-true}"; MAXPR="${MAX_BURN_PRS:-15}"
IREPO="${ISSUE_REPO:-hypatia}"; HERE="$(cd "$(dirname "$0")" && pwd)"
TITLE="🩺 CI-health: estate failure-class report"
findings=$(mktemp); burned=0

echo "::group::Enumerate owner repos (own, non-archived)"
mapfile -t REPOS < <(gh repo list "$O" --source --no-archived --limit 1000 --json name --jq '.[].name' | sort)
echo "repos to scan: ${#REPOS[@]}  (dry_run=$DRY)"
echo "::endgroup::"

for r in "${REPOS[@]}"; do
  OWNER="$O" "$HERE/detect.sh" "$r" 2>/dev/null >>"$findings" || true
done

# Remediate
while IFS=$'\t' read -r repo cls sev det; do
  case "$cls" in
    B-ALLOWLIST) OWNER="$O" "$HERE/remediate.sh" "$repo" "$cls" "$DRY" || true ;;
    D-BURN)
      if [ "$burned" -lt "$MAXPR" ]; then
        out=$(OWNER="$O" "$HERE/remediate.sh" "$repo" "$cls" "$DRY" || true); echo "$out"
        echo "$out" | grep -q '^FIXED' && burned=$((burned+1))
      else echo "CAP $repo/D-BURN max-burn-prs($MAXPR) reached — deferred"; fi ;;
  esac
done < <(sort -u "$findings")

# Build report
rep=$(mktemp)
{
  echo "## $TITLE"
  echo "_Generated $(date -u +%Y-%m-%dT%H:%MZ) · owner: $O · dry_run: $DRY · scanned ${#REPOS[@]} repos_"
  echo ""
  echo "### 🔴 A-BILLING — OWNER action required (account spending-limit/payment wall)"
  grep -P '\tA-BILLING\t' "$findings" | awk -F'\t' '{print "- **"$1"** — "$4}' || true
  grep -qP '\tA-BILLING\t' "$findings" || echo "- _none_"
  echo ""
  echo "### 🟠 B — allow-list / startup_failure"
  grep -P '\tB-(ALLOWLIST|STARTUPFAIL)\t' "$findings" | awk -F'\t' '{print "- "$1" ("$2"): "$4}' || true
  grep -qP '\tB-' "$findings" || echo "- _none_"
  echo ""
  echo "### 🟡 D-BURN — push/PR double-trigger"
  grep -P '\tD-BURN\t' "$findings" | awk -F'\t' '{print "- "$1": "$4}' || true
  grep -qP '\tD-BURN\t' "$findings" || echo "- _none_"
  echo ""
  echo "> Auto-remediation: B-ALLOWLIST applied in place; D-BURN opened as PRs (cap $MAXPR/run); A-BILLING is owner-only. See \`scripts/ci-health/README.adoc\`."
} >"$rep"
[ -n "${GITHUB_STEP_SUMMARY:-}" ] && cat "$rep" >>"$GITHUB_STEP_SUMMARY"

# Upsert the rolling tracking issue
num=$(gh issue list --repo "$O/$IREPO" --state open --search "$TITLE in:title" --json number --jq '.[0].number // empty' 2>/dev/null || true)
if [ -n "${num:-}" ]; then gh issue edit "$num" --repo "$O/$IREPO" --body-file "$rep" >/dev/null && echo "updated issue #$num"
else gh issue create --repo "$O/$IREPO" --title "$TITLE" --body-file "$rep" >/dev/null && echo "opened tracking issue"; fi
