#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
# Owner: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# baton-bridge.sh — run estate CI checks as Batons on OWNED COMPUTE (zero GitHub
# Actions minutes) and post the verdicts to a rolling tracking issue.
#
# Complement to detect.sh/sweep.sh: where those DETECT that GitHub CI is blocked
# (A-BILLING spending-limit wall / B-ALLOWLIST startup_failure), this RUNS the
# checks anyway — on a node we already own — via hyperpolymath/bag-of-actions
# (`mix bag.sweep`). The check GitHub refused to start runs regardless; its
# HMAC-attested verdict is the portable artifact.
#
# IMPORTANT: invoke this from a cron / systemd timer on OWNED COMPUTE (the
# .git-private-farm), NOT from a GitHub Action — running it in Actions would burn
# the very minutes this exists to avoid, and would re-couple it to the billing
# state it is designed to escape.
#
# Env:
#   BAG_OF_ACTIONS_DIR  path to the bag-of-actions clone (REQUIRED)
#   MANIFEST            checks manifest (default $BAG_OF_ACTIONS_DIR/ci-checks.exs)
#   OWNER               GitHub owner (default hyperpolymath)
#   ISSUE_REPO          repo holding the tracking issue (default hypatia)
#   DRY_RUN             true|false (default false; true = print, don't touch issue)
#
# Exit: 0 if every check passed, else the sweep's non-zero gate code.
set -euo pipefail

BOA="${BAG_OF_ACTIONS_DIR:?set BAG_OF_ACTIONS_DIR to the bag-of-actions clone}"
MANIFEST="${MANIFEST:-$BOA/ci-checks.exs}"
O="${OWNER:-hyperpolymath}"
IREPO="${ISSUE_REPO:-hypatia}"
DRY="${DRY_RUN:-false}"
TITLE="🧺 Baton sweep — estate CI on owned compute (0 GitHub minutes)"

[ -d "$BOA" ]      || { echo "ERROR: BAG_OF_ACTIONS_DIR '$BOA' not found"; exit 1; }
[ -f "$MANIFEST" ] || { echo "ERROR: manifest '$MANIFEST' not found"; exit 1; }

echo "::group::Build bag-of-actions host"
( cd "$BOA" && zig build )
echo "::endgroup::"

echo "::group::Run Baton sweep ($MANIFEST)"
raw=$(mktemp); tsv=$(mktemp); gate=0
# mix bag.sweep: TSV on stdout (check_id<TAB>BATON-VERDICT<TAB>node),
# human summary on stderr, exit 1 if any check did not pass.
( cd "$BOA/bag" && mix bag.sweep "$MANIFEST" ) >"$raw" || gate=$?
# Defensively keep only well-formed Baton verdict lines (ignore any stray stdout).
grep -P '\tBATON-(PASS|FAIL|SUSPENDED|TAMPERED|ERROR)\t' "$raw" > "$tsv" || true
cat "$tsv"
echo "::endgroup::"

# Build the report.
rep=$(mktemp)
total=$(wc -l <"$tsv" | tr -d ' ')
passed=$(grep -c 'BATON-PASS' "$tsv" || true)
{
  echo "## $TITLE"
  echo "_Generated $(date -u +%Y-%m-%dT%H:%MZ) · owner: $O · source: $O/bag-of-actions · 0 GitHub Actions minutes_"
  echo ""
  echo "**$passed/$total checks passed** on owned compute."
  echo ""
  echo "| Check | Verdict | Node |"
  echo "|---|---|---|"
  while IFS=$'\t' read -r cid verdict node; do
    [ -z "$cid" ] && continue
    echo "| \`$cid\` | $verdict | \`$node\` |"
  done <"$tsv"
  echo ""
  echo "> Verdicts are HMAC-attested CI-check Batons (\`bag_of_actions thaw\` verifies them). This sweep ran with **zero GitHub Actions minutes** and does not depend on the account billing state or any GitHub run — see \`scripts/ci-health/README.adoc\`."
} >"$rep"
[ -n "${GITHUB_STEP_SUMMARY:-}" ] && cat "$rep" >>"$GITHUB_STEP_SUMMARY"

# Upsert the rolling tracking issue (unless dry-run).
if [ "$DRY" = "true" ]; then
  echo "DRY_RUN=true — not touching the tracking issue. Report would be:"; echo; cat "$rep"
else
  num=$(gh issue list --repo "$O/$IREPO" --state open --search "$TITLE in:title" \
        --json number --jq '.[0].number // empty' 2>/dev/null || true)
  if [ -n "${num:-}" ]; then
    gh issue edit "$num" --repo "$O/$IREPO" --body-file "$rep" >/dev/null && echo "updated issue #$num"
  else
    gh issue create --repo "$O/$IREPO" --title "$TITLE" --body-file "$rep" >/dev/null && echo "opened tracking issue"
  fi
fi

exit "$gate"
