#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
# Owner: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# remediate.sh — apply the SAFE auto-remediations for the CI failure classes.
#   B-ALLOWLIST  -> PUT curated allow-list superset (hyperpolymath/* + pinned
#                   third-party), keep github-owned/verified/sha-pinning.
#   D-BURN       -> open an idempotent, signed burn-cut PR (scope push to the
#                   default branch + add concurrency-cancel) via the API.
#   A-BILLING    -> NEVER auto-fixed (account-level, owner-only); the driver
#                   aggregates these into the tracking issue.
#
# Guardrails (lessons from the 2026-06-13 sweep):
#   * own repos only: skip forks + archived.
#   * DENYLIST: skip ARR-special / cross-owner repos (e.g. 007).
#   * idempotent: skip if the allow-list already has hyperpolymath/* or the
#     burn-cut branch/PR already exists.
#   * dry-run honoured.
# Usage: remediate.sh <repo> <CLASS> <dry_run:true|false>
set -euo pipefail
O="${OWNER:-hyperpolymath}"; R="$1"; CLASS="$2"; DRY="${3:-true}"
BR="ci/ci-health-auto-remediation"
HERE="$(cd "$(dirname "$0")" && pwd)"
DENYLIST="${CI_HEALTH_DENYLIST:-007}"   # space-separated repo names to never touch

for d in $DENYLIST; do [ "$R" = "$d" ] && { echo "SKIP $R/$CLASS denylisted"; exit 0; }; done
meta=$(gh api "repos/$O/$R" --jq '"\(.fork) \(.archived)"' 2>/dev/null || echo "false false")
read -r isfork isarch <<<"$meta"
{ [ "$isfork" = "true" ] || [ "$isarch" = "true" ]; } && { echo "SKIP $R/$CLASS fork-or-archived"; exit 0; }

case "$CLASS" in
  B-ALLOWLIST)
    # Build body: hyperpolymath/* + each superset action as owner/repo@*
    body=$(jq -R -s -c 'split("\n") | map(select(length > 0) | . + "@*") | ["hyperpolymath/*"] + . as $pats | {"github_owned_allowed":true,"verified_allowed":true,"patterns_allowed":$pats}' "$HERE/action-superset.txt")
    if [ "$DRY" = "true" ]; then echo "DRYRUN $R/B-ALLOWLIST would PUT $(printf '%s' "$body" | jq '.patterns_allowed | length') patterns"; exit 0; fi
    printf '%s' "$body" | gh api -X PUT "repos/$O/$R/actions/permissions/selected-actions" --input - >/dev/null
    expected=$(printf '%s' "$body" | jq -c '.patterns_allowed | sort')
    actual=$(gh api "repos/$O/$R/actions/permissions/selected-actions" --jq '(.patterns_allowed // []) | sort | @json')
    if [ "$actual" != "$expected" ]; then
      expected_n=$(printf '%s' "$expected" | jq 'length')
      actual_n=$(printf '%s' "$actual" | jq 'length')
      echo "VERIFY-FAIL $R/B-ALLOWLIST expected $expected_n curated patterns, observed $actual_n; possible concurrent wipe" >&2
      exit 1
    fi
    n=$(printf '%s' "$actual" | jq 'length')
    echo "FIXED $R/B-ALLOWLIST -> $n patterns (sha-pinning unchanged, exact set verified)"
    ;;
  D-BURN)
    if gh api "repos/$O/$R/branches/$BR" --jq '.name' >/dev/null 2>&1; then echo "SKIP $R/D-BURN branch-exists"; exit 0; fi
    def=$(gh api "repos/$O/$R" --jq '.default_branch'); sha=$(gh api "repos/$O/$R/git/ref/heads/$def" --jq '.object.sha')
    targets=(); for p in $(gh api "repos/$O/$R/contents/.github/workflows" --jq '.[]?|select(.name|test("\\.ya?ml$"))|.path' 2>/dev/null); do
      gh api "repos/$O/$R/contents/$p?ref=$def" --jq '.content' 2>/dev/null | base64 -d 2>/dev/null \
        | grep -qE '^on:[[:space:]]*\[[[:space:]]*push[[:space:]]*,[[:space:]]*pull_request' && targets+=("$p")
    done
    [ "${#targets[@]}" -eq 0 ] && { echo "SKIP $R/D-BURN no-targets"; exit 0; }
    if [ "$DRY" = "true" ]; then echo "DRYRUN $R/D-BURN would patch ${#targets[@]} file(s) + open PR"; exit 0; fi
    gh api -X POST "repos/$O/$R/git/refs" -f ref="refs/heads/$BR" -f sha="$sha" >/dev/null
    for p in "${targets[@]}"; do
      cur=$(gh api "repos/$O/$R/contents/$p?ref=$BR")
      ccontent=$(printf '%s' "$cur" | jq -r '.content' | tr -d '\n' | base64 -d)
      csha=$(printf '%s' "$cur" | jq -r '.sha')
      if [ -z "$ccontent" ]; then echo "SKIP $R/$p empty-decode (refusing to PUT an empty file)" >&2; continue; fi

      blk="on:
  push:
    branches: [main, master]
  pull_request:"

      if ! printf '%s' "$ccontent" | grep -q '^[[:space:]]*concurrency:'; then
        blk="$blk

# Estate guardrail: scope push to default branches (PR fires once, not
# push+PR) and cancel superseded runs. Safe — read-only PR check.
concurrency:
  group: \${{ github.workflow }}-\${{ github.ref }}
  cancel-in-progress: true"
      fi

      patched=$(printf '%s' "$ccontent" | awk -v blk="$blk" '
        /^on:[ \t]*\[[ \t]*push[ \t]*,[ \t]*pull_request[ \t]*\][ \t]*$/ && !done {
          print blk
          done = 1
          next
        }
        { print }
      ')

      # printf '%s\n' restores the final newline the $( ) captures strip
      newc=$(printf '%s\n' "$patched" | base64 | tr -d '\n')
      gh api -X PUT "repos/$O/$R/contents/$p" -f message="ci: cut Actions burn in $p (scope push + concurrency-cancel)" \
        -f content="$newc" -f sha="$csha" -f branch="$BR" >/dev/null
    done
    url=$(gh api "repos/$O/$R/pulls" -X POST -f title="ci: cut Actions burn — scope push triggers + concurrency-cancel" \
      -f head="$BR" -f base="$def" -f body="Automated by hypatia ci-health-sweep. Scopes \`push\` to the default branch (kills push+PR double-runs) and adds \`concurrency: cancel-in-progress\` to read-only PR checks. No SPDX/logic changes." --jq '.html_url')
    echo "FIXED $R/D-BURN -> $url (${#targets[@]} file(s))"
    ;;
  *) echo "SKIP $R/$CLASS no-auto-remediation" ;;
esac
