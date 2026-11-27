#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
# Owner: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# remediate.sh — apply the SAFE auto-remediations for the CI failure classes.
#   B-ALLOWLIST  -> PUT current allow-list UNION the curated superset
#                   (hyperpolymath/* + pinned third-party); additive only, the
#                   github-owned/verified flags and sha-pinning are untouched.
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
# shellcheck source=SCRIPTDIR/burn-trigger.sh
. "$HERE/burn-trigger.sh"   # BURN_TRIGGER_ERE (D-BURN), shared with detect.sh
DENYLIST="${CI_HEALTH_DENYLIST:-007}"   # space-separated repo names to never touch

# D-BURN rewrite of one workflow text: the first line on the bare trigger
# becomes push scoped to the default branches plus pull_request, and a
# concurrency-cancel block is added when the file has none. A text with no
# such line comes back unchanged.
burn_patch() {
  local text=$1
  local blk="on:
  push:
    branches: [main, master]
  pull_request:"

  if ! grep -q '^[[:space:]]*concurrency:' <<<"$text"; then
    blk="$blk

# Estate guardrail: scope push to default branches (PR fires once, not
# push+PR) and cancel superseded runs. Safe — read-only PR check.
concurrency:
  group: \${{ github.workflow }}-\${{ github.ref }}
  cancel-in-progress: true"
  fi

  printf '%s' "$text" | RE="$BURN_TRIGGER_ERE" awk -v blk="$blk" '
    $0 ~ ENVIRON["RE"] && !done {
      print blk
      done = 1
      next
    }
    { print }
  '
}

for d in $DENYLIST; do [ "$R" = "$d" ] && { echo "SKIP $R/$CLASS denylisted"; exit 0; }; done
meta=$(gh api "repos/$O/$R" --jq '"\(.fork) \(.archived)"' 2>/dev/null || echo "false false")
read -r isfork isarch <<<"$meta"
{ [ "$isfork" = "true" ] || [ "$isarch" = "true" ]; } && { echo "SKIP $R/$CLASS fork-or-archived"; exit 0; }

case "$CLASS" in
  B-ALLOWLIST)
    # Additive only: the new list is the repo's current patterns UNION the
    # required set (hyperpolymath/* + each superset line as owner/repo@*), and
    # github_owned_allowed / verified_allowed are written back exactly as read.
    # Nothing a repo allows today is ever removed and no trust flag is widened
    # (model and proofs: verification/proofs/idris2/AllowlistRemediation.idr).
    # The current state must be read first; if it cannot be, nothing is written.
    if ! cur=$(gh api "repos/$O/$R/actions/permissions/selected-actions" 2>/dev/null); then
      echo "REFUSE $R/B-ALLOWLIST cannot read the current allow-list; nothing written" >&2
      exit 1
    fi
    req=$(jq -R -s -c 'split("\n") | map(select(length > 0) | . + "@*") | ["hyperpolymath/*"] + .' "$HERE/action-superset.txt")
    body=$(printf '%s' "$cur" | jq -c --argjson req "$req" '
      {patterns_allowed: (((.patterns_allowed // []) + $req) | unique)}
      + (if has("github_owned_allowed") then {github_owned_allowed} else {} end)
      + (if has("verified_allowed") then {verified_allowed} else {} end)')
    added=$(printf '%s' "$cur" | jq --argjson req "$req" '($req - (.patterns_allowed // [])) | unique | length')
    kept=$(printf '%s' "$cur" | jq '(.patterns_allowed // []) | unique | length')
    if [[ "$added" -eq 0 ]]; then echo "SKIP $R/B-ALLOWLIST already covers the required set ($kept patterns)"; exit 0; fi
    if [[ "$DRY" = "true" ]]; then echo "DRYRUN $R/B-ALLOWLIST would add $added pattern(s), keeping all $kept current"; exit 0; fi
    printf '%s' "$body" | gh api -X PUT "repos/$O/$R/actions/permissions/selected-actions" --input - >/dev/null
    expected=$(printf '%s' "$body" | jq -cS '.patterns_allowed |= sort')
    actual=$(gh api "repos/$O/$R/actions/permissions/selected-actions" | jq -cS --argjson b "$body" '
      {patterns_allowed: ((.patterns_allowed // []) | sort)}
      + (if ($b | has("github_owned_allowed")) then {github_owned_allowed} else {} end)
      + (if ($b | has("verified_allowed")) then {verified_allowed} else {} end)')
    if [ "$actual" != "$expected" ]; then
      echo "VERIFY-FAIL $R/B-ALLOWLIST read-back differs from current+required; possible concurrent change" >&2
      exit 1
    fi
    echo "FIXED $R/B-ALLOWLIST -> added $added, kept all $kept (trust flags unchanged, exact set verified)"
    ;;
  D-BURN)
    if gh api "repos/$O/$R/branches/$BR" --jq '.name' >/dev/null 2>&1; then echo "SKIP $R/D-BURN branch-exists"; exit 0; fi
    def=$(gh api "repos/$O/$R" --jq '.default_branch'); sha=$(gh api "repos/$O/$R/git/ref/heads/$def" --jq '.object.sha')
    # Every patch is computed before anything is written, from the commit the
    # branch will start at. A file is a target only when its patch changes it,
    # so the PR and the FIXED count name only files that really change.
    targets=(); patches=(); shas=()
    for p in $(gh api "repos/$O/$R/contents/.github/workflows?ref=$sha" --jq '.[]?|select(.name|test("\\.ya?ml$"))|.path' 2>/dev/null); do
      # A failed fetch or decode, or an empty file, is not a target.
      cur=$(gh api "repos/$O/$R/contents/$p?ref=$sha" 2>/dev/null) || continue
      ccontent=$(jq -r '.content' <<<"$cur" | tr -d '\n' | base64 -d 2>/dev/null) || continue
      [[ -n "$ccontent" ]] || continue
      patched=$(burn_patch "$ccontent")
      [[ "$patched" != "$ccontent" ]] || continue
      targets+=("$p"); patches+=("$patched"); shas+=("$(jq -r '.sha' <<<"$cur")")
    done
    [ "${#targets[@]}" -eq 0 ] && { echo "SKIP $R/D-BURN no-targets"; exit 0; }
    if [ "$DRY" = "true" ]; then echo "DRYRUN $R/D-BURN would patch ${#targets[@]} file(s) + open PR"; exit 0; fi
    gh api -X POST "repos/$O/$R/git/refs" -f ref="refs/heads/$BR" -f sha="$sha" >/dev/null
    for i in "${!targets[@]}"; do
      p=${targets[$i]}
      # printf '%s\n' restores the final newline the $( ) captures strip
      newc=$(printf '%s\n' "${patches[$i]}" | base64 | tr -d '\n')
      gh api -X PUT "repos/$O/$R/contents/$p" -f message="ci: cut Actions burn in $p (scope push + concurrency-cancel)" \
        -f content="$newc" -f sha="${shas[$i]}" -f branch="$BR" >/dev/null
    done
    url=$(gh api "repos/$O/$R/pulls" -X POST -f title="ci: cut Actions burn — scope push triggers + concurrency-cancel" \
      -f head="$BR" -f base="$def" -f body="Automated by hypatia ci-health-sweep. Scopes \`push\` to the default branch (kills push+PR double-runs) and adds \`concurrency: cancel-in-progress\` to read-only PR checks. No SPDX/logic changes." --jq '.html_url')
    echo "FIXED $R/D-BURN -> $url (${#targets[@]} file(s))"
    ;;
  *) echo "SKIP $R/$CLASS no-auto-remediation" ;;
esac
