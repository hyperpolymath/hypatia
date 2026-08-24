#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
# Owner: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# detect.sh — classify the infrastructure CI failure modes that repeatedly
# redden estate CI (diagnosed 2026-06-13). API-only; safe to run in CI with
# no local checkout. Emits TSV: <repo>\t<CLASS>\t<SEV>\t<detail → remedy>
#
# Classes:
#   A-BILLING      account Actions spending-limit/payment wall (OWNER-ONLY fix)
#   B-ALLOWLIST    allowed_actions=selected + no hyperpolymath/* → reusables/
#                  non-verified actions startup_failure (auto-remediable)
#   B-LOCKFILE     GitHub annotation identifies an actions.lock mismatch
#                  (reported; repair with gh actions-lock in a checkout)
#   B-STARTUPFAIL  startup_failure observed in the bounded recent-runs page;
#                  cause remains unclassified (reported, never auto-fixed)
#   D-BURN         workflow(s) on bare [push,pull_request] = 2x runs/PR
#                  (auto-remediable: scope push + concurrency-cancel)
#
# The API NEVER exposes the startup_failure reason; the GitHub web-UI red
# banner does (it names the blocked action). That is the human diagnostic.
set -euo pipefail
O="${OWNER:-hyperpolymath}"; R="$1"
HERE="$(cd "$(dirname "$0")" && pwd)"   # for action-superset.txt (allow-list coverage)
emit(){ printf '%s\t%s\t%s\t%s\n' "$R" "$1" "$2" "$3"; }
api() {
  local out
  if ! out=$(gh api "$@" 2>&1); then
    printf 'WARN %s/%s: gh api %s failed: %s\n' "$O" "$R" "$1" "$out" >&2
    return 1
  fi
  printf '%s\n' "$out"
}

# --- Skip logic (own repos only; one API call for both flags)
af=$(api "repos/$O/$R" --jq '[.archived, .fork] | @tsv' || printf 'false\tfalse')
is_archived=${af%%$'\t'*}; is_fork=${af##*$'\t'}
if [ "$is_archived" = "true" ] || [ "$is_fork" = "true" ]; then
    echo "SKIP $R archived/fork" >&2
    exit 0
fi

# --- A/B: exact failure annotations exposed by GitHub.
# Inspect every job in the five most recent failed runs. This is still a bounded
# horizon, but unlike a bare startup_failure conclusion the annotation names the
# mechanism and supports a precise recommendation.
billing_seen=false
lockfile_seen=false
mapfile -t fail_ids < <(api "repos/$O/$R/actions/runs?status=failure&per_page=5" --jq '.workflow_runs[].id' || true)
for fail_id in "${fail_ids[@]}"; do
  [ -n "${fail_id:-}" ] || continue
  mapfile -t job_ids < <(api "repos/$O/$R/actions/runs/$fail_id/jobs?per_page=100" --jq '.jobs[] | select(.id != null) | .id' || true)
  for job_id in "${job_ids[@]}"; do
    [ -n "${job_id:-}" ] || continue
    msg=$(api "repos/$O/$R/check-runs/$job_id/annotations?per_page=100" --jq '.[].message // empty' || true)
    if [ "$billing_seen" = false ] && printf '%s' "$msg" | grep -qiE 'payments have failed|spending limit'; then
      emit A-BILLING CRITICAL "Actions billing/spending-limit wall blocks all billable jobs → OWNER: GitHub Settings -> Billing & plans"
      billing_seen=true
    fi
    if [ "$lockfile_seen" = false ] && printf '%s' "$msg" | grep -qiE 'lockfile pin .* does not match ref|lockfile verification did not produce a result|unreachable-pin'; then
      first_lock_msg=$(printf '%s\n' "$msg" | grep -iEm1 'lockfile pin .* does not match ref|lockfile verification did not produce a result|unreachable-pin')
      emit B-LOCKFILE HIGH "Actions lockfile verification failed (${first_lock_msg}) → run gh actions-lock --verify, repair, then commit actions.lock"
      lockfile_seen=true
    fi
  done
done

# --- B: allow-list under-coverage (the root cause of estate startup_failure).
# Fire when selected-mode and the allow-list does NOT cover the full curated
# superset that remediate.sh PUTs. Catches BOTH an empty list (post-wipe:
# hyperpolymath/* absent) AND an incomplete one (has hyperpolymath/* but is
# missing a third-party action, e.g. gitleaks — previously only B-STARTUPFAIL,
# which has no remediation). The required set mirrors remediate.sh's PUT body
# exactly (hyperpolymath/* + each superset line as owner/repo@*), so a remediate
# converges this to zero-missing and detect stops re-firing (idempotent).
aa=$(api "repos/$O/$R/actions/permissions" --jq '.allowed_actions // empty' || true)
if [ "$aa" = "selected" ]; then
  cur=$(api "repos/$O/$R/actions/permissions/selected-actions" --jq '(.patterns_allowed // [])[]' || true)
  req=$(printf 'hyperpolymath/*\n'; sed 's/^[[:space:]]*//;s/[[:space:]]*$//;/^$/d;s/$/@*/' "$HERE/action-superset.txt")
  miss=$(printf '%s\n' "$req" | grep -vxF -f <(printf '%s\n' "$cur" | grep .) - || true)
  if [ -n "$miss" ]; then
    ntot=$(printf '%s\n' "$req" | grep -c .)
    nmiss=$(printf '%s\n' "$miss" | grep -c .)
    first=$(printf '%s\n' "$miss" | head -n1)
    emit B-ALLOWLIST HIGH "ERR-SEC-003: selected + allow-list missing $nmiss/$ntot curated pattern(s) (e.g. $first) → apply curated superset"
  fi
fi

# --- B: observed startup_failure runs (bounded symptom, not a diagnosis).
# The API page is capped at 30. Report the denominator and mark saturation so a
# page-size result can never be mistaken for an estate measurement.
sample=$(api "repos/$O/$R/actions/runs?per_page=30" --jq '[.workflow_runs | length, [.[] | select(.conclusion=="startup_failure")] | length] | @tsv' || printf '0\t0')
sampled=${sample%%$'\t'*}
sf=${sample##*$'\t'}
if [ "${sf:-0}" -gt 0 ]; then
  saturation=""
  [ "${sampled:-0}" -ge 30 ] && saturation="; page saturated at 30, older runs not measured"
  emit B-STARTUPFAIL HIGH "$sf startup_failure conclusion(s) among the latest $sampled run(s)$saturation → cause unclassified; inspect the web banner and actions.lock before choosing a remedy"
fi

# --- D: burn anti-pattern (bare [push, pull_request] double-trigger), via API
for path in $(api "repos/$O/$R/contents/.github/workflows" --jq '.[]?|select(.name|test("\\.ya?ml$"))|.path' || true); do
  if api "repos/$O/$R/contents/$path" --jq '.content' | base64 -d \
       | grep -qE '^on:[[:space:]]*\[[[:space:]]*push[[:space:]]*,[[:space:]]*pull_request[[:space:]]*\]'; then
    emit D-BURN MEDIUM "ERR-WF-014: $path on bare [push,pull_request] (2x runs/PR) → scope push to default branch + concurrency-cancel"
  fi
done
