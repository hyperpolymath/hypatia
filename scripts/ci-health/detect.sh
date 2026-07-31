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
#   B-STARTUPFAIL  observed startup_failure runs (symptom; check allow-list)
#   D-BURN         workflow(s) on bare [push,pull_request] = 2x runs/PR
#                  (auto-remediable: scope push + concurrency-cancel)
#
# The API NEVER exposes the startup_failure reason; the GitHub web-UI red
# banner does (it names the blocked action). That is the human diagnostic.
set -euo pipefail
O="${OWNER:-hyperpolymath}"; R="$1"
HERE="$(cd "$(dirname "$0")" && pwd)"   # for action-superset.txt (allow-list coverage)
emit(){ printf '%s\t%s\t%s\t%s\n' "$R" "$1" "$2" "$3"; }

# --- A: billing wall (a failure run whose job annotation matches the signature)
fail_id=$(gh api "repos/$O/$R/actions/runs?status=failure&per_page=5" --jq '.workflow_runs[0].id // empty' 2>/dev/null || true)
if [ -n "${fail_id:-}" ]; then
  job_id=$(gh api "repos/$O/$R/actions/runs/$fail_id/jobs" --jq '.jobs[0].id // empty' 2>/dev/null || true)
  if [ -n "${job_id:-}" ]; then
    msg=$(gh api "repos/$O/$R/check-runs/$job_id/annotations" --jq '.[0].message // empty' 2>/dev/null || true)
    if printf '%s' "$msg" | grep -qiE 'payments have failed|spending limit'; then
      emit A-BILLING CRITICAL "Actions billing/spending-limit wall blocks all billable jobs → OWNER: GitHub Settings -> Billing & plans"
    fi
  fi
fi

# --- B: allow-list under-coverage (the root cause of estate startup_failure).
# Fire when selected-mode and the allow-list does NOT cover the full curated
# superset that remediate.sh PUTs. Catches BOTH an empty list (post-wipe:
# hyperpolymath/* absent) AND an incomplete one (has hyperpolymath/* but is
# missing a third-party action, e.g. gitleaks — previously only B-STARTUPFAIL,
# which has no remediation). The required set mirrors remediate.sh's PUT body
# exactly (hyperpolymath/* + each superset line as owner/repo@*), so a remediate
# converges this to zero-missing and detect stops re-firing (idempotent).
aa=$(gh api "repos/$O/$R/actions/permissions" --jq '.allowed_actions // empty' 2>/dev/null || true)
if [ "$aa" = "selected" ]; then
  cur=$(gh api "repos/$O/$R/actions/permissions/selected-actions" --jq '(.patterns_allowed // [])[]' 2>/dev/null || true)
  req=$(printf 'hyperpolymath/*\n'; sed 's/^[[:space:]]*//;s/[[:space:]]*$//;/^$/d;s/$/@*/' "$HERE/action-superset.txt")
  miss=$(printf '%s\n' "$req" | grep -vxF -f <(printf '%s\n' "$cur" | grep .) - || true)
  if [ -n "$miss" ]; then
    ntot=$(printf '%s\n' "$req" | grep -c .)
    nmiss=$(printf '%s\n' "$miss" | grep -c .)
    first=$(printf '%s\n' "$miss" | head -n1)
    emit B-ALLOWLIST HIGH "selected + allow-list missing $nmiss/$ntot curated pattern(s) (e.g. $first) → apply curated superset"
  fi
fi

# --- B: observed startup_failure runs (symptom)
sf=$(gh api "repos/$O/$R/actions/runs?per_page=30" --jq '[.workflow_runs[]|select(.conclusion=="startup_failure")]|length' 2>/dev/null || echo 0)
[ "${sf:-0}" -gt 0 ] && emit B-STARTUPFAIL HIGH "$sf recent startup_failure run(s) → web-UI banner names the blocked action; populate allow-list patterns"

# --- D: burn anti-pattern (bare [push, pull_request] double-trigger), via API
for path in $(gh api "repos/$O/$R/contents/.github/workflows" --jq '.[]?|select(.name|test("\\.ya?ml$"))|.path' 2>/dev/null || true); do
  if gh api "repos/$O/$R/contents/$path" --jq '.content' 2>/dev/null | base64 -d 2>/dev/null \
       | grep -qE '^on:[[:space:]]*\[[[:space:]]*push[[:space:]]*,[[:space:]]*pull_request[[:space:]]*\]'; then
    emit D-BURN MEDIUM "$path on bare [push,pull_request] (2x runs/PR) → scope push to default branch + concurrency-cancel"
  fi
done
