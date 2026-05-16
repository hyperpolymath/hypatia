#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Layer-1 propagation sweep for hyperpolymath/hypatia#252.
#
# Consuming repos carry their OWN copy of .github/workflows/hypatia-scan.yml.
# The Phase-2 "Submit findings to gitbot-fleet" step hard-fails the security
# gate (exit 127) for any repo with >=1 finding, because gitbot-fleet's
# submit-finding.sh no longer exists. This sweep replaces that one step with
# the canonical fixed block (continue-on-error + self-healing body).
#
# Layer-2 (GS005 CI false-positive) needs NO propagation: it ships in the
# scanner binary, which every consuming repo clones from hypatia main at
# scan time.
#
# Default = DRY RUN (clone, patch, diff, discard). Pass --apply to branch,
# commit, push and open PRs. Idempotent: already-patched repos are skipped.
#
# Usage:
#   ./resync-hypatia-scan-phase2.sh                 # dry run, all consumers
#   ./resync-hypatia-scan-phase2.sh --apply         # open PRs
#   ./resync-hypatia-scan-phase2.sh --repos r1,r2   # restrict target set
#
set -euo pipefail

ORG="hyperpolymath"
BRANCH="fix/hypatia-scan-phase2-resync"
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FRAGMENT="${HERE}/phase2-canonical.fragment.yml"
PATCHER="${HERE}/patch_phase2.py"
WF_PATH=".github/workflows/hypatia-scan.yml"
APPLY=0
REPO_CSV=""

# --- Exclusions (per estate policy) -----------------------------------------
# ReScript hands-off: rescript-ecosystem / ReScript-language repos and rescript
# adapters are excluded from bulk sweeps. hypatia itself is already fixed via
# #252. Review/extend this list before an --apply run.
EXCLUDE=(
  "hypatia"
  "stapeln"
  "rescript-dom-mounter"
  "idaptik-rescript13-staging"
  "poly-observability-mcp"
)

while [ $# -gt 0 ]; do
  case "$1" in
    --apply) APPLY=1 ;;
    --repos) REPO_CSV="${2:-}"; shift ;;
    -h|--help) sed -n '2,30p' "$0"; exit 0 ;;
    *) echo "unknown arg: $1" >&2; exit 1 ;;
  esac
  shift
done

[ -f "$FRAGMENT" ] || { echo "missing canonical fragment: $FRAGMENT" >&2; exit 1; }
[ -f "$PATCHER" ]  || { echo "missing patcher: $PATCHER" >&2; exit 1; }
command -v gh  >/dev/null || { echo "gh not found" >&2; exit 1; }
command -v git >/dev/null || { echo "git not found" >&2; exit 1; }

excluded() {
  local name="${1##*/}"
  for e in "${EXCLUDE[@]}"; do [ "$name" = "$e" ] && return 0; done
  return 1
}

# --- Target list ------------------------------------------------------------
if [ -n "$REPO_CSV" ]; then
  IFS=',' read -r -a REPOS <<< "$REPO_CSV"
else
  mapfile -t REPOS < <(
    gh search code --owner "$ORG" "Submit findings to gitbot-fleet" \
      --filename hypatia-scan.yml --limit 100 \
      --json repository -q '.[].repository.nameWithOwner' | sort -u
  )
fi

echo "Mode: $([ $APPLY -eq 1 ] && echo APPLY || echo DRY-RUN)"
echo "Candidates: ${#REPOS[@]}"
echo

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
SUM_PATCHED=(); SUM_SKIP=(); SUM_NA=(); SUM_EXCL=(); SUM_ERR=()

for full in "${REPOS[@]}"; do
  [ -z "$full" ] && continue
  repo="${full#"$ORG"/}"; repo="${repo##*/}"
  full="${ORG}/${repo}"

  if excluded "$full"; then
    echo "EXCL  $full (policy exclusion)"; SUM_EXCL+=("$full"); continue
  fi

  dir="${WORK}/${repo}"
  if ! gh repo clone "$full" "$dir" -- --depth 1 --quiet 2>/dev/null; then
    echo "ERR   $full (clone failed)"; SUM_ERR+=("$full"); continue
  fi

  if [ ! -f "${dir}/${WF_PATH}" ]; then
    echo "N/A   $full (no ${WF_PATH})"; SUM_NA+=("$full"); continue
  fi

  set +e
  python3 "$PATCHER" "${dir}/${WF_PATH}" "$FRAGMENT"
  rc=$?
  set -e

  case $rc in
    2) echo "SKIP  $full (already patched)"; SUM_SKIP+=("$full"); continue ;;
    3) echo "N/A   $full (no Phase-2 block)"; SUM_NA+=("$full"); continue ;;
    0) : ;;
    *) echo "ERR   $full (patcher rc=$rc)"; SUM_ERR+=("$full"); continue ;;
  esac

  # Validate YAML before going further.
  if ! python3 -c "import yaml,sys; yaml.safe_load(open(sys.argv[1]))" \
        "${dir}/${WF_PATH}" 2>/dev/null; then
    echo "ERR   $full (post-patch YAML invalid — NOT touched)"
    SUM_ERR+=("$full"); continue
  fi

  if [ $APPLY -eq 0 ]; then
    echo "PATCH $full (dry-run) — diff:"
    git -C "$dir" --no-pager diff --stat -- "$WF_PATH" | sed 's/^/      /'
    SUM_PATCHED+=("$full"); continue
  fi

  git -C "$dir" checkout -q -b "$BRANCH"
  git -C "$dir" add "$WF_PATH"
  git -C "$dir" -c commit.gpgsign=false -c user.name="Jonathan D.A. Jewell" \
      -c user.email="jonathan.jewell@gmail.com" commit -q -F - <<'MSG'
fix(ci): Phase-2 fleet submission must not fail the security gate

Layer-1 propagation of hyperpolymath/hypatia#252. This repo's own copy
of hypatia-scan.yml hard-failed (exit 127) for any commit with >=1
finding: the "Submit findings to gitbot-fleet (Phase 2)" step cloned
gitbot-fleet and exec'd scripts/submit-finding.sh, which no longer
exists on gitbot-fleet's default branch.

Phase 2 is the collaborative LEARNING side-channel, not the security
gate. Fix: continue-on-error + self-healing body (non-fatal clone,
probe known script paths, graceful ::warning:: skip). Security
enforcement (the baseline-aware critical/high step) is unchanged.

Refs hyperpolymath/hypatia#252

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>
MSG

  # No -u on the (token-bearing) push URL, per estate policy.
  if ! git -C "$dir" push -q origin "HEAD:${BRANCH}" 2>/dev/null; then
    echo "ERR   $full (push failed)"; SUM_ERR+=("$full"); continue
  fi

  pr_url=$(gh pr create --repo "$full" --base main --head "$BRANCH" \
    --title "fix(ci): Phase-2 fleet submission must not fail the security gate" \
    --body "$(cat <<'BODY'
Layer-1 propagation of **hyperpolymath/hypatia#252**.

This repo carries its own copy of `.github/workflows/hypatia-scan.yml`.
The **"Submit findings to gitbot-fleet (Phase 2)"** step hard-failed the
job (exit 127) for any commit with ≥1 finding — it clones `gitbot-fleet`
and execs `scripts/submit-finding.sh`, which no longer exists on
gitbot-fleet's default branch. That is the estate-wide "Hypatia
Neurosymbolic Analysis fails regardless of content" symptom.

Phase 2 is the collaborative **learning** side-channel, not the security
gate (the gate is the separate baseline-aware critical/high step, which
is untouched). Fix mirrors the canonical workflow:

- `continue-on-error: true` on the Phase-2 step.
- Self-healing body: non-fatal clone, probe known submit-script paths,
  skip with `::warning::` if absent or non-zero.

Surgical: only the Phase-2 step changed; every other step preserved.
Security enforcement is unchanged.

Refs hyperpolymath/hypatia#252

🤖 Generated with [Claude Code](https://claude.com/claude-code)
BODY
)" 2>/dev/null) || { echo "ERR   $full (pr create failed)"; SUM_ERR+=("$full"); continue; }

  echo "PR    $full -> $pr_url"
  SUM_PATCHED+=("$full")
done

echo
echo "==================== SUMMARY ===================="
echo "patched/PR : ${#SUM_PATCHED[@]}"
echo "skip(done) : ${#SUM_SKIP[@]}"
echo "n/a        : ${#SUM_NA[@]}"
echo "excluded   : ${#SUM_EXCL[@]}"
echo "errors     : ${#SUM_ERR[@]}"
[ ${#SUM_ERR[@]} -gt 0 ] && printf 'ERR: %s\n' "${SUM_ERR[@]}"
exit 0
