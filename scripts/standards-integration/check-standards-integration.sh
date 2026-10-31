#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# check-standards-integration.sh — verify this repo's shared-governance
# integration with metadatastician/standards is published, immutable and
# internally consistent. Offline by default; --verify-remote additionally
# asks GitHub (via gh) that the pinned standards commit exists.
#
# Outcome semantics (never collapse these into "green"):
#   exit 0  outcome=pass        category=findings-channel-ready
#             The integration contract holds. This says NOTHING about
#             whether a Hypatia scan found anything; it only proves the
#             channel through which findings would arrive is sound.
#   exit 1  outcome=fail        category=config
#             A real contract violation (floating/divergent pin, lock
#             drift, floating scanner ref, secrets: inherit). Treat as a
#             category-3 tool/configuration failure, not a code finding.
#   exit 3  outcome=unavailable category=tool-unavailable
#             A required tool (or the remote check) could not run. This
#             is NOT a pass and NOT a finding; report it as unavailable.
# Usage: check-standards-integration.sh [--root DIR] [--verify-remote]
set -uo pipefail

ROOT="."
VERIFY_REMOTE=0
while [ $# -gt 0 ]; do
  case "$1" in
    --root) ROOT="${2:?--root needs a directory}"; shift 2 ;;
    --verify-remote) VERIFY_REMOTE=1; shift ;;
    *) echo "outcome=unavailable category=tool-unavailable severity=error detail=unknown argument: $1" >&2; exit 3 ;;
  esac
done

fail() { echo "outcome=fail category=config severity=high detail=$1"; exit 1; }
unavailable() { echo "outcome=unavailable category=tool-unavailable severity=error detail=$1"; exit 3; }

for tool in grep sed awk; do
  command -v "$tool" >/dev/null 2>&1 || unavailable "required tool '$tool' not on PATH"
done

WF="$ROOT/.github/workflows"
LOCK="$WF/actions.lock"
CALLERS="governance.yml hypatia-scan.yml mirror.yml scorecard.yml secret-scanner.yml"

for f in $CALLERS; do
  [ -f "$WF/$f" ] || fail "missing standards caller workflow: $f"
done
[ -f "$LOCK" ] || fail "missing .github/workflows/actions.lock"

# --- Admission/source resolution: every standards reusable must resolve --
# to exactly one immutable 40-hex commit across all callers. A branch,
# tag-only or divergent pin is what produced zero-job startup_failure and
# stale-policy failures estate-wide; it is rejected here, before CI.
pins=""
for f in $CALLERS; do
  uses_lines=$(grep -E 'uses:[[:space:]]*metadatastician/standards/\.github/workflows/' "$WF/$f" || true)
  [ -n "$uses_lines" ] || fail "$f declares no metadatastician/standards reusable"
  if printf '%s\n' "$uses_lines" | grep -vqE '@[0-9a-f]{40}([[:space:]#]|$)'; then
    fail "$f has a non-immutable standards ref (must be a full 40-hex SHA): $uses_lines"
  fi
  pin=$(printf '%s\n' "$uses_lines" | sed -E 's/.*@([0-9a-f]{40}).*/\1/' | head -n1)
  pins="$pins $pin"
done
# shellcheck disable=SC2086
distinct=$(printf '%s\n' $pins | sort -u | grep -c . || true)
[ "$distinct" = "1" ] || fail "standards pins diverge across callers:$pins (bump all five + actions.lock together)"
PIN=$(printf '%s\n' $pins | sort -u)

# --- Lock consistency: actions.lock must record the same pin per workflow
# and as a dependency resolution. A drifted lock kills workflows at
# startup_failure with zero jobs (hypatia#723), so this is checked offline.
for f in $CALLERS; do
  block=$(awk -v want="'.github/workflows/$f':" '
    index($0, want) { inblock=1; print; next }
    inblock && /^    / && $0 !~ /^        / { exit }
    inblock { print }' "$LOCK")
  printf '%s\n' "$block" | grep -qF "metadatastician/standards@$PIN" \
    || fail "actions.lock workflows entry for $f does not record standards@$PIN"
done
grep -qF "'metadatastician/standards@$PIN':" "$LOCK" \
  || fail "actions.lock has no dependency entry for metadatastician/standards@$PIN"
grep -A2 -F "'metadatastician/standards@$PIN':" "$LOCK" | grep -qF "commit: 'sha1-$PIN'" \
  || fail "actions.lock dependency for standards@$PIN does not resolve to sha1-$PIN"

# --- Scanner source: Hypatia must never scan a PR with the PR's own rules,
# and must never float. Callers pass the base/event SHA explicitly.
for f in governance.yml hypatia-scan.yml; do
  grep -q 'hypatia-ref:' "$WF/$f" || fail "$f does not pass hypatia-ref explicitly (scanner source would float)"
  if grep -E 'hypatia-ref:' "$WF/$f" | grep -qE '(HEAD|main|master|pull_request\.head)'; then
    fail "$f passes a floating or PR-head hypatia-ref"
  fi
  grep -E 'hypatia-ref:' "$WF/$f" | grep -qE 'pull_request\.base\.sha \|\| github\.sha|[0-9a-f]{40}' \
    || fail "$f hypatia-ref is neither the base/event SHA expression nor a 40-hex SHA"
done

# --- Least privilege: reusables get only their declared secrets.
if grep -rn 'secrets:[[:space:]]*inherit' $WF/governance.yml $WF/hypatia-scan.yml $WF/mirror.yml $WF/scorecard.yml $WF/secret-scanner.yml >/dev/null 2>&1; then
  fail "a standards caller uses 'secrets: inherit' (must pass only declared secrets)"
fi

# --- Optional remote proof: the pin must be a published commit.
if [ "$VERIFY_REMOTE" = "1" ]; then
  command -v gh >/dev/null 2>&1 \
    || unavailable "gh not on PATH; remote pin verification not run (offline checks above are not a substitute)"
  resolved=$(gh api "repos/metadatastician/standards/commits/$PIN" --jq .sha 2>/dev/null) \
    || unavailable "gh could not resolve metadatastician/standards@$PIN (network/auth); pin not remotely verified"
  [ "$resolved" = "$PIN" ] || fail "remote standards commit mismatch: pinned $PIN, resolved $resolved"
  echo "remote=verified repo=metadatastician/standards commit=$PIN"
fi

echo "outcome=pass category=findings-channel-ready severity=none standards_pin=$PIN callers=5 detail=immutable pin, lock and scanner-source contract hold; this is not a scan result"
exit 0
