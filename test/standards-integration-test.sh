#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Repo-specific tests for the metadatastician/standards shared-governance
# integration. Positive (real tree passes), negative (floating pin, lock
# drift, divergent pins, PR-head scanner ref are rejected) and
# unavailable-tool (missing gh for remote verification is exit 3 /
# outcome=unavailable — never reported as a pass).
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
CHECK="$ROOT/scripts/standards-integration/check-standards-integration.sh"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
chmod +x "$CHECK"

expect_outcome() { # name expected_exit expected_outcome root extra...
  local name="$1" want_rc="$2" want_outcome="$3"; shift 3
  local out rc=0
  out=$("$CHECK" "$@" 2>&1) || rc=$?
  if [ "$rc" != "$want_rc" ] || ! printf '%s' "$out" | grep -q "outcome=$want_outcome"; then
    echo "FAIL($name): want rc=$want_rc outcome=$want_outcome, got rc=$rc: $out" >&2
    exit 1
  fi
  echo "ok($name): rc=$rc $out"
}

fixture() { # fixture <dir>: minimal copy of the integration surface
  local dir="$1"
  mkdir -p "$dir/.github/workflows"
  for f in governance.yml hypatia-scan.yml mirror.yml scorecard.yml secret-scanner.yml actions.lock; do
    cp "$ROOT/.github/workflows/$f" "$dir/.github/workflows/$f"
  done
}

# Positive: the real tree satisfies the contract.
expect_outcome positive 0 pass --root "$ROOT"

# Negative 1: floating @main standards ref (the historical HEAD-clone class).
fixture "$TMP/floating"
sed -i -E 's|(metadatastician/standards/\.github/workflows/[^@]+)@[0-9a-f]{40}|\1@main|' "$TMP/floating/.github/workflows/governance.yml"
expect_outcome floating-pin 1 fail --root "$TMP/floating"

# Negative 2: lock drift (workflow pin and actions.lock disagree).
fixture "$TMP/lockdrift"
sed -i 's|metadatastician/standards@[0-9a-f]\{40\}|metadatastician/standards@0000000000000000000000000000000000000000|g' "$TMP/lockdrift/.github/workflows/actions.lock"
expect_outcome lock-drift 1 fail --root "$TMP/lockdrift"

# Negative 3: divergent pins across callers.
fixture "$TMP/divergent"
sed -i 's|@[0-9a-f]\{40\}|@1111111111111111111111111111111111111111|' "$TMP/divergent/.github/workflows/scorecard.yml"
expect_outcome divergent-pins 1 fail --root "$TMP/divergent"

# Negative 4: scanner would run the PR head's own rules.
fixture "$TMP/prhead"
sed -i 's@hypatia-ref:.*@hypatia-ref: ${{ github.event.pull_request.head.sha }}@' "$TMP/prhead/.github/workflows/hypatia-scan.yml"
expect_outcome pr-head-scanner 1 fail --root "$TMP/prhead"

# Negative 5: a 40-hex string in a comment must not disguise a floating
# scanner value (github.sha on pull_request is the PR merge commit).
fixture "$TMP/comment-sha"
sed -i 's@hypatia-ref:.*@hypatia-ref: ${{ github.sha }} # 0000000000000000000000000000000000000000@' "$TMP/comment-sha/.github/workflows/hypatia-scan.yml"
expect_outcome comment-disguised-scanner 1 fail --root "$TMP/comment-sha"

# Negative 6: a second standards reusable job in one caller, pinned to a
# different revision, must not hide behind the first job's pin.
fixture "$TMP/second-job"
cat >> "$TMP/second-job/.github/workflows/governance.yml" <<'YAML'
  second:
    uses: metadatastician/standards/.github/workflows/governance-reusable.yml@1111111111111111111111111111111111111111
YAML
expect_outcome second-job-divergent 1 fail --root "$TMP/second-job"

# Negative 7: dangling transitive lock record — a nested uses: with no
# exact top-level dependency entry (the PR #992 download-artifact case).
fixture "$TMP/dangling"
sed -i "s|            - 'actions/cache@55cc8345863c7cc4c66a329aec7e433d2d1c52a9'|            - 'actions/cache@55cc8345863c7cc4c66a329aec7e433d2d1c52a9'\n            - 'acme/missing@2222222222222222222222222222222222222222'|" \
  "$TMP/dangling/.github/workflows/actions.lock"
expect_outcome dangling-transitive 1 fail --root "$TMP/dangling"

# Positive 2: an inline comment after a valid pin is not part of the ref.
fixture "$TMP/inline-comment"
sed -i -E 's|(uses:.*@[0-9a-f]{40})$|\1 # pinned standards revision|' "$TMP/inline-comment/.github/workflows/scorecard.yml"
expect_outcome inline-comment-pin 0 pass --root "$TMP/inline-comment"

# Unavailable tool: remote verification requested but gh absent from PATH.
# Build a PATH with only the checker's offline tools — no gh. The checker
# must say unavailable (exit 3), never pass.
mkdir -p "$TMP/nogh"
for t in bash grep sed awk sort head env; do
  src=$(command -v "$t" || true); [ -n "$src" ] && ln -s "$src" "$TMP/nogh/$t"
done
rc=0
out=$(PATH="$TMP/nogh" "$TMP/nogh/bash" "$CHECK" --root "$ROOT" --verify-remote 2>&1) || rc=$?
if [ "$rc" != "3" ] || ! printf '%s' "$out" | grep -q 'outcome=unavailable'; then
  echo "FAIL(unavailable-tool): want rc=3 outcome=unavailable, got rc=$rc: $out" >&2
  exit 1
fi
if printf '%s' "$out" | grep -q 'outcome=pass'; then
  echo "FAIL(unavailable-tool): unavailable was reported as pass" >&2
  exit 1
fi
echo "ok(unavailable-tool): rc=$rc $out"

# Unavailable tool 2: an offline core tool (sort) missing must also be
# exit 3 / unavailable, not misreported as divergent pins (exit 1).
mkdir -p "$TMP/nosort"
for t in bash grep sed awk head env; do
  src=$(command -v "$t" || true); [ -n "$src" ] && ln -s "$src" "$TMP/nosort/$t"
done
rc=0
out=$(PATH="$TMP/nosort" "$TMP/nosort/bash" "$CHECK" --root "$ROOT" 2>&1) || rc=$?
if [ "$rc" != "3" ] || ! printf '%s' "$out" | grep -q 'outcome=unavailable'; then
  echo "FAIL(unavailable-sort): want rc=3 outcome=unavailable, got rc=$rc: $out" >&2
  exit 1
fi
echo "ok(unavailable-sort): rc=$rc $out"

echo "PASS: standards-integration positive, negative and unavailable-tool cases"
