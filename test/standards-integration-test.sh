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

echo "PASS: standards-integration positive, negative and unavailable-tool cases"
