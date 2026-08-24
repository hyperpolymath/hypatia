#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
MOCK_BIN=$(mktemp -d)
trap 'rm -rf "$MOCK_BIN"' EXIT
ln -s "$ROOT/test/fixtures/ci-health/mock-gh" "$MOCK_BIN/gh"

run_detect() {
  PATH="$MOCK_BIN:$PATH" OWNER=test-owner MOCK_SCENARIO="$1" \
    "$ROOT/scripts/ci-health/detect.sh" test-repo
}

lock_output=$(run_detect lockfile)
printf '%s\n' "$lock_output" | grep -q $'test-repo\tB-LOCKFILE\tHIGH\t'
printf '%s\n' "$lock_output" | grep -q '1 startup_failure conclusion(s) among the latest 3 run(s)'
if printf '%s\n' "$lock_output" | grep -q 'populate allow-list'; then
  echo "FAIL: unclassified startup failure prescribed an allow-list mutation" >&2
  exit 1
fi

saturated_output=$(run_detect saturated)
printf '%s\n' "$saturated_output" | grep -q '30 startup_failure conclusion(s) among the latest 30 run(s); page saturated at 30, older runs not measured'
if printf '%s\n' "$saturated_output" | grep -q $'\tB-LOCKFILE\t'; then
  echo "FAIL: saturated scenario was incorrectly classified as a lockfile failure" >&2
  exit 1
fi

ok_output=$(PATH="$MOCK_BIN:$PATH" OWNER=test-owner MOCK_SCENARIO=remediate-ok \
  MOCK_SUPERSET_FILE="$ROOT/scripts/ci-health/action-superset.txt" \
  "$ROOT/scripts/ci-health/remediate.sh" test-repo B-ALLOWLIST false)
printf '%s\n' "$ok_output" | grep -q 'exact set verified'

if PATH="$MOCK_BIN:$PATH" OWNER=test-owner MOCK_SCENARIO=remediate-mismatch \
  MOCK_SUPERSET_FILE="$ROOT/scripts/ci-health/action-superset.txt" \
  "$ROOT/scripts/ci-health/remediate.sh" test-repo B-ALLOWLIST false >/dev/null 2>&1; then
  echo "FAIL: allow-list remediation accepted a mismatched read-back" >&2
  exit 1
fi

echo "PASS: CI-health classification and remediation fixtures"
