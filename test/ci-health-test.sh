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
grep -q $'test-repo\tB-LOCKFILE\tHIGH\t' <<<"$lock_output"
grep -q '1 startup_failure conclusion(s) among the latest 3 run(s)' <<<"$lock_output"
if grep -q 'populate allow-list' <<<"$lock_output"; then
  echo "FAIL: unclassified startup failure prescribed an allow-list mutation" >&2
  exit 1
fi

saturated_output=$(run_detect saturated)
grep -q '30 startup_failure conclusion(s) among the latest 30 run(s); page saturated at 30, older runs not measured' <<<"$saturated_output"
if grep -q $'\tB-LOCKFILE\t' <<<"$saturated_output"; then
  echo "FAIL: saturated scenario was incorrectly classified as a lockfile failure" >&2
  exit 1
fi

# --- B-ALLOWLIST is additive: the corpus in test/fixtures/ci-health/allowlist/.
# Each row: the repo's current selected-actions object and the expected counts.
# The Idris2 model (verification/proofs/idris2/AllowlistRemediation.idr) states
# the same rows on `-- corpus:` lines; the last check below binds the two.
STATE_DIR=$(mktemp -d)
trap 'rm -rf "$MOCK_BIN" "$STATE_DIR"' EXIT
model="$ROOT/verification/proofs/idris2/AllowlistRemediation.idr"
required=$(jq -R -s -c 'split("\n") | map(select(length > 0) | . + "@*") | ["hyperpolymath/*"] + . | unique' \
  "$ROOT/scripts/ci-health/action-superset.txt")

remediate() {
  PATH="$MOCK_BIN:$PATH" OWNER=test-owner MOCK_STATE="$STATE_DIR/state.json" \
    MOCK_PUT_LOG="$STATE_DIR/puts.log" MOCK_SCENARIO="${1:-remediate}" \
    "$ROOT/scripts/ci-health/remediate.sh" test-repo B-ALLOWLIST false
}

for fixture in "$ROOT"/test/fixtures/ci-health/allowlist/*.json; do
  name=$(jq -r '.name' "$fixture")
  jq -c '.current' "$fixture" > "$STATE_DIR/state.json"
  : > "$STATE_DIR/puts.log"
  before=$(jq -c '.patterns_allowed' "$STATE_DIR/state.json")
  out=$(remediate)
  after=$(cat "$STATE_DIR/state.json")
  added=$(jq '.expect.added' "$fixture")

  # Nothing allowed before is missing after (monotone), and every required
  # pattern is present (covering).
  jq -e --argjson b "$before" '($b - .patterns_allowed) == []' \
    <<<"$after" >/dev/null || { echo "FAIL [$name]: a current pattern was dropped" >&2; exit 1; }
  jq -e --argjson r "$required" '($r - .patterns_allowed) == []' <<<"$after" >/dev/null ||
    { echo "FAIL [$name]: a required pattern is missing" >&2; exit 1; }
  # Exactly current UNION required, and the trust flags exactly as found.
  jq -e --slurpfile f "$fixture" '
      (.patterns_allowed | length) == $f[0].expect.result_size
      and .github_owned_allowed == $f[0].expect.github_owned_allowed
      and .verified_allowed == $f[0].expect.verified_allowed' <<<"$after" >/dev/null ||
    { echo "FAIL [$name]: result is not current UNION required with flags kept: $after" >&2; exit 1; }

  if [[ "$added" -eq 0 ]]; then
    [[ ! -s "$STATE_DIR/puts.log" ]] || { echo "FAIL [$name]: wrote although nothing was missing" >&2; exit 1; }
    grep -q '^SKIP ' <<<"$out" || { echo "FAIL [$name]: expected SKIP, got: $out" >&2; exit 1; }
  else
    grep -q "^FIXED .* added $added, kept all $(jq '.expect.kept' "$fixture")" <<<"$out" ||
      { echo "FAIL [$name]: unexpected report: $out" >&2; exit 1; }
  fi

  # Idempotent: a second run finds nothing missing and writes nothing.
  : > "$STATE_DIR/puts.log"
  grep -q '^SKIP ' <<<"$(remediate)" || { echo "FAIL [$name]: second run was not a no-op" >&2; exit 1; }
  [[ ! -s "$STATE_DIR/puts.log" ]] || { echo "FAIL [$name]: second run wrote" >&2; exit 1; }

  # Converges with detect.sh: B-ALLOWLIST no longer fires.
  # Captured, then matched (RE011). A failed detect.sh proves nothing, so it fails the test.
  detect_out=$(PATH="$MOCK_BIN:$PATH" OWNER=test-owner MOCK_STATE="$STATE_DIR/state.json" MOCK_SCENARIO=converge \
    "$ROOT/scripts/ci-health/detect.sh" test-repo 2>/dev/null) ||
    { echo "FAIL [$name]: detect.sh exited $? after remediation" >&2; exit 1; }
  if grep -q $'\tB-ALLOWLIST\t' <<<"$detect_out"; then
    echo "FAIL [$name]: detect.sh still reports B-ALLOWLIST after remediation" >&2; exit 1
  fi

  # Bound to the model: its `-- corpus:` line states the same counts.
  want="-- corpus: $name added=$added kept=$(jq '.expect.kept' "$fixture") result=$(jq '.expect.result_size' "$fixture")"
  grep -qxF -- "$want" "$model" || { echo "FAIL [$name]: model has no line '$want'" >&2; exit 1; }
  eqn="= ($added, $(jq '.expect.kept' "$fixture"), $(jq '.expect.result_size' "$fixture"))"
  block=$(grep -A1 -xF -- "$want" "$model") ||
    { echo "FAIL [$name]: cannot read the model block under '$want'" >&2; exit 1; }
  grep -qF -- "$eqn" <<<"${block##*$'\n'}" ||
    { echo "FAIL [$name]: the equation under '$want' does not state $eqn" >&2; exit 1; }
done
[[ "$(grep -c '^-- corpus: ' "$model")" -eq "$(find "$ROOT/test/fixtures/ci-health/allowlist" -name '*.json' | wc -l)" ]] ||
  { echo "FAIL: the model states a different number of corpus rows than the fixtures" >&2; exit 1; }

# Fail closed: an unreadable allow-list is never overwritten.
jq -c '.current' "$ROOT/test/fixtures/ci-health/allowlist/extras.json" > "$STATE_DIR/state.json"
: > "$STATE_DIR/puts.log"
if remediate remediate-unreadable >/dev/null 2>&1; then
  echo "FAIL: remediation proceeded without reading the current allow-list" >&2; exit 1
fi
[[ ! -s "$STATE_DIR/puts.log" ]] || { echo "FAIL: wrote an allow-list it could not read" >&2; exit 1; }

# A read-back that differs from current UNION required is a failure.
jq -c '.current' "$ROOT/test/fixtures/ci-health/allowlist/empty.json" > "$STATE_DIR/state.json"
if remediate remediate-mismatch >/dev/null 2>&1; then
  echo "FAIL: allow-list remediation accepted a mismatched read-back" >&2; exit 1
fi

# --- D-BURN: detect.sh reports, and remediate.sh selects and rewrites, the same
# files (one pattern, scripts/ci-health/burn-trigger.sh). A file the rewrite
# would leave unchanged is never a target, never written and never counted.
burn() { # <fixture dir> <detect|remediate> [dry_run]
  local dir=$1 tool=$2
  if [ "$tool" = detect ]; then
    PATH="$MOCK_BIN:$PATH" OWNER=test-owner MOCK_SCENARIO=burn MOCK_BURN_DIR="$dir" \
      MOCK_WRITE_LOG="$STATE_DIR/writes.log" "$ROOT/scripts/ci-health/detect.sh" test-repo 2>/dev/null
  else
    PATH="$MOCK_BIN:$PATH" OWNER=test-owner MOCK_BURN_DIR="$dir" MOCK_WRITE_LOG="$STATE_DIR/writes.log" \
      "$ROOT/scripts/ci-health/remediate.sh" test-repo D-BURN "${3:-false}"
  fi
}
mixed="$ROOT/test/fixtures/ci-health/burn/mixed"
: > "$STATE_DIR/writes.log"
burn_detect=$(burn "$mixed" detect) || { echo "FAIL [D-BURN]: detect.sh failed" >&2; exit 1; }
reported=$(grep $'\tD-BURN\t' <<<"$burn_detect" | grep -oE '\.github/workflows/[^ ]+' | sort | paste -sd' ') || true
[ "$reported" = ".github/workflows/commented.yml .github/workflows/has-concurrency.yml" ] ||
  { echo "FAIL [D-BURN]: detect.sh reported '$reported'" >&2; exit 1; }

dry=$(burn "$mixed" remediate true)
[ "$dry" = "DRYRUN test-repo/D-BURN would patch 2 file(s) + open PR" ] ||
  { echo "FAIL [D-BURN]: dry run said '$dry'" >&2; exit 1; }
[[ ! -s "$STATE_DIR/writes.log" ]] || { echo "FAIL [D-BURN]: the dry run wrote" >&2; exit 1; }

live=$(burn "$mixed" remediate)
[ "$live" = "FIXED test-repo/D-BURN -> https://github.com/test-owner/test-repo/pull/1 (2 file(s))" ] ||
  { echo "FAIL [D-BURN]: live run said '$live'" >&2; exit 1; }
put_paths=$(grep '^PUT ' "$STATE_DIR/writes.log" | cut -d' ' -f2 | sort | paste -sd' ')
[ "$put_paths" = "repos/test-owner/test-repo/contents/.github/workflows/commented.yml repos/test-owner/test-repo/contents/.github/workflows/has-concurrency.yml" ] ||
  { echo "FAIL [D-BURN]: wrote '$put_paths'" >&2; exit 1; }
grep -qx 'POST repos/test-owner/test-repo/git/refs | ref=refs/heads/ci/ci-health-auto-remediation | sha=cafe0001' "$STATE_DIR/writes.log" ||
  { echo "FAIL [D-BURN]: the branch was not cut from the commit the patches were read at" >&2; exit 1; }
put_content() { # the decoded content written to <file>
  local file=$1 line
  line=$(grep "^PUT repos/test-owner/test-repo/contents/.github/workflows/$file " "$STATE_DIR/writes.log") ||
    { echo "FAIL [D-BURN]: no PUT for $file" >&2; exit 1; }
  jq -r . <<<"$(sed -E 's/.* \| content=(".*") \| sha=.*/\1/' <<<"$line")"
}
for f in commented.yml has-concurrency.yml; do
  written=$(put_content "$f")
  if grep -qE "$(sed -n "s/^BURN_TRIGGER_ERE='\(.*\)'$/\1/p" "$ROOT/scripts/ci-health/burn-trigger.sh")" <<<"$written"; then
    echo "FAIL [D-BURN]: $f still has the bare trigger after the rewrite" >&2; exit 1
  fi
  [ "$(grep -c '^concurrency:' <<<"$written")" -eq 1 ] ||
    { echo "FAIL [D-BURN]: $f does not have exactly one concurrency block" >&2; exit 1; }
  grep -qx '    branches: \[main, master\]' <<<"$written" ||
    { echo "FAIL [D-BURN]: $f push is not scoped to the default branches" >&2; exit 1; }
done
grep -q 'sha=sha-commented.yml | branch=ci/ci-health-auto-remediation' "$STATE_DIR/writes.log" ||
  { echo "FAIL [D-BURN]: the PUT did not carry the blob sha it was computed from" >&2; exit 1; }

: > "$STATE_DIR/writes.log"
none=$(burn "$ROOT/test/fixtures/ci-health/burn/none" remediate)
[ "$none" = "SKIP test-repo/D-BURN no-targets" ] || { echo "FAIL [D-BURN]: no-target run said '$none'" >&2; exit 1; }
[[ ! -s "$STATE_DIR/writes.log" ]] || { echo "FAIL [D-BURN]: a run with no targets wrote" >&2; exit 1; }

echo "PASS: CI-health classification and remediation fixtures"
