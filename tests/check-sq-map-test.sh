#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Controls for scripts/check-sq-map.sh, against a checkout of
# hyperpolymath/proven-cicd-and-workflows (its test and map are copied, so the
# controls do not depend on what main's map holds today):
#   * a registry whose ids all have rows passes (0);
#   * a new ci_failure_triage id fails (1) and the cure names its exact row;
#   * a deleted ADR 0004 workflow_audit row fails (1);
#   * a missing or partial checkout, a missing deed, and a test that cannot
#     judge are NOT RUN (2), never a pass;
#   * with no deed the generator runs, and its registry is judged (0 or 1).
#
#   tests/check-sq-map-test.sh PROVEN_CICD_DIR
set -euo pipefail
cd "$(dirname "$0")/.."
src=${1:?usage: tests/check-sq-map-test.sh PROVEN_CICD_DIR}
[[ -f $src/tests/registry-sq-map.sh && -f $src/proven/registry/sq-map.tsv ]] ||
  { echo "check-sq-map-test: $src is not a proven-cicd checkout; NOT RUN" >&2; exit 2; }
work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
pass=0 fail=0
ok() { pass=$((pass + 1)); echo "  ok   $1"; }
bad() { fail=$((fail + 1)); echo "  FAIL $1" >&2; }
fresh() { # -> $work/p, a copy of the test and the map
  rm -rf -- "$work/p"; mkdir -p "$work/p/tests" "$work/p/proven/registry"
  cp "$src/tests/registry-sq-map.sh" "$work/p/tests/"
  cp "$src/proven/registry/sq-map.tsv" "$work/p/proven/registry/"
}
expect() { # NAME WANT ARGS...
  local name=$1 want=$2 rc=0; shift 2
  env -u GITHUB_STEP_SUMMARY bash scripts/check-sq-map.sh "$@" >"$work/out" 2>&1 || rc=$?
  if [[ $rc == "$want" ]]; then ok "$name"; else bad "$name: wanted $want, got $rc"; sed 's/^/       /' "$work/out" | tail -12 >&2; fi
}

fresh
# A registry of exactly the map's own ids: every required id has a row.
grep -oE 'hypatia/[a-z0-9_]+/[A-Za-z0-9_]+' "$work/p/proven/registry/sq-map.tsv" | sort -u >"$work/own.deed"
expect 'a registry whose ids all have rows passes' 0 "$work/p" "$work/own.deed"

{ cat "$work/own.deed"; echo hypatia/ci_failure_triage/brand_new_rule; } >"$work/new.deed"
expect 'a new ci_failure_triage id with no row fails' 1 "$work/p" "$work/new.deed"
row=$(printf 'hypatia/ci_failure_triage/brand_new_rule\tunassigned\tunmapped\tpresent\t')
grep -qF -- "$row" "$work/out" && ok 'the cure names the exact unmapped row' || bad 'the cure does not name the row'
grep -q 'proven-cicd-and-workflows first' "$work/out" && ok 'the cure says where to add it' || bad 'the cure does not say where'
# The printed row, appended to the map, makes the same registry pass.
grep -F -- "$row" "$work/out" | head -n 1 >>"$work/p/proven/registry/sq-map.tsv"
expect 'adding the printed row makes it pass' 0 "$work/p" "$work/new.deed"

fresh
grep -v '^hypatia/workflow_audit/actions_lock_stale	' "$src/proven/registry/sq-map.tsv" >"$work/p/proven/registry/sq-map.tsv"
expect 'a deleted ADR 0004 workflow_audit row fails' 1 "$work/p" "$work/own.deed"
grep -q '^hypatia/workflow_audit/actions_lock_stale	<SQ id from ADR 0004>	mapped' "$work/out" &&
  ok 'the cure asks for the mapped row back' || bad 'the cure does not name the mapped row'

fresh
expect 'a missing checkout is NOT RUN' 2 "$work/absent" "$work/own.deed"
rm "$work/p/tests/registry-sq-map.sh"
expect 'a checkout without the test is NOT RUN' 2 "$work/p" "$work/own.deed"
fresh
expect 'a missing deed is NOT RUN' 2 "$work/p" "$work/absent.deed"
echo 'no ids here' >"$work/empty.deed"
expect 'a registry the test cannot read (its exit 2) is NOT RUN' 2 "$work/p" "$work/empty.deed"
expect 'no PROVEN_CICD_DIR is NOT RUN' 2

fresh
rc=0; env -u GITHUB_STEP_SUMMARY bash scripts/check-sq-map.sh "$work/p" >"$work/out" 2>&1 || rc=$?
if [[ $rc == 0 || $rc == 1 ]] && grep -q 'gen-rules-praxis.sh --stdout' "$work/out"; then
  ok "with no deed the generator's registry is judged (exit $rc)"
else bad "generator path: exit $rc"; tail -8 "$work/out" >&2; fi

echo "check-sq-map-test: $pass passed, $fail failed"
[[ $fail == 0 ]]
