#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Offline test for scripts/sweeps/estate-pr-automerge.sh and lib/merge-ready.sh.
# A stub `gh` serves JSON fixtures and applies each --jq expression with the
# real jq, so the expressions the sweep ships are the ones under test.
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
FIX="$WORK/fix"; BIN="$WORK/bin"; mkdir -p "$FIX" "$BIN"

cat > "$BIN/gh" <<'STUB'
#!/usr/bin/env bash
# gh api PATH [--jq EXPR] [--paginate] | gh pr merge|comment|close ...
if [ "$1" = "pr" ]; then printf '%s\n' "$*" >> "$FIX/pr.log"; exit 0; fi
[ "$1" = "api" ] || exit 1
echo "$*" >> "$FIX/calls.log"
shift; path=""; expr=""
while [ $# -gt 0 ]; do
  case "$1" in --jq) expr="$2"; shift 2 ;; --paginate) shift ;; *) path="$1"; shift ;; esac
done
name="${path%%\?*}"; name="${name//\//__}"
if [ -f "$FIX/$name.err" ]; then cat "$FIX/$name.err" >&2; exit 1; fi
[ -f "$FIX/$name.json" ] || { echo "gh: Not Found (HTTP 404)" >&2; exit 1; }
if [ -n "$expr" ]; then jq -r "$expr" "$FIX/$name.json"; else cat "$FIX/$name.json"; fi
STUB
chmod +x "$BIN/gh"
export PATH="$BIN:$PATH" FIX

fail() { echo "FAIL: $*" >&2; exit 1; }
put() { printf '%s\n' "$2" > "$FIX/$1.json"; }

# ── 1. merge_ready: one scenario per gate ───────────────────────────────────
# shellcheck source=../scripts/sweeps/lib/merge-ready.sh
. "$ROOT/scripts/sweeps/lib/merge-ready.sh"
R=o__r   # fixture prefix for repo o/r
ready_fixtures() {
  rm -f "$FIX"/*.err "$FIX"/*release*
  put repos__o__r '{"default_branch":"main"}'
  put "repos__o__r__commits__main__check-suites" '{"check_suites":[{"id":1,"conclusion":"success"}]}'
  put "repos__o__r__branches__main__protection__required_status_checks" '{"contexts":["ci"],"checks":[]}'
  put "repos__o__r__rules__branches__main" '[]'
  put "repos__o__r__commits__main__check-runs" '{"check_runs":[{"name":"ci","conclusion":"success"}]}'
  put "repos__o__r__commits__main__status" '{"statuses":[]}'
  put "repos__o__r__rulesets" '[]'
}
expect() { # EXPECTED_REASON_OR_EMPTY LABEL
  local got rc=0
  got=$(merge_ready o/r "${BR:-main}") || rc=$?
  if [ -z "$1" ]; then [ "$rc" -eq 0 ] || fail "$2: expected ready, got '$got'"
  else [ "$rc" -eq 1 ] && [ "$got" = "$1" ] || fail "$2: expected '$1', got rc=$rc '$got'"; fi
}

ready_fixtures;                                                                   expect "" "all gates hold"
rm "$FIX/repos__o__r__branches__main__protection__required_status_checks.json"
put repos__o__r__rules__branches__main '[]';                                      expect "no_required_checks" "404 + no rules"
put repos__o__r__rules__branches__main '[{"type":"required_status_checks","parameters":{"required_status_checks":[{"context":"ci"}]}}]'
                                                                                  expect "" "required via ruleset only"
ready_fixtures
put repos__o__r__branches__main__protection__required_status_checks '{"contexts":["ci","lint"],"checks":[]}'
                                                                                  expect "required_check_never_reported:lint" "never reported"
ready_fixtures
put repos__o__r__commits__main__check-runs '{"check_runs":[{"name":"ci","conclusion":"success"},{"name":"x","conclusion":"failure"}]}'
                                                                                  expect "default_branch_red" "red head"
ready_fixtures
put repos__o__r__rulesets '[{"id":5,"enforcement":"active"}]'
put repos__o__r__rulesets__5 '{"id":5,"bypass_actors":[{"actor_id":1}]}';         expect "ruleset_bypass_actor_present" "bypass actor"
ready_fixtures
put repos__o__r__rulesets '[{"id":5,"enforcement":"disabled"}]';                  expect "" "inactive ruleset ignored"
ready_fixtures
put repos__o__r__commits__main__check-suites '{"check_suites":[{"id":1,"conclusion":"success"},{"id":2,"conclusion":"startup_failure"}]}'
                                                                                  expect "default_branch_red" "startup_failure suite with no check run"
ready_fixtures
put repos__o__r__rulesets '[{"id":5,"enforcement":"active"}]'
put repos__o__r__rulesets__5 '{"id":5}';                                          expect "check_unavailable:rulesets" "bypass_actors omitted fails closed"
ready_fixtures
for f in "$FIX"/repos__o__r__*main*; do g="${f//__main__/__release__}"; cp "$f" "${g/__main.json/__release.json}"; done
put repos__o__r__commits__main__check-runs '{"check_runs":[{"name":"ci","conclusion":"failure"}]}'
BR=release expect "default_branch_red" "PR targets release, default branch is red"
ready_fixtures
for f in "$FIX"/repos__o__r__*main*; do g="${f//__main__/__release__}"; cp "$f" "${g/__main.json/__release.json}"; done
put repos__o__r__commits__release__check-runs '{"check_runs":[{"name":"ci","conclusion":"failure"}]}'
BR=release expect "target_branch_red" "PR target branch is red";                  ready_fixtures
for want in "rulesets?per_page=100 --paginate" "rules/branches/main?per_page=100 --paginate" "status?per_page=100 --paginate"; do
  grep -qF -- "$want" "$FIX/calls.log" || fail "lists must be paginated: missing '$want'"
done
ready_fixtures
echo "HTTP 500" > "$FIX/repos__o__r__commits__main__status.err";                  expect "check_unavailable:branch_checks" "API error fails closed"

# ── 2. end to end: plan, then execute against the live PR ───────────────────
SHA1=$(printf 'a%.0s' {1..40}); SHA2=$(printf 'b%.0s' {1..40})
pr() { # NUMBER LOGIN SHA
  jq -cn --argjson n "$1" --arg login "$2" --arg sha "$3" \
   '{number:$n,title:"Bump serde from 1.0.199 to 1.0.200",draft:false,mergeable:true,mergeable_state:"clean",
     created_at:"2026-11-01T00:00:00Z",user:{login:$login},base:{ref:"main",repo:{name:"r"}},
     head:{sha:$sha,ref:"dependabot/cargo/serde"},changed_files:1,labels:[],
     body:"Bumps [serde](https://github.com/serde-rs/serde) from 1.0.199 to 1.0.200."}'
}
estate() { # head sha of PR 7
  put orgs__hyperpolymath__repos '[{"name":"r","fork":false,"disabled":false,"archived":false}]'
  put repos__hyperpolymath__r__pulls "[$(pr 7 'dependabot[bot]' "$1"),$(pr 8 alice "$SHA1")]"
  put repos__hyperpolymath__r '{"archived":false,"default_branch":"main"}'
  put repos__hyperpolymath__r__pulls__7 "$(pr 7 'dependabot[bot]' "$1")"
  put repos__hyperpolymath__r__pulls__8 "$(pr 8 alice "$SHA1")"
  put search__issues '{"total_count":2}'
  for n in 7 8; do put "repos__hyperpolymath__r__pulls__${n}__files" '[{"filename":"Cargo.lock","patch":"@@ -1 +1 @@\n-version = \"1.0.199\"\n+version = \"1.0.200\""}]'; done
  # the merge_ready fixtures, for hyperpolymath/r
  for f in "$FIX"/repos__o__r__*; do cp "$f" "${f/repos__o__r__/repos__hyperpolymath__r__}"; done
}
ready_fixtures
run() { (cd "$WORK" && "$ROOT/scripts/sweeps/estate-pr-automerge.sh" --out "$WORK/out" --cache "$WORK/cache" "$@" 2>"$WORK/log") || [ $? -eq 2 ]; }
disp() { jq -r --argjson n "$1" 'select(.pr.number == $n) | .disposition + "/" + .blocked_by' "$WORK/out/pr-decisions.jsonl"; }

estate "$SHA1"; run
[ "$(disp 7)" = "auto_merge/awaiting_required_checks" ] || fail "PR 7 should be auto_merge, got $(disp 7)"
[ "$(disp 8)" = "flag/not_a_dependency_bot" ] || fail "PR 8 should be flagged, got $(disp 8)"
cp "$WORK/out/pr-decisions.jsonl" "$WORK/plan.jsonl"

rm -f "$FIX/pr.log"; run --execute --from "$WORK/plan.jsonl"
grep -q "pr merge 7 --repo hyperpolymath/r --squash --auto --delete-branch --match-head-commit $SHA1" "$FIX/pr.log" \
  || fail "PR 7 should merge, bound to its head SHA"
grep -q "pr merge 8" "$FIX/pr.log" && fail "human PR must never merge"

rm -f "$FIX/pr.log"; estate "$SHA2"; run --execute --from "$WORK/plan.jsonl"
[ ! -f "$FIX/pr.log" ] || fail "head moved after the plan: nothing may merge"

estate "$SHA1"; rm "$FIX/repos__hyperpolymath__r__pulls__7__files.json"
rm -f "$FIX/pr.log"; run --execute --from "$WORK/plan.jsonl"
[ ! -f "$FIX/pr.log" ] || fail "unreadable live diff: nothing may merge"

estate "$SHA1"; rm "$FIX/repos__hyperpolymath__r__branches__main__protection__required_status_checks.json"
put repos__hyperpolymath__r__rules__branches__main '[]'
rm -f "$FIX/pr.log"; run --execute --from "$WORK/plan.jsonl"
[ ! -f "$FIX/pr.log" ] || fail "no required checks: --auto would merge unchecked"
grep -q "no_required_checks" "$WORK/log" || fail "skip reason must be logged"

echo "ok: pr-automerge sweep"
