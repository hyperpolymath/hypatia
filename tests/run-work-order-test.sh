#!/usr/bin/env bash
# shellcheck disable=SC2015 # ok() cannot fail, so "test && ok || fail" is an if-then-else here
# SPDX-License-Identifier: MPL-2.0
#
# run-work-order-test.sh: the launch script builds the right command, refuses
# the unsafe cases, and writes one canonical ledger line for each run.
#
# It uses a stub `claude` and a throw-away git repository, so it makes no API
# call and costs nothing. Run: bash tests/run-work-order-test.sh

set -euo pipefail

here=$(cd "$(dirname "$0")/.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
pass=0
ok() { pass=$((pass + 1)); echo "ok   $1"; }
fail() { echo "FAIL $1" >&2; exit 1; }

# A throw-away checkout with the script, the profile and 2 fixture work orders.
repo="$work/repo"
mkdir -p "$repo/scripts/api-agents" "$repo/.claude/profiles" "$repo/wo"
cp "$here/scripts/api-agents/run-work-order.sh" "$repo/scripts/api-agents/"
cp "$here/.claude/profiles/e0-headless.json" "$repo/.claude/profiles/"
order() { # order ID REPO
  local id=$1 wo_repo=$2
  printf '= %s: fixture\n:wo-id: %s\n:wo-repo: %s\n:wo-size: small\n:wo-agent: e0-executor\n:wo-model: sonnet\n:wo-effort: medium\n:wo-cap-usd: 5\n:wo-max-turns: 120\n:wo-branch: fix/x\n:wo-depends: none\n' \
    "$id" "$id" "$wo_repo" > "$repo/wo/$id-fixture.adoc"
}
order E0-98 hyperpolymath/hypatia
order E0-99 hyperpolymath/gitbot-fleet
git -C "$repo" init -q
git -C "$repo" remote add origin https://example.invalid/hyperpolymath/hypatia
git -C "$repo" add -A
git -C "$repo" -c user.name=test -c user.email=test@example.invalid -c commit.gpgsign=false commit -q -m fixture

# A stub claude: it records its arguments and prints a result.
mkdir -p "$work/bin"
cat > "$work/bin/claude" <<'STUB'
#!/bin/sh
printf '%s\n' "$*" >> "$STUB_LOG"
printf '{"total_cost_usd":1.25,"num_turns":7,"duration_ms":1000,"subtype":"success","session_id":"test-session","result":"done"}\n'
exit "${STUB_EXIT:-0}"
STUB
chmod +x "$work/bin/claude"

export STUB_LOG="$work/args.log" HYPATIA_LEDGER="$work/ledger.jsonl" WO_DIR="$repo/wo"
export PATH="$work/bin:$PATH"
: > "$STUB_LOG"
run() { (cd "$repo" && sh scripts/api-agents/run-work-order.sh "$@"); }
lines() { if [[ -f "$HYPATIA_LEDGER" ]]; then wc -l < "$HYPATIA_LEDGER"; else echo 0; fi; }
want="--agent e0-executor --model sonnet --effort medium --max-budget-usd 5 --max-turns 120 --permission-mode dontAsk"

out=$(run E0-98 --dry-run)
[[ "$out" == *"$want"* ]] && [[ ! -s "$STUB_LOG" ]] && ok "dry run prints the command and calls nothing" || fail "dry run: $out"

if (unset ANTHROPIC_API_KEY; run E0-98) 2>/dev/null; then fail "ran without an API key"; fi
[[ ! -s "$STUB_LOG" ]] && [[ "$(lines)" == 0 ]] && ok "no API key: refused, nothing called, no ledger line" || fail "no API key"

export ANTHROPIC_API_KEY=test-not-a-real-key
run E0-98 >/dev/null
[[ "$(cat "$STUB_LOG")" == *"$want"* ]] && ok "the run passes the model, effort, cap and turns of the work order" || fail "arguments: $(cat "$STUB_LOG")"
line=$(tail -n 1 "$HYPATIA_LEDGER")
[[ "$(lines)" == 1 ]] && [[ "$(jq -cS . <<<"$line")" == "$line" ]] && ok "one ledger line, in canonical form" || fail "ledger: $line"
[[ "$(jq -r '[.work_order, .cost_usd, .turns, .exit, .model] | join(" ")' <<<"$line")" == "E0-98 1.25 7 0 sonnet" ]] &&
  ok "the ledger line has the work order, cost, turns, exit code and model" || fail "ledger fields: $line"

code=0; STUB_EXIT=3 run E0-98 >/dev/null || code=$?
[[ "$code" == 3 ]] && [[ "$(tail -n 1 "$HYPATIA_LEDGER" | jq -r .exit)" == 3 ]] && ok "a failed run keeps its exit code and still gets a ledger line" || fail "exit code $code"

before=$(wc -l < "$STUB_LOG")
for bad in E0-97 E0-99 '../E0-98' 'E0-98;id'; do
  if run "$bad" >/dev/null 2>&1; then fail "accepted $bad"; fi
done
[[ "$(wc -l < "$STUB_LOG")" == "$before" ]] && ok "an unknown id, a work order for another repo and an unsafe id are refused" || fail "a refused id reached claude"

echo change > "$repo/untracked.txt"
if run E0-98 >/dev/null 2>&1; then fail "ran with a dirty working tree"; fi
[[ "$(wc -l < "$STUB_LOG")" == "$before" ]] && ok "a dirty working tree is refused" || fail "dirty tree reached claude"

echo "run-work-order: $pass checks passed"
