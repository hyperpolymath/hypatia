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

# The launch script refuses a locale that is not UTF-8, so run under one.
for loc in C.UTF-8 en_US.UTF-8; do
  if [[ "$(LC_ALL=$loc locale charmap 2>/dev/null)" == UTF-8 ]]; then export LC_ALL=$loc; break; fi
done

here=$(cd "$(dirname "$0")/.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
pass=0
ok() { pass=$((pass + 1)); echo "ok   $1"; }
fail() { echo "FAIL $1" >&2; exit 1; }

# A throw-away checkout with the script, the profile, 1 agent, the skill and the fixture work orders.
repo="$work/repo"
mkdir -p "$repo/scripts/api-agents" "$repo/.claude/profiles" "$repo/.claude/agents" "$repo/.claude/skills/e0-work-order" "$repo/wo"
cp "$here/scripts/api-agents/run-work-order.sh" "$repo/scripts/api-agents/"
cp "$here/.claude/profiles/e0-headless.json" "$repo/.claude/profiles/"
printf -- '---\nname: e0-executor\nmodel: sonnet\neffort: medium\n---\nfixture\n' > "$repo/.claude/agents/e0-executor.md"
skill="$repo/.claude/skills/e0-work-order/SKILL.md"
echo fixture > "$skill"
order() { # order ID REPO [MODEL [BRANCH]]
  local id=$1 wo_repo=$2 model=${3:-sonnet} branch=${4:-fix/x}
  printf '= %s: fixture\n:wo-id: %s\n:wo-repo: %s\n:wo-size: small\n:wo-agent: e0-executor\n:wo-model: %s\n:wo-effort: medium\n:wo-cap-usd: 5\n:wo-max-turns: 120\n:wo-branch: %s\n:wo-depends: none\n' \
    "$id" "$id" "$wo_repo" "$model" "$branch" > "$repo/wo/$id-fixture.adoc"
}
order E0-98 hyperpolymath/hypatia
order E0-99 hyperpolymath/gitbot-fleet
order E0-97 hyperpolymath/hypatia opus # the agent definition says sonnet
order E0-95 hyperpolymath/hypatia
order E0-93 hyperpolymath/hypatia sonnet --mirror     # an option of git push, not a branch
order E0-92 hyperpolymath/hypatia sonnet +HEAD:topic  # a refspec that forces
sed -i "s/^:wo-id: E0-95$/:wo-id: E0-94/" "$repo/wo/E0-95-fixture.adoc" # the header names another id
url=https://example.invalid/hyperpolymath/hypatia
git -C "$repo" init -q
git -C "$repo" remote add origin "$url"
git -C "$repo" add -A
git -C "$repo" -c user.name=test -c user.email=test@example.invalid -c commit.gpgsign=false commit -q -m fixture

# A stub claude: it records its arguments and the effort variable, and prints a result.
mkdir -p "$work/bin"
cat > "$work/bin/claude" <<'STUB'
#!/bin/sh
printf '%s effort_env=%s\n' "$*" "${CLAUDE_CODE_EFFORT_LEVEL:-unset}" >> "$STUB_LOG"
[ -n "${STUB_EMPTY:-}" ] ||
  printf '{"total_cost_usd":1.25,"num_turns":7,"duration_ms":1000,"subtype":"success","session_id":"test-session","result":"done"}\n'
exit "${STUB_EXIT:-0}"
STUB
chmod +x "$work/bin/claude"

export STUB_LOG="$work/args.log" HYPATIA_LEDGER="$work/ledger.jsonl" WO_DIR="$repo/wo"
export PATH="$work/bin:$PATH"
export CLAUDE_CODE_EFFORT_LEVEL=max # the launch script must not pass this on
# The test sets the routing variables itself. A Claude Code cloud session sets
# ANTHROPIC_BASE_URL, and that must not change the result.
unset CLAUDE_CODE_USE_BEDROCK CLAUDE_CODE_USE_VERTEX CLAUDE_CODE_USE_FOUNDRY CLAUDE_CODE_USE_MANTLE \
  CLAUDE_CODE_USE_ANTHROPIC_AWS CLAUDE_CODE_USE_ANTHROPIC_GOOGLE_CLOUD ANTHROPIC_AUTH_TOKEN ANTHROPIC_BASE_URL
: > "$STUB_LOG"
run() { (cd "$repo" && sh scripts/api-agents/run-work-order.sh "$@"); }
lines() { if [[ -f "$HYPATIA_LEDGER" ]]; then wc -l < "$HYPATIA_LEDGER"; else echo 0; fi; }
calls() { wc -l < "$STUB_LOG"; }
want="--agent e0-executor --model sonnet --effort medium --max-budget-usd 5 --max-turns 120 --permission-mode dontAsk --setting-sources project"

out=$(run E0-98 --dry-run)
[[ "$out" == *"$want"* ]] && [[ ! -s "$STUB_LOG" ]] && ok "dry run prints the command and calls nothing" || fail "dry run: $out"

if (unset ANTHROPIC_API_KEY; run E0-98) 2>/dev/null; then fail "ran without an API key"; fi
[[ ! -s "$STUB_LOG" ]] && [[ "$(lines)" == 0 ]] && ok "no API key: refused, nothing called, no ledger line" || fail "no API key"

export ANTHROPIC_API_KEY=test-not-a-real-key
run E0-98 >/dev/null
[[ "$(cat "$STUB_LOG")" == *"$want"* ]] && ok "the run passes the model, effort, cap and turns, and reads project settings only" || fail "arguments: $(cat "$STUB_LOG")"
[[ "$(cat "$STUB_LOG")" == *"effort_env=unset"* ]] && ok "an effort variable of the caller does not reach the run" || fail "effort variable: $(cat "$STUB_LOG")"
[[ "$(cat "$STUB_LOG")" == *"--allowedTools Bash(git push -u origin fix/x) --output-format json"* ]] &&
  ok "the one push rule of the run names the branch of the work order" || fail "push rule: $(cat "$STUB_LOG")"
line=$(tail -n 1 "$HYPATIA_LEDGER")
[[ "$(lines)" == 1 ]] && [[ "$(jq -cS . <<<"$line")" == "$line" ]] && ok "one ledger line, in canonical form" || fail "ledger: $line"
[[ "$(jq -r '[.work_order, .cost_usd, .turns, .exit, .model] | join(" ")' <<<"$line")" == "E0-98 1.25 7 0 sonnet" ]] &&
  ok "the ledger line has the work order, cost, turns, exit code and model" || fail "ledger fields: $line"

code=0; STUB_EXIT=3 run E0-98 >/dev/null || code=$?
[[ "$code" == 3 ]] && [[ "$(tail -n 1 "$HYPATIA_LEDGER" | jq -r .exit)" == 3 ]] && ok "a failed run keeps its exit code and still gets a ledger line" || fail "exit code $code"

code=0; STUB_EMPTY=1 STUB_EXIT=7 run E0-98 >/dev/null || code=$?
line=$(tail -n 1 "$HYPATIA_LEDGER")
[[ "$code" == 7 ]] && [[ "$(lines)" == 3 ]] && [[ "$(jq -cS . <<<"$line")" == "$line" ]] &&
  [[ "$(jq -r '[.exit, .stop] | join(" ")' <<<"$line")" == "7 no_result" ]] &&
  ok "a run with no result still gets a canonical ledger line" || fail "empty output: code $code, line $line"

before=$(calls)
for bad in E0-96 E0-99 E0-97 E0-95 E0-93 E0-92 '../E0-98' 'E0-98;id'; do
  if run "$bad" >/dev/null 2>&1; then fail "accepted $bad"; fi
done
[[ "$(calls)" == "$before" ]] && ok "an unknown id, another repo, a model that the agent does not have, a header with another id, a branch that is an option or a refspec, and an unsafe id are refused" || fail "a refused id reached claude"

if run E0-98 --dryrun >/dev/null 2>&1; then fail "accepted a mistyped option"; fi
if run E0-98 --dry-run extra >/dev/null 2>&1; then fail "accepted an extra argument"; fi
[[ "$(calls)" == "$before" ]] && ok "a mistyped option and an extra argument are refused, and nothing runs" || fail "a bad option reached claude"

git -C "$repo" remote set-url origin "$url-other"
if run E0-98 >/dev/null 2>&1; then fail "accepted a repository whose name only starts with the right one"; fi
git -C "$repo" remote set-url origin git@example.invalid:hyperpolymath/hypatia.git
out=$(run E0-98 --dry-run)
git -C "$repo" remote set-url origin "$url"
[[ "$(calls)" == "$before" ]] && [[ "$out" == *"$want"* ]] && ok "the repository must match exactly, in the https and the ssh form" || fail "repository match"

echo change > "$repo/untracked.txt"
if run E0-98 >/dev/null 2>&1; then fail "ran with a dirty working tree"; fi
[[ "$(calls)" == "$before" ]] && ok "a dirty working tree is refused" || fail "dirty tree reached claude"
rm "$repo/untracked.txt"

if LC_ALL=C run E0-98 >/dev/null 2>&1; then fail "ran with a locale that is not UTF-8"; fi
[[ "$(calls)" == "$before" ]] && ok "a locale that is not UTF-8 is refused" || fail "a non-UTF-8 locale reached claude"

if CLAUDE_CODE_USE_BEDROCK=1 run E0-98 >/dev/null 2>&1; then fail "ran with a provider variable"; fi
if ANTHROPIC_BASE_URL=https://example.invalid run E0-98 >/dev/null 2>&1; then fail "ran with another endpoint"; fi
[[ "$(calls)" == "$before" ]] && ok "a provider variable and another endpoint are refused" || fail "a routed run reached claude"

mv "$skill" "$skill.off"
if run E0-98 --dry-run >/dev/null 2>&1; then fail "accepted a checkout without the skill"; fi
mv "$skill.off" "$skill"
ok "a checkout without the e0-work-order skill is refused"

echo "run-work-order: $pass checks passed"
