#!/bin/sh
# SPDX-License-Identifier: MPL-2.0
#
# run-work-order.sh: run one work order as a headless Claude Code session that
# is billed to the API key, with the model, the effort and the cost cap that
# the work order names, and write one line to the cost ledger.
#
# Usage: scripts/api-agents/run-work-order.sh ID [--dry-run]
#   ID         a work-order id, for example E0-06
#   --dry-run  print the command and stop
# Environment:
#   ANTHROPIC_API_KEY  required: without it Claude Code bills the signed-in plan
#   WO_DIR             work-order folder (default: docs/operations/e0-closeout/work-orders)
#   HYPATIA_LEDGER     ledger file (default: $XDG_STATE_HOME/hypatia/api-ledger.jsonl)
#
# The cap is a client estimate and the spend can pass it. The hard stops are
# the workspace spend limit and the prepaid credit in the Console.
#
# The run reads the project settings and the headless profile only
# (--setting-sources project), so a personal allow rule of the caller does not
# widen what the unattended run can do.

set -eu

die() {
  msg=$1
  printf 'run-work-order: %s\n' "$msg" >&2
  exit 1
}

id=${1:-}
dry=${2:-}
# A mistyped --dry-run must not start a paid run, so anything else stops here.
case "$#:$dry" in
  1:) ;;
  2:--dry-run) ;;
  *) die "usage: run-work-order.sh ID [--dry-run]" ;;
esac
[ -n "$id" ] || die "usage: run-work-order.sh ID [--dry-run]"
case "$id" in
  *[!A-Za-z0-9-]*) die "the id has a character that is not a letter, a digit or a hyphen: $id" ;;
  *) ;;
esac

repo=$(git rev-parse --show-toplevel 2>/dev/null) || die "run this inside a git checkout"
dir=${WO_DIR:-$repo/docs/operations/e0-closeout/work-orders}
ledger=${HYPATIA_LEDGER:-${XDG_STATE_HOME:-$HOME/.local/state}/hypatia/api-ledger.jsonl}

file=""
for f in "$dir/$id"-*.adoc; do
  [ -f "$f" ] || continue
  [ -z "$file" ] || die "more than 1 work order matches $id"
  file=$f
done
[ -n "$file" ] || die "no work order $id in $dir"

# attr NAME: the value of the `:wo-NAME:` header line of the work order.
attr() {
  name=$1
  awk -v k=":wo-$name:" 'index($0, k) == 1 { sub(/^[^ ]+ +/, ""); print; exit }' "$file"
}
# The id in the header must be the id that the caller asked for. A file under
# the wrong name would run one work order and record another.
[ "$(attr id)" = "$id" ] || die "$file declares the id $(attr id), not $id"
agent=$(attr agent); model=$(attr model); effort=$(attr effort)
cap=$(attr cap-usd); turns=$(attr max-turns); wo_repo=$(attr repo)
for v in "$agent" "$model" "$effort" "$cap" "$turns" "$wo_repo"; do
  [ -n "$v" ] || die "$file lacks one of the header lines agent, model, effort, cap-usd, max-turns, repo"
done
case "$cap$turns" in
  *[!0-9]*) die "cap-usd and max-turns must be whole numbers in $file" ;;
  *) ;;
esac
case "$agent" in
  *[!A-Za-z0-9-]*) die "the agent name has a character that is not a letter, a digit or a hyphen: $agent" ;;
  *) ;;
esac

# The remote must be exactly the repository of the work order. A name that
# only starts with it (owner/name-other) is a different repository.
origin=$(git -C "$repo" remote get-url origin 2>/dev/null || true)
case "${origin%.git}" in
  */"$wo_repo" | *:"$wo_repo") ;;
  *) die "work order $id is for $wo_repo, and this checkout is $origin" ;;
esac

# The agent definition can set a model and an effort of its own. They must
# equal the work order, or the ledger would record values that did not run.
agent_file="$repo/.claude/agents/$agent.md"
[ -f "$agent_file" ] || die "missing $agent_file"
front() {
  key=$1
  awk -v k="$key:" 'NR == 1 && $0 != "---" { exit } NR > 1 && $0 == "---" { exit } $1 == k { print $2; exit }' "$agent_file"
}
a_model=$(front model); a_effort=$(front effort)
if [ -n "$a_model" ] && [ "$a_model" != "$model" ]; then
  die "work order $id names the model $model, and the agent $agent is defined with $a_model"
fi
if [ -n "$a_effort" ] && [ "$a_effort" != "$effort" ]; then
  die "work order $id names the effort $effort, and the agent $agent is defined with $a_effort"
fi

prompt="Run work order $id with the e0-work-order skill. The work order is ${file#"$repo"/}. Stop at its cost cap of $cap US dollars."
profile="$repo/.claude/profiles/e0-headless.json"
[ -f "$profile" ] || die "missing $profile"

if [ "$dry" = "--dry-run" ]; then
  printf 'claude -p "%s" --agent %s --model %s --effort %s --max-budget-usd %s --max-turns %s --permission-mode dontAsk --setting-sources project --settings %s --output-format json\n' \
    "$prompt" "$agent" "$model" "$effort" "$cap" "$turns" "$profile"
  exit 0
fi

[ -n "${ANTHROPIC_API_KEY:-}" ] || die "ANTHROPIC_API_KEY is not set: the run would bill the signed-in plan, not the API credit"
# Measured on 2026-11-14: 1 test of the suite fails when the locale is not
# UTF-8, so a run would stop on a red baseline after it spent credit.
charmap=$(locale charmap 2>/dev/null) || charmap=""
case "$charmap" in
  UTF-8 | "") ;;
  *) die "the locale is $charmap, not UTF-8: 1 test of the suite fails then. Set LANG to a UTF-8 locale, for example C.UTF-8" ;;
esac
[ -z "$(git -C "$repo" status --porcelain)" ] || die "the working tree has changes: commit or stash them first"
command -v claude >/dev/null 2>&1 || die "the claude command is not on PATH"
command -v jq >/dev/null 2>&1 || die "jq is not on PATH"

# This variable takes precedence over --effort, so the run would not use the
# effort of the work order.
unset CLAUDE_CODE_EFFORT_LEVEL

out=$(mktemp)
trap 'rm -f "$out"' EXIT
status=0
(cd "$repo" && claude -p "$prompt" --agent "$agent" --model "$model" --effort "$effort" \
  --max-budget-usd "$cap" --max-turns "$turns" --permission-mode dontAsk \
  --setting-sources project --settings "$profile" --output-format json) > "$out" || status=$?

ended=$(date -u +%Y-%m-%dT%H:%M:%SZ)
line=""
if [ -s "$out" ]; then
  line=$(jq -cS --arg wo "$id" --arg model "$model" --arg effort "$effort" --argjson cap "$cap" \
    --argjson exit "$status" --arg ended "$ended" \
    '{work_order: $wo, model: $model, effort: $effort, cap_usd: $cap, exit: $exit, ended: $ended,
      cost_usd: (.total_cost_usd // null), turns: (.num_turns // null),
      duration_ms: (.duration_ms // null), stop: (.subtype // null),
      session_id: (.session_id // null)}' "$out" 2>/dev/null) || line=""
fi
# No result object (the start, the sign-in or the network failed): the attempt
# still gets its line, so that the ledger has each run.
if [ -z "$line" ]; then
  line=$(jq -cnS --arg wo "$id" --arg model "$model" --arg effort "$effort" --argjson cap "$cap" \
    --argjson exit "$status" --arg ended "$ended" \
    '{work_order: $wo, model: $model, effort: $effort, cap_usd: $cap, exit: $exit, ended: $ended,
      cost_usd: null, turns: null, duration_ms: null, stop: "no_result", session_id: null}')
fi
mkdir -p "$(dirname "$ledger")"
printf '%s\n' "$line" >> "$ledger"

echo "work order $id: exit $status, ledger line:"
printf '%s\n' "$line"
jq -r '.result // empty' "$out" 2>/dev/null || true
exit "$status"
