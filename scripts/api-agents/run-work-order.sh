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

set -eu

die() {
  msg=$1
  printf 'run-work-order: %s\n' "$msg" >&2
  exit 1
}

id=${1:-}
dry=${2:-}
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
agent=$(attr agent); model=$(attr model); effort=$(attr effort)
cap=$(attr cap-usd); turns=$(attr max-turns); wo_repo=$(attr repo)
for v in "$agent" "$model" "$effort" "$cap" "$turns" "$wo_repo"; do
  [ -n "$v" ] || die "$file lacks one of the header lines agent, model, effort, cap-usd, max-turns, repo"
done
case "$cap$turns" in
  *[!0-9]*) die "cap-usd and max-turns must be whole numbers in $file" ;;
  *) ;;
esac

origin=$(git -C "$repo" remote get-url origin 2>/dev/null || true)
case "$origin" in
  *"$wo_repo"*) ;;
  *) die "work order $id is for $wo_repo, and this checkout is $origin" ;;
esac

prompt="Run work order $id with the e0-work-order skill. The work order is ${file#"$repo"/}. Stop at its cost cap of $cap US dollars."
profile="$repo/.claude/profiles/e0-headless.json"
[ -f "$profile" ] || die "missing $profile"

if [ "$dry" = "--dry-run" ]; then
  printf 'claude -p "%s" --agent %s --model %s --effort %s --max-budget-usd %s --max-turns %s --permission-mode dontAsk --settings %s --output-format json\n' \
    "$prompt" "$agent" "$model" "$effort" "$cap" "$turns" "$profile"
  exit 0
fi

[ -n "${ANTHROPIC_API_KEY:-}" ] || die "ANTHROPIC_API_KEY is not set: the run would bill the signed-in plan, not the API credit"
[ -z "$(git -C "$repo" status --porcelain)" ] || die "the working tree has changes: commit or stash them first"
command -v claude >/dev/null 2>&1 || die "the claude command is not on PATH"
command -v jq >/dev/null 2>&1 || die "jq is not on PATH"

out=$(mktemp)
trap 'rm -f "$out"' EXIT
status=0
(cd "$repo" && claude -p "$prompt" --agent "$agent" --model "$model" --effort "$effort" \
  --max-budget-usd "$cap" --max-turns "$turns" --permission-mode dontAsk \
  --settings "$profile" --output-format json) > "$out" || status=$?

mkdir -p "$(dirname "$ledger")"
jq -cS --arg wo "$id" --arg model "$model" --arg effort "$effort" --argjson cap "$cap" \
  --argjson exit "$status" --arg ended "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
  '{work_order: $wo, model: $model, effort: $effort, cap_usd: $cap, exit: $exit, ended: $ended,
    cost_usd: (.total_cost_usd // null), turns: (.num_turns // null),
    duration_ms: (.duration_ms // null), stop: (.subtype // null),
    session_id: (.session_id // null)}' "$out" >> "$ledger" 2>/dev/null ||
  printf '{"cap_usd":%s,"ended":"%s","exit":%s,"work_order":"%s"}\n' \
    "$cap" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$status" "$id" >> "$ledger"

echo "work order $id: exit $status, ledger line:"
tail -n 1 "$ledger"
jq -r '.result // empty' "$out" 2>/dev/null || true
exit "$status"
