#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# estate-absence-intake.sh — turn unresolvable repository absences into issues,
# exactly once each, without turning the tracker into a landfill.
#
# WHY THE CAPS ARE THE FEATURE
#
# The estate has 880 open issues. The complaint was not that findings are
# reported — it is that reporting had no ceiling and no deduplication, so the
# signal drowned. Rules:
#
#   * one issue per (repository, absence class), keyed by an in-body marker
#     `<!-- hypatia-absence:ABS-00N -->` — a second run comments, never re-files
#   * closed issues are NEVER reopened — a human closing it is a decision
#   * at most N new issues per repository per run (policy: intake_limits)
#   * a class that exceeds the roll-up threshold becomes ONE estate-wide
#     listing issue rather than N repository issues
#   * an absence hypatia can fix unambiguously is fixed, not filed; only the
#     ones that need a judgement call become issues
#
# DRY RUN by default; --execute files them.
#
# usage: estate-absence-intake.sh [--org ORG]... [--policy FILE] [--out DIR]
#                                 [--execute] [--max-new N]
set -euo pipefail

SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SELF_DIR/../.." && pwd)"

POLICY="$REPO_ROOT/.machine_readable/merge-orchestration/pr-automerge-policy.json"
OUT_DIR="${PWD}/.absence-intake"
ORGS=()
EXECUTE=0
MAX_NEW=""

while [ $# -gt 0 ]; do
  case "$1" in
    --org)      ORGS+=("$2"); shift 2 ;;
    --policy)   POLICY="$2"; shift 2 ;;
    --out)      OUT_DIR="$2"; shift 2 ;;
    --max-new)  MAX_NEW="$2"; shift 2 ;;
    --execute)  EXECUTE=1; shift ;;
    -h|--help)  sed -n '2,30p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "unknown argument: $1" >&2; exit 1 ;;
  esac
done

[ ${#ORGS[@]} -eq 0 ] && ORGS=("hyperpolymath" "metadatastician")
command -v gh >/dev/null || { echo "gh is required" >&2; exit 1; }
command -v jq >/dev/null || { echo "jq is required" >&2; exit 1; }

mkdir -p "$OUT_DIR"
PLAN="$OUT_DIR/absence-plan.jsonl"
: > "$PLAN"
log() { printf '%s\n' "$*" >&2; }

[ -n "$MAX_NEW" ] || MAX_NEW=$(jq -r '.intake_limits.max_new_issues_per_run' "$POLICY")
PER_REPO=$(jq -r '.intake_limits.max_new_issues_per_repo_per_run' "$POLICY")
ROLLUP=$(jq -r '.intake_limits.roll_up_when_class_exceeds' "$POLICY")
MIN_TOPICS=$(jq -r '.absence_rules[] | select(.id == "ABS-002") | .min' "$POLICY")
RUN_DATE=$(date -u +%Y-%m-%d)

list_repos() {
  local org="$1"
  gh api "orgs/${org}/repos?per_page=100" --paginate \
    --jq '.[] | select(.archived == false and .fork == false and .disabled == false)
          | {name:.name, description:(.description // ""), topics:(.topics // []),
             has_issues:(.has_issues // false)}' 2>/dev/null \
  || gh api "users/${org}/repos?per_page=100" --paginate \
    --jq '.[] | select(.archived == false and .fork == false and .disabled == false)
          | {name:.name, description:(.description // ""), topics:(.topics // []),
             has_issues:(.has_issues // false)}' 2>/dev/null
}

# ─── Enumerate candidates ────────────────────────────────────────────────
CAND="$OUT_DIR/candidates.jsonl"
: > "$CAND"

for org in "${ORGS[@]}"; do
  list_repos "$org" | jq -c --arg org "$org" '. + {org:$org}' >> "$CAND"
done

# ABS-001 · empty description · ABS-002 · fewer than MIN_TOPICS topics
jq -c --argjson min "$MIN_TOPICS" '
  select(.has_issues == true)
  | if (.description | length) == 0 then {rule:"ABS-001", repo:"\(.org)/\(.name)", detail:"description is empty"}
    elif (.topics | length) < $min then {rule:"ABS-002", repo:"\(.org)/\(.name)",
         detail:"\(.topics | length) topic(s): \(.topics | join(", "))"}
    else empty end
' "$CAND" > "$OUT_DIR/absences.jsonl"

abs1=$(jq -r 'select(.rule=="ABS-001") | .repo' "$OUT_DIR/absences.jsonl" | wc -l)
abs2=$(jq -r 'select(.rule=="ABS-002") | .repo' "$OUT_DIR/absences.jsonl" | wc -l)
log "candidates: ABS-001 (empty description) ${abs1} · ABS-002 (topics < ${MIN_TOPICS}) ${abs2}"

# A class over the roll-up threshold becomes ONE listing issue. 43 repositories
# with thin topics is a piece of estate context, not 43 bugs.
if [ "$abs2" -gt "$ROLLUP" ]; then
  jq -cn --argjson n "$abs2" --arg min "$MIN_TOPICS" --arg date "$RUN_DATE" \
    --argjson list "$(jq -sc '[.[] | select(.rule == "ABS-002") | .repo]' "$OUT_DIR/absences.jsonl")" \
    '{rule:"ABS-002", repo:"hyperpolymath/standards", kind:"rollup",
      detail:(($n|tostring) + " repositories have fewer than " + ($min|tostring) + " topics"),
      marker:"<!-- hypatia-absence:ABS-002 -->",
      title_key:"Estate listing:",
      title:"Estate listing: \($n) repositories are below the RSR 7-topic minimum",
      body:("The RSR standard requires at least " + ($min|tostring) + " topics per repository. " + ($n|tostring) + " active repositories are below it.\n\n" +
            "This is one listing rather than " + ($n|tostring) + " issues on purpose: which topics a project should carry is a judgement call, and a hundred parallel judgement calls are not actionable. Set topics per repository at leisure; close this when the count is zero or when the standard changes.\n\n" +
            "Repositories:\n\n```\n" + ($list | join("\n")) + "\n```\n\nFiled by `hypatia` ABS-002 on " + $date + ". Deduplicated by this marker: `<!-- hypatia-absence:ABS-002 -->`")}' \
    >> "$PLAN"
  log "ABS-002 rolled up into one listing issue (over the ${ROLLUP} threshold)"
else
  jq -c 'select(.rule == "ABS-002")' "$OUT_DIR/absences.jsonl" \
    | while IFS= read -r row; do
        repo=$(jq -r '.repo' <<<"$row"); detail=$(jq -r '.detail' <<<"$row")
        jq -cn --arg repo "$repo" --arg detail "$detail" --arg date "$RUN_DATE" \
          '{rule:"ABS-002", repo:$repo, kind:"per-repo", detail:$detail,
            marker:"<!-- hypatia-absence:ABS-002 -->",
            title_key:"Repository topics are below the RSR minimum of 7",
            title:"Repository topics are below the RSR minimum of 7",
            body:("This repository has " + $detail + ".\n\n**What would close this:** at least 7 topics covering language, ecosystem, status, licence, domain, maturity and one distinguishing topic.\n\nFiled by `hypatia` ABS-002 on " + $date + ". Deduplicated by this marker: `<!-- hypatia-absence:ABS-002 -->`")}' \
          >> "$PLAN"
      done
fi

jq -c 'select(.rule == "ABS-001")' "$OUT_DIR/absences.jsonl" \
  | while IFS= read -r row; do
      repo=$(jq -r '.repo' <<<"$row")
      jq -cn --arg repo "$repo" --arg date "$RUN_DATE" \
        '{rule:"ABS-001", repo:$repo, kind:"per-repo", detail:"description is empty",
          marker:"<!-- hypatia-absence:ABS-001 -->",
          title_key:"Repository description is empty",
          title:"Repository description is empty",
          body:("Repository metadata carries no description.\n\nThis is an absence `hypatia` cannot fix unambiguously: a description has to say what the project *is*, which is a claim only its maintainer can make. Inventing one would be worse than the gap.\n\n**What would close this:** a one-line description in repository settings.\n\nFiled by `hypatia` ABS-001 on " + $date + ". Deduplicated by this marker: `<!-- hypatia-absence:ABS-001 -->`")}' \
        >> "$PLAN"
    done

plan_count=$(grep -c . "$PLAN" 2>/dev/null || echo 0)

# ─── Dedup against what is already filed ─────────────────────────────────
: > "$OUT_DIR/to-file.jsonl"
to_file=0
while IFS= read -r row; do
  [ -n "$row" ] || continue
  repo=$(jq -r '.repo' <<<"$row"); marker=$(jq -r '.marker' <<<"$row")
  title_key=$(jq -r '.title_key // empty' <<<"$row")
  # Live issue list, matched locally. The search index lags — an issue filed
  # minutes ago can still be missing from it — and a raw HTML-comment marker
  # in a query string makes gh stall on the malformed URL. Open issues only:
  # a closed issue is a human decision and is never reopened.
  existing=$(timeout 60 gh issue list --repo "$repo" --state open --limit 400 \
      --json number,title,body 2>/dev/null \
    | jq -r --arg m "$marker" --arg t "$title_key" '
        [ .[] | select(((.body // "") | contains($m))
                       or ($t != "" and (.title | startswith($t)))) ]
        | if length > 0 then "\(.[0].number):open" else empty end' 2>/dev/null \
    | head -n1 || true)

  if [ -n "$existing" ]; then
    state="${existing##*:}"
    jq -c --arg existing "$existing" '. + {already: $existing}' <<<"$row" >> "$OUT_DIR/skipped.jsonl"
    log "  exists (${state}) ${repo} ${marker}"
  else
    jq -c --argjson cap "$PER_REPO" '. + {per_repo_cap: $cap}' <<<"$row" >> "$OUT_DIR/to-file.jsonl"
    to_file=$((to_file + 1))
  fi
done < "$PLAN"

log ""
log "─── absence intake ─────────────────────────────────────────────────"
log "  planned issues       : ${plan_count}"
log "  already filed        : $(grep -c . "$OUT_DIR/skipped.jsonl" 2>/dev/null || echo 0)"
log "  would file           : ${to_file} (caps: ${MAX_NEW}/run, ${PER_REPO}/repo/run)"
log "  plan                 : ${PLAN}"

if [ "$EXECUTE" -ne 1 ]; then
  log ""
  log "  dry run — nothing filed. Pass --execute (with a token that may write"
  log "  issues on the target repositories) to file them."
  exit 2
fi

filed=0
while IFS= read -r row; do
  [ -n "$row" ] || continue
  [ "$filed" -ge "$MAX_NEW" ] && { log "  per-run cap (${MAX_NEW}) reached"; break; }
  repo=$(jq -r '.repo' <<<"$row"); title=$(jq -r '.title' <<<"$row"); body=$(jq -r '.body' <<<"$row")
  timeout 60 gh issue create --repo "$repo" --title "$title" --body "$body" >/dev/null 2>&1 \
    && { filed=$((filed + 1)); log "  filed ${repo}: ${title}"; } \
    || log "  could not file ${repo}"
done < "$OUT_DIR/to-file.jsonl"

log "  filed: ${filed}"
exit 0
