#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Hypatia M0-D: bounded, GET-only PR evidence collection; never dispatch/merge.
set -euo pipefail

usage() {
  cat <<'HELP'
Usage: estate-pr-enrich-readonly.sh --inventory-dir DIR --out-dir DIR
       [--limit N] [--repo OWNER/REPO] [--number N]

Reads a completed M0 inventory and makes GET-only calls for PR metadata,
check-runs (all pages), review submissions (all pages), and combined commit
status. Default limit is 10; --limit 0 selects all matching PRs. No required-
check set or merge authorization is inferred, even when checks are green.
Each output directory must be unused. Partial/unavailable observations are
explicit in the output and never counted as verified clean.
HELP
}
IN_DIR=''
OUT_DIR=''
LIMIT=10
REPO=''
NUMBER=0
while (($#)); do
  case "$1" in
    --inventory-dir) (($# >= 2)) || exit 2; IN_DIR=$2; shift 2 ;;
    --out-dir) (($# >= 2)) || exit 2; OUT_DIR=$2; shift 2 ;;
    --limit) (($# >= 2)) || exit 2; LIMIT=$2; shift 2 ;;
    --repo) (($# >= 2)) || exit 2; REPO=$2; shift 2 ;;
    --number) (($# >= 2)) || exit 2; NUMBER=$2; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) usage >&2; exit 2 ;;
  esac
done
[[ -n "$IN_DIR" && -n "$OUT_DIR" ]] || { usage >&2; exit 2; }
[[ "$LIMIT" =~ ^(0|[1-9][0-9]*)$ && "$NUMBER" =~ ^(0|[1-9][0-9]*)$ ]] || exit 2
if [[ -n "$REPO" ]]; then
  [[ "$REPO" =~ ^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$ ]] || exit 2
else
  ((NUMBER == 0)) || { echo '--number requires --repo' >&2; exit 2; }
fi
[[ -s "$IN_DIR/pr-census.json" && -f "$IN_DIR/pr-inventory.jsonl" ]] || { echo 'missing input census' >&2; exit 2; }
command -v jq >/dev/null || exit 2
command -v gh >/dev/null || exit 2
mkdir -p -- "$OUT_DIR"
for f in pr-enrichment.jsonl pr-enrichment-summary.json; do
  [[ ! -e "$OUT_DIR/$f" ]] || { echo 'output directory contains prior enrichment' >&2; exit 2; }
done
TMP=$(mktemp -d)
trap 'rm -rf -- "$TMP"' EXIT
: > "$TMP/enriched.jsonl"

# Explicitly refuse a partial or tampered inventory; do not infer source integrity
# from the presence of a summary file.
if ! jq -e --slurpfile inv "$IN_DIR/pr-inventory.jsonl" '
  .status == "complete_first_pass" and (.total_open | type == "number") and
  (.total_open == ($inv|length)) and
  (($inv|map(.repository + "#" + (.number|tostring))|unique|length) == ($inv|length)) and
  ($inv|all(.[]; ((.repository|type)=="string") and ((.repository|test("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$")) and ((.repository|contains(".."))|not)) and ((.number|type)=="number") and (.number>0) and (.number==(.number|floor))))
' "$IN_DIR/pr-census.json" >/dev/null; then
  echo 'incomplete or inconsistent input census' >&2
  exit 2
fi

jq -c -s --arg repo "$REPO" --argjson number "$NUMBER" --argjson limit "$LIMIT" '
  [ .[] | select(($repo == "" or .repository == $repo) and
                ($number == 0 or .number == $number)) ] |
  (if $limit == 0 then . else .[:$limit] end)[]
' "$IN_DIR/pr-inventory.jsonl" > "$TMP/selected.jsonl"

# No mutation is permitted anywhere in this program. The sole API primitive is GET.
api() { GH_PROMPT_DISABLED=1 gh api --method GET "$1"; }

# Produce a structured unavailable row without discarding source identity.
missing() {
  jq -nc --argjson source "$1" --arg reason "$2" '{
    source:$source,
    observation:"unavailable",
    reason:$reason,
    current_head_sha:null,
    stable_head:false,
    check_runs:{state:"unmeasured",total:null,summary:null},
    reviews:{state:"unmeasured",total:null,by_state:null},
    combined_status:{state:"unmeasured",value:null},
    required_checks:{state:"unmeasured"},
    merge_eligibility:{state:"unmeasured"}
  }'
}

# GET each page, enforce stable page-level totals, a bounded 1000-entry ceiling,
# complete retrieval and no repeated source event IDs. No silently lost pages.
collect_checks() {
  local repo=$1 sha=$2 page=1 expected=-1 n total
  : > "$TMP/checks.jsonl"
  while :; do
    api "repos/$repo/commits/$sha/check-runs?per_page=100&page=$page" > "$TMP/checks-page.json" || return 1
    jq -e '(.total_count|type)=="number" and (.total_count>=0) and
           (.check_runs|type)=="array" and
           (.check_runs|all(.[]; (.id|type)=="number" and (.name|type)=="string" and (.status|type)=="string"))' \
      "$TMP/checks-page.json" >/dev/null || return 1
    total=$(jq -r '.total_count' "$TMP/checks-page.json")
    n=$(jq '.check_runs|length' "$TMP/checks-page.json")
    ((total <= 1000)) || return 1
    if ((expected < 0)); then expected=$total; fi
    ((total == expected)) || return 1
    jq -c '.check_runs[]|{id,name,status,conclusion:(.conclusion//null),url:(.html_url//null)}' \
      "$TMP/checks-page.json" >> "$TMP/checks.jsonl" || return 1
    ((n <= 100)) || return 1
    ((n == 100)) || break
    page=$((page + 1))
    ((page <= 11)) || return 1
  done
  jq -e -s --argjson expected "$expected" '
    length == $expected and (map(.id)|unique|length) == length
  ' "$TMP/checks.jsonl" >/dev/null || return 1
  return 0
}

collect_reviews() {
  local repo=$1 number=$2 page=1 n
  : > "$TMP/reviews.jsonl"
  while :; do
    api "repos/$repo/pulls/$number/reviews?per_page=100&page=$page" > "$TMP/reviews-page.json" || return 1
    jq -e 'type=="array" and all(.[]; (.id|type)=="number" and (.state|type)=="string" and (.user.login|type)=="string")' \
      "$TMP/reviews-page.json" >/dev/null || return 1
    n=$(jq 'length' "$TMP/reviews-page.json")
    jq -c '.[]|{id,state,reviewer:.user.login,commit_id:(.commit_id//null),submitted_at:(.submitted_at//null)}' \
      "$TMP/reviews-page.json" >> "$TMP/reviews.jsonl" || return 1
    ((n <= 100)) || return 1
    ((n == 100)) || break
    page=$((page + 1))
    ((page <= 11)) || return 1
  done
  jq -e -s '(map(.id)|unique|length) == length and length <= 1000' "$TMP/reviews.jsonl" >/dev/null || return 1
  return 0
}

while IFS= read -r source; do
  [[ -n "$source" ]] || continue
  repo=$(jq -r '.repository' <<< "$source")
  number=$(jq -r '.number' <<< "$source")
  if ! api "repos/$repo/pulls/$number" > "$TMP/pr-before.json"; then
    missing "$source" 'pr_metadata_get_failed' >> "$TMP/enriched.jsonl"
    continue
  fi
  if ! jq -e --arg repo "$repo" --argjson number "$number" '
    .number == $number and .base.repo.full_name == $repo and .state == "open" and
    (.head.sha|type)=="string" and (.head.sha|test("^[0-9a-fA-F]{40}$")) and
    (.base.ref|type)=="string"
  ' "$TMP/pr-before.json" >/dev/null; then
    missing "$source" 'pr_closed_or_malformed_or_unavailable' >> "$TMP/enriched.jsonl"
    continue
  fi
  sha=$(jq -r '.head.sha|ascii_downcase' "$TMP/pr-before.json")
  base=$(jq -r '.base.ref' "$TMP/pr-before.json")
  draft=$(jq -r 'if (.draft|type)=="boolean" then .draft else "null" end' "$TMP/pr-before.json")

  checks_state=observed
  if ! collect_checks "$repo" "$sha"; then checks_state=unavailable; fi
  reviews_state=observed
  if ! collect_reviews "$repo" "$number"; then reviews_state=unavailable; fi
  status_state=observed
  if ! api "repos/$repo/commits/$sha/status" > "$TMP/combined-status.json" ||
     ! jq -e '(.state|type)=="string" and (.state=="success" or .state=="failure" or .state=="pending" or .state=="error")' "$TMP/combined-status.json" >/dev/null; then
    status_state=unavailable
  fi
  stable=false
  if api "repos/$repo/pulls/$number" > "$TMP/pr-after.json" &&
     jq -e --arg repo "$repo" --argjson number "$number" --arg sha "$sha" '
       .state == "open" and .number == $number and .base.repo.full_name == $repo and
       (.head.sha|ascii_downcase) == $sha
     ' "$TMP/pr-after.json" >/dev/null; then stable=true; fi

  # All three sub-sources are non-authorising, even when observed and green.
  jq -nc --argjson source "$source" --arg sha "$sha" --arg base "$base" --argjson draft "$draft" \
    --arg checks_state "$checks_state" --arg reviews_state "$reviews_state" \
    --arg status_state "$status_state" --argjson stable "$stable" \
    --slurpfile checks "$TMP/checks.jsonl" --slurpfile reviews "$TMP/reviews.jsonl" \
    --slurpfile commit_status "$TMP/combined-status.json" '
    {
      source:$source,
      observation:(if ($stable|not) then "head_changed_or_unverified"
                   elif $checks_state!="observed" or $reviews_state!="observed" or $status_state!="observed" then "partial"
                   else "head_stable" end),
      current_head_sha:$sha,
      base_ref:$base,
      draft:$draft,
      stable_head:$stable,
      check_runs:(if $checks_state == "observed" then {
        state:"observed",total:($checks|length),
        summary:($checks|group_by([.status,.conclusion])|map({key:(.[0].status+"/"+(.[0].conclusion//"none")),value:length})|from_entries),
        failures:[$checks[]|select(.conclusion=="failure" or .conclusion=="startup_failure" or .conclusion=="timed_out" or .conclusion=="action_required")|{name,url}]
      } else {state:"unavailable",total:null,summary:null,failures:null} end),
      reviews:(if $reviews_state == "observed" then {
        state:"observed", total:($reviews|length),
        by_state:($reviews|group_by(.state)|map({key:.[0].state,value:length})|from_entries),
        requested_changes_observed:($reviews|any(.state=="CHANGES_REQUESTED"))
      } else {state:"unavailable",total:null,by_state:null} end),
      combined_status:(if $status_state=="observed" then {state:"observed",value:$commit_status[0].state}
                       else {state:"unavailable",value:null} end),
      required_checks:{state:"unmeasured"},
      merge_eligibility:{state:"unmeasured"},
      provenance:{check_runs:"github-rest-check-runs",reviews:"github-rest-pr-reviews",combined_status:"github-rest-commit-status",snapshot:"two-pr-head-reads;non-atomic"}
    }
  ' >> "$TMP/enriched.jsonl"
done < "$TMP/selected.jsonl"

selected=$(wc -l < "$TMP/selected.jsonl" | tr -d ' ')
if ! jq -e -s --argjson selected "$selected" 'length == $selected' "$TMP/enriched.jsonl" >/dev/null; then
  echo 'inconsistent output row count' >&2; exit 1
fi
jq -n --arg generated "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
  --argjson input_total "$(jq '.total_open' "$IN_DIR/pr-census.json")" \
  --argjson selected "$selected" --arg repo "$REPO" --argjson number "$NUMBER" \
  --slurpfile rows "$TMP/enriched.jsonl" '
  {
    schema_version:"m0-pr-enrichment-1", generated_at:$generated,
    source:"GitHub REST GET; non-atomic bounded observations",
    input_total_open:$input_total,selected:$selected,
    selector:{repository:(if $repo=="" then null else $repo end),number:(if $number==0 then null else $number end)},
    head_stable:($rows|map(select(.observation=="head_stable"))|length),
    head_unverified:($rows|map(select(.observation=="head_changed_or_unverified"))|length),
    partial:($rows|map(select(.observation=="partial"))|length),
    unavailable:($rows|map(select(.observation=="unavailable"))|length),
    observed_check_failures:($rows|map(.check_runs.failures//[])|add|length),
    status:(if ($rows|all(.[];.observation=="head_stable")) and ($selected==$input_total)
            then "all_input_rows_observed" else "bounded_or_incomplete" end),
    warning:"No required-check qualification, reviewer success attestation, policy authorization, or merge readiness is inferred."
  }
' > "$TMP/pr-enrichment-summary.json"
mv -- "$TMP/enriched.jsonl" "$OUT_DIR/pr-enrichment.jsonl"
mv -- "$TMP/pr-enrichment-summary.json" "$OUT_DIR/pr-enrichment-summary.json"
echo "Enrichment recorded: $selected selected PRs (no GitHub writes)" >&2
