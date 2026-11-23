#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Read-only GitHub PR backlog census. No repository modifications, PR comments,
# review requests, dispatches, or repairs. Requires authenticated gh and jq.
set -euo pipefail

usage() {
  cat <<'HELP'
Usage: estate-pr-census-readonly.sh --out-dir DIRECTORY [--org OWNER ...]

Queries open PRs using GitHub's REST search API (GET only). Defaults to the
four Hyperpolymath estate owners. Writes pr-inventory.jsonl and pr-census.json
locally only after a complete, duplicate-free, non-truncated fetch. Individual
PR head SHAs, reviews, and required-check verdicts are NOT measured by this
first-pass census and are explicitly marked unknown.
HELP
}

OUT_DIR=''
ORGS=()
while (($#)); do
  case "$1" in
    --out-dir) (($# >= 2)) || { echo 'missing --out-dir value' >&2; exit 2; }; OUT_DIR=$2; shift 2 ;;
    --org) (($# >= 2)) || { echo 'missing --org value' >&2; exit 2; }; ORGS+=("$2"); shift 2 ;;
    --help|-h) usage; exit 0 ;;
    *) echo "unknown option: $1" >&2; usage >&2; exit 2 ;;
  esac
done
[[ -n $OUT_DIR ]] || { echo '--out-dir is required' >&2; exit 2; }
if ((${#ORGS[@]} == 0)); then
  ORGS=(hyperpolymath catachresis-colloquium metadatastician stuff-of-awe)
fi
for org in "${ORGS[@]}"; do
  [[ $org =~ ^[A-Za-z0-9][A-Za-z0-9-]*$ ]] || { echo "invalid owner: $org" >&2; exit 2; }
done
command -v gh >/dev/null || { echo 'gh CLI is required' >&2; exit 2; }
command -v jq >/dev/null || { echo 'jq is required' >&2; exit 2; }

mkdir -p -- "$OUT_DIR"
TMP=$(mktemp -d)
trap 'rm -rf -- "$TMP"' EXIT
: > "$TMP/pr-inventory.jsonl"
: > "$TMP/pages.jsonl"

# A query is complete only if every page agrees on total_count, no search is
# incomplete, and the number of distinct identities equals the declared total.
for org in "${ORGS[@]}"; do
  page=1
  expected=-1
  seen=0
  query=$(jq -nr --arg q "is:pr is:open org:$org" '$q|@uri')
  while :; do
    endpoint="search/issues?q=${query}&per_page=100&page=${page}"
    # gh api defaults to GET when no fields or body is supplied; be explicit.
    if ! GH_PROMPT_DISABLED=1 gh api --method GET "$endpoint" > "$TMP/response.json"; then
      echo "GitHub request failed: $org page $page" >&2
      exit 1
    fi
    if ! jq -e '
      (.total_count | type == "number") and
      (.incomplete_results == false) and
      (.items | type == "array") and
      (.items | all(.[];
        (.number | type == "number") and
        (.repository_url | type == "string") and
        (.html_url | type == "string") and
        (.user.login | type == "string") and
        (.updated_at | type == "string") and
        (.created_at | type == "string")
      ))
    ' "$TMP/response.json" >/dev/null; then
      echo "incomplete or invalid GitHub page: $org page $page" >&2
      exit 1
    fi
    total=$(jq -r '.total_count' "$TMP/response.json")
    count=$(jq -r '.items | length' "$TMP/response.json")
    if ((total > 1000)); then
      echo "GitHub search cap reached: $org $total PRs. Split query by date." >&2
      exit 1
    fi
    if ((expected < 0)); then expected=$total; fi
    if ((total != expected)); then
      echo "inventory changed mid-pagination: $org $expected -> $total" >&2
      exit 1
    fi
    jq -c --arg org "$org" '.items[] | {
      owner: $org,
      repository: (.repository_url | sub("^https://api.github.com/repos/"; "")),
      number: .number,
      url: .html_url,
      title: .title,
      author: .user.login,
      draft: (if (.draft | type) == "boolean" then .draft else null end),
      draft_state: (if (.draft | type) == "boolean" then "observed" else "unmeasured" end),
      created_at: .created_at,
      updated_at: .updated_at,
      labels: [.labels[]?.name],
      head_sha: null,
      head_sha_state: "unmeasured",
      required_checks_state: "unmeasured",
      review_state: "unmeasured",
      merge_conflict_state: "unmeasured"
    }' "$TMP/response.json" >> "$TMP/pr-inventory.jsonl"
    jq -nc --arg org "$org" --argjson page "$page" --argjson count "$count" --argjson expected "$expected" \
      '{owner:$org,page:$page,count:$count,expected:$expected}' >> "$TMP/pages.jsonl"
    seen=$((seen + count))
    if ((count < 100)); then break; fi
    page=$((page + 1))
    if ((page > 10)); then echo "search pagination exceeded cap: $org" >&2; exit 1; fi
  done
  if ((seen != expected)); then
    echo "pagination mismatch for $org: got $seen of $expected" >&2
    exit 1
  fi
  echo "$org: $seen open PRs fetched" >&2
done

if ! jq -e -s '
  (length == (map(.repository + "#" + (.number | tostring)) | unique | length)) and
  (all(.[]; (.repository | split("/")[0]) == .owner))
' "$TMP/pr-inventory.jsonl" >/dev/null; then
  echo 'duplicate PR identity or owner mismatch; refusing to publish census' >&2
  exit 1
fi

GENERATED_AT=$(date -u +%Y-%m-%dT%H:%M:%SZ)
jq -n --arg generated "$GENERATED_AT" --slurpfile prs "$TMP/pr-inventory.jsonl" \
  --slurpfile pages "$TMP/pages.jsonl" '
  ($prs | group_by(.owner) | map({key:.[0].owner,value:length}) | from_entries) as $orgs |
  ($prs | group_by(.repository) | map({repo:.[0].repository,count:length}) | sort_by(-.count, .repo)) as $repos |
  {
    schema_version:"m0-pr-census-1",
    generated_at:$generated,
    status:"complete_first_pass",
    source:"GitHub REST search/issues; GET; open PRs",
    query_by_owner:"is:pr is:open org:OWNER",
    total_open:($prs|length),
    unique_repositories:($repos|length),
    by_owner:$orgs,
    draft:($prs|map(select(.draft == true))|length),
    draft_unmeasured:($prs|map(select(.draft == null))|length),
    bots:($prs|map(select(.author|endswith("[bot]")))|length),
    dependabot:($prs|map(select(.author=="dependabot[bot]"))|length),
    top_repositories:($repos[:20]),
    pages:$pages,
    unknowns:["head_sha","required_checks","review_state","merge_conflicts","policy_authorisation"],
    limitations:[
      "GitHub search is not an atomic snapshot; updates between pages can move entries without changing total_count",
      "Only repositories visible to the authenticated connection are represented",
      "No paid review, remediation, merge, check rerun or mutation was invoked",
      "Counts do not represent distinct root causes or actionable repair jobs"
    ]
  }
' > "$TMP/pr-census.json"

# A completed summary is written last. Downstream readers require both files,
# the complete_first_pass marker, and a matching inventory row count.
mv -- "$TMP/pr-inventory.jsonl" "$OUT_DIR/pr-inventory.jsonl"
mv -- "$TMP/pr-census.json" "$OUT_DIR/pr-census.json"
echo "Complete first-pass census: $(jq -r '.total_open' "$OUT_DIR/pr-census.json") open PRs" >&2
