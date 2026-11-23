#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Hermetic read-only contract tests; no GitHub API requests.
set -euo pipefail
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
CENSUS="$ROOT/scripts/sweeps/estate-pr-census-readonly.sh"
TMP=$(mktemp -d)
trap 'rm -rf -- "$TMP"' EXIT
mkdir -p "$TMP/bin"
cat > "$TMP/bin/gh" <<'MOCK'
#!/usr/bin/env bash
set -euo pipefail
[[ $# == 4 && $1 == api && $2 == --method && $3 == GET ]] || exit 91
case ${MOCK_SCENARIO:-success} in
  network) exit 92 ;;
  zero) printf '{"total_count":0,"incomplete_results":false,"items":[]}\n'; exit 0 ;;
  cap) printf '{"total_count":1001,"incomplete_results":false,"items":[]}\n'; exit 0 ;;
  *) : ;;
esac
printf -v q '%s' "$4"
if [[ "$q" == *hyperpolymath* ]]; then
  base='hyperpolymath/hypatia'
  owner='hyperpolymath'
else
  base='metadatastician/kyq'
  owner='metadatastician'
fi
repo=$base
[[ ${MOCK_SCENARIO:-} == wrong_owner ]] && repo='elsewhere/hypatia'
num=1
[[ $owner == metadatastician ]] && num=2
jq -nc --arg repo "$repo" --arg owner "$owner" --argjson n "$num" --arg scenario "${MOCK_SCENARIO:-success}" '
  {total_count:(if $scenario == "duplicate" then 2 else 1 end),
   incomplete_results:($scenario == "incomplete"),
   items:[{number:$n,repository_url:("https://api.github.com/repos/"+$repo),
   html_url:("https://github.com/"+$repo+"/pull/"+($n|tostring)),
   title:"fixture",user:(if $scenario == "missing_author" then {} else {login:"human"} end),
   draft:(if $scenario == "missing_draft" then null else true end),
   created_at:"2026-11-01T00:00:00Z",updated_at:"2026-11-23T00:00:00Z",labels:[]}]
  } | if $scenario == "duplicate" then .items += [.items[0]] else . end'
MOCK
chmod +x "$TMP/bin/gh"
export PATH="$TMP/bin:$PATH"
run_ok() {
  local name=$1; shift
  MOCK_SCENARIO=$name "$CENSUS" --out-dir "$TMP/$name" --org hyperpolymath --org metadatastician "$@" > "$TMP/$name.log" 2>&1
  jq -e '.status == "complete_first_pass" and .total_open == 2 and .unique_repositories == 2' "$TMP/$name/pr-census.json" > /dev/null
  [[ $(wc -l < "$TMP/$name/pr-inventory.jsonl") == 2 ]]
}
run_fail() {
  local name=$1
  if MOCK_SCENARIO=$name "$CENSUS" --out-dir "$TMP/$name" --org hyperpolymath > "$TMP/$name.log" 2>&1; then
    echo "FAIL: expected rejection for $name" >&2; exit 1
  fi
  [[ ! -f "$TMP/$name/pr-census.json" && ! -f "$TMP/$name/pr-inventory.jsonl" ]] || {
    echo "FAIL: partial output for $name" >&2; exit 1;
  }
}
run_ok success
run_ok missing_draft
jq -e '.draft_unmeasured == 2' "$TMP/missing_draft/pr-census.json" >/dev/null
for name in incomplete duplicate missing_author wrong_owner cap network; do run_fail "$name"; done
MOCK_SCENARIO=zero "$CENSUS" --out-dir "$TMP/zero" --org hyperpolymath >/dev/null 2>&1
jq -e '.total_open == 0 and .status == "complete_first_pass"' "$TMP/zero/pr-census.json" >/dev/null
bash -n "$CENSUS"
echo 'PASS: success, unknown draft, incomplete, duplicate, author, owner, cap, network, zero, shell syntax'
