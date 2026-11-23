#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
set -euo pipefail
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
SCRIPT="$ROOT/scripts/sweeps/estate-pr-enrich-readonly.sh"
T=$(mktemp -d)
trap 'rm -rf -- "$T"' EXIT
mkdir -p "$T/bin" "$T/in"
printf '%s\n' '{"status":"complete_first_pass","total_open":1}' > "$T/in/pr-census.json"
printf '%s\n' '{"repository":"hyperpolymath/hypatia","number":7,"author":"human"}' > "$T/in/pr-inventory.jsonl"
cat > "$T/bin/gh" <<'MOCK'
#!/usr/bin/env bash
set -euo pipefail
[[ $# == 4 && $1 == api && $2 == --method && $3 == GET ]] || exit 99
addr=$4
sha=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
case "$addr" in
  repos/hyperpolymath/hypatia/pulls/7)
    if [[ ${MOCK_MODE:-success} == moved ]]; then
      n=$(cat "$MOCK_COUNT_FILE" 2>/dev/null || echo 0)
      n=$((n+1)); echo "$n" > "$MOCK_COUNT_FILE"
      ((n>1)) && sha=bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
    fi
    jq -nc --arg sha "$sha" '{number:7,state:"open",draft:false,base:{repo:{full_name:"hyperpolymath/hypatia"},ref:"main"},head:{sha:$sha}}'
    ;;
  repos/hyperpolymath/hypatia/commits/*/check-runs*)
    [[ ${MOCK_MODE:-success} == checks_fail ]] && exit 81
    if [[ ${MOCK_MODE:-success} == paginated ]]; then
      if [[ "$addr" == *"&page=1"* ]]; then
        jq -nc '{total_count:101,check_runs:[range(1;101)|{id:.,name:("check-"+(.|tostring)),status:"completed",conclusion:"success"}]}'
      else
        jq -nc '{total_count:101,check_runs:[{id:101,name:"security",status:"completed",conclusion:"failure"}]}'
      fi
    else
      jq -nc '{total_count:2,check_runs:[{id:1,name:"tests",status:"completed",conclusion:"success"},{id:2,name:"security",status:"completed",conclusion:"failure"}]}'
    fi
    ;;
  repos/hyperpolymath/hypatia/pulls/7/reviews*)
    jq -nc '[{id:98,state:"CHANGES_REQUESTED",user:{login:"reviewer"},commit_id:"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}]'
    ;;
  repos/hyperpolymath/hypatia/commits/*/status)
    jq -nc '{state:"pending"}'
    ;;
  *) echo "unexpected GET $addr" >&2; exit 89 ;;
esac
MOCK
chmod +x "$T/bin/gh"
export PATH="$T/bin:$PATH" MOCK_COUNT_FILE="$T/gh-count"
MOCK_MODE=success bash "$SCRIPT" --inventory-dir "$T/in" --out-dir "$T/success" --limit 1 >/dev/null
jq -e '.selected==1 and .head_stable==1 and .observed_check_failures==1 and .status=="all_input_rows_observed"' "$T/success/pr-enrichment-summary.json" >/dev/null
jq -e '.observation=="head_stable" and .check_runs.total==2 and .combined_status.value=="pending" and .required_checks.state=="unmeasured" and .reviews.requested_changes_observed==true' "$T/success/pr-enrichment.jsonl" >/dev/null
MOCK_MODE=paginated bash "$SCRIPT" --inventory-dir "$T/in" --out-dir "$T/paginated" --limit 1 >/dev/null
jq -e '.check_runs.total==101 and (.check_runs.failures|length)==1' "$T/paginated/pr-enrichment.jsonl" >/dev/null
MOCK_MODE=checks_fail bash "$SCRIPT" --inventory-dir "$T/in" --out-dir "$T/checks-fail" --limit 1 >/dev/null
jq -e '.observation=="partial" and .check_runs.state=="unavailable" and .check_runs.total==null' "$T/checks-fail/pr-enrichment.jsonl" >/dev/null
MOCK_MODE=moved bash "$SCRIPT" --inventory-dir "$T/in" --out-dir "$T/moved" --limit 1 >/dev/null
jq -e '.observation=="head_changed_or_unverified" and .stable_head==false' "$T/moved/pr-enrichment.jsonl" >/dev/null
if MOCK_MODE=success bash "$SCRIPT" --inventory-dir "$T/in" --out-dir "$T/success" >/dev/null 2>&1; then
  echo 'expected stale-output rejection' >&2; exit 1
fi
printf '%s\n' '{"status":"complete_first_pass","total_open":2}' > "$T/in/pr-census.json"
if MOCK_MODE=success bash "$SCRIPT" --inventory-dir "$T/in" --out-dir "$T/bad-input" >/dev/null 2>&1; then
  echo 'expected incomplete-input rejection' >&2; exit 1
fi
bash -n "$SCRIPT"
echo 'PASS: success, check-run pagination, failed subsource, head drift, stale output, input integrity'
