#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
set -euo pipefail
script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cases=0
check() {
  local expected=$1 label=$2 content=$3 actual=0
  printf '%s' "$content" > "$work/report.json"
  bash "$script_dir/check-bridge-report.sh" "$work/report.json" > "$work/output" 2>&1 || actual=$?
  if [[ "$actual" != "$expected" ]]; then
    printf 'FAIL %s: expected exit %s, got %s\n' "$label" "$expected" "$actual" >&2
    cat "$work/output" >&2
    exit 1
  fi
  cases=$((cases + 1))
}
clean='{"schema_version":"0.3.0","vulnerability_data":"queried","total_dependencies":1,"cves":[]}'
finding='{"classification":"unmitigable","reachability":{"status":"reachable"}}'
blocked=$(jq --argjson finding "$finding" '.cves=[$finding]' <<< "$clean")
check 0 clean "$clean"
check 1 blocking "$blocked"
check 0 mitigable "$(jq '.cves[0].classification="mitigable"' <<< "$blocked")"
check 0 informational "$(jq '.cves[0].classification="informational" | .cves[0].reachability.status="phantom-declared"' <<< "$blocked")"
check 0 no-dependencies "$(jq '.vulnerability_data="no-dependencies" | .total_dependencies=0' <<< "$clean")"
check 2 offline "$(jq '.vulnerability_data="offline-unavailable"' <<< "$clean")"
check 2 missing-data "$(jq 'del(.vulnerability_data)' <<< "$clean")"
check 2 missing-cves "$(jq 'del(.cves)' <<< "$clean")"
check 2 null-cves "$(jq '.cves=null' <<< "$clean")"
check 2 object-cves "$(jq '.cves={}' <<< "$clean")"
check 2 unknown-schema "$(jq '.schema_version="0.4.0"' <<< "$clean")"
check 2 legacy-schema "$(jq 'del(.schema_version)' <<< "$clean")"
check 2 uppercase-verdict "$(jq '.cves[0].classification="Unmitigable"' <<< "$blocked")"
check 2 uppercase-reachability "$(jq '.cves[0].reachability.status="Reachable"' <<< "$blocked")"
check 2 missing-classification "$(jq 'del(.cves[0].classification)' <<< "$blocked")"
check 2 missing-reachability "$(jq 'del(.cves[0].reachability)' <<< "$blocked")"
check 2 null-finding "$(jq '.cves=[null]' <<< "$clean")"
check 2 bogus-empty "$(jq '.vulnerability_data="no-dependencies"' <<< "$blocked")"
check 2 queried-zero "$(jq '.total_dependencies=0' <<< "$clean")"
check 2 negative-count "$(jq '.total_dependencies=-1' <<< "$clean")"
check 2 fractional-count "$(jq '.total_dependencies=1.5' <<< "$clean")"
check 2 string-count "$(jq '.total_dependencies="1"' <<< "$clean")"
check 2 empty ''
check 2 malformed '{'
check 2 null null
check 2 array '[]'
check 2 multiple-documents "$blocked $clean"
check 2 multiple-clean-documents "$clean $clean"
actual=0
bash "$script_dir/check-bridge-report.sh" "$work/missing.json" > /dev/null 2>&1 || actual=$?
[[ "$actual" == 2 ]]
cases=$((cases + 1))
printf 'PASS: %s bridge report contract cases\n' "$cases"
