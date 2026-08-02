#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# bridge-verdict.sh -- the B1 core of bridge-gate.yml as a standalone, testable unit.
# (The workflow's "Apply B1 verdict" step inlines this same jq.)
#
# Reads a panic-attack BridgeReport JSON ($1). Exit 0 = PASS (CVE-clear -> arm-eligible);
# exit 1 = FLAG (a reachable Unmitigable CVE -> safety=flag, no auto-merge).
set -euo pipefail
report="${1:?usage: bridge-verdict.sh <bridge-report.json>}"

blockers=$(jq '[.cves[]? | select(.classification=="Unmitigable" and .reachability.status=="Reachable")] | length' "$report")

if [ "$blockers" -gt 0 ]; then
  echo "B1 FLAG: $blockers reachable unmitigable CVE(s) -> safety=flag (no auto-merge)"
  jq -r '.cves[]? | select(.classification=="Unmitigable" and .reachability.status=="Reachable")
         | "  - \(.vulnerability.cve // .vulnerability.id): \(.rationale)"' "$report"
  exit 1
fi
echo "B1 PASS: no reachable unmitigable CVEs -> arm-eligible (pool-gated)"
exit 0
