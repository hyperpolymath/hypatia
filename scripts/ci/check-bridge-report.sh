#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
# Exit 0: qualified report has no B1 blockers; 1: blockers; 2: invalid/unavailable data.
set -euo pipefail
if [[ $# != 1 || ! -f "$1" ]]; then
  echo '::error::B1 assessment unavailable: expected one report file.' >&2
  exit 2
fi
# Read exactly one JSON document. jq's default stream handling could otherwise
# accept an empty file or let a second document overwrite the first verdict.
if ! blockers=$(jq -ers '
  def natural: type == "number" and . >= 0 and . == floor;
  if length != 1 then error("expected one report") else .[0] end
  | if (
      type == "object"
      and .schema_version == "0.3.0"
      and (.total_dependencies | natural)
      and (.cves | type == "array")
      and (
        (.vulnerability_data == "queried" and .total_dependencies > 0)
        or (.vulnerability_data == "no-dependencies" and .total_dependencies == 0 and (.cves | length) == 0)
      )
      and all(.cves[];
        type == "object"
        and (.classification | IN("mitigable", "unmitigable", "informational"))
        and (.reachability | type == "object")
        and (.reachability.status | IN("reachable", "unreachable", "phantom-declared", "phantom-transitive"))
      )
    ) then [.cves[] | select(.classification == "unmitigable" and .reachability.status == "reachable")] | length
    else error("unsupported, incomplete or unavailable bridge assessment") end
' "$1"); then
  echo '::error::B1 assessment unavailable: invalid report or missing vulnerability data.' >&2
  exit 2
fi
if [[ "$blockers" -gt 0 ]]; then
  printf '::error::B1: %s reachable unmitigable CVE(s); auto-merge blocked.\n' "$blockers" >&2
  exit 1
fi
echo 'B1 PASS: qualified report contains no reachable unmitigable CVEs.'
