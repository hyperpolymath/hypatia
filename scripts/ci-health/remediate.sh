#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
# Owner: Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# remediate.sh — apply the SAFE auto-remediations for the CI failure classes.
#   B-ALLOWLIST  -> PUT curated allow-list superset (hyperpolymath/* + pinned
#                   third-party), keep github-owned/verified/sha-pinning.
#   D-BURN       -> open an idempotent, signed burn-cut PR (scope push to the
#                   default branch + add concurrency-cancel) via the API.
#   A-BILLING    -> NEVER auto-fixed (account-level, owner-only); the driver
#                   aggregates these into the tracking issue.
#
# Guardrails (lessons from the 2026-06-13 sweep):
#   * own repos only: skip forks + archived.
#   * DENYLIST: skip ARR-special / cross-owner repos (e.g. 007).
#   * idempotent: skip if the allow-list already has hyperpolymath/* or the
#     burn-cut branch/PR already exists.
#   * dry-run honoured.
# Usage: remediate.sh <repo> <CLASS> <dry_run:true|false>
set -euo pipefail
O="${OWNER:-hyperpolymath}"; R="$1"; CLASS="$2"; DRY="${3:-true}"
BR="ci/ci-health-auto-remediation"
HERE="$(cd "$(dirname "$0")" && pwd)"
DENYLIST="${CI_HEALTH_DENYLIST:-007}"   # space-separated repo names to never touch

for d in $DENYLIST; do [ "$R" = "$d" ] && { echo "SKIP $R/$CLASS denylisted"; exit 0; }; done
meta=$(gh api "repos/$O/$R" --jq '"\(.fork) \(.archived)"' 2>/dev/null || echo "false false")
read -r isfork isarch <<<"$meta"
{ [ "$isfork" = "true" ] || [ "$isarch" = "true" ]; } && { echo "SKIP $R/$CLASS fork-or-archived"; exit 0; }

case "$CLASS" in
  B-ALLOWLIST)
    # Build body: hyperpolymath/* + each superset action as owner/repo@*
    body=$(python3 - "$HERE/action-superset.txt" <<'PY'
import json,sys
pats=["hyperpolymath/*"]+[l.strip()+"@*" for l in open(sys.argv[1]) if l.strip()]
print(json.dumps({"github_owned_allowed":True,"verified_allowed":True,"patterns_allowed":pats}))
PY
)
    if [ "$DRY" = "true" ]; then echo "DRYRUN $R/B-ALLOWLIST would PUT $(printf '%s' "$body"|python3 -c 'import json,sys;print(len(json.load(sys.stdin)["patterns_allowed"]))') patterns"; exit 0; fi
    printf '%s' "$body" | gh api -X PUT "repos/$O/$R/actions/permissions/selected-actions" --input - >/dev/null
    n=$(gh api "repos/$O/$R/actions/permissions/selected-actions" --jq '.patterns_allowed|length')
    echo "FIXED $R/B-ALLOWLIST -> $n patterns (sha-pinning unchanged)"
    ;;
  D-BURN)
    if gh api "repos/$O/$R/branches/$BR" --jq '.name' >/dev/null 2>&1; then echo "SKIP $R/D-BURN branch-exists"; exit 0; fi
    def=$(gh api "repos/$O/$R" --jq '.default_branch'); sha=$(gh api "repos/$O/$R/git/ref/heads/$def" --jq '.object.sha')
    targets=(); for p in $(gh api "repos/$O/$R/contents/.github/workflows" --jq '.[]?|select(.name|test("\\.ya?ml$"))|.path' 2>/dev/null); do
      gh api "repos/$O/$R/contents/$p?ref=$def" --jq '.content' 2>/dev/null | base64 -d 2>/dev/null \
        | grep -qE '^on:[[:space:]]*\[[[:space:]]*push[[:space:]]*,[[:space:]]*pull_request' && targets+=("$p")
    done
    [ "${#targets[@]}" -eq 0 ] && { echo "SKIP $R/D-BURN no-targets"; exit 0; }
    if [ "$DRY" = "true" ]; then echo "DRYRUN $R/D-BURN would patch ${#targets[@]} file(s) + open PR"; exit 0; fi
    gh api -X POST "repos/$O/$R/git/refs" -f ref="refs/heads/$BR" -f sha="$sha" >/dev/null
    for p in "${targets[@]}"; do
      cur=$(gh api "repos/$O/$R/contents/$p?ref=$BR")
      newc=$(printf '%s' "$cur" | python3 -c 'import json,sys,re,base64
d=json.load(sys.stdin); s=base64.b64decode(d["content"]).decode("utf-8")
pat=re.compile(r"(?m)^on:[ \t]*\[[ \t]*push[ \t]*,[ \t]*pull_request[ \t]*\][ \t]*$")
blk="on:\n  push:\n    branches: [main, master]\n  pull_request:\n"
if not re.search(r"(?m)^concurrency:",s):
    blk+="\n# Estate guardrail: scope push to default branches (PR fires once, not\n# push+PR) and cancel superseded runs. Safe — read-only PR check.\nconcurrency:\n  group: ${{ github.workflow }}-${{ github.ref }}\n  cancel-in-progress: true\n"
print(base64.b64encode(pat.sub(blk.rstrip(chr(10)),s,count=1).encode()).decode())')
      csha=$(printf '%s' "$cur" | python3 -c 'import json,sys;print(json.load(sys.stdin)["sha"])')
      gh api -X PUT "repos/$O/$R/contents/$p" -f message="ci: cut Actions burn in $p (scope push + concurrency-cancel)" \
        -f content="$newc" -f sha="$csha" -f branch="$BR" >/dev/null
    done
    url=$(gh api "repos/$O/$R/pulls" -X POST -f title="ci: cut Actions burn — scope push triggers + concurrency-cancel" \
      -f head="$BR" -f base="$def" -f body="Automated by hypatia ci-health-sweep. Scopes \`push\` to the default branch (kills push+PR double-runs) and adds \`concurrency: cancel-in-progress\` to read-only PR checks. No SPDX/logic changes." --jq '.html_url')
    echo "FIXED $R/D-BURN -> $url (${#targets[@]} file(s))"
    ;;
  *) echo "SKIP $R/$CLASS no-auto-remediation" ;;
esac
