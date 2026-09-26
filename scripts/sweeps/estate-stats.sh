#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# estate-stats.sh — build the single estate-wide statistics artifact that the
# private farm dashboard reads.
#
# WHY
#
# The complaint was specific: after the issue count ran away, the one thing
# that became impossible to see was the *state of the estate* — how many PRs
# are open and what class they are, which repositories have no tests or no
# benches at all, which suites are failing, which benches are over their
# limit. Those numbers existed in five places and were displayed in none.
#
# This emits ONE file, `estate-stats.json`, with a frozen schema version, the
# run time, and — critically — an explicit provenance per section saying where
# each number came from and whether it was actually available. A metric that
# could not be measured is reported as unavailable, never as 0. (An earlier
# generation of this estate's dashboards showed zeros for data that had simply
# never been fetched, which is how "green" became meaningless.)
#
# SOURCES
#
#   repositories, pull requests, issues   GitHub API + a decisions manifest
#   security weak points                  --scans (panic-attack scan store)
#   test surface, failing suites          --checks (GitHub Actions API)
#   bench coverage / over-limit ratio     no producer in the estate yet —
#                                         reported unavailable with the
#                                         contract it must satisfy
#
# The `paging` block at the top level evaluates every threshold in
# policy.stats_thresholds against the measured value, so the dashboard does
# not have to know which number is "serious". A metric with no source is
# `unavailable` there too — a threshold cannot be breached by data that was
# never collected.
#
# Consumer contract (see pr-automerge-policy.json → stats_output):
#   producer:  hypatia  docs/status/estate-stats.json
#   consumer:  hyperpolymath/.git-private-farm
#              metadatastician/berrywiki/data/estate-stats.json
#
# EXIT CODES: 0 ok, 1 error
#
# usage: estate-stats.sh [--org ORG]... [--out FILE] [--decisions FILE]
#                        [--scans DIR] [--checks] [--max-repos N]
#                        [--max-issues N]
set -euo pipefail

SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SELF_DIR/../.." && pwd)"

POLICY="$REPO_ROOT/.machine_readable/merge-orchestration/pr-automerge-policy.json"
OUT="${PWD}/estate-stats.json"
DECISIONS=""
SCANS_DIR=""
ORGS=()
MAX_ISSUES=1000
CHECKS=0
MAX_REPOS=0

while [ $# -gt 0 ]; do
  case "$1" in
    --org)        ORGS+=("$2"); shift 2 ;;
    --out)        OUT="$2"; shift 2 ;;
    --decisions)  DECISIONS="$2"; shift 2 ;;
    --scans)      SCANS_DIR="$2"; shift 2 ;;
    --checks)     CHECKS=1; shift ;;
    --max-repos)  MAX_REPOS="$2"; shift 2 ;;
    --max-issues) MAX_ISSUES="$2"; shift 2 ;;
    -h|--help)    sed -n '2,54p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "unknown argument: $1" >&2; exit 1 ;;
  esac
done

[ ${#ORGS[@]} -eq 0 ] && ORGS=("hyperpolymath" "metadatastician")
command -v gh >/dev/null || { echo "gh is required" >&2; exit 1; }
command -v jq >/dev/null || { echo "jq is required" >&2; exit 1; }
log() { printf '%s\n' "$*" >&2; }

RUN_AT=$(date -u +%Y-%m-%dT%H:%M:%SZ)
policy_version=$(jq -r '.version' "$POLICY")
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

list_repos() {
  local org="$1"
  gh api "orgs/${org}/repos?per_page=100" --paginate \
    --jq '.[] | {name:.name, archived:.archived, fork:.fork, disabled:.disabled,
                  description:(.description // ""), topics:(.topics // []),
                  default_branch:(.default_branch // "main"),
                  pushed_at:(.pushed_at // ""), has_issues:(.has_issues // false)}' 2>/dev/null \
  || gh api "users/${org}/repos?per_page=100" --paginate \
    --jq '.[] | {name:.name, archived:.archived, fork:.fork, disabled:.disabled,
                  description:(.description // ""), topics:(.topics // []),
                  default_branch:(.default_branch // "main"),
                  pushed_at:(.pushed_at // ""), has_issues:(.has_issues // false)}' 2>/dev/null
}

# ─── Repositories + absences ─────────────────────────────────────────────
: > "$TMP/repos.jsonl"
for org in "${ORGS[@]}"; do
  list_repos "$org" | jq -c --arg org "$org" '. + {org:$org}' >> "$TMP/repos.jsonl"
done
repo_count=$(jq -s 'length' "$TMP/repos.jsonl")
log "repos: ${repo_count}"

MIN_TOPICS=$(jq -r '.absence_rules[] | select(.id == "ABS-002") | .min' "$POLICY")

jq -s --argjson min "$MIN_TOPICS" '
  {
    total: length,
    active: (map(select(.archived == false)) | length),
    archived: (map(select(.archived == true)) | length),
    empty_description: {
      count: (map(select(.archived == false and (.description | length) == 0)) | length),
      repos: [.[] | select(.archived == false and (.description | length) == 0) | "\(.org)/\(.name)"]
    },
    topics_below_min: {
      min: $min,
      count: (map(select(.archived == false and (.topics | length) < $min)) | length),
      repos: [map(select(.archived == false and (.topics | length) < $min))
              | sort_by(.topics | length) | .[] | {repo: "\(.org)/\(.name)", topics: (.topics | length)}]
    }
  }
' "$TMP/repos.jsonl" > "$TMP/absences.json"

# ─── Open PRs by class, from the decisions manifest ──────────────────────
if [ -z "$DECISIONS" ]; then
  for cand in "${PWD}/.pr-automerge/pr-decisions.jsonl" "$REPO_ROOT/.pr-automerge/pr-decisions.jsonl"; do
    [ -s "$cand" ] && DECISIONS="$cand" && break
  done
fi

if [ -n "$DECISIONS" ] && [ -s "$DECISIONS" ]; then
  jq -s '
    {
      available: true,
      source: "pr-decisions.jsonl",
      total: length,
      by_disposition: (group_by(.disposition) | map({key: .[0].disposition, value: length}) | from_entries),
      by_blocked_by: (group_by(.blocked_by) | map({key: .[0].blocked_by, value: length}) | from_entries),
      by_repo_archived: (map(select(.pr.repo_archived == true)) | length),
      oldest_age_days: (map(.age_days) | max // 0),
      median_age_days: (map(.age_days) | sort | (if length == 0 then 0 else .[length / 2 | floor] end)),
      poison_carrying: (map(select(.denylisted_hits > 0)) | length),
      rows: [.[] | {repo: .pr.repo, number: .pr.number, disposition: .disposition,
                    blocked_by: .blocked_by, age_days: .age_days,
                    denylisted_hits: .denylisted_hits}]
    }
  ' "$DECISIONS" > "$TMP/prs.json"
else
  echo '{"available": false, "source": null, "reason": "no pr-decisions.jsonl — run scripts/sweeps/estate-pr-automerge.sh first"}' > "$TMP/prs.json"
fi

# ─── Open issues ─────────────────────────────────────────────────────────
: > "$TMP/issues.jsonl"
for org in "${ORGS[@]}"; do
  for page in $(seq 1 10); do
    n=$(gh api "search/issues?q=org:${org}+is:issue+is:open&per_page=100&page=${page}" \
          --jq '.items[] | {org:"'"$org"'", repo:(.repository_url | split("/") | last), n:.number,
                            title:.title, created:.created_at, author:.user.login,
                            labels:[.labels[].name], comments:.comments}' 2>/dev/null \
          | tee -a "$TMP/issues.jsonl" | wc -l)
    [ "$n" -lt 100 ] && break
    sleep 3
  done
  sleep 2
done
issue_count=$(wc -l < "$TMP/issues.jsonl")
log "open issues: ${issue_count}"

# Duplicate-cause detection: normalise titles so that "3 pre-existing red
# checks" and "7 pre-existing red checks" collapse into one class. This is
# the number that matters for the sprawl question — 774 issues is a symptom,
# "N distinct causes" is the diagnosis.
jq -s '
  def norm: gsub("[0-9]+"; "N") | gsub("`[^`]*`"; "X");
  {
    available: true,
    total: length,
    truncated: (. >= '"$MAX_ISSUES"'),
    by_org: (group_by(.org) | map({key: .[0].org, value: length}) | from_entries),
    by_author: (group_by(.author) | map({key: (.[0].author // "unknown"), value: length}) | sort_by(-.value) | .[0:6] | from_entries),
    by_label: ([.[].labels[]] | sort | group_by(.) | map({key: (.[0] // "none"), value: length}) | sort_by(-.value) | .[0:12] | from_entries),
    by_month: (group_by(.created[0:7]) | map({key: .[0].created[0:7], value: length}) | from_entries),
    distinct_causes: (map(.title | norm) | unique | length),
    top_causes: ([.[].title | norm] | group_by(.) | map({cause: .[0], count: length})
                 | sort_by(-.count) | .[0:12]),
    never_commented: (map(select(.comments == 0)) | length),
    by_repo_top: (group_by(.repo) | map({key: .[0].repo, value: length})
                  | sort_by(-.value) | .[0:12])
  }
' "$TMP/issues.jsonl" > "$TMP/issues.json"

# ─── Test surface and suite outcomes (--checks) ──────────────────────────
# There is no test/bench data store in this estate: verisimdb-data/scans holds
# panic-attack security scans, not suite results. So "which repositories have
# no test surface at all" and "whose suites are failing" are measured from the
# only source that actually knows — the repositories' own workflow definitions
# and their runs on the default branch.
#
# It is a proxy and it says so in the artifact: a workflow counts as a test
# surface when its name or path matches (test|spec|ci|check|conformance|gate|
# audit|verify). Two API calls per active repository.
if [ "$CHECKS" -eq 1 ]; then
  active_repos=$(jq -s '[.[] | select(.archived == false and .fork == false and .disabled == false)] | length' "$TMP/repos.jsonl")
  log "checks: test surface + default-branch outcomes across ${active_repos} repos (2 calls each)…"
  : > "$TMP/checks.jsonl"
  i=0
  while IFS= read -r row; do
    org=$(jq -r '.org' <<<"$row"); name=$(jq -r '.name' <<<"$row")
    branch=$(jq -r '.default_branch // "main"' <<<"$row")
    full="${org}/${name}"
    i=$((i + 1))
    [ $((i % 25)) -eq 0 ] && log "  … ${i}/${active_repos}"

    # A failed call is NOT an absence of tests. If either call fails the
    # repository is recorded as unmeasured and excluded from the counts —
    # otherwise a rate limit or a 409 on an empty repo would report as
    # "this project has no test surface", which is a lie in the direction
    # that matters.
    measured=true
    if wf_raw=$(timeout 45 gh api "repos/${full}/actions/workflows?per_page=100" 2>/dev/null); then
      wf=$(jq -c '[.workflows[]? | {name:(.name // ""), path:(.path // ""), state:(.state // "active")}]' <<<"$wf_raw" 2>/dev/null) || wf='[]'
    else
      wf='[]'; measured=false
    fi
    if runs_raw=$(timeout 45 gh api "repos/${full}/actions/runs?per_page=20&branch=${branch}" 2>/dev/null); then
      runs=$(jq -c '[.workflow_runs[]? | {name:(.name // ""), path:(.path // ""), head_branch:(.head_branch // ""),
                    status:(.status // ""), conclusion:(.conclusion // null), created:(.created_at // null),
                    updated:(.updated_at // null)}]' <<<"$runs_raw" 2>/dev/null) || runs='[]'
    else
      runs='[]'; measured=false
    fi
    [ -n "$wf" ] || wf='[]'
    [ -n "$runs" ] || runs='[]'

    jq -cn --arg repo "$full" --arg branch "$branch" --argjson measured "$measured" --argjson wf "$wf" --argjson runs "$runs" '
      def is_test($w): (($w.name // "") | test("(test|spec|ci|check|conformance|gate|audit|verify)"; "i"))
                    or (($w.path // "") | test("(test|spec|ci|check|conformance|gate|audit|verify)"; "i"));
      ([ $wf[] | select((.state // "active") == "active") | select(is_test(.)) ]) as $surface
      # Explicitly newest-first by creation time: run ordering in the API
      # response is not something to rely on when the answer is "this repo
      # is red". (No apostrophes in here: this jq program lives inside a
      # single-quoted shell string.)
      | ([ $runs[] | select(.head_branch == $branch) | select(.status == "completed") | select(is_test(.)) ]
         | sort_by(.created) | reverse) as $done
      | {
          repo: $repo,
          default_branch: $branch,
          measured: $measured,
          workflows: ($wf | length),
          test_surface: ($surface | length),
          test_workflow: ($done[0].name // null),
          test_conclusion: ($done[0].conclusion // null),
          test_updated: ($done[0].updated // null)
        }' >> "$TMP/checks.jsonl"
  done < <(jq -c 'select(.archived == false and .fork == false and .disabled == false)' "$TMP/repos.jsonl")
  log "checks: done"
fi

# ─── Signals ─────────────────────────────────────────────────────────────
# Three sub-blocks, each with its own provenance. They are never merged into
# one "health score": a security finding and a failing suite are different
# facts about different surfaces.
weak='{"available": false, "reason": "no scan store passed via --scans; nothing was measured, so nothing is reported"}'
if [ -n "$SCANS_DIR" ] && [ -d "$SCANS_DIR" ]; then
  weak=$(jq -s '
    {
      available: true,
      source: "panic-attack scan store (verisimdb-data/scans)",
      repos_scanned: length,
      repositories_with_weak_points: ([.[] | select(((.weak_points // []) | length) > 0)] | length),
      weak_point_classes: ([.[].weak_points[]?.category] | map(select(. != null)) | sort | group_by(.)
                           | map({key: (.[0] // "uncategorised"), value: length}) | sort_by(-.value) | .[0:20] | from_entries)
    }
  ' "$SCANS_DIR"/*.json 2>/dev/null) || \
    weak='{"available": false, "reason": "scan store present but unreadable"}'
  [ -n "$weak" ] || weak='{"available": false, "reason": "scan store present but unreadable"}'
fi

tests='{"available": false, "reason": "no --checks run; test surface and suite outcomes were not measured"}'
if [ "$CHECKS" -eq 1 ] && [ -s "$TMP/checks.jsonl" ]; then
  tests=$(jq -s '
    {
      available: true,
      source: "github actions api — active workflow definitions + runs on the default branch",
      definition: "a workflow is a test surface when its name or path matches (test|spec|ci|check|conformance|gate|audit|verify)",
      repos_measured: (map(select(.measured == true)) | length),
      repos_unmeasured: (map(select(.measured != true)) | length),
      with_test_surface: (map(select(.measured == true and .test_surface > 0)) | length),
      coverage_empties: (map(select(.measured == true and .test_surface == 0)) | length),
      coverage_empty_repos: ([.[] | select(.measured == true and .test_surface == 0) | .repo] | sort | .[0:60]),
      failing: (map(select(.measured == true and .test_conclusion == "failure")) | length),
      failing_repos: ([.[] | select(.measured == true and .test_conclusion == "failure")
                       | {repo, workflow: .test_workflow, at: .test_updated}] | sort_by(.repo) | .[0:60]),
      never_concluded: (map(select(.measured == true and .test_surface > 0 and .test_conclusion == null)) | length),
      last_run: ([.[].test_updated] | map(select(. != null)) | max // null)
    }
  ' "$TMP/checks.jsonl")
fi

# Benches have no producer. The threshold block still evaluates them — as
# unavailable, because a threshold cannot be breached by data nobody collected.
benches='{"available": false, "reason": "no bench-result producer anywhere in the estate — no store, no workflow artefact contract", "expected": {"per_repo_file": "benches.json", "fields": ["repo", "bench", "value_ns", "limit_ns", "measured_at"]}}'

signals=$(jq -cn --argjson weak "$weak" --argjson tests "$tests" --argjson benches "$benches" '
  {
    available: ($weak.available or $tests.available),
    security_weak_points: $weak,
    tests: $tests,
    benches: $benches
  }')

# ─── Paging: every policy threshold against the measured value ───────────
# The dashboard reads this block instead of re-implementing the thresholds.
# `unavailable` for a metric with no source, never a silent "ok".
paging=$(jq -cn \
  --argjson thr "$(jq '.stats_thresholds' "$POLICY")" \
  --argjson prs "$(cat "$TMP/prs.json")" \
  --argjson issues "$(cat "$TMP/issues.json")" \
  --argjson signals "$signals" '
  def lvl($v; $w; $c):
    if $v == null then "unavailable"
    elif $v >= $c then "critical"
    elif $v >= $w then "warn"
    else "ok" end;
  def row($v; $w; $c): {value: $v, warn: $w, critical: $c, level: lvl($v; $w; $c)};
  {
    open_issue_count:       row($issues.total; $thr.open_issue_count_warn; $thr.open_issue_count_critical),
    unmerged_pr_count:      row($prs.total; $thr.unmerged_pr_count_warn; $thr.unmerged_pr_count_critical),
    unmerged_pr_age_days:   row($prs.oldest_age_days; $thr.unmerged_pr_age_days_warn; $thr.unmerged_pr_age_days_critical),
    failing_tests:          row($signals.tests.failing; $thr.failing_tests_warn; $thr.failing_tests_critical),
    test_coverage_empties:  row($signals.tests.coverage_empties; $thr.test_coverage_empties_warn; $thr.test_coverage_empties_critical),
    bench_coverage_empties: row(null; $thr.bench_coverage_empties_warn; $thr.bench_coverage_empties_critical),
    bench_over_limit_ratio: row(null; $thr.bench_over_limit_ratio_warn; $thr.bench_over_limit_ratio_critical)
  }')

# ─── Assemble ────────────────────────────────────────────────────────────
jq -n \
  --arg version "$(jq -r '.stats_output.schema_version' "$POLICY")" \
  --arg policy "$(jq -r '.version' "$POLICY")" \
  --arg run_at "$RUN_AT" \
  --argjson thresholds "$(jq '.stats_thresholds' "$POLICY")" \
  --argjson repos "$(cat "$TMP/absences.json")" \
  --argjson prs "$(cat "$TMP/prs.json")" \
  --argjson issues "$(cat "$TMP/issues.json")" \
  --argjson signals "$signals" \
  --argjson paging "$paging" \
  '{
     schema_version: $version,
     policy_version: $policy,
     generated_at: $run_at,
     producer: "hypatia/scripts/sweeps/estate-stats.sh",
     thresholds: $thresholds,
     repositories: $repos,
     pull_requests: $prs,
     issues: $issues,
     signals: $signals,
     paging: $paging
   }' > "$OUT"

# ─── Human-readable companion ────────────────────────────────────────────
{
  echo "= Estate statistics"
  echo
  echo "Generated ${RUN_AT} by \`estate-stats.sh\` (policy ${policy_version})."
  echo
  echo "== Pull requests"
  jq -r 'if .available then
      "- Open PRs: \(.total)\n- By disposition: \(.by_disposition | to_entries | map("\(.key)=\(.value)") | join(", "))\n- Carrying a denylisted pin: \(.poison_carrying)\n- Oldest: \(.oldest_age_days) days; median: \(.median_age_days) days"
    else "- unavailable: \(.reason)" end' "$TMP/prs.json"
  echo
  echo "== Issues"
  jq -r '"- Open: \(.total)\n- Distinct causes after normalisation: \(.distinct_causes)\n- Never commented on: \(.never_commented)\n- Top cause: \((.top_causes[0] // {cause:"-",count:0}) | "\(.count)× \(.cause)")"' "$TMP/issues.json"
  echo
  echo "== Repository absences"
  jq -r '"- Repositories: \(.total) (\(.active) active, \(.archived) archived)\n- Empty description: \(.empty_description.count)\n- Fewer than \(.topics_below_min.min) topics: \(.topics_below_min.count)"' "$TMP/absences.json"
  echo
  echo "== Test surface"
  jq -r 'if .available then
      "- Repositories measured: \(.repos_measured) (unmeasured: \(.repos_unmeasured))\n- With a test surface: \(.with_test_surface)\n- No test surface at all: \(.coverage_empties)\n- Suites failing on the default branch: \(.failing)\n- Never concluded a test run: \(.never_concluded)"
    else "- unavailable: \(.reason)" end' <<<"$tests"
  echo
  echo "== Benches"
  jq -r 'if .available then "- measured" else "- unavailable: \(.reason)" end' <<<"$benches"
  echo
  echo "== Security weak points"
  jq -r 'if .available then "- Repositories in the scan store: \(.repos_scanned)\n- With weak points: \(.repositories_with_weak_points)" else "- unavailable: \(.reason)" end' <<<"$weak"
  echo
  echo "== Thresholds (paging)"
  jq -r 'to_entries[] | "- \(.key): \(.value.level)\(if .value.value != null then " (\(.value.value), warn \(.value.warn) / critical \(.value.critical))" else "" end)"' <<<"$paging"
} > "${OUT%.json}.adoc" 2>/dev/null || true

log ""
log "─── estate stats ──────────────────────────────────────────────────"
jq -r '"  repos            : \(.repositories.total) (\(.repositories.active) active)\n  empty descr.     : \(.repositories.empty_description.count)\n  <\(.repositories.topics_below_min.min) topics       : \(.repositories.topics_below_min.count)\n  open PRs         : \(if .pull_requests.available then .pull_requests.total else "unavailable" end)\n  open issues      : \(if .issues.available then .issues.total else "unavailable" end)\n  distinct causes  : \(if .issues.available then .issues.distinct_causes else "unavailable" end)\n  test surface     : \(if .signals.tests.available then "\(.signals.tests.with_test_surface) with / \(.signals.tests.coverage_empties) without / \(.signals.tests.failing) failing" else "unavailable" end)\n  benches          : \(if .signals.benches.available then "measured" else "unavailable (no producer)" end)"' "$OUT" >&2
log "  paging           : $(jq -r '[.paging[] | select(.level != "ok") | .level] | group_by(.) | map("\(length) \(.[0])") | join(", ")' "$OUT" | sed 's/^$/all ok/')"
log "  artifact         : ${OUT}"
log "  human summary    : ${OUT%.json}.adoc"

exit 0
