# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# merge-ready.sh: per-repository gates that must hold before the sweep queues
# `gh pr merge --auto`. Sourced by estate-pr-automerge.sh; no side effects.
#
# WHY: with no required checks, `--auto` merges at once, unchecked. With a
# required check that never reported, it waits forever. On a red branch it
# merges code nobody can verify. A bypass actor can skip the checks entirely.
#
#   merge_ready FULL_NAME BASE_BRANCH
#     exit 0                      the repository may be given --auto
#     exit 1 + one reason line    skip it, and record the reason
#
# Required checks are read on BASE_BRANCH (the PR target, where the merge
# lands). Health is read on BASE_BRANCH and on the repository default branch.
#
# Every API failure fails CLOSED (`check_unavailable:<what>`). A 404 on the
# classic protection endpoint is not a failure: it means "no protection".
# Every list is read with --paginate, so a later page cannot hide an entry.
# Expect most repositories to be skipped at first. That is correct behaviour.
#
# NB: the bypass gate is deliberately conservative. It skips on ANY active
# ruleset with a bypass actor, because the sweep cannot tell whether the
# merging token belongs to one. Relax it only with that evidence.

# _gh_lines CMD...: run gh, print stdout. Exit 0 ok, 2 = HTTP 404, 1 = other error.
_gh_lines() {
  local out rc
  out=$(gh "$@" 2>&1) && { printf '%s' "$out"; return 0; }
  rc=$?
  case "$out" in *"HTTP 404"*) return 2 ;; esac
  return "$rc"
}

# _required_contexts FULL BASE: required status-check contexts, from classic
# protection AND rulesets, one per line.
_required_contexts() {
  local full="$1" base="$2" a b rc
  a=$(_gh_lines api "repos/${full}/branches/${base}/protection/required_status_checks" \
        --jq '[.contexts[]?, .checks[]?.context] | .[]') && rc=0 || rc=$?
  [ "$rc" -eq 0 ] || [ "$rc" -eq 2 ] || return 1
  [ "$rc" -eq 2 ] && a=""
  b=$(_gh_lines api "repos/${full}/rules/branches/${base}?per_page=100" --paginate \
        --jq '.[] | select(.type == "required_status_checks") | .parameters.required_status_checks[].context') || return 1
  printf '%s\n%s\n' "$a" "$b" | sed '/^$/d' | sort -u
}

# _reported_contexts FULL REF: every context that has reported on REF
# (check runs + commit statuses), one per line.
_reported_contexts() {
  local full="$1" ref="$2" runs stats
  runs=$(_gh_lines api "repos/${full}/commits/${ref}/check-runs?per_page=100" --paginate \
           --jq '.check_runs[].name') || return 1
  stats=$(_gh_lines api "repos/${full}/commits/${ref}/status?per_page=100" --paginate \
           --jq '.statuses[].context') || return 1
  printf '%s\n%s\n' "$runs" "$stats" | sed '/^$/d' | sort -u
}

# _head_is_red FULL REF: print "red" if anything on REF failed. Check SUITES are
# read as well as runs: a `startup_failure` is a suite conclusion and may have
# created no check run at all.
_head_is_red() {
  local full="$1" ref="$2" runs stats suites
  runs=$(_gh_lines api "repos/${full}/commits/${ref}/check-runs?per_page=100" --paginate \
           --jq '.check_runs[] | select(.conclusion == "failure" or .conclusion == "timed_out" or .conclusion == "startup_failure") | .name') || return 1
  stats=$(_gh_lines api "repos/${full}/commits/${ref}/status?per_page=100" --paginate \
           --jq '.statuses[] | select(.state == "failure" or .state == "error") | .context') || return 1
  suites=$(_gh_lines api "repos/${full}/commits/${ref}/check-suites?per_page=100" --paginate \
           --jq '.check_suites[] | select(.conclusion == "failure" or .conclusion == "timed_out" or .conclusion == "startup_failure") | .id') || return 1
  [ -n "${runs}${stats}${suites}" ] && echo red
  return 0
}

# _has_bypass_actor FULL: print "bypass" if any active ruleset carries a bypass
# actor. GitHub OMITS bypass_actors when the caller cannot see it, so an absent
# field is "unknown" and fails closed; it is never read as "no actors".
_has_bypass_actor() {
  local full="$1" ids id n
  ids=$(_gh_lines api "repos/${full}/rulesets?per_page=100" --paginate \
          --jq '.[] | select(.enforcement == "active") | .id') || return 1
  for id in $ids; do
    n=$(_gh_lines api "repos/${full}/rulesets/${id}" \
          --jq 'if (.bypass_actors | type) == "array" then (.bypass_actors | length) else "unknown" end') || return 1
    [ "$n" = "unknown" ] && return 1
    [ "${n:-0}" -gt 0 ] && { echo bypass; return 0; }
  done
  return 0
}

# merge_ready FULL BASE: see the header.
merge_ready() {
  local full="$1" base="$2" required reported c default ref refs

  required=$(_required_contexts "$full" "$base") || { echo "check_unavailable:required_checks"; return 1; }
  [ -n "$required" ] || { echo "no_required_checks"; return 1; }

  reported=$(_reported_contexts "$full" "$base") || { echo "check_unavailable:branch_checks"; return 1; }
  while IFS= read -r c; do
    grep -Fxq -- "$c" <<<"$reported" || { echo "required_check_never_reported:${c}"; return 1; }
  done <<<"$required"

  default=$(_gh_lines api "repos/${full}" --jq '.default_branch') || { echo "check_unavailable:default_branch"; return 1; }
  [ -n "$default" ] || { echo "check_unavailable:default_branch"; return 1; }
  refs="$base"
  [ "$default" = "$base" ] || refs="$base $default"   # git refs never contain spaces
  for ref in $refs; do
    c=$(_head_is_red "$full" "$ref") || { echo "check_unavailable:branch_state"; return 1; }
    if [ -n "$c" ]; then
      [ "$ref" = "$default" ] && echo "default_branch_red" || echo "target_branch_red"
      return 1
    fi
  done

  c=$(_has_bypass_actor "$full") || { echo "check_unavailable:rulesets"; return 1; }
  [ -z "$c" ] || { echo "ruleset_bypass_actor_present"; return 1; }
  return 0
}

# pr_threads_ready FULL NUM: owner ruling R6 (2026-11-13). Auto-merge only
# when every review thread on the PR is resolved. A bot finding left open is
# not a pass, and GitHub's native auto-merge does not wait for threads unless
# the ruleset requires conversation resolution. Resolution state exists only
# in GraphQL. Fails closed: an unreadable answer, or more than 100 threads
# with none open among them, blocks the merge. Prints the reason on failure.
pr_threads_ready() {
  local full="$1" num="$2" out
  # shellcheck disable=SC2016  # $owner/$name/$number are GraphQL variables
  out=$(gh api graphql -F owner="${full%%/*}" -F name="${full#*/}" -F number="$num" \
          -f query='query($owner:String!,$name:String!,$number:Int!){repository(owner:$owner,name:$name){pullRequest(number:$number){reviewThreads(first:100){pageInfo{hasNextPage} nodes{isResolved}}}}}' \
          --jq '.data.repository.pullRequest.reviewThreads | "\([.nodes[] | select(.isResolved | not)] | length) \(.pageInfo.hasNextPage)"' 2>/dev/null) \
    || { echo "check_unavailable:review_threads"; return 1; }
  case "$out" in
    "0 false") return 0 ;;
    [1-9]*" "*) echo "unresolved_review_threads:${out%% *}"; return 1 ;;
    *) echo "check_unavailable:review_threads"; return 1 ;;
  esac
}
