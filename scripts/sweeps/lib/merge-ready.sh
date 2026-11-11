# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# merge-ready.sh: per-repository gates that must hold before the sweep queues
# `gh pr merge --auto`. Sourced by estate-pr-automerge.sh; no side effects.
#
# WHY: with no required checks, `--auto` merges at once, unchecked. With a
# required check that never reported, it waits forever. On a red default branch
# it merges code nobody can verify. A bypass actor can skip the checks entirely.
#
#   merge_ready FULL_NAME BASE_BRANCH
#     exit 0                      the repository may be given --auto
#     exit 1 + one reason line    skip it, and record the reason
#
# Every API failure fails CLOSED (`check_unavailable:<what>`). A 404 on the
# classic protection endpoint is not a failure: it means "no protection".
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

# Required status-check contexts, from classic protection AND rulesets.
_required_contexts() {
  local full="$1" base="$2" a b rc
  a=$(_gh_lines api "repos/${full}/branches/${base}/protection/required_status_checks" \
        --jq '[.contexts[]?, .checks[]?.context] | .[]') && rc=0 || rc=$?
  [ "$rc" -eq 0 ] || [ "$rc" -eq 2 ] || return 1
  [ "$rc" -eq 2 ] && a=""
  b=$(_gh_lines api "repos/${full}/rules/branches/${base}" \
        --jq '.[] | select(.type == "required_status_checks") | .parameters.required_status_checks[].context') || return 1
  printf '%s\n%s\n' "$a" "$b" | sed '/^$/d' | sort -u
}

# Every context that has reported on the branch head (check runs + statuses).
_reported_contexts() {
  local full="$1" base="$2" runs stats
  runs=$(_gh_lines api "repos/${full}/commits/${base}/check-runs?per_page=100" --paginate \
           --jq '.check_runs[].name') || return 1
  stats=$(_gh_lines api "repos/${full}/commits/${base}/status" --jq '.statuses[].context') || return 1
  printf '%s\n%s\n' "$runs" "$stats" | sed '/^$/d' | sort -u
}

# Print "red" if anything on the branch head failed.
_head_is_red() {
  local full="$1" base="$2" runs stats
  runs=$(_gh_lines api "repos/${full}/commits/${base}/check-runs?per_page=100" --paginate \
           --jq '.check_runs[] | select(.conclusion == "failure" or .conclusion == "timed_out" or .conclusion == "startup_failure") | .name') || return 1
  stats=$(_gh_lines api "repos/${full}/commits/${base}/status" \
           --jq '.statuses[] | select(.state == "failure" or .state == "error") | .context') || return 1
  [ -n "${runs}${stats}" ] && echo red
  return 0
}

# Print "bypass" if any active ruleset carries a bypass actor.
_has_bypass_actor() {
  local full="$1" ids id n
  ids=$(_gh_lines api "repos/${full}/rulesets" --jq '.[] | select(.enforcement == "active") | .id') || return 1
  for id in $ids; do
    n=$(_gh_lines api "repos/${full}/rulesets/${id}" --jq '(.bypass_actors // []) | length') || return 1
    [ "${n:-0}" -gt 0 ] && { echo bypass; return 0; }
  done
  return 0
}

merge_ready() {
  local full="$1" base="$2" required reported c

  required=$(_required_contexts "$full" "$base") || { echo "check_unavailable:required_checks"; return 1; }
  [ -n "$required" ] || { echo "no_required_checks"; return 1; }

  reported=$(_reported_contexts "$full" "$base") || { echo "check_unavailable:default_branch_checks"; return 1; }
  while IFS= read -r c; do
    grep -Fxq -- "$c" <<<"$reported" || { echo "required_check_never_reported:${c}"; return 1; }
  done <<<"$required"

  c=$(_head_is_red "$full" "$base") || { echo "check_unavailable:default_branch_state"; return 1; }
  [ -z "$c" ] || { echo "default_branch_red"; return 1; }

  c=$(_has_bypass_actor "$full") || { echo "check_unavailable:rulesets"; return 1; }
  [ -z "$c" ] || { echo "ruleset_bypass_actor_present"; return 1; }
  return 0
}
