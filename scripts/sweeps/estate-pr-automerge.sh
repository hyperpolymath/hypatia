#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# estate-pr-automerge.sh — decide, for every open PR in an estate, whether it
# is an unambiguous bump/chore/pin that may simply be merged and its branch
# deleted; whether it carries a poisoned pin that must be excised first; or
# whether it is one of the cases that only *looks* routine.
#
# WHY THIS IS NOT A TITLE MATCHER
#
# On 2026-09-26 the estate had 34 open PRs, every one of them a
# `chore(deps): bump …` from dependabot. Every one of them looked safe.
#
#   * 24 of them re-introduce github/codeql-action v4.38.1 — the commit
#     GitHub rejects at workflow start-up. Merging any of them breaks CodeQL
#     and the Hypatia scan on that repository. This is not hypothetical:
#     nexia-list#107 and dictask#65 both merged one, and both repositories
#     are red on main today.
#   * at least two of them hide a *major* bump inside a "grouped" chore
#     (`actions/checkout` v4.1.7 → v7.0.1, labelled `# v4` on both sides).
#
# A title matcher merges all 34. This classifier merges 9 and explains the
# other 25. That difference is the whole point of the exercise.
#
# WHAT MAKES A PR ELIGIBLE (all must hold)
#
#   1. authored by a dependency bot
#   2. every changed line is either a `uses:` pin line, a lockfile line, or
#      in a non-code metadata path
#   3. no denylisted token appears in an added line
#   4. the pin delta is *resolvable* and is not a major-version increase —
#      resolved from the action's own tag list, not from the inline comment,
#      because the comments in this estate demonstrably lie
#
# Anything failing (4) is not rejected as unsafe; it is reported as
# unverifiable. "Cannot prove safe" and "proved unsafe" are different verdicts
# and the manifest keeps them different.
#
# EXIT CODES: 0 nothing to decide, 1 error, 2 decisions written
#
# usage: estate-pr-automerge.sh [--org ORG]... [--policy FILE] [--out DIR]
#                               [--max-repos N] [--cache DIR] [--execute]
set -euo pipefail

SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SELF_DIR/../.." && pwd)"

POLICY="$REPO_ROOT/.machine_readable/merge-orchestration/pr-automerge-policy.json"
DECISIONS_IN=""
OUT_DIR="${PWD}/.pr-automerge"
CACHE_DIR="${PWD}/.pr-automerge/cache"
ORGS=()
MAX_REPOS=0
EXECUTE=0

while [ $# -gt 0 ]; do
  case "$1" in
    --org)       ORGS+=("$2"); shift 2 ;;
    --policy)    POLICY="$2"; shift 2 ;;
    --out)       OUT_DIR="$2"; shift 2 ;;
    --cache)     CACHE_DIR="$2"; shift 2 ;;
    --max-repos) MAX_REPOS="$2"; shift 2 ;;
    --execute)   EXECUTE=1; shift ;;
    --from)      DECISIONS_IN="$2"; shift 2 ;;
    -h|--help)   sed -n '2,45p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "unknown argument: $1" >&2; exit 1 ;;
  esac
done

[ ${#ORGS[@]} -eq 0 ] && ORGS=("hyperpolymath")
[ -f "$POLICY" ] || { echo "policy not found: $POLICY" >&2; exit 1; }
command -v gh >/dev/null || { echo "gh is required" >&2; exit 1; }
command -v jq >/dev/null || { echo "jq is required" >&2; exit 1; }

mkdir -p "$OUT_DIR" "$CACHE_DIR"
DECISIONS="$OUT_DIR/pr-decisions.jsonl"
: > "$DECISIONS"

log() { printf '%s\n' "$*" >&2; }

# `hyperpolymath` is a user account and `metadatastician` is an organisation;
# the two need different endpoints. Trying orgs first keeps the common case to
# one request.
# Emits `name<TAB>archived`. Archived repositories are INCLUDED: a dependabot
# PR left open on an archived repo is unmergeable by construction and is one of
# the "chores that are obviously going to be merged some day" that never will.
# Knowing about it is the difference between a backlog and an explanation.
list_repos() {
  local org="$1"
  gh api "orgs/${org}/repos?per_page=100" --paginate \
    --jq '.[] | select(.fork == false and .disabled == false) | "\(.name)\t\(.archived)"' 2>/dev/null \
  || gh api "users/${org}/repos?per_page=100" --paginate \
    --jq '.[] | select(.fork == false and .disabled == false) | "\(.name)\t\(.archived)"' 2>/dev/null
}

mapfile -t DENY_SHAS   < <(jq -r '.pin_denylist[] | .blocked_shas[]?' "$POLICY")
mapfile -t DENY_VERS   < <(jq -r '.pin_denylist[] | .blocked_versions[]?' "$POLICY")
mapfile -t DEP_BOTS    < <(jq -r '.dependency_bots[]' "$POLICY")
mapfile -t LOCKFILES   < <(jq -r '.lockfile_names[]' "$POLICY")

REF_ALT=$(
  { printf '%s\n' "${DENY_SHAS[@]}"
    for v in "${DENY_VERS[@]}"; do printf '%s\n' "$v" "v$v"; done
  } | sed 's/\./\\./g' | paste -sd'|' -
)

is_dep_bot() {
  local a="$1"
  for b in "${DEP_BOTS[@]}"; do [ "$a" = "$b" ] && return 0; done
  return 1
}

is_lockfile() {
  local base
  base="$(basename "$1")"
  for l in "${LOCKFILES[@]}"; do [ "$base" = "$l" ] && return 0; done
  return 1
}

is_metadata_path() {
  case "$1" in
    *.md|*.adoc|*.rst|*.txt|CODEOWNERS|.gitattributes|.editorconfig) return 0 ;;
    docs/*|doc/*|.github/ISSUE_TEMPLATE/*) return 0 ;;
    *) return 1 ;;
  esac
}

# Licence/SPDX files are metadata *paths* but never a metadata *change*:
# the decision contract routes them to a null authority (owner review).
# Auto-merging a licence edit would let the robot make a claim about what
# the project is.
is_licence_path() {
  local base
  base="$(basename "$1")"
  case "$base" in
    *LICENSE*|*LICENCE*|*COPYING*|*NOTICE*|*SPDX*) return 0 ;;
  esac
  case "$1" in
    LICENSES/*) return 0 ;;
  esac
  return 1
}

# ─── SHA → version, from the action's own tag list ───────────────────────
# The estate's inline comments were wrong often enough (a `# v4` label on the
# v7.0.1 commit) that they cannot be the source of truth. This resolves
# against the upstream tags and caches per action.
resolve_version() {
  local action="$1" sha="$2"
  local cache="$CACHE_DIR/$(printf '%s' "$action" | tr '/' '_').json"
  if [ ! -s "$cache" ]; then
    gh api "repos/${action}/tags?per_page=100" \
      --jq '[.[] | {tag: .name, sha: .commit.sha}]' > "$cache" 2>/dev/null || echo '[]' > "$cache"
  fi
  jq -r --arg sha "$sha" '
    [.[] | select(.sha == $sha) | .tag] | first // empty
  ' "$cache" 2>/dev/null | sed 's/^v//'
}

major_of() { printf '%s' "${1:-}" | sed -E 's/^v?([0-9]+).*/\1/' ; }

# ─── Enumerate open PRs ──────────────────────────────────────────────────
PRS="$OUT_DIR/open-prs.jsonl"
: > "$PRS"

for org in "${ORGS[@]}"; do
  mapfile -t reponames < <(list_repos "$org")
  [ "$MAX_REPOS" -gt 0 ] && reponames=("${reponames[@]:0:$MAX_REPOS}")
  log "scanning ${#reponames[@]} repo(s) in ${org} for open PRs"

  for row in "${reponames[@]}"; do
    name="${row%%$'\t'*}"
    archived="${row##*$'\t'}"
    # NB: `gh api --jq` does not accept --arg; the org is injected with a
    # real jq stage instead. (Silently swallowing this is how a sweep reports
    # "0 open PRs" against an estate with 35 of them.)
    gh api "repos/${org}/${name}/pulls?state=open&per_page=100" 2>/dev/null \
      | jq -c --arg org "$org" --argjson arch "${archived:-false}" '.[] | {org:$org, repo:.base.repo.name, number:.number, title:.title,
          repo_archived:$arch,
          author:.user.login, draft:.draft, mergeable:.mergeable, mergeable_state:.mergeable_state,
          created:.created_at, head_sha:.head.sha, head_ref:.head.ref, base_ref:.base.ref,
          body:(.body // ""), changed_files:.changed_files, labels:[.labels[].name]}' 2>/dev/null >> "$PRS" || true
  done
done

total=$(wc -l < "$PRS")
log "found ${total} open PR(s)"
open_repos=$(gh api "search/issues?q=org:${ORGS[0]}+is:pr+is:open&per_page=1" --jq '.total_count' 2>/dev/null || echo "?")
log "  (GitHub search index reports ${open_repos} open PR(s) for ${ORGS[0]}; a large gap means the enumeration is broken)"
if [ "$total" -eq 0 ]; then
  echo "nothing to decide"
  exit 0
fi

# ─── Classify ────────────────────────────────────────────────────────────
auto=0; excise=0; close_poison=0; flagged=0

while IFS= read -r pr; do
  org=$(jq -r '.org' <<<"$pr")
  repo=$(jq -r '.repo' <<<"$pr")
  num=$(jq -r '.number' <<<"$pr")
  title=$(jq -r '.title' <<<"$pr")
  author=$(jq -r '.author' <<<"$pr")
  draft=$(jq -r '.draft' <<<"$pr")
  mstate=$(jq -r '.mergeable_state' <<<"$pr")
  head_sha=$(jq -r '.head_sha' <<<"$pr")
  created=$(jq -r '.created' <<<"$pr")
  base_ref=$(jq -r '.base_ref' <<<"$pr")
  repo_archived=$(jq -r '.repo_archived // false' <<<"$pr")
  body=$(jq -r '.body // ""' <<<"$pr")

  # Dependabot states its own version delta in the body:
  #   "Updates `haskell-actions/setup` from 2.12.0 to 2.12.1"
  # That claim is used as a SECOND source, never as the only one. When the diff
  # and the claim disagree the PR is flagged, not merged.
  # Grouped PRs write:   Updates `owner/action` from A to B
  # Single-dep PRs write: Bumps [name](https://…) from A to B.
  # Both are captured; neither is trusted alone. A body can be edited by
  # anyone who can edit the PR, so it is corroboration, not authority.
  body_deltas=$(grep -oE '(Updates|Bumps) (\[)?`?[A-Za-z0-9_.-]+(/[A-Za-z0-9_.-]+)*`?(\])?(\([^)]*\))? from [0-9][^ ]* to [0-9][^ ]*' <<<"$body" \
    | sed -E 's/^(Updates|Bumps) (\[)?`?([A-Za-z0-9_.-]+(\/[A-Za-z0-9_.-]+)*)`?(\])?(\([^)]*\))? from ([^ ]+) to (.*)$/\3|\7|\8/; s/\.+$//' \
    | jq -Rsc 'split("\n") | map(select(. != "")) | map(split("|") | {action: .[0], from: .[1], to: .[2]})' 2>/dev/null || echo '[]')

  body_major=0
  while IFS= read -r c; do
    [ -n "$c" ] || continue
    cf=$(jq -r '.from' <<<"$c"); ct=$(jq -r '.to' <<<"$c")
    [ "$(major_of "$cf")" != "$(major_of "$ct")" ] && body_major=1
  done < <(jq -c '.[]' <<<"$body_deltas" 2>/dev/null || true)

  now_epoch=$(date -u +%s)
  created_epoch=$(date -u -d "$created" +%s 2>/dev/null || echo "$now_epoch")
  age_days=$(( (now_epoch - created_epoch) / 86400 ))

  full="$org/$repo"
  patches=$(gh api "repos/${full}/pulls/${num}/files?per_page=100" \
    --jq '.[] | {filename:.filename, patch:(.patch // "")}' 2>/dev/null || echo '')

  # file-level analysis
  files_total=$(jq -c . <<<"$patches" | wc -l)
  poison_hits=0; nonpin_change=0; lock_only=1; meta_only=1; licence_touch=0; pin_files=0
  declare -a deltas=()

  while IFS= read -r fobj; do
    [ -n "$fobj" ] || continue
    fn=$(jq -r '.filename' <<<"$fobj")
    fp=$(jq -r '.patch' <<<"$fobj")

    is_lockfile "$fn" || lock_only=0
    is_metadata_path "$fn" || meta_only=0
    is_licence_path "$fn" && licence_touch=1

    # Every +/- content line, ignoring hunk headers and file markers.
    content_lines=$(grep -E '^[-+]' <<<"$fp" | grep -vE '^(\+\+\+|---)' || true)
    [ -z "$content_lines" ] && content_lines=""

    added=$(grep -E '^\+' <<<"$content_lines" | grep -vE '^\+\+\+' || true)
    removed=$(grep -E '^-' <<<"$content_lines" | grep -vE '^---' || true)

    if [ -n "$added" ] && grep -qE "@(${REF_ALT})([[:space:]#]|$)" <<<"$added"; then
      poison_hits=$((poison_hits + 1))
    fi

    # Is every changed line a `uses:` line?
    if grep -qvE '^\s*-?\s*uses:' <<<"$content_lines" 2>/dev/null; then
      :
    fi
    only_uses=1
    while IFS= read -r cl; do
      [ -n "$cl" ] || continue
      printf '%s' "$cl" | grep -qE '^[-+]\s*-?\s*uses:' || only_uses=0
    done <<<"$content_lines"
    [ "$only_uses" -eq 1 ] && [ -n "$content_lines" ] && pin_files=$((pin_files + 1))
    [ "$only_uses" -eq 0 ] && nonpin_change=$((nonpin_change + 1))

    # pin deltas: pair removed and added `uses:` lines per action
    while IFS= read -r rline; do
      [ -n "$rline" ] || continue
      raction=$(sed -E 's/^[-+]\s*-?\s*uses:\s*([^@]+)@.*/\1/' <<<"$rline")
      rref=$(sed -E 's/^[-+]\s*-?\s*uses:\s*[^@]+@([^[:space:]#]+).*/\1/' <<<"$rline")
      base="${raction%%/*}/$(printf '%s' "$raction" | cut -d/ -f2)"
      aline=$(grep -E "^\+.*uses:\s*${raction}@" <<<"$added" | head -n1 || true)
      [ -n "$aline" ] || continue
      aref=$(sed -E 's/^\+.*uses:\s*[^@]+@([^[:space:]#]+).*/\1/' <<<"$aline")
      [ "$rref" = "$aref" ] && continue
      oldv=$(resolve_version "$base" "$rref")
      newv=$(resolve_version "$base" "$aref")
      deltas+=("${base}|${oldv:-?}|${newv:-?}")
    done <<<"$removed"
  done <<<"$(jq -c . <<<"$patches")"

  delta_json=$(printf '%s\n' "${deltas[@]:-}" | jq -Rsc 'split("\n") | map(select(. != "")) | map(split("|") | {action: .[0], from: .[1], to: .[2]})')

  # Version delta. Three sources, in decreasing authority:
  #   1. the diff      — which actions changed, and to which refs (ground truth)
  #   2. upstream tags — what version a ref is (resolved via the action's tags)
  #   3. the PR body   — dependabot's own claim, used only to fill a gap
  # A gap in (2) filled by (3) is recorded with source `body-claim`, so a
  # reviewer can see exactly which claim gated the merge. Where (2) and (3)
  # disagree the PR is flagged: a disagreement means one of them is wrong and
  # the estate has already been burned once by exactly that.
  major_delta=0; unresolved=0; conflict=0
  resolved_deltas=()

  while IFS= read -r d; do
    [ -n "$d" ] || continue
    daction=$(jq -r '.action' <<<"$d")
    from=$(jq -r '.from' <<<"$d"); to=$(jq -r '.to' <<<"$d")
    source="tags"

    shorter=$(printf '%s' "$daction" | awk -F/ '{print $NF}')
    bclaim=$(jq -c --arg a "$daction" --arg b "$shorter" '.[] | select(.action == $a or .action == $b)' \
      <<<"$body_deltas" 2>/dev/null | head -n1 || true)

    if [ -n "$bclaim" ]; then
      bfrom=$(jq -r '.from' <<<"$bclaim"); bto=$(jq -r '.to' <<<"$bclaim")
      if [ "$from" != "?" ] && [ "$to" != "?" ]; then
        { [ "$from" != "$bfrom" ] || [ "$to" != "$bto" ]; } && conflict=1
      else
        [ "$from" = "?" ] && from="$bfrom"
        [ "$to" = "?" ] && to="$bto"
        source="body-claim"
      fi
    fi

    if [ "$from" = "?" ] || [ "$to" = "?" ]; then
      unresolved=$((unresolved + 1))
      resolved_deltas+=("${daction}|?|?|unresolved")
      continue
    fi

    [ "$(major_of "$from")" != "$(major_of "$to")" ] && major_delta=1
    resolved_deltas+=("${daction}|${from}|${to}|${source}")
  done < <(jq -c '.[]' <<<"$delta_json" 2>/dev/null || true)

  # A lockfile-only PR has no `uses:` lines to attach a delta to, so the claim
  # list itself is the delta list. Without a claim there is nothing to check,
  # and "nothing to check" is not a licence to merge.
  if [ "$lock_only" -eq 1 ]; then
    [ "$body_major" -eq 1 ] && major_delta=1
    if [ "$(jq 'length' <<<"$body_deltas" 2>/dev/null || echo 0)" -eq 0 ]; then
      unresolved=1
    else
      while IFS= read -r c; do
        [ -n "$c" ] || continue
        resolved_deltas+=("$(jq -r '.action' <<<"$c")|$(jq -r '.from' <<<"$c")|$(jq -r '.to' <<<"$c")|body-claim")
      done < <(jq -c '.[]' <<<"$body_deltas" 2>/dev/null || true)
    fi
  fi

  delta_json=$(printf '%s\n' "${resolved_deltas[@]:-}" \
    | jq -Rsc 'split("\n") | map(select(. != "")) | map(split("|") | {action: .[0], from: .[1], to: .[2], source: .[3]})')

  # ── Verdict ────────────────────────────────────────────────────────────
  change_class="chore"; change_level="object"; safety="flag"; method="squash"
  pool="P3"; route="rhodibot"; disposition="flag"; blocked_by=""

  if [ "$repo_archived" = "true" ]; then
    disposition="close_archived_repo"
    blocked_by="repository_is_archived_cannot_merge"
    safety="flag"; route="rhodibot"; pool="P3"
  elif ! is_dep_bot "$author"; then
    blocked_by="not_a_dependency_bot"
  elif [ "$draft" = "true" ]; then
    blocked_by="draft"
  elif [ "$mstate" = "dirty" ] || [ "$mstate" = "CONFLICTING" ]; then
    blocked_by="merge_conflict"
  elif [ "$poison_hits" -gt 0 ] && [ "$files_total" -gt 0 ]; then
    change_class="bump"
    if [ "$major_delta" -eq 1 ]; then
      # Nothing in this PR is mergeable as-is: it is poisoned AND it carries
      # major bumps. Closing is the only correct disposition, and the reason
      # must say so rather than blaming the pin alone.
      disposition="close_poison_and_majors"
      blocked_by="introduces_denylisted_pin_and_major_bumps"
    elif [ "$nonpin_change" -eq 0 ] && [ "$lock_only" -eq 0 ]; then
      disposition="close_poison_only"
      blocked_by="introduces_denylisted_pin"
    else
      disposition="excise_poison_then_merge"
      blocked_by="introduces_denylisted_pin_alongside_wanted_updates"
    fi
    safety="flag"; route="Patch-Bridge"; pool="P1"
  elif [ "$major_delta" -eq 1 ]; then
    change_class="bump"; disposition="flag"; safety="flag"
    blocked_by="major_version_delta"
    route="rhodibot"; pool="P2"
  elif [ "$conflict" -eq 1 ]; then
    change_class="bump"; disposition="flag"; safety="flag"
    blocked_by="version_claims_conflict"
    route="rhodibot"; pool="P2"
  elif [ "$unresolved" -gt 0 ]; then
    change_class="bump"; disposition="flag"; safety="flag"
    blocked_by="pin_delta_unresolvable"
    route="rhodibot"; pool="P2"
  elif [ "$pin_files" -eq 0 ] && [ "$lock_only" -eq 0 ] &&
       jq -r '.filename' <<<"$patches" 2>/dev/null \
         | grep -qE '(^|/)(package\.json|Cargo\.toml|Project\.toml|mix\.exs|go\.mod|pyproject\.toml|composer\.json|Gemfile)$'; then
    change_class="bump"; disposition="flag"; safety="flag"
    blocked_by="dependency_manifest_not_lockfile"
    route="rhodibot"; pool="P2"
  elif [ "$lock_only" -eq 1 ]; then
    change_class="bump"; disposition="auto_merge"; safety="arm_auto"
    route="Patch-Bridge"; pool="P2"
    blocked_by="awaiting_required_checks"
  elif [ "$pin_files" -gt 0 ] && [ "$nonpin_change" -eq 0 ]; then
    change_class="bump"; disposition="auto_merge"; safety="arm_auto"
    route="Patch-Bridge"; pool="P2"
    blocked_by="awaiting_required_checks"
  elif [ "$licence_touch" -eq 1 ]; then
    change_class="chore"; disposition="flag"; safety="flag"
    route="rhodibot"; pool="P2"
    blocked_by="touches_licence_requires_owner_review"
  elif [ "$meta_only" -eq 1 ]; then
    change_class="chore"; disposition="auto_merge"; safety="arm_auto"
    route="rhodibot"; pool="P3"
    blocked_by="awaiting_required_checks"
  else
    blocked_by="not_unambiguously_classifiable"
  fi

  # Workflow edits are `meta` by the decision contract unless the change is a
  # pure pin substitution. The actuator re-proves this; the brain states it.
  if [ "$pin_files" -gt 0 ] && [ "$nonpin_change" -eq 0 ]; then
    meta_exemption="MGX-001"
  else
    meta_exemption=""
    if grep -q '\.github/workflows/' <<<"$(jq -r '.[].filename' <<<"$patches" 2>/dev/null)"; then
      change_level="meta"
      [ "$safety" = "arm_auto" ] && { safety="flag"; disposition="flag"; blocked_by="meta_no_pin_exemption"; }
    fi
  fi

  case "$disposition" in
    auto_merge)        auto=$((auto + 1)) ;;
    excise_poison_then_merge) excise=$((excise + 1)) ;;
    close_poison_only|close_poison_and_majors) close_poison=$((close_poison + 1)) ;;
    *)                 flagged=$((flagged + 1)) ;;
  esac

  vetoes='[]'
  if [ "$poison_hits" -gt 0 ]; then
    vetoes=$(jq -cn --arg r "$blocked_by" '[{bot:"Patch-Bridge", reason:$r}]')
  fi
  if [ "$change_level" = "meta" ]; then
    vetoes=$(jq -cn --argjson v "$vetoes" '$v + [{bot:"hypatia", reason:"change_level=meta"}]')
  fi

  jq -cn \
    --arg repo "$full" --argjson n "$num" --arg head "$head_sha" --arg base "$base_ref" \
    --arg author "$author" --arg author_kind "dependabot" \
    --arg cc "$change_class" --arg cl "$change_level" --arg route "$route" \
    --arg method "$method" --arg safety "$safety" --arg pool "$pool" \
    --arg disposition "$disposition" --arg blocked "$blocked_by" \
    --argjson age "$age_days" --argjson delta "$delta_json" \
    --argjson vetoes "$vetoes" --argjson poison "$poison_hits" \
    --argjson files "$files_total" --arg mex "$meta_exemption" \
    --arg version "$(jq -r '.version' "$POLICY")" \
    '{pr:{repo:$repo, number:$n, head_sha:$head, base:$base, author:$author, author_kind:$author_kind},
      change_class:$cc, change_level:$cl,
      route:{authority_bot:$route, contributing_bots:["hypatia"]},
      method:$method, method_basis:"repo-default", safety:$safety, pool:$pool,
      confidence:null,
      attestations:[{bot:"hypatia", verdict:"approve", confidence:0.9,
                     rationale:"classified from the diff, not the title"}],
      vetoes:$vetoes, clamped_by:(if $vetoes == [] then null else "veto" end),
      rationale:("policy " + $version + " · " + $disposition + " · blocked_by=" + $blocked),
      disposition:$disposition, blocked_by:$blocked, age_days:$age,
      pin_delta:$delta, denylisted_hits:$poison, files_changed:$files,
      meta_guard_exemption:$mex,
      timestamp:(now | todate)}' >> "$DECISIONS"

done < "$PRS"

log ""
log "─── PR automerge decisions ─────────────────────────────────────────"
log "  open PRs examined     : ${total}"
log "  auto_merge            : ${auto}"
log "  excise_then_merge     : ${excise}"
log "  close_and_delete      : ${close_poison}"
log "  flagged for review    : ${flagged}"
log "  decisions             : ${DECISIONS}"

if [ "$EXECUTE" -ne 1 ]; then
  exit 2
fi

# ─── Actuator ────────────────────────────────────────────────────────────
# Two rules, neither negotiable:
#
#   1. Re-verify, never trust. Every decision is re-derived from the PR as it
#      is NOW: the head commit must be the one the decision was made about,
#      and the denylist check is re-run against the live diff. A manifest is
#      evidence, not authority — the same reasoning as `--auto` merges in
#      estate-rescan.yml.
#   2. Delete the branch. A merged or closed chore whose branch lingers is
#      the same backlog one layer down.
log ""
log "─── actuating (re-verifying every decision against the live PR) ────"

MANIFEST="${DECISIONS_IN:-$DECISIONS}"
applied=0; skipped=0

while IFS= read -r d; do
  repo=$(jq -r '.pr.repo' <<<"$d"); num=$(jq -r '.pr.number' <<<"$d")
  want_sha=$(jq -r '.pr.head_sha' <<<"$d"); disp=$(jq -r '.disposition' <<<"$d")
  blocked=$(jq -r '.blocked_by' <<<"$d")

  live_sha=$(gh api "repos/${repo}/pulls/${num}" --jq '.head.sha' 2>/dev/null || echo "")
  if [ "$live_sha" != "$want_sha" ]; then
    log "  skip ${repo}#${num}: head moved (${want_sha:0:8} → ${live_sha:0:8}) — decision is stale"
    skipped=$((skipped + 1)); continue
  fi

  # Independent re-proof of the pin verdict, from the live diff.
  live_added=$(gh api "repos/${repo}/pulls/${num}/files?per_page=100" --jq '.[].patch // ""' 2>/dev/null     | grep -E '^\+' | grep -vE '^\+\+\+' || true)
  live_poison=0
  grep -qE "@(${REF_ALT})([[:space:]#]|$)" <<<"$live_added" && live_poison=1

  case "$disp" in
    auto_merge)
      if [ "$live_poison" -eq 1 ]; then
        log "  REFUSE ${repo}#${num}: live diff carries a denylisted pin — the manifest is wrong"
        skipped=$((skipped + 1)); continue
      fi
      gh pr merge "$num" --repo "$repo" --squash --auto --delete-branch >/dev/null 2>&1 \
        && { log "  merged   ${repo}#${num} (auto, branch deleted)"; applied=$((applied + 1)); } \
        || { log "  could not queue ${repo}#${num}"; skipped=$((skipped + 1)); }
      ;;
    close_poison_only|close_poison_and_majors|close_archived_repo)
      if [ "$disp" = "close_poison_only" ] && [ "$live_poison" -eq 0 ]; then
        log "  REFUSE ${repo}#${num}: manifest says poisoned, live diff disagrees"
        skipped=$((skipped + 1)); continue
      fi
      body="Closed by hypatia's estate sweep: ${blocked}.

This pull request cannot be merged as it stands. It would put a pin on the
estate denylist (\`pin_denylist\` in \`pr-automerge-policy.json\`) back into the
tree, so merging it would break the workflows it touches rather than update
them. Nothing here is lost: dependabot re-raises the wanted bumps on its next
run, and \`exclude-patterns\` in \`.github/dependabot.yml\` stops the poisoned
one being raised again.

Reopen freely if this disposition is wrong — the sweep is a robot, the
repository is yours."
      gh pr comment "$num" --repo "$repo" --body "$body" >/dev/null 2>&1 || true
      gh pr close "$num" --repo "$repo" --delete-branch >/dev/null 2>&1 \
        && { log "  closed   ${repo}#${num} (branch deleted)"; applied=$((applied + 1)); } \
        || { log "  could not close ${repo}#${num}"; skipped=$((skipped + 1)); }
      ;;
    *)
      skipped=$((skipped + 1))
      ;;
  esac
done < "$MANIFEST"

log ""
log "  applied  : ${applied}"
log "  skipped  : ${skipped} (flags, stale heads, and refused re-verifications)"
exit 0
