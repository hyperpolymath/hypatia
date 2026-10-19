#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# estate-pin-integrity.sh — find every denylisted action pin in an estate and,
# with --rewrite, produce the substituted file trees ready for a branch + PR.
#
# WHY THIS EXISTS
#
# github/codeql-action v4.38.1 (1c5b675653bb5c22dbe9b12b556ec555138e09fd) is
# rejected by GitHub at workflow start-up: startup_failure, zero jobs, no logs.
# The estate rolled it back on 2026-09-22. It came back within hours, because
# the rollback was applied as a *relabel*: files across the estate now carry
#
#     uses: github/codeql-action/init@1c5b6756...  # v4.38.0
#
# i.e. the poisoned commit wearing a correct-looking comment. A gate that greps
# for the version string sees nothing. A gate that greps for the commit sees
# ~131 files. That is the failure this sweep exists to make impossible.
#
# SAFETY
#
#   * Dry run unless --rewrite (which only writes patched trees under --out;
#     it never touches a remote).
#   * A repair that cannot be proved is never emitted: the substitution must
#     leave the file's `uses:` site count unchanged AND remove every denylisted
#     token from it. Both are asserted per file before the tree is written.
#   * `--via clone` is the authoritative mode (reads the files themselves);
#     `--via search` is the fast reconnaissance mode and can lag the index.
#
# EXIT CODES: 0 clean, 1 error, 2 findings present (or the sweep could not
#             see everything: a search that failed or overflowed, a
#             candidate that could not be read — never reported clean),
#             3 nothing to do
#
# usage: estate-pin-integrity.sh [--org ORG]... [--policy FILE] [--out DIR]
#                                [--via search|clone] [--rewrite]
#                                [--max-repos N] [--sample N]
set -euo pipefail

SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SELF_DIR/../.." && pwd)"

POLICY="$REPO_ROOT/.machine_readable/merge-orchestration/pr-automerge-policy.json"
OUT_DIR="${PWD}/.pin-integrity"
ORGS=()
VIA="search"
REWRITE=0
MAX_REPOS=0

while [ $# -gt 0 ]; do
  case "$1" in
    --org)        ORGS+=("$2"); shift 2 ;;
    --policy)     POLICY="$2"; shift 2 ;;
    --out)        OUT_DIR="$2"; shift 2 ;;
    --via)        VIA="$2"; shift 2 ;;
    --rewrite)    REWRITE=1; shift ;;
    --max-repos)  MAX_REPOS="$2"; shift 2 ;;
    -h|--help)    sed -n '2,40p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "unknown argument: $1" >&2; exit 1 ;;
  esac
done

[ ${#ORGS[@]} -eq 0 ] && ORGS=("hyperpolymath")
[ -f "$POLICY" ] || { echo "policy not found: $POLICY" >&2; exit 1; }
command -v gh >/dev/null || { echo "gh is required" >&2; exit 1; }
command -v jq >/dev/null || { echo "jq is required" >&2; exit 1; }

mkdir -p "$OUT_DIR"
FINDINGS="$OUT_DIR/pin-findings.jsonl"
PLAN="$OUT_DIR/pin-repair-plan.jsonl"
: > "$FINDINGS"
: > "$PLAN"

log() { printf '%s\n' "$*" >&2; }

# `hyperpolymath` is a user account and `metadatastician` is an organisation;
# the two need different endpoints. Trying orgs first keeps the common case to
# one request.
list_repos() {
  local org="$1"
  gh api "orgs/${org}/repos?per_page=100" --paginate \
    --jq '.[] | select(.archived == false and .fork == false and .disabled == false) | .name' 2>/dev/null \
  || gh api "users/${org}/repos?per_page=100" --paginate \
    --jq '.[] | select(.archived == false and .fork == false and .disabled == false) | .name' 2>/dev/null
}

# ─── Policy extraction ───────────────────────────────────────────────────
mapfile -t DENY_SHAS < <(jq -r '.pin_denylist[] | .blocked_shas[]?' "$POLICY")
mapfile -t DENY_VERSIONS < <(jq -r '.pin_denylist[] | .blocked_versions[]?' "$POLICY")

if [ ${#DENY_SHAS[@]} -eq 0 ] && [ ${#DENY_VERSIONS[@]} -eq 0 ]; then
  log "policy carries no blocked pins — nothing to sweep"
  exit 3
fi

# Every token form a poisoned pin can take: bare SHA, bare version, v-version.
REF_ALTERNATION=$(
  { printf '%s\n' "${DENY_SHAS[@]}"
    for v in "${DENY_VERSIONS[@]}"; do printf '%s\n' "$v" "v$v"; done
  } | sed 's/\./\\./g' | paste -sd'|' -
)

# What may follow a ref token: a closing quote (`uses: "…@sha"`), a flow
# delimiter (`{ uses: …@sha }`, `…@sha,`), whitespace, a comment, or the end
# of the line. A pin written in any of these forms is the same pin.
REF_END='([]"'"'"'[:space:]#,}]|$)'
# The `uses` key, quoted or bare, with the quote a quoted value opens with:
# `uses: x@…`, `"uses": "x@…"`, `{ uses: x@… }` all name an action.
USES_KEY='["'"'"']?uses["'"'"']?[[:space:]]*:[[:space:]]*["'"'"']?'

known_good_sha_for() {
  jq -r --arg a "$1" '.pin_denylist[] | select(.action == $a) | .known_good_sha' "$POLICY" | head -n1
}

known_good_version_for() {
  jq -r --arg a "$1" '.pin_denylist[] | select(.action == $a) | .known_good_version' "$POLICY" | head -n1
}

# Relabel a pin's inline comment ONLY when the comment leads with a version
# claim. Both estate shapes are covered — `# v4.38.0` and
# `# v4.38.0 (4.38.1 blocked estate-wide; nexia-list#100)` — and prose that
# merely mentions a version mid-sentence is never rewritten.
#
# Takes the comment and a replacement version without `v`; writes the result
# to stdout without a newline. A rewritten comment has one leading `#`;
# a bare claim gains `# `, and existing spacing after `#` is preserved.
# An empty version or the literal `null` returns the comment unchanged.
#
# This must stay byte-for-byte identical in spirit to PinIntegrity.relabel/2 in
# lib/rules/pin_integrity.ex. Two readers, one policy, same edit.
relabel_comment() {
  local comment="$1" version="$2"
  [ -n "$version" ] && [ "$version" != "null" ] || { printf '%s' "$comment"; return; }
  local body="${comment#\#}"
  local lead="${body%%[![:space:]]*}"
  local trimmed="${body#"$lead"}"
  # A bare claim (no `#`, no leading space) normalises to `# vX`, as relabel/2 does.
  [ "$body" = "$comment" ] && [ -z "$lead" ] && lead=" "
  if printf '%s' "$trimmed" | grep -qE '^v?[0-9]+(\.[0-9]+)*([[:space:]]|$)'; then
    printf '%s%s' "#${lead}" \
      "$(printf '%s' "$trimmed" | sed -E "0,/^v?[0-9]+(\.[0-9]+)*/s//v${version}/")"
  else
    printf '%s' "$comment"
  fi
}

log "policy $(jq -r '.version' "$POLICY") · blocked SHAs ${#DENY_SHAS[@]} · blocked versions ${#DENY_VERSIONS[@]} · orgs ${ORGS[*]} · via ${VIA}"

# ─── Phase 1: enumerate candidate files ──────────────────────────────────
HITS="$OUT_DIR/hits.tsv"
: > "$HITS"

# Search mode is reconnaissance: one code-search query per token, two seconds
# apart. An entry that blocks a window of commits (PIN-003 names 545) would
# cost one query per SHA — ~18 minutes of sleeps for one action — so such an
# entry is searched by its action path instead (one query), and phase 2
# confirms each hit against the full token alternation. Small denylists keep
# the exact per-SHA search.
mapfile -t SEARCH_TOKENS < <(jq -r '
  .pin_denylist[]
  | (if ((.blocked_shas // []) | length) > 8 then [.action] else (.blocked_shas // []) end)[],
    (.blocked_versions // [])[]' "$POLICY" | sort -u)

# GitHub's code search returns at most 1,000 results per query (ten pages
# of 100), whatever the true count. A broad query that overflows the cap
# leaves candidates phase 2 never sees, so the sweep says so and refuses a
# clean verdict: a search that could not see everything is INCOMPLETE, and
# only `--via clone` can clear the org. A query that FAILS (expired auth,
# rate limit, outage: `gh api` exits non-zero) is the same case, not an
# empty page — zero results from a request that never answered is not a
# clean org.
incomplete=0
failed=0
if [ "$VIA" = "search" ]; then
  for org in "${ORGS[@]}"; do
    for token in "${SEARCH_TOKENS[@]}"; do
      page=1
      while :; do
        if ! gh api "search/code?q=${token}+org:${org}&per_page=100&page=${page}" \
             > "$OUT_DIR/.search.json" 2> "$OUT_DIR/.search.err"; then
          log "  ! ${org}: search for '${token}' (page ${page}) failed: $(head -c 200 "$OUT_DIR/.search.err" | tr '\n' ' ') — phase 1 is INCOMPLETE for this org, use --via clone"
          failed=$((failed + 1))
          break
        fi
        n=$(jq -r '.items[]? | "'"${org}"'\t\(.repository.name)\t\(.path)"' "$OUT_DIR/.search.json" 2>/dev/null \
              | tee -a "$HITS" | wc -l)
        # GitHub answers a timed-out search with HTTP 200, the matches found
        # so far and `incomplete_results: true` — a partial page, on any page.
        if [ "$(jq -r '.incomplete_results // false' "$OUT_DIR/.search.json" 2>/dev/null)" = "true" ]; then
          log "  ! ${org}: '${token}' (page ${page}) is marked incomplete_results: GitHub timed the search out and answered with a partial page — phase 1 is INCOMPLETE for this org, use --via clone"
          incomplete=$((incomplete + 1))
        fi
        if [ "$page" -eq 1 ]; then
          total=$(jq -r '.total_count // 0' "$OUT_DIR/.search.json" 2>/dev/null || echo 0)
          if [ "${total:-0}" -gt 1000 ] 2>/dev/null; then
            log "  ! ${org}: '${token}' has ${total} results; GitHub shows only the first 1000 — phase 1 is INCOMPLETE for this org, use --via clone"
            incomplete=$((incomplete + 1))
          fi
        fi
        [ "$n" -lt 100 ] && break
        page=$((page + 1))
        [ "$page" -gt 10 ] && break
        sleep 2
      done
      sleep 2
    done
  done
  sort -u "$HITS" -o "$HITS"
else
  WORK="$OUT_DIR/clones"
  mkdir -p "$WORK"
  for org in "${ORGS[@]}"; do
    mapfile -t reponames < <(list_repos "$org")
    [ "$MAX_REPOS" -gt 0 ] && reponames=("${reponames[@]:0:$MAX_REPOS}")
    log "  cloning ${#reponames[@]} repo(s) in ${org}"
    for name in "${reponames[@]}"; do
      d="$WORK/$name"
      [ -d "$d/.git" ] || git clone --depth 1 --quiet "https://github.com/${org}/${name}.git" "$d" 2>/dev/null || {
        log "  ! clone failed: ${org}/${name}"; continue; }
      while IFS= read -r f; do
        printf '%s\t%s\t%s\n' "$org" "$name" "${f#"$d"/}" >> "$HITS"
      done < <(grep -rlE "$REF_ALTERNATION" "$d" --include='*.yml' --include='*.yaml' --include='*.lock' --include='*.toml' 2>/dev/null || true)
    done
  done
  sort -u "$HITS" -o "$HITS"
fi

total_hits=$(wc -l < "$HITS")
log "phase 1: ${total_hits} candidate file(s)"

# What phase 1 could not see. A sweep that saw everything and found nothing
# is clean; one that could not see everything never is.
unseen() {
  local parts=()
  [ "$incomplete" -gt 0 ] && parts+=("${incomplete} search page(s) overflowed GitHub's 1,000-result cap or came back marked incomplete")
  [ "$failed" -gt 0 ] && parts+=("${failed} search(es) failed")
  [ "${unread:-0}" -gt 0 ] && parts+=("${unread} candidate file(s) could not be read")
  local IFS=';'
  printf '%s' "${parts[*]}"
}

if [ "$total_hits" -eq 0 ]; then
  if [ "$incomplete" -gt 0 ] || [ "$failed" -gt 0 ]; then
    echo "INCOMPLETE: no denylisted pin tokens in the visible search results for ${ORGS[*]}, but $(unseen); a clean verdict needs --via clone"
    exit 2
  fi
  echo "clean: no denylisted pin tokens found in ${ORGS[*]}"
  exit 0
fi

# ─── Phase 2: confirm, classify, and build the repaired tree ─────────────
# A phase-1 candidate is a file the search (or grep) associated with a
# token; for a broad action-path query that includes every healthy caller.
# Only a file in which a denylisted token is CONFIRMED is a finding, and
# only confirmed findings decide the exit code.
repairable=0
flagged=0
confirmed=0
unread=0

while IFS=$'\t' read -r org repo path; do
  [ -n "${path:-}" ] || continue

  content=$(gh api "repos/${org}/${repo}/contents/${path}?ref=HEAD" --jq '.content' 2>/dev/null | base64 -d 2>/dev/null || true)
  if [ -z "$content" ]; then
    log "  ! cannot read ${org}/${repo}/${path} (binary, missing, or no permission)"
    unread=$((unread + 1))
    continue
  fi

  printf '%s\n' "$content" > "$OUT_DIR/.current"

  sites_before=$(grep -cE "(^|[{,[:space:]])${USES_KEY}" "$OUT_DIR/.current" || true)
  denied_before=$(grep -cE "@(${REF_ALTERNATION})(${REF_END})" "$OUT_DIR/.current" || true)
  [ "$denied_before" -eq 0 ] && continue
  confirmed=$((confirmed + 1))

  # Scope guard: a repair is only meaningful where a pin is *live*. A
  # documentation code block quoting the poisoned SHA is a finding, not a
  # repair — rewriting prose is how a sweep generates false confidence.
  case "$path" in
    .github/workflows/*|.github/actions/*)    in_scope=1 ;;
    */.github/workflows/*|*/.github/actions/*) in_scope=1 ;;
    *.lock)                                   in_scope=1 ;;
    *)                                        in_scope=0 ;;
  esac

  # Rewrite one line at a time so each pin gets the known-good SHA for ITS
  # action, and so an action with no known-good pin blocks the whole file.
  fixable=1
  actions=""
  [ "$in_scope" -eq 0 ] && fixable=0
  : > "$OUT_DIR/.rewritten"
  while IFS= read -r line || [ -n "$line" ]; do
    if printf '%s\n' "$line" | grep -qE "@(${REF_ALTERNATION})(${REF_END})"; then
      # The action named before the `@`, whatever quotes or flow braces
      # surround the key and the value.
      action=$(printf '%s\n' "$line" | grep -oE "${USES_KEY}[^@\"'[:space:]]+@" | head -n1 \
                 | sed -E "s/^${USES_KEY}//; s/@\$//")
      base=$(printf '%s\n' "$action" | awk -F/ '{print $1"/"$2}')
      good=$(known_good_sha_for "$base")
      actions="${actions}${base} "
      if [ -z "$good" ] || [ "$good" = "null" ]; then
        fixable=0
        printf '%s\n' "$line" >> "$OUT_DIR/.rewritten"
      else
        good_version=$(known_good_version_for "$base")
        swapped=$(printf '%s\n' "$line" | sed -E "s%@(${REF_ALTERNATION})(${REF_END})%@${good}\2%g")
        head="${swapped%%#*}"
        if [ "$head" != "$swapped" ]; then
          tail_comment="#${swapped#*#}"
          printf '%s%s\n' "$head" "$(relabel_comment "$tail_comment" "$good_version")" >> "$OUT_DIR/.rewritten"
        else
          printf '%s\n' "$swapped" >> "$OUT_DIR/.rewritten"
        fi
      fi
    else
      printf '%s\n' "$line" >> "$OUT_DIR/.rewritten"
    fi
  done < "$OUT_DIR/.current"

  sites_after=$(grep -cE "(^|[{,[:space:]])${USES_KEY}" "$OUT_DIR/.rewritten" || true)
  denied_after=$(grep -cE "@(${REF_ALTERNATION})(${REF_END})" "$OUT_DIR/.rewritten" || true)

  status="flag"
  reason="owner_review"
  if [ "$fixable" -eq 1 ] && [ "$sites_after" -eq "$sites_before" ] && [ "$denied_after" -eq 0 ] \
     && ! cmp -s "$OUT_DIR/.current" "$OUT_DIR/.rewritten"; then
    status="repairable"
    reason="substitution"
  elif [ "$in_scope" -eq 0 ]; then
    reason="out_of_scope_file"
  elif [ "$fixable" -eq 0 ]; then
    reason="no_known_good_pin_for_action"
  elif [ "$sites_after" -ne "$sites_before" ]; then
    reason="repair_would_change_pin_site_count"
  fi

  jq -cn \
    --arg repo "${org}/${repo}" --arg path "$path" --arg status "$status" \
    --arg reason "$reason" --arg actions "$(printf '%s' "$actions" | tr -s '[:space:]' ' ' | sed -E 's/^ //; s/ $//')" \
    --argjson denied "$denied_before" --argjson sites "$sites_before" \
    '{repo:$repo, path:$path, rule:"PI001", status:$status, repair:$reason,
      actions:($actions|split(" ")|map(select(.!=""))),
      denylisted_pin_sites:$denied, uses_sites:$sites}' >> "$FINDINGS"

  if [ "$status" = "repairable" ]; then
    repairable=$((repairable + 1))
    if [ "$REWRITE" -eq 1 ]; then
      mkdir -p "$OUT_DIR/patched/${repo}/$(dirname "$path")"
      cp "$OUT_DIR/.rewritten" "$OUT_DIR/patched/${repo}/${path}"
    fi
  else
    flagged=$((flagged + 1))
    log "  flagged ${org}/${repo}/${path}: ${reason}"
  fi
done < "$HITS"

if [ "$confirmed" -eq 0 ]; then
  if [ "$incomplete" -gt 0 ] || [ "$failed" -gt 0 ] || [ "$unread" -gt 0 ]; then
    echo "INCOMPLETE: none of the ${total_hits} candidate file(s) carries a denylisted pin, but $(unseen); a clean verdict needs --via clone"
    exit 2
  fi
  echo "clean: ${total_hits} candidate file(s) read, none carries a denylisted pin in ${ORGS[*]}"
  exit 0
fi

# One repair PR per repository.
if [ "$repairable" -gt 0 ]; then
  jq -src '
    map(select(.status == "repairable"))
    | group_by(.repo)
    | .[]
    | {repo: .[0].repo,
       files: length,
       paths: [.[].path],
       action: "pin_rollback_denylisted",
       method: "squash",
       safety: "arm_auto",
       route: "Patch-Bridge",
       branch: "hypatia/pin-integrity-known-good",
       rule: "PI001"}
  ' "$FINDINGS" > "$PLAN"
fi

log ""
log "─── pin-integrity sweep ────────────────────────────────────────────"
log "  candidate files            : ${total_hits}"
log "  confirmed denylisted       : ${confirmed}"
log "  mechanically repairable    : ${repairable}"
log "  needs owner review         : ${flagged}"
log "  repos with a repair ready  : $(grep -c . "$PLAN" 2>/dev/null || echo 0)"
log "  findings                   : ${FINDINGS}"
log "  repair plan                : ${PLAN}"
[ "$REWRITE" -eq 1 ] && log "  repaired trees             : ${OUT_DIR}/patched"
log "  NOTE: this sweep never writes to a remote. Branch + PR creation is"
log "        the token-bearing actuator's job — docs/operations/estate-automerge.adoc"
if [ "$incomplete" -gt 0 ] || [ "$failed" -gt 0 ] || [ "$unread" -gt 0 ]; then
  log "  INCOMPLETE: $(unseen); what they hid was not seen — rerun with --via clone"
fi

exit 2
