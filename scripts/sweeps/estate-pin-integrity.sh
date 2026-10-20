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
# EXIT CODES: 0 clean, 1 error, 2 confirmed findings or an INCOMPLETE run
#             (failed/partial searches, listings or clones, unread candidates,
#             or limits imposed by --max-repos or --sample),
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
SAMPLE=0

while [ $# -gt 0 ]; do
  case "$1" in
    --org)        ORGS+=("$2"); shift 2 ;;
    --policy)     POLICY="$2"; shift 2 ;;
    --out)        OUT_DIR="$2"; shift 2 ;;
    --via)        VIA="$2"; shift 2 ;;
    --rewrite)    REWRITE=1; shift ;;
    --max-repos)  MAX_REPOS="$2"; shift 2 ;;
    --sample)     SAMPLE="$2"; shift 2 ;;
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
REF_END_CLASS='[]"'"'"'[:space:]#,}]'
# One `uses:` site: the action and a denylisted token, bounded. Matched in
# either case (-i): a commit ID in upper-case hex is the same object.
SITE_RE="${USES_KEY}[^@\"'[:space:],}]+@(${REF_ALTERNATION})(${REF_END})"
SHA_ALTERNATION=$(printf '%s\n' "${DENY_SHAS[@]}" | paste -sd'|' -)

lc() { printf '%s' "$1" | tr '[:upper:]' '[:lower:]'; }

# Does the policy deny <owner/repo> at <ref>? A blocked version is blocked
# for ITS action only (`some/action@v4.38.1` is no finding when only
# codeql-action's 4.38.1 is denied); a SHA is one object and meets its entry
# in either spelling. The action is compared case-folded too: GitHub resolves
# `GitHub/CodeQL-Action` and `github/codeql-action` to one repository, so a
# mixed-case caller meets the entry its lower-case spelling would.
denylisted_pair() {
  jq -e --arg a "$(lc "$1")" --arg r "$(lc "$2")" '
    [.pin_denylist[] | select((.action | ascii_downcase) == $a)
     | (.blocked_shas // []), (.blocked_versions // []), ((.blocked_versions // []) | map("v" + .))]
    | add // [] | map(ascii_downcase) | index($r) != null' "$POLICY" >/dev/null 2>&1
}

# The action and ref of one site as `grep -o` printed it: the key and the
# trailing boundary stripped, into site_action and site_ref.
site_parts() {
  local t
  t=$(printf '%s' "$1" | sed -E "s/^${USES_KEY}//; s/${REF_END_CLASS}\$//")
  site_action="${t%@*}"; site_ref="${t##*@}"
}

# Everything that is not YAML structure, blanked IN PLACE, one output line
# per input line: a quoted scalar that is neither a key nor the value of a
# `uses` key has its text replaced by `x` (its quotes kept), a comment by
# spaces, and the content lines of a block scalar (`run: |`, `script: >-`)
# entirely by `x` after their indentation. Lengths are kept, so a site found
# on a masked line sits at the same column of the real one. A `uses:` written
# inside `- run: "echo uses: a/b@<sha>"`, a `run: |` script or a comment is
# therefore no site this sweep rewrites; a denylisted SHA there is still
# seen by line_has_denied_sha on the raw line, and flagged for the owner.
# shellcheck disable=SC2016  # an awk program, not a shell expansion
MASK_AWK='
function key_col(s,   t) { t = s; sub(/[^[:space:]-].*$/, "", t); return length(t) }
function xs(n,   t) { t = ""; while (n-- > 0) t = t "x"; return t }
function mask(s,   out, n, i, c, j, body) {
  out = ""; n = length(s); i = 1
  while (i <= n) {
    c = substr(s, i, 1)
    if (c == "#" && (i == 1 || substr(s, i - 1, 1) ~ /[[:space:]]/)) { while (i <= n) { out = out " "; i++ }; break }
    if (c == "\"" || c == sq) {
      j = i + 1
      if (c == "\"") { while (j <= n && substr(s, j, 1) != "\"") { if (substr(s, j, 1) == "\\") j++; j++ } }
      else { while (j <= n && substr(s, j, 1) != sq) j++ }
      if (j > n) j = n
      body = substr(s, i + 1, j - i - 1)
      if (substr(s, j + 1) ~ /^[[:space:]]*:/ || tolower(out) ~ USES_TAIL) out = out substr(s, i, j - i + 1)
      else out = out c xs(length(body)) (j > i ? substr(s, j, 1) : "")
      i = j + 1; continue
    }
    out = out c; i++
  }
  return out
}
BEGIN { USES_TAIL = "(^|[-{,[:space:]])[\"" sq "]?uses[\"" sq "]?[[:space:]]*:[[:space:]]*$"; in_scalar = 0 }
{
  if (in_scalar) {
    if ($0 ~ /^[[:space:]]*$/ || key_col($0) > scalar_col) { t = $0; sub(/[^[:space:]].*$/, "", t); print t xs(length($0) - length(t)); next }
    in_scalar = 0
  }
  m = mask($0); print m
  if (m ~ /:[[:space:]]*[|>][-+0-9]*[[:space:]]*$/) { in_scalar = 1; scalar_col = key_col($0) }
}'

# The masked form of one file, line for line (block scalars tracked).
mask_file() { awk -v sq="'" "$MASK_AWK" "$1"; }

# The masked form of one line on its own: used after a site on that line has
# been rewritten, which a line inside a block scalar never is.
mask_line() { printf '%s\n' "$1" | awk -v sq="'" "$MASK_AWK"; }

# The `uses:` sites of a masked line, as grep -o prints them.
masked_sites() { printf '%s\n' "$1" | grep -oiE "$SITE_RE" || true; }

# Replace ONE site on a line — the action and ref exactly as written — with
# the known-good SHA for that action; the other sites on the line keep theirs.
# The site is located on the masked line, so the same `action@ref` text
# inside a quoted scalar or a comment earlier on the line is never the one
# replaced.
replace_site() { # <line> <masked-line> <action> <ref> <good>
  PIN_LINE="$1" PIN_MASKED="$2" PIN_ACTION="$3" PIN_REF="$4" PIN_GOOD="$5" \
  awk -v sq="'" 'BEGIN {
    line = ENVIRON["PIN_LINE"]; masked = ENVIRON["PIN_MASKED"]
    a = ENVIRON["PIN_ACTION"]; r = ENVIRON["PIN_REF"]; good = ENVIRON["PIN_GOOD"]
    tail = "(^|[-{,[:space:]])[\"" sq "]?uses[\"" sq "]?[[:space:]]*:[[:space:]]*[\"" sq "]?$"
    ends = "]\"" sq " \t#,}"
    lm = tolower(masked); needle = tolower(a "@" r); from = 1
    while ((p = index(substr(lm, from), needle)) > 0) {
      p += from - 1
      nxt = substr(lm, p + length(needle), 1)
      if ((nxt == "" || index(ends, nxt) > 0) && substr(lm, 1, p - 1) ~ tail) {
        print substr(line, 1, p + length(a)) good substr(line, p + length(needle))
        exit
      }
      from = p + 1
    }
    print line
  }'
}

# A denylisted SHA token in any position (a lock record key, a fixture): a
# finding in either spelling, but not a `uses:` site this sweep rewrites.
line_has_denied_sha() {
  [ -n "$SHA_ALTERNATION" ] && printf '%s\n' "$1" | grep -qiE "@(${SHA_ALTERNATION})(${REF_END})"
}

# Lines carrying a denied pin: a `uses:` site whose action/ref pair the
# policy denies, or a denylisted SHA token anywhere else.
count_denied_lines() {
  local n=0 line mline site site_action site_ref masked
  masked=$(mktemp)
  mask_file "$1" > "$masked"
  while IFS= read -r line <&3 && IFS= read -r mline <&4; do
    if line_has_denied_sha "$line"; then n=$((n + 1)); continue; fi
    while IFS= read -r site; do
      [ -n "$site" ] || continue
      site_parts "$site"
      if denylisted_pair "$(printf '%s' "$site_action" | awk -F/ '{print $1"/"$2}')" "$site_ref"; then
        n=$((n + 1)); break
      fi
    done < <(masked_sites "$mline")
  done 3< "$1" 4< "$masked"
  rm -f "$masked"
  printf '%s' "$n"
}

# The column of a line's comment, found outside quoted scalars: a `#` at
# column 1 or after whitespace. `name: "C# build"` carries no comment; 0.
comment_column() {
  printf '%s\n' "$1" | awk -v sq="'" '{
    q = ""
    for (i = 1; i <= length($0); i++) {
      c = substr($0, i, 1)
      if (q == "") {
        if (c == "\"" || c == sq) q = c
        else if (c == "#" && (i == 1 || substr($0, i - 1, 1) ~ /[[:space:]]/)) { print i; exit }
      } else if (c == q) q = ""
    }
    print 0
  }'
}

known_good_sha_for() {
  jq -r --arg a "$(lc "$1")" '.pin_denylist[] | select((.action | ascii_downcase) == $a) | .known_good_sha' "$POLICY" | head -n1
}

known_good_version_for() {
  jq -r --arg a "$(lc "$1")" '.pin_denylist[] | select((.action | ascii_downcase) == $a) | .known_good_version' "$POLICY" | head -n1
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
skipped_repos=0
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
    # A listing or a clone that fails is a repository the authoritative mode
    # never inspected: INCOMPLETE, never a clean verdict — as a failed search is.
    if ! names=$(list_repos "$org"); then
      log "  ! ${org}: listing repositories failed — phase 1 is INCOMPLETE for this org"
      failed=$((failed + 1)); continue
    fi
    reponames=()
    [ -n "$names" ] && mapfile -t reponames <<<"$names"
    if [ "$MAX_REPOS" -gt 0 ] && [ "${#reponames[@]}" -gt "$MAX_REPOS" ]; then
      skipped_repos=$((skipped_repos + ${#reponames[@]} - MAX_REPOS))
      reponames=("${reponames[@]:0:$MAX_REPOS}")
    fi
    log "  cloning ${#reponames[@]} repo(s) in ${org}"
    # Clones live under the organisation: two organisations may each own a
    # repository of the same name, and the second must not reuse the first's.
    mkdir -p "$WORK/$org"
    for name in "${reponames[@]}"; do
      d="$WORK/$org/$name"
      [ -d "$d/.git" ] || git clone --depth 1 --quiet "https://github.com/${org}/${name}.git" "$d" 2>/dev/null || {
        log "  ! clone failed: ${org}/${name} — phase 1 is INCOMPLETE for this org"; failed=$((failed + 1)); continue; }
      while IFS= read -r f; do
        printf '%s\t%s\t%s\n' "$org" "$name" "${f#"$d"/}" >> "$HITS"
      done < <(grep -rliE "$REF_ALTERNATION" "$d" --include='*.yml' --include='*.yaml' --include='*.lock' --include='*.toml' 2>/dev/null || true)
    done
  done
  sort -u "$HITS" -o "$HITS"
fi

total_hits=$(wc -l < "$HITS")
log "phase 1: ${total_hits} candidate file(s)"
sampled=0
if [ "$SAMPLE" -gt 0 ] && [ "$total_hits" -gt "$SAMPLE" ]; then
  # A bounded run reads the first N candidates only: INCOMPLETE by construction.
  sampled=$((total_hits - SAMPLE))
  head -n "$SAMPLE" "$HITS" > "$HITS.sample" && mv "$HITS.sample" "$HITS"
  log "  --sample ${SAMPLE}: ${sampled} candidate file(s) left unread; the sweep cannot report clean"
fi

# What phase 1 could not see. A sweep that saw everything and found nothing
# is clean; one that could not see everything never is.
unseen() {
  local parts=()
  [ "$incomplete" -gt 0 ] && parts+=("${incomplete} search page(s) overflowed GitHub's 1,000-result cap or came back marked incomplete")
  [ "$failed" -gt 0 ] && parts+=("${failed} search(es), listing(s) or clone(s) failed")
  [ "$skipped_repos" -gt 0 ] && parts+=("${skipped_repos} repo(s) skipped by --max-repos")
  [ "${unread:-0}" -gt 0 ] && parts+=("${unread} candidate file(s) could not be read")
  [ "${sampled:-0}" -gt 0 ] && parts+=("${sampled} candidate file(s) left unread by --sample")
  local IFS=';'
  printf '%s' "${parts[*]}"
}

if [ "$total_hits" -eq 0 ]; then
  if [ "$incomplete" -gt 0 ] || [ "$failed" -gt 0 ] || [ "$skipped_repos" -gt 0 ] || [ "$sampled" -gt 0 ]; then
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
: > "$OUT_DIR/.unread-repos"

while IFS=$'\t' read -r org repo path; do
  [ -n "${path:-}" ] || continue

  content=$(gh api "repos/${org}/${repo}/contents/${path}?ref=HEAD" --jq '.content' 2>/dev/null | base64 -d 2>/dev/null || true)
  if [ -z "$content" ]; then
    log "  ! cannot read ${org}/${repo}/${path} (binary, missing, or no permission)"
    unread=$((unread + 1))
    printf '%s\n' "${org}/${repo}" >> "$OUT_DIR/.unread-repos"
    continue
  fi

  printf '%s\n' "$content" > "$OUT_DIR/.current"

  sites_before=$(grep -cE "(^|[{,[:space:]])${USES_KEY}" "$OUT_DIR/.current" || true)
  denied_before=$(count_denied_lines "$OUT_DIR/.current")
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

  # Rewrite one pin SITE at a time: each `uses:` site gets the known-good
  # SHA for ITS action (two actions on one flow line get two replacements);
  # an action with no known-good pin blocks the whole file; a denylisted
  # token outside a `uses:` key (a lock record, a fixture) is a finding the
  # owner reviews, never a rewrite. Only the comment after the line's one
  # replaced pin is relabelled, found outside quoted scalars.
  fixable=1
  actions=""
  outside_uses=0
  [ "$in_scope" -eq 0 ] && fixable=0
  : > "$OUT_DIR/.rewritten"
  mask_file "$OUT_DIR/.current" > "$OUT_DIR/.masked"
  while IFS= read -r line <&3 && IFS= read -r mline <&4; do
    out="$line"; mout="$mline"; replaced=0; site_seen=0; good_version=""
    while IFS= read -r site; do
      [ -n "$site" ] || continue
      site_parts "$site"
      base=$(lc "$(printf '%s' "$site_action" | awk -F/ '{print $1"/"$2}')")
      denylisted_pair "$base" "$site_ref" || continue
      site_seen=1
      actions="${actions}${base} "
      good=$(known_good_sha_for "$base")
      if [ -z "$good" ] || [ "$good" = "null" ]; then fixable=0; continue; fi
      out=$(replace_site "$out" "$mout" "$site_action" "$site_ref" "$good")
      mout=$(mask_line "$out")
      replaced=$((replaced + 1))
      good_version=$(known_good_version_for "$base")
    done < <(masked_sites "$mline")
    # A denylisted SHA left on the line that is not at a `uses:` site — the
    # line had none, or the SHA sits in a quoted scalar or a comment the
    # mask blanked — is a finding for the owner, never part of a repair.
    if line_has_denied_sha "$out" && { [ "$site_seen" -eq 0 ] || ! line_has_denied_sha "$mout"; }; then
      outside_uses=$((outside_uses + 1)); fixable=0
    fi
    # The comment is relabelled only when the line has ONE `uses:` site: on a
    # flow line with several, the trailing comment cannot be tied to the
    # replaced one (`{ uses: <blocked> }, { uses: actions/checkout@<sha> } # v7`
    # annotates checkout), so it is kept as written.
    nsites=$(printf '%s\n' "$mline" | grep -oiE "(^|[-{,[:space:]])${USES_KEY}[^@\"'[:space:],}]+@" | wc -l || true)
    if [ "$replaced" -eq 1 ] && [ "$nsites" -eq 1 ] && [ -n "$good_version" ] && [ "$good_version" != "null" ]; then
      at=$(comment_column "$out")
      if [ "$at" -gt 0 ]; then
        out="${out:0:$((at - 1))}$(relabel_comment "${out:$((at - 1))}" "$good_version")"
      fi
    fi
    printf '%s\n' "$out" >> "$OUT_DIR/.rewritten"
  done 3< "$OUT_DIR/.current" 4< "$OUT_DIR/.masked"

  sites_after=$(grep -cE "(^|[{,[:space:]])${USES_KEY}" "$OUT_DIR/.rewritten" || true)
  denied_after=$(count_denied_lines "$OUT_DIR/.rewritten")

  status="flag"
  reason="owner_review"
  if [ "$fixable" -eq 1 ] && [ "$sites_after" -eq "$sites_before" ] && [ "$denied_after" -eq 0 ] \
     && ! cmp -s "$OUT_DIR/.current" "$OUT_DIR/.rewritten"; then
    status="repairable"
    reason="substitution"
  elif [ "$in_scope" -eq 0 ]; then
    reason="out_of_scope_file"
  elif [ "$outside_uses" -gt 0 ]; then
    reason="denylisted_token_outside_uses_key"
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
  if [ "$incomplete" -gt 0 ] || [ "$failed" -gt 0 ] || [ "$skipped_repos" -gt 0 ] || [ "$unread" -gt 0 ] || [ "$sampled" -gt 0 ]; then
    echo "INCOMPLETE: none of the ${total_hits} candidate file(s) carries a denylisted pin, but $(unseen); a clean verdict needs --via clone"
    exit 2
  fi
  echo "clean: ${total_hits} candidate file(s) read, none carries a denylisted pin in ${ORGS[*]}"
  exit 0
fi

# One repair PR per repository, and only for a repository the sweep has
# proved whole: every confirmed file repairable and every candidate file
# read. Repairing one workflow while a flagged lock entry or fixture in the
# same repository keeps the denylisted pin (or an unread file may) is a
# partial repair, and arming it would merge a repository that is still
# poisoned. Such a repository is held for owner review with its findings.
held=0
if [ "$repairable" -gt 0 ]; then
  unread_repos=$(jq -Rsc 'split("\n") | map(select(. != "")) | unique' "$OUT_DIR/.unread-repos")
  jq -sr --argjson unread "$unread_repos" '
    group_by(.repo)[]
    | select(any(.status == "repairable"))
    | .[0].repo as $r
    | if any($unread[]; . == $r) then "\($r)\tunread candidate file"
      elif all(.status == "repairable") | not then
        "\($r)\tflagged sibling file(s): \([.[] | select(.status != "repairable") | .path] | join(", "))"
      else empty end
  ' "$FINDINGS" > "$OUT_DIR/.held"
  held=$(grep -c . "$OUT_DIR/.held" || true)
  while IFS=$'\t' read -r r why; do
    log "  held ${r}: repairable file(s) not planned — ${why}"
  done < "$OUT_DIR/.held"

  jq -src --argjson unread "$unread_repos" '
    group_by(.repo)
    | .[]
    | select(all(.status == "repairable"))
    | select(.[0].repo as $r | any($unread[]; . == $r) | not)
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
planned=$(grep -c . "$PLAN" 2>/dev/null || true)
log "  repos with a repair ready  : ${planned:-0}"
log "  repos held (partial repair): ${held}"
log "  findings                   : ${FINDINGS}"
log "  repair plan                : ${PLAN}"
[ "$REWRITE" -eq 1 ] && log "  repaired trees             : ${OUT_DIR}/patched"
log "  NOTE: this sweep never writes to a remote. Branch + PR creation is"
log "        the token-bearing actuator's job — docs/operations/estate-automerge.adoc"
if [ "$incomplete" -gt 0 ] || [ "$failed" -gt 0 ] || [ "$skipped_repos" -gt 0 ] || [ "$unread" -gt 0 ] || [ "$sampled" -gt 0 ]; then
  log "  INCOMPLETE: $(unseen); what they hid was not seen — rerun with --via clone"
fi

exit 2
