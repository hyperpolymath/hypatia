#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# estate-dependabot-hold.sh — stop the generator, not just the symptom.
#
# WHY THIS IS NOT THE PIN SWEEP
#
# estate-pin-integrity.sh repairs the *symptom*: it removes the poisoned pin
# from a file. But the pin came back twice because something keeps proposing it.
# On 2026-09-26 the census was:
#
#   133 poisoned files across 104 repositories
#   103 of those 104 use dependabot `groups:`
#    73 have no `ignore` rule for the action at all
#    31 have `ignore: - dependency-name: "github/codeql-action"`
#     0 have the cure
#
# and the 31 are the trap, not the protection: standards#1037 established that
# an update-level `ignore` is NOT honoured inside `groups:` — the poisoned bump
# arrives anyway, SHA-swapped, inline comment and all. A closed pull request
# without a hold is a pull request you will see again next week.
#
# Dependabot's group config has `exclude-patterns` for exactly this. It is the
# only form of the hold that survives grouping, and it is scoped to the action
# rather than holding the whole group (a blanket `*` hold is a maintenance
# hostage: it stops every unrelated bump too).
#
# WHAT IT EDITS
#
# One insertion, under the wildcard pattern of a `github-actions` group:
#
#         patterns:
#           - "*"
#   +     exclude-patterns:
#   +       - "github/codeql-action*"
#
# Nothing else is touched. The existing `ignore` rule is left in place: it is
# harmless, and it is what protects any update that is not part of a group.
# If the anchor cannot be found the repository is reported as a finding, never
# guessed at — a repair that cannot be proved is not a repair.
#
# IDEMPOTENT BY CONSTRUCTION. Run it twice: the second run reports `ok` for
# every repository it rewrote. That is what makes this a one-time fix rather
# than a chore: --verify-only exits 1 when any repository is still exposed, so
# the same script is both the cure and the check that the cure is holding.
#
# EXIT CODES
#   0 nothing to do / all verified
#   1 error (bad arguments, missing tooling)
#   2 work to do (dry run with exposed repositories), --rewrite or --verify-only
#   3 no repositories scanned
#
# usage: estate-dependabot-hold.sh [--org ORG]... [--policy FILE] [--out DIR]
#                                  [--repo OWNER/NAME] [--only-file FILE]
#                                  [--rewrite] [--verify-only] [--max-repos N]
#
#   --only-file FILE   restrict to repositories listed one per line. Use the
#                      pin sweep's census ("the repositories that actually
#                      carry the poison") when you do not want a defensive
#                      hold in all 300+ repositories that merely have the
#                      vulnerable dependabot shape.
set -euo pipefail

SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SELF_DIR/../.." && pwd)"

POLICY="$REPO_ROOT/.machine_readable/merge-orchestration/pr-automerge-policy.json"
OUT_DIR="${PWD}/.dependabot-hold"
ORGS=()
ONE_REPO=""
ONLY_FILE=""
REWRITE=0
VERIFY_ONLY=0
MAX_REPOS=0

while [ $# -gt 0 ]; do
  case "$1" in
    --org)         ORGS+=("$2"); shift 2 ;;
    --policy)      POLICY="$2"; shift 2 ;;
    --out)         OUT_DIR="$2"; shift 2 ;;
    --repo)        ONE_REPO="$2"; shift 2 ;;
    --only-file)   ONLY_FILE="$2"; shift 2 ;;
    --rewrite)     REWRITE=1; shift ;;
    --verify-only) VERIFY_ONLY=1; shift ;;
    --max-repos)   MAX_REPOS="$2"; shift 2 ;;
    -h|--help)     sed -n '2,52p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "unknown argument: $1" >&2; exit 1 ;;
  esac
done

[ ${#ORGS[@]} -eq 0 ] && ORGS=("hyperpolymath" "metadatastician")
[ -f "$POLICY" ] || { echo "policy not found: $POLICY" >&2; exit 1; }
command -v gh >/dev/null || { echo "gh is required" >&2; exit 1; }
command -v jq >/dev/null || { echo "jq is required" >&2; exit 1; }
command -v python3 >/dev/null || { echo "python3 is required (YAML edit)" >&2; exit 1; }

mkdir -p "$OUT_DIR"
findings="$OUT_DIR/hold-findings.jsonl"
plan="$OUT_DIR/hold-plan.jsonl"
: > "$findings"
: > "$plan"

log() { printf '%s\n' "$*" >&2; }
cleanup() { rm -f "$OUT_DIR"/.db.*.tmp 2>/dev/null || true; }

# The action and the glob that excludes it, from the policy — one source of
# truth, so a second denylisted action is a policy edit and not a code change.
mapfile -t HELD_ACTIONS < <(jq -r '.pin_denylist[] | select((.blocked_shas | length) > 0) | .action' "$POLICY")
[ ${#HELD_ACTIONS[@]} -gt 0 ] || { echo "policy denylist is empty" >&2; exit 1; }

GLOBS=()
for a in "${HELD_ACTIONS[@]}"; do GLOBS+=("${a}*"); done
GLOB_JSON=$(printf '%s\n' "${GLOBS[@]}" | jq -Rsc 'split("\n") | map(select(. != ""))')

log "policy $(jq -r '.version' "$POLICY") · holding ${#HELD_ACTIONS[@]} action(s) · orgs ${ORGS[*]}"

list_repos() {
  local org="$1"
  gh api "orgs/${org}/repos?per_page=100" --paginate \
    --jq '.[] | select(.archived == false and .fork == false and .disabled == false) | "\(.name)"' 2>/dev/null \
  || gh api "users/${org}/repos?per_page=100" --paginate \
    --jq '.[] | select(.archived == false and .fork == false and .disabled == false) | "\(.name)"' 2>/dev/null
}

# ─── The edit ────────────────────────────────────────────────────────────
# Deliberately a line-oriented insertion rather than a YAML round-trip: a
# re-serialised dependabot.yml would reorder keys and reformat comments in
# every repository, which makes the diff unreviewable and the sweep
# unusable. This touches exactly two lines.
edit_dependabot() {
  local file="$1" globs_json="$2"
  python3 - "$file" "$globs_json" <<'PY'
import json, sys

path, globs_json = sys.argv[1], sys.argv[2]
globs = json.loads(globs_json)

with open(path) as fh:
    raw = fh.read()

original_had_newline = raw.endswith("\n")
lines = raw.split("\n")
if lines and lines[-1] == "":
    # split() leaves a trailing empty element; the join below restores it.
    lines = lines[:-1]


def indent_of(line):
    return len(line) - len(line.lstrip(" "))


# Find every `patterns:` list inside a groups: block and take the ones whose
# list contains a bare "*", i.e. the group that swallows everything.
anchors = []
for i, line in enumerate(lines):
    if line.strip() != "patterns:":
        continue
    base = indent_of(line)
    j = i + 1
    has_wildcard = False
    last_content = i
    while j < len(lines) and (lines[j].strip() == "" or indent_of(lines[j]) > base):
        if lines[j].strip() != "":
            last_content = j
        s = lines[j].strip()
        if s in ('- "*"', "- '*'", "- *"):
            has_wildcard = True
        j += 1
    if not has_wildcard:
        continue
    # Only groups inside a github-actions update: walk back to the nearest
    # `- package-ecosystem:` line.
    ecosystem = None
    k = i
    while k >= 0:
        if lines[k].strip().startswith("- package-ecosystem:"):
            ecosystem = lines[k].split(":", 1)[1].strip().strip('"\'')
            break
        k -= 1
    if ecosystem == "github-actions":
        # Attach to the last real entry, never to a trailing blank line.
        anchors.append((i, last_content, base))

if not anchors:
    print("NO_ANCHOR")
    sys.exit(3)

# Insert after the last wildcard entry of each anchor, deepest first so the
# earlier indices stay valid.
inserted = 0
for _, last_idx, base in reversed(anchors):
    # Align the new entry with the wildcard entry above it, not with some
    # indentation of our own: the diff should look like the file's own style.
    item_indent = indent_of(lines[last_idx])
    block = [f"{' ' * max(base, 0)}exclude-patterns:"]
    block += [f"{' ' * item_indent}- \"{g}\"" for g in globs]
    if any("exclude-patterns:" in ln for ln in lines[last_idx + 1:last_idx + 4]):
        continue
    lines[last_idx + 1:last_idx + 1] = block
    inserted += 1

if inserted == 0:
    print("ALREADY_HELD")
    sys.exit(4)

text = "\n".join(lines)
if original_had_newline and not text.endswith("\n"):
    text += "\n"

# Prove the patch before writing it. This is the check that caught an anchor
# attached to a trailing blank line silently emitting an entry at column 0 in
# a 102-file run — one broken file among a hundred is exactly the kind of
# thing a sweep must not do.
try:
    import yaml  # noqa: F401
except ImportError:
    yaml = None

if yaml is not None:
    try:
        yaml.safe_load(text)
    except Exception as exc:  # pragma: no cover - depends on the file
        print("EDIT_UNPARSEABLE " + str(exc).split("\n")[0])
        sys.exit(5)

with open(path, "w") as fh:
    fh.write(text)

print(f"HELD {inserted}")
PY
}

# ─── Scan ────────────────────────────────────────────────────────────────
exposed=0
held=0
acked=0
scanned=0

trap cleanup EXIT

# The list is built first and the loop reads it as a redirect, not a pipeline:
# a piped loop runs in a subshell, and a subshell is how a sweep reports
# "nothing to do" while holding a file full of findings.
if [ -n "$ONLY_FILE" ]; then
  [ -s "$ONLY_FILE" ] || { echo "only-file is empty or missing: $ONLY_FILE" >&2; exit 1; }
  grep -v '^[[:space:]]*$' "$ONLY_FILE" | sort -u > "$OUT_DIR/repos.txt"
elif [ -n "$ONE_REPO" ]; then
  printf '%s\n' "$ONE_REPO" > "$OUT_DIR/repos.txt"
else
  : > "$OUT_DIR/repos.txt"
  for org in "${ORGS[@]}"; do
    list_repos "$org" | sed "s|^|${org}/|" >> "$OUT_DIR/repos.txt"
  done
fi
log "repositories to check: $(wc -l < "$OUT_DIR/repos.txt")"

while read -r full; do
  [ -n "$full" ] || continue
  scanned=$((scanned + 1))
  [ $((scanned % 25)) -eq 0 ] && log "  … ${scanned}"

  # Fetch to a file, not into a variable: `$(...)` strips trailing newlines,
  # and a rewritten file that silently loses its final newline is a diff
  # nobody will trust.
  db=""
  db_tmp="$OUT_DIR/.db.$$.tmp"
  : > "$db_tmp"
  for p in .github/dependabot.yml .github/dependabot.yaml dependabot.yml; do
    if gh api "repos/${full}/contents/${p}" --jq '.content' 2>/dev/null | base64 -d > "$db_tmp" 2>/dev/null && [ -s "$db_tmp" ]; then
      db="$p"; break
    fi
  done
  content=$(cat "$db_tmp")

  if [ -z "$db" ]; then
    jq -cn --arg repo "$full" '{repo:$repo, status:"no_dependabot", detail:"no dependabot.yml anywhere — nothing to hold"}' >> "$plan"
    continue
  fi

  cures=$(printf '%s' "$content" | grep -c 'exclude-patterns' || true)
  gulps=$(printf '%s' "$content" | grep -cE '^\s+- "\*"|^\s+- .\*.$' || true)

  if [ "$cures" -gt 0 ]; then
    held=$((held + 1))
    jq -cn --arg repo "$full" --arg path "$db" \
      '{repo:$repo, path:$path, status:"held", detail:"exclude-patterns present"}' >> "$plan"
    continue
  fi

  if [ "$gulps" -eq 0 ]; then
    # No wildcard group: a plain `ignore` is honoured here, so this is a
    # judgement call, not a mechanical edit.
    acked=$((acked + 1))
    jq -cn --arg repo "$full" --arg path "$db" \
      '{repo:$repo, path:$path, status:"no_group_wildcard", detail:"no grouped wildcard; the update-level ignore is honoured — verify by hand"}' >> "$plan"
    continue
  fi

  exposed=$((exposed + 1))
  if [ "$REWRITE" -eq 1 ]; then
    mkdir -p "$OUT_DIR/rewritten/${full}/$(dirname "$db")"
    cp "$db_tmp" "$OUT_DIR/rewritten/${full}/${db}"
    result=$(edit_dependabot "$OUT_DIR/rewritten/${full}/${db}" "$GLOB_JSON" 2>&1 || true)
    case "$result" in
      HELD*)
        jq -cn --arg repo "$full" --arg path "$db" --arg r "$result" \
          '{repo:$repo, path:$path, status:"rewritten", detail:$r}' >> "$plan" ;;
      ALREADY_HELD)
        jq -cn --arg repo "$full" --arg path "$db" \
          '{repo:$repo, path:$path, status:"held", detail:"already carried the exclusion"}' >> "$plan" ;;
      EDIT_UNPARSEABLE*)
        jq -cn --arg repo "$full" --arg path "$db" --arg r "$result" \
          '{repo:$repo, path:$path, status:"flag", detail:("edit would not parse, left untouched: " + $r)}' >> "$plan"
        # Put the original back: an unparseable dependabot.yml is worse than
        # an exposed one.
        gh api "repos/${full}/contents/${db}" --jq '.content' 2>/dev/null | base64 -d > "$OUT_DIR/rewritten/${full}/${db}" 2>/dev/null || true ;;
      NO_ANCHOR)
        jq -cn --arg repo "$full" --arg path "$db" \
          '{repo:$repo, path:$path, status:"flag", detail:"no github-actions wildcard group to hang the exclusion on — needs a human"}' >> "$plan"
        rm -rf "$OUT_DIR/rewritten/${full}" ;;
      *)
        jq -cn --arg repo "$full" --arg path "$db" --arg r "$result" \
          '{repo:$repo, path:$path, status:"flag", detail:("edit failed: " + $r)}' >> "$plan"
        rm -rf "$OUT_DIR/rewritten/${full}" ;;
    esac
  else
    jq -cn --arg repo "$full" --arg path "$db" \
      '{repo:$repo, path:$path, status:"exposed", detail:"wildcard group with no exclusion — dependabot will re-raise the held action here"}' >> "$plan"
  fi
done < "$OUT_DIR/repos.txt"

# The loop above writes the plan; the summary reads it back so the two can
# never disagree.
summary() {
  jq -s '
    {
      repositories_scanned: length,
      by_status: (group_by(.status) | map({key: .[0].status, value: length}) | from_entries),
      held: [.[] | select(.status == "held") | .repo],
      exposed: [.[] | select(.status == "exposed" or .status == "rewritten") | .repo],
      flags: [.[] | select(.status == "flag" or .status == "no_group_wildcard") | {repo, detail}]
    }' "$plan"
}

log ""
log "─── dependabot hold ───────────────────────────────────────────────"
summary > "$OUT_DIR/hold-summary.json"
jq -r '"  scanned          : \(.repositories_scanned)\n  already held     : \(.by_status.held // 0)\n  exposed          : \((.exposed | length))\n  flagged for hand : \((.flags | length))"' "$OUT_DIR/hold-summary.json" >&2
log "  plan             : ${plan}"
log "  summary          : ${OUT_DIR}/hold-summary.json"

if [ "$VERIFY_ONLY" -eq 1 ]; then
  n_exposed=$(jq -r '(.exposed | length)' "$OUT_DIR/hold-summary.json")
  if [ "$n_exposed" -gt 0 ]; then
    log ""
    log "  VERIFY FAILED: ${n_exposed} repositories can still receive the held action."
    exit 2
  fi
  log ""
  log "  VERIFY OK: every scanned repository holds the pinned action."
  exit 0
fi

if [ "$REWRITE" -eq 1 ]; then
  log ""
  log "  rewritten trees  : ${OUT_DIR}/rewritten/ (one dependabot.yml per repository)"
  log "  next             : review a diff, then branch + PR per repository"
  exit 0
fi

if [ "$exposed" -eq 0 ]; then
  log ""
  log "  nothing to do — every scanned repository already holds the action."
  exit 0
fi

log ""
log "  dry run — nothing written. Pass --rewrite to produce the corrected"
log "  dependabot.yml files, or --verify-only to use this as a check."
exit 2
