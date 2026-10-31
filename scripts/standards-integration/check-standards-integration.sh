#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# check-standards-integration.sh — verify this repo's shared-governance
# integration with metadatastician/standards is published, immutable and
# internally consistent. Offline by default; --verify-remote additionally
# asks GitHub (via gh) that the pinned standards commit exists.
#
# Outcome semantics (never collapse these into "green"):
#   exit 0  outcome=pass        category=findings-channel-ready
#             The integration contract holds. This says NOTHING about
#             whether a Hypatia scan found anything; it only proves the
#             channel through which findings would arrive is sound.
#   exit 1  outcome=fail        category=config
#             A real contract violation (floating/divergent pin, lock
#             drift, dangling transitive lock record, floating scanner
#             ref, secrets: inherit). Treat as a category-3
#             tool/configuration failure, not a code finding.
#   exit 3  outcome=unavailable category=tool-unavailable
#             A required tool (or the remote check) could not run. This
#             is NOT a pass and NOT a finding; report it as unavailable.
# Usage: check-standards-integration.sh [--root DIR] [--verify-remote]
set -uo pipefail

ROOT="."
VERIFY_REMOTE=0
while [ $# -gt 0 ]; do
  case "$1" in
    --root) ROOT="${2:?--root needs a directory}"; shift 2 ;;
    --verify-remote) VERIFY_REMOTE=1; shift ;;
    *) echo "outcome=unavailable category=tool-unavailable severity=error detail=unknown argument: $1" >&2; exit 3 ;;
  esac
done

fail() { echo "outcome=fail category=config severity=high detail=$1"; exit 1; }
unavailable() { echo "outcome=unavailable category=tool-unavailable severity=error detail=$1"; exit 3; }

# Every external command invoked below is preflighted, so missing
# tooling is outcome=unavailable (exit 3), never misreported as a
# repository configuration defect (review feedback, PR #992).
for tool in grep sed awk sort head; do
  command -v "$tool" >/dev/null 2>&1 || unavailable "required tool '$tool' not on PATH"
done

WF="$ROOT/.github/workflows"
LOCK="$WF/actions.lock"
CALLERS=(governance.yml hypatia-scan.yml mirror.yml scorecard.yml secret-scanner.yml)

# Parse actual YAML `uses:` lines only. Strip inline comments first so
# comment text cannot create a phantom reusable ref or disguise its value.
standards_uses() {
  sed -E 's/[[:space:]]+#.*$//' "$1" \
    | grep -E '^[[:space:]]*uses:[[:space:]]*metadatastician/standards/\.github/workflows/' || true
}

for f in "${CALLERS[@]}"; do
  [ -f "$WF/$f" ] || fail "missing standards caller workflow: $f"
done
[ -f "$LOCK" ] || fail "missing .github/workflows/actions.lock"

# --- Admission/source resolution: every standards reusable must resolve --
# to exactly one immutable 40-hex commit across all callers. A branch,
# tag-only or divergent pin is what produced zero-job startup_failure and
# stale-policy failures estate-wide; it is rejected here, before CI.
# Inline comments are stripped and EVERY matching uses: entry is
# validated and retained — a second reusable job in one caller cannot
# hide behind the first one's pin.
pins=()
for f in "${CALLERS[@]}"; do
  uses_lines=$(standards_uses "$WF/$f")
  [ -n "$uses_lines" ] || fail "$f declares no metadatastician/standards reusable"
  while IFS= read -r line; do
    ref=$(printf '%s' "$line" | sed -E 's/.*uses:[[:space:]]*//; s/[[:space:]].*$//')
    if ! printf '%s' "$ref" | grep -qE '^metadatastician/standards/\.github/workflows/[A-Za-z0-9._-]+\.yml@[0-9a-f]{40}$'; then
      fail "$f has a non-immutable standards ref (must be a full 40-hex SHA): $ref"
    fi
    pins+=("${ref##*@}")
  done <<EOF_USES
$uses_lines
EOF_USES
done
distinct=$(printf '%s\n' "${pins[@]}" | sort -u | grep -c . || true)
[ "$distinct" = "1" ] || fail "standards pins diverge across callers: ${pins[*]} (bump all five + actions.lock together)"
PIN=$(printf '%s\n' "${pins[@]}" | sort -u)

# --- Lock consistency: actions.lock must record the same pin per workflow
# and as a dependency resolution. A drifted lock kills workflows at
# startup_failure with zero jobs (hypatia#723), so this is checked offline.
for f in "${CALLERS[@]}"; do
  block=$(awk -v want="'.github/workflows/$f':" '
    index($0, want) { inblock=1; print; next }
    inblock && /^    / && $0 !~ /^        / { exit }
    inblock { print }' "$LOCK")
  # Every pin used by this caller (normally exactly one) must be locked.
  while IFS= read -r line; do
    ref=$(printf '%s' "$line" | sed -E 's/.*uses:[[:space:]]*//; s/[[:space:]].*$//')
    printf '%s\n' "$block" | grep -qF "metadatastician/standards@${ref##*@}" \
      || fail "actions.lock workflows entry for $f does not record ${ref}"
  done <<EOF_LOCK
$(standards_uses "$WF/$f")
EOF_LOCK
done
grep -qF "'metadatastician/standards@$PIN':" "$LOCK" \
  || fail "actions.lock has no dependency entry for metadatastician/standards@$PIN"
grep -A2 -F "'metadatastician/standards@$PIN':" "$LOCK" | grep -qF "commit: 'sha1-$PIN'" \
  || fail "actions.lock dependency for standards@$PIN does not resolve to sha1-$PIN"

# --- Lock transitive closure: every workflow ref and every transitive
# uses: recorded in the lock must resolve to an exact top-level
# dependency record (same invariant as
# Hypatia.Rules.ActionsLock.validate_transitive_dependencies/1, whose
# authoritative-lockfile test caught a dangling
# actions/download-artifact@9000827… record on PR #992). Normalisation
# mirrors ActionsLock.normalise_ref/1: owner/repo lower-cased, ref kept.
closure=$(awk '
  function norm(r,   n, parts, a) {
    n = split(r, a, "@")
    if (n < 2) return tolower(r)
    split(a[1], parts, "/")
    return tolower(parts[1] "/" parts[2]) "@" a[2]
  }
  /^workflows:/ { section="workflows"; next }
  /^dependencies:/ { section="deps"; next }
  section == "workflows" && /^        - / {
    line=$0; sub(/^        - /, "", line); gsub(/['"'"' ]/, "", line)
    wf[++nwf] = line
  }
  section == "deps" && /^    '\''/ {
    line=$0; sub(/^    '\''/, "", line); sub(/'\'':.*/, "", line)
    deps[norm(line)] = 1
  }
  section == "deps" && /^            - / {
    line=$0; sub(/^            - /, "", line); gsub(/['"'"' ]/, "", line)
    trans[++ntrans] = line
  }
  END {
    missing = ""
    for (i=1; i<=nwf; i++) if (!(norm(wf[i]) in deps)) missing = missing " workflow:" wf[i]
    for (i=1; i<=ntrans; i++) if (!(norm(trans[i]) in deps)) missing = missing " transitive:" trans[i]
    print missing
  }' "$LOCK")
[ -z "$closure" ] || fail "actions.lock dependency closure is dangling:$closure"

# --- Scanner source: Hypatia must never scan a PR with the PR's own rules,
# and must never float. The ACTUAL hypatia-ref value is compared, with
# inline comments stripped: a 40-hex string in a comment does not make a
# floating value pinned, and github.sha on a pull_request (the merge
# commit, containing the PR's own changes) is not an acceptable source.
for f in governance.yml hypatia-scan.yml; do
  raw=$(grep -E '^[[:space:]]*hypatia-ref:' "$WF/$f" | head -n1 || true)
  [ -n "$raw" ] || fail "$f does not pass hypatia-ref explicitly (scanner source would float)"
  value=$(printf '%s' "$raw" | sed -E 's/^[[:space:]]*hypatia-ref:[[:space:]]*//; s/[[:space:]]+#.*$//; s/[[:space:]]+$//')
  if [ "$value" != '${{ github.event.pull_request.base.sha || github.sha }}' ] \
     && ! printf '%s' "$value" | grep -qE '^[0-9a-f]{40}$'; then
    fail "$f hypatia-ref is neither the base/event SHA expression nor a 40-hex SHA: $value"
  fi
done

# --- Least privilege: reusables get only their declared secrets.
caller_paths=()
for f in "${CALLERS[@]}"; do caller_paths+=("$WF/$f"); done
if grep -qE 'secrets:[[:space:]]*inherit' "${caller_paths[@]}"; then
  fail "a standards caller uses 'secrets: inherit' (must pass only declared secrets)"
fi

# --- Optional remote proof: the pin must be a published commit.
if [ "$VERIFY_REMOTE" = "1" ]; then
  command -v gh >/dev/null 2>&1 \
    || unavailable "gh not on PATH; remote pin verification not run (offline checks above are not a substitute)"
  resolved=$(gh api "repos/metadatastician/standards/commits/$PIN" --jq .sha 2>/dev/null) \
    || unavailable "gh could not resolve metadatastician/standards@$PIN (network/auth); pin not remotely verified"
  [ "$resolved" = "$PIN" ] || fail "remote standards commit mismatch: pinned $PIN, resolved $resolved"
  echo "remote=verified repo=metadatastician/standards commit=$PIN"
fi

echo "outcome=pass category=findings-channel-ready severity=none standards_pin=$PIN callers=5 detail=immutable pin, lock closure and scanner-source contract hold; this is not a scan result"
exit 0
