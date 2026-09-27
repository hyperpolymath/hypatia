#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# check-trusted-base.sh — enumerate soundness-relevant escape-hatch markers
# in the proof corpus and fail on any that is neither a deliberate scanner
# fixture nor annotated with a `hypatia: allow` pragma.
#
# This is the script docs/proof-debt.adoc instructs readers to run (#831).
# It exists so the document's inventory can be re-derived by command rather
# than edited by hand, and so a new marker can never be simultaneously
# un-annotated AND un-enumerated: the `check-trusted-base` CI job runs this
# on every PR that touches a proof file.
#
# Marker kinds (the same set verification/PROOF-STATUS.adoc audits):
#   idris-believe-or-assert   believe_me, assert_total
#   agda-postulate            postulate
#   lean-sorry-or-axiom       sorry, admit, native_decide
#   coq-axiom-or-admit        Admitted
#   rust-or-hs-unsafe         unsafeCoerce, Obj.magic
#
# Classification per match:
#   FIXTURE  under test/soundness/fixtures/  — deliberate known-bad sample
#            (expected; counted and listed, never a failure)
#   ALLOWED  comment/code line carrying `hypatia: allow <reason>`           — annotated by a human, recorded in
#            docs/proof-debt.adoc's inventory
#   COMMENT  match inside a comment-only line without a pragma              — prose, not a call site (reported)
#   DEBT     anything else                                                  — FAILS this check
#
# Scans git-tracked files only. Untracked local shadows (e.g. stale agent
# worktrees) are invisible on a fresh CI checkout and are the local tree's
# business; pass `--all` to scan untracked files too when reconciling a
# local count against docs/proof-debt.adoc.
#
# Usage: scripts/check-trusted-base.sh [--all] [root]
# Exit:  0 = no un-annotated markers outside fixtures; 1 = DEBT found; 2 = usage.

set -uo pipefail

SCAN_UNTRACKED=0
if [ "${1:-}" = "--all" ]; then
  SCAN_UNTRACKED=1
  shift
fi
ROOT="${1:-.}"

FIXTURE_PREFIX="test/soundness/fixtures/"

MARKER_RE='believe_me|assert_total|postulate|sorry|admit|native_decide|Admitted|unsafeCoerce|Obj\.magic'
EXTS='*.idr *.lean *.agda *.v *.hs *.ml'

cd "$ROOT" || exit 2

if [ "$SCAN_UNTRACKED" -eq 1 ]; then
  FILES=$(find . -type f \( -name '*.idr' -o -name '*.lean' -o -name '*.agda' -o -name '*.v' -o -name '*.hs' -o -name '*.ml' \) \
    -not -path './.git/*' -not -path './_build/*' -not -path './deps/*' | sed 's|^\./||')
else
  FILES=$(git ls-files -- $EXTS)
fi

FIXTURES=0
ALLOWED=0
COMMENTS=0
DEBT=0

while IFS= read -r file; do
  [ -f "$file" ] || continue
  # grep -n the markers; then classify each hit line.
  while IFS= read -r hit; do
    [ -z "$hit" ] && continue
    lineno="${hit%%:*}"
    line="${hit#*:}"
    case "$file" in
      ${FIXTURE_PREFIX}*)
        class=FIXTURE
        ;;
      *)
        case "$line" in
          *'hypatia: allow'*)
            class=ALLOWED
            ;;
          *)
            # Comment-only lines are prose, not call sites; anything else is
            # a call site and must be annotated or it is debt.
            stripped="$(printf '%s' "$line" | sed 's/^[[:space:]]*//')"
            case "$stripped" in
              --*|//*|\#*|\(*|\**) class=COMMENT ;;
              *) class=DEBT ;;
            esac
            ;;
        esac
        ;;
    esac

    case "$class" in
      FIXTURE) FIXTURES=$((FIXTURES + 1)) ;;
      ALLOWED) ALLOWED=$((ALLOWED + 1)) ;;
      COMMENT) COMMENTS=$((COMMENTS + 1)) ;;
      DEBT)
        DEBT=$((DEBT + 1))
        echo "::error::un-annotated trusted-base marker at ${file}:${lineno}: ${line}"
        ;;
    esac
    echo "${class} ${file}:${lineno}"
  done < <(grep -nE "$MARKER_RE" "$file" 2>/dev/null)
done <<< "$FILES"

echo "trusted-base markers: fixture=${FIXTURES} allowed=${ALLOWED} comment=${COMMENTS} debt=${DEBT}"

if [ "$DEBT" -gt 0 ]; then
  echo "check-trusted-base: FAIL — ${DEBT} marker(s) outside ${FIXTURE_PREFIX} carry no 'hypatia: allow' pragma. Add the proof, annotate the debt in docs/proof-debt.adoc, or move a deliberate fixture under ${FIXTURE_PREFIX}."
  exit 1
fi

echo "check-trusted-base: OK — every marker is a fixture or annotated"
exit 0
