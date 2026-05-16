#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Surgically replace the "Submit findings to gitbot-fleet (Phase 2)" step in a
# consuming repo's hypatia-scan.yml with the canonical fixed block (Layer-1
# propagation of hyperpolymath/hypatia#252). Pure shell — the estate bans
# Python, so this replaces the earlier patch_phase2.py.
#
# Idempotent and conservative:
#   * Replaces ONLY the Phase-2 step block; every other step (incl. the
#     repo's own critical/high gate) is preserved verbatim.
#   * Already-patched (block has `continue-on-error: true`) -> exit 2.
#   * No workflow file / no Phase-2 block -> exit 3.
#   * The canonical fragment carries the conventional trailing blank-line
#     separator, so the splice yields exactly one separator.
#
# Exit codes: 0 patched, 2 already-patched, 3 not-applicable, 1 error.
#
# usage: patch_phase2.sh <workflow.yml> <fragment.yml>
set -euo pipefail

WF="${1:-}"
FRAG="${2:-}"
MARKER='      - name: Submit findings to gitbot-fleet (Phase 2)'
NEXT_RE='^      - name: '

[ -n "$WF" ] && [ -n "$FRAG" ] || { echo "usage: patch_phase2.sh <wf> <frag>" >&2; exit 1; }
[ -f "$FRAG" ] || { echo "missing fragment: $FRAG" >&2; exit 1; }
[ -f "$WF" ] || exit 3

# Start line of the Phase-2 step (exact, fixed-string, whole-line match).
START=$(grep -nFx -- "$MARKER" "$WF" | head -n1 | cut -d: -f1 || true)
[ -n "${START:-}" ] || exit 3

TOTAL=$(wc -l < "$WF")

# End = first subsequent top-level step line; else EOF+1.
END=$(awk -v s="$START" 'NR>s && /'"$NEXT_RE"'/ {print NR; exit}' "$WF" || true)
[ -n "${END:-}" ] || END=$((TOTAL + 1))

# Idempotency: bail if the existing block already carries the fix.
if sed -n "${START},$((END - 1))p" "$WF" | grep -q '^        continue-on-error: true$'; then
  exit 2
fi

TMP="$(mktemp)"
trap 'rm -f "$TMP"' EXIT
{
  [ "$START" -gt 1 ] && head -n "$((START - 1))" "$WF"
  cat "$FRAG"
  [ "$END" -le "$TOTAL" ] && tail -n "+${END}" "$WF"
} > "$TMP"

cp "$TMP" "$WF"
exit 0
