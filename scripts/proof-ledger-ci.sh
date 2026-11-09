#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# proof-ledger-ci.sh — run one checker and record its receipts, so the proof
# ledger's statuses are derived from what this run checked
# (scripts/proof-ledger.sh derive). The checker's own exit status is the
# step's: a receipt is written for a failure too, then the step fails.
#
#   proof-ledger-ci.sh ipkg PKG.ipkg     idris2 --build; one receipt per module
#   proof-ledger-ci.sh verdicts          scripts/check-verdicts.sh; tests/accept, tests/reject
#   proof-ledger-ci.sh tlc SPEC.tla CFG  TLC; the receipt carries distinct states and depth
#   proof-ledger-ci.sh exunit FILE...    mix test FILE...; one receipt per file
#
# Receipts go to $PROOF_RECEIPTS. Run from the repository root.

set -uo pipefail

ledger="scripts/proof-ledger.sh"
rc=0

case "${1:-}" in
  ipkg)
    lane=idris2
    pkg=${2:?ipkg}
    mapfile -t files < <(bash "$ledger" ipkg-files "$pkg")
    [ "${#files[@]}" -gt 0 ] || { echo "::error::proof-ledger-ci: $pkg lists no modules"; exit 2; }
    idris2 --build "$pkg" || rc=$?
    ;;
  verdicts)
    lane=verdicts
    files=(tests/accept/*.idr tests/reject/*.idr)
    bash scripts/check-verdicts.sh || rc=$?
    ;;
  tlc)
    spec=${2:?spec} cfg=${3:?cfg} jar=${TLA2TOOLS_JAR:?TLA2TOOLS_JAR}
    files=("$spec")
    log=$(mktemp)
    ( cd "$(dirname "$spec")" && java -XX:+UseParallelGC -cp "$jar" tlc2.TLC \
        -workers auto -config "$(basename "$cfg")" "$(basename "$spec")" ) | tee "$log" || rc=$?
    if grep -q "Error:" "$log"; then
      echo "::error::TLC found an invariant / property violation"; rc=1
    fi
    if ! grep -q "Model checking completed. No error has been found." "$log"; then
      echo "::error::TLC did not report clean completion"; rc=1
    fi
    states=$(grep -oE '[0-9]+ distinct states found, 0 states left on queue' "$log" | tail -1 | cut -d' ' -f1)
    depth=$(grep -oE 'depth of the complete state graph search is [0-9]+' "$log" | grep -oE '[0-9]+$')
    cp "$log" "$(dirname "$spec")/tlc.log"
    if [ "$rc" -eq 0 ] && [ -n "$states" ] && [ -n "$depth" ]; then
      bash "$ledger" receipt tlc pass "${files[@]}" -- distinct_states="$states" depth="$depth"
    else
      bash "$ledger" receipt tlc fail "${files[@]}"
      [ "$rc" -ne 0 ] || { echo "::error::TLC reported no state count or depth"; rc=1; }
    fi
    exit "$rc"
    ;;
  exunit)
    lane=exunit
    shift
    files=("$@")
    [ "${#files[@]}" -gt 0 ] || { echo "::error::proof-ledger-ci: exunit needs test files"; exit 2; }
    mix test "${files[@]}" || rc=$?
    ;;
  *)
    echo "usage: $0 ipkg PKG | verdicts | tlc SPEC CFG | exunit FILE..." >&2
    exit 2
    ;;
esac

if [ "$rc" -eq 0 ]; then
  bash "$ledger" receipt "$lane" pass "${files[@]}"
else
  bash "$ledger" receipt "$lane" fail "${files[@]}"
fi
exit "$rc"
