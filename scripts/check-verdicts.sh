#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# check-verdicts.sh — the mechanical falsifier for HYP-01's verdict record.
#
# src/Hypatia/ABI/Verdict.idr claims that a `Sound` verdict can be built only
# from a `Mechanical` support whose prover re-check `Confirms`.  A claim about
# what does NOT typecheck is tested by asking the typechecker:
#
#   1. the ABI package builds (the positive set, first: a broken toolchain or
#      a missing import must never turn into a "successful" rejection);
#   2. Verdict.idr and Retraction.idr contain no escape-hatch marker at all;
#   3. every module under tests/accept/ is ACCEPTED (positive controls);
#   4. every module under tests/reject/ is REFUSED, and refused only at its
#      declaration `invalid`, with the diagnostic its `-- EXPECT:` line
#      names — an error anywhere else, a different diagnostic (an arity
#      slip is also "refused at invalid"), or a refusal for a missing name,
#      import or parse, is a broken fixture, not a pass.
#
# An empty tests/accept/ or tests/reject/ is a broken checkout, not a pass.
# The Lean half of the positive control (re-running the prover and comparing
# digests) is scripts/recheck-lean-hole.sh.
#
# Usage: scripts/check-verdicts.sh
# Env:   IDRIS2 (default: idris2)
# Exit:  0 = all controls behave; 1 = a control misbehaved.

set -euo pipefail
cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.."

idris2="${IDRIS2:-idris2}"
logs=_build/check-verdicts
mkdir -p "$logs"

"$idris2" --version

# 1. Positive set: the ABI package, Verdict included.
"$idris2" --build src/abi/hypatia-abi.ipkg
export IDRIS2_PATH="$PWD/build/abi/ttc${IDRIS2_PATH:+:$IDRIS2_PATH}"

# 2. No escape hatch in the modules the rejections are about.
for module in src/Hypatia/ABI/Verdict.idr src/Hypatia/ABI/Retraction.idr; do
  if grep -nE 'believe_me|assert_total|assert_smaller|idris_crash|postulate|sorry|admit|native_decide|Admitted|unsafeCoerce|Obj\.magic' \
       "$module"; then
    echo "ERROR: escape-hatch marker in $module" >&2
    exit 1
  fi
  echo "PASS: no escape-hatch marker in $module"
done

shopt -s nullglob
accepts=(tests/accept/*.idr)
rejects=(tests/reject/*.idr)
shopt -u nullglob
if (( ${#accepts[@]} == 0 )); then
  echo 'ERROR: no positive controls found under tests/accept' >&2
  exit 1
fi
if (( ${#rejects[@]} == 0 )); then
  echo 'ERROR: no expected-rejection modules found under tests/reject' >&2
  exit 1
fi

# 3. Positive controls must typecheck.
for file in "${accepts[@]}"; do
  name="$(basename -- "$file" .idr)"
  log="$logs/accept-$name.log"
  if ! "$idris2" --source-dir tests/accept --build-dir "$logs/build" --check "$file" >"$log" 2>&1; then
    cat "$log"
    echo "ERROR: positive control refused: $name" >&2
    exit 1
  fi
  if grep -q '^Error' "$log"; then
    cat "$log"
    echo "ERROR: positive control reported an error: $name" >&2
    exit 1
  fi
  echo "PASS: accepted positive control $name"
done

# 4. Expected rejections must be refused, at `invalid`, for a typing reason.
for file in "${rejects[@]}"; do
  name="$(basename -- "$file" .idr)"
  log="$logs/reject-$name.log"
  if "$idris2" --source-dir tests/reject --build-dir "$logs/build" --check "$file" >"$log" 2>&1; then
    cat "$log"
    echo "ERROR: invalid verdict accepted: $name" >&2
    exit 1
  fi
  expect="$(sed -n 's/^-- EXPECT: //p' "$file" | head -1)"
  if [[ -z "$expect" ]]; then
    echo "ERROR: $file declares no '-- EXPECT: <diagnostic>' line" >&2
    exit 1
  fi
  errors="$(grep -c '^Error' "$log" || true)"
  if [[ "$errors" -ne 1 ]] ||
     ! grep -qF -- "$expect" "$log" ||
     ! grep -q '^Error: While processing right hand side of invalid\.' "$log" ||
     ! grep -Eq "^${name}:[0-9]+:[0-9]+--[0-9]+:[0-9]+" "$log" ||
     grep -Eq 'Undefined name|Possible cause|Can.t find import|not in the source directory|Couldn.t parse|Parse error|is not a valid' "$log"; then
    cat "$log"
    echo "ERROR: unexpected failure while checking $name (want exactly one typing error, at invalid: $expect)" >&2
    exit 1
  fi
  echo "PASS: rejected $name at invalid: $expect"
done

echo "PASS: ABI package, ${#accepts[@]} positive control(s), ${#rejects[@]} expected rejection(s)"
