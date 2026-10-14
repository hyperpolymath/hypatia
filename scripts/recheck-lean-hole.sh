#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# recheck-lean-hole.sh — tie HYP-01's positive control to a prover run.
#
# tests/accept/LeanHoleRecheckSound.idr builds a `SoundVerdict` for the
# code_safety Lean proof-hole detector from a Lean 4 re-check of the
# detector's soundness fixture.  Idris2 only checks that record's shape; the
# digests in it are claims about a run.  This script makes them falsifiable:
#
#   1. read fixturePath, proverVersion, targetHash, artefactHash and the span
#      line from the control (the control is the single source; nothing is
#      restated here);
#   2. the fixture's SHA-256 must equal targetHash;
#   3. `lean --version` must report proverVersion;
#   4. run `lean <fixturePath>` from the repository root; the combined output
#      must carry Lean's own "declaration uses" diagnostic at the span line,
#      and its SHA-256 must equal artefactHash.
#
# Negative controls, run every time, so a pass is never vacuous:
#   N1. Lean on a hole-free module prints no such diagnostic (the diagnostic
#       is evidence, not boilerplate);
#   N2. the same fixture under another path yields a different artefact
#       digest (the digest is sensitive to the run it records).
#
# Usage: scripts/recheck-lean-hole.sh
# Env:   LEAN (default: lean)
# Exit:  0 = control agrees with a fresh run; 1 = disagreement; 2 = usage/IO.

set -euo pipefail
cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.."

lean="${LEAN:-lean}"
control=tests/accept/LeanHoleRecheckSound.idr
work=_build/recheck-lean-hole
mkdir -p "$work"

[[ -f "$control" ]] || { echo "ERROR: $control not found" >&2; exit 2; }

field() {
  # $1 = extended regex with one capture group, applied to the control.
  sed -nE "s/$1/\\1/p" "$control" | head -1
}

target="$(field '^fixturePath = "([^"]+)".*')"
version="$(field '^  "([0-9]+\.[0-9]+\.[0-9]+)"$')"
target_hash="$(field '^  \(MkSha256 "([0-9a-f]{64})"\) -- targetHash$')"
artefact_hash="$(field '^  \(MkSha256 "([0-9a-f]{64})"\) -- artefactHash$')"
span_line="$(field '^ +\(MkSpan fixturePath ([0-9]+) [0-9]+\) [0-9]+$')"

for v in target version target_hash artefact_hash span_line; do
  if [[ -z "${!v}" ]]; then
    echo "ERROR: could not read $v from $control" >&2
    exit 2
  fi
done
echo "control: target=$target lean=$version span=$span_line"

sha() { sha256sum "$1" | cut -d' ' -f1; }

# 2. The bytes Lean reads are the bytes the control names.
actual="$(sha "$target")"
if [[ "$actual" != "$target_hash" ]]; then
  echo "ERROR: $target digest $actual != targetHash $target_hash" >&2
  echo "       The fixture changed. If that was deliberate, re-run Lean on it and" >&2
  echo "       re-measure both digests in $control (see its header)." >&2
  exit 1
fi
echo "PASS: targetHash matches $target"

# 3. The prover is the one the control names.
lean_version="$("$lean" --version)"
echo "$lean_version"
if [[ "$lean_version" != *"version $version,"* ]]; then
  echo "ERROR: control names Lean $version, toolchain is: $lean_version" >&2
  exit 1
fi

# 4. A fresh re-check reproduces the artefact.
artefact="$work/artefact.log"
"$lean" "$target" >"$artefact" 2>&1 || true
cat "$artefact"
if ! grep -Eq "^${target}:${span_line}:[0-9]+: warning: declaration uses \`sorry\`$" "$artefact"; then
  echo "ERROR: Lean did not report the proof hole at ${target}:${span_line}" >&2
  exit 1
fi
actual="$(sha "$artefact")"
if [[ "$actual" != "$artefact_hash" ]]; then
  echo "ERROR: artefact digest $actual != artefactHash $artefact_hash" >&2
  exit 1
fi
echo "PASS: Lean re-check confirms the hole at ${target}:${span_line}; artefactHash matches"

# N1. No hole, no diagnostic.
printf '%s\n' 'theorem ok : 1 + 1 = 2 := rfl' >"$work/NoHole.lean"
if ! "$lean" "$work/NoHole.lean" >"$work/nohole.log" 2>&1; then
  cat "$work/nohole.log"
  echo "ERROR: negative control N1 did not typecheck" >&2
  exit 1
fi
if grep -q 'declaration uses' "$work/nohole.log"; then
  cat "$work/nohole.log"
  echo "ERROR: negative control N1: diagnostic printed for a hole-free module" >&2
  exit 1
fi
echo "PASS: N1 hole-free module prints no proof-hole diagnostic"

# N2. Another run, another digest.
cp "$target" "$work/Moved.lean"
"$lean" "$work/Moved.lean" >"$work/moved.log" 2>&1 || true
if [[ "$(sha "$work/moved.log")" == "$artefact_hash" ]]; then
  echo "ERROR: negative control N2: a different run produced the recorded digest" >&2
  exit 1
fi
echo "PASS: N2 a different run yields a different artefact digest"

echo "PASS: positive control agrees with a fresh Lean $version re-check"
