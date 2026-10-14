#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# check-sibling-citations.sh — keep hypatia's citations of sibling proofs
# honest.  Run scripts/fetch-siblings.sh first.
#
# A module that transcribes a sibling's definitions names the sibling, the
# commit, the file and the definitions in its header.  This script checks,
# for every row of the table below:
#
#   1. the citing module names the commit that scripts/fetch-siblings.sh
#      fetched (prose and pin agree);
#   2. the cited file exists in the sibling at that commit;
#   3. the cited definition is declared in that file (a declaration, not
#      merely a mention: `record N`, `data N`, or `N :` at the start of a
#      line, after indentation);
#   4. the citing module's header mentions the definition by name.
#
# What this does NOT check: that the transcription corresponds to the Agda
# definition.  That correspondence stays ASSUMED until a checked one exists.
#
# Usage: scripts/check-sibling-citations.sh
# Exit:  0 = every citation resolves at its pin; 1 = a citation is stale;
#        2 = a sibling has not been fetched.

set -uo pipefail
cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." || exit 2
deps=_build/dependencies

# citing module | sibling | file in sibling | definition
citations=(
  "src/Hypatia/ABI/Verdict.idr|epistemic-types|src/EpistemicTypes/Warrant.agda|Warrant"
  "src/Hypatia/ABI/Verdict.idr|epistemic-types|src/EpistemicTypes/Warrant.agda|Epi"
  "src/Hypatia/ABI/Verdict.idr|epistemic-types|src/EpistemicTypes/Warrant.agda|SoundWarrant"
  "src/Hypatia/ABI/Verdict.idr|epistemic-types|src/EpistemicTypes/Warrant.agda|sound-epi"
  "src/Hypatia/ABI/Verdict.idr|epistemic-types|src/EpistemicTypes/Applications/RapidNJSkip.agda|no-receipt-reflection"
)

failures=0
checked=0
for row in "${citations[@]}"; do
  IFS='|' read -r citer sibling file def <<<"$row"
  dir="$deps/$sibling"
  if [[ ! -d "$dir/.git" ]]; then
    echo "::error::$sibling not fetched; run scripts/fetch-siblings.sh"
    exit 2
  fi
  pin="$(git -C "$dir" rev-parse HEAD)"
  checked=$((checked + 1))
  before=$failures

  if ! grep -qF "hyperpolymath/$sibling @ $pin" "$citer"; then
    echo "::error::$citer does not name hyperpolymath/$sibling @ $pin (the fetched pin)"
    failures=$((failures + 1))
  fi
  if [[ ! -f "$dir/$file" ]]; then
    echo "::error::$sibling @ ${pin:0:7} has no $file (cited by $citer)"
    failures=$((failures + 1))
    continue
  fi
  re="^[[:space:]]*((record|data)[[:space:]]+)?${def}([[:space:]]|$)"
  if ! grep -Eq "$re" "$dir/$file"; then
    echo "::error::$sibling @ ${pin:0:7}: $file declares no '$def' (cited by $citer)"
    failures=$((failures + 1))
  fi
  if ! grep -qw -- "$def" "$citer"; then
    echo "::error::$citer cites $sibling but never names '$def'"
    failures=$((failures + 1))
  fi
  [[ "$failures" -eq "$before" ]] && echo "ok  $citer -> $sibling @ ${pin:0:7} $file :: $def"
done

echo "sibling citations: checked ${checked}, stale ${failures}"
[[ "$failures" -eq 0 ]] || exit 1
