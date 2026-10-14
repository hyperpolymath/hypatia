#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Fetch the sibling proof repositories hypatia cites into _build/dependencies,
# each at a recorded commit, with plain git (the residual-evidence-types
# pattern).  A checkout that does not resolve to the pinned commit is a
# failure, never a fallback to whatever the remote currently serves.
#
# Hypatia does not import these sources: Idris2 cannot consume Agda, so the
# ABI records that follow them are transcriptions (ASSUMED correspondence).
# The pins make the citation checkable — scripts/check-sibling-citations.sh
# fails if a cited name is absent at the pin, or if a module header names a
# different commit from the one fetched here.  Re-pin deliberately, one
# commit per pin.
set -euo pipefail
cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.."
deps=_build/dependencies

fetch() {
  local name=$1 repo=$2 sha=$3 dir
  dir=$deps/$name
  if [[ ! -d "$dir/.git" ]]; then
    git init -q "$dir"
    git -C "$dir" remote add origin "https://github.com/$repo.git"
  fi
  git -C "$dir" fetch -q --depth 1 origin "$sha"
  git -C "$dir" checkout -q --detach FETCH_HEAD
  test "$(git -C "$dir" rev-parse HEAD)" = "$sha"
  echo "$name @ $sha"
}

mkdir -p "$deps"
# HYP-01 verdict record (src/Hypatia/ABI/Verdict.idr): Warrant / Epi / SoundWarrant.
fetch epistemic-types hyperpolymath/epistemic-types 6a68de3141a981d766fd006b312f0919ade334bf
