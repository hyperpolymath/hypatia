#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# check-findings-schema.sh — validate scan output against schema/findings.ncl.
#
#   scripts/check-findings-schema.sh <findings.json>          escript array
#   scripts/check-findings-schema.sh --bash <report.json>     bash fallback
#   scripts/check-findings-schema.sh --self-test              fixtures
#
# `nickel export` serialises the whole value, which forces every lazy
# contract, so a pass means every finding carries `support`, `evidence` and a
# `level` that agrees with them (schema/findings.ncl `LevelDerived`).
#
# --self-test runs the schema over test/fixtures/findings-schema/: every
# `good-*` file must be accepted and every `bad-*` file refused, each with the
# message its sidecar `<name>.expect` names — so a schema that accepts
# everything cannot pass.
#
# Env:  NICKEL (default: nickel)
# Exit: 0 = valid; 1 = invalid / a control misbehaved; 2 = usage/IO.

set -uo pipefail
cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." || exit 2

nickel="${NICKEL:-nickel}"
work=_build/check-findings-schema
mkdir -p "$work"
schema="$PWD/schema/findings.ncl"

# validate <contract> <json-file> <log>  — 0 if the file satisfies the contract
validate() {
  local contract=$1 json=$2 log=$3 abs
  abs="$(cd -- "$(dirname -- "$json")" && pwd)/$(basename -- "$json")"
  printf 'let s = import "%s" in (import "%s") | s.%s\n' "$schema" "$abs" "$contract" \
    >"$work/check.ncl"
  "$nickel" export --format json "$work/check.ncl" >/dev/null 2>"$log"
}

contract_for() {
  case "$(basename -- "$1")" in
    *bash*) echo BashReport ;;
    *) echo Findings ;;
  esac
}

case "${1:-}" in
  --self-test)
    dir=test/fixtures/findings-schema
    shopt -s nullglob
    goods=("$dir"/good-*.json)
    bads=("$dir"/bad-*.json)
    shopt -u nullglob
    if (( ${#goods[@]} == 0 || ${#bads[@]} == 0 )); then
      echo "ERROR: $dir needs both good-*.json and bad-*.json fixtures" >&2
      exit 1
    fi
    for f in "${goods[@]}"; do
      log="$work/$(basename -- "$f" .json).log"
      if ! validate "$(contract_for "$f")" "$f" "$log"; then
        cat "$log"
        echo "ERROR: schema refused valid fixture $f" >&2
        exit 1
      fi
      echo "PASS: accepted $f"
    done
    for f in "${bads[@]}"; do
      log="$work/$(basename -- "$f" .json).log"
      expect_file="${f%.json}.expect"
      [[ -f "$expect_file" ]] || { echo "ERROR: $f has no ${expect_file}" >&2; exit 1; }
      expect="$(head -1 "$expect_file")"
      if validate "$(contract_for "$f")" "$f" "$log"; then
        echo "ERROR: schema accepted invalid fixture $f" >&2
        exit 1
      fi
      if ! grep -qF -- "$expect" "$log" || grep -q 'error: import of' "$log"; then
        cat "$log"
        echo "ERROR: $f refused for the wrong reason (want: $expect)" >&2
        exit 1
      fi
      echo "PASS: refused $f: $expect"
    done
    echo "PASS: findings schema self-test, ${#goods[@]} accepted, ${#bads[@]} refused"
    ;;
  --bash)
    [[ -f "${2:-}" ]] || { echo "usage: $0 --bash <report.json>" >&2; exit 2; }
    if ! validate BashReport "$2" "$work/bash.log"; then
      cat "$work/bash.log"
      echo "ERROR: $2 does not satisfy schema/findings.ncl BashReport" >&2
      exit 1
    fi
    echo "PASS: $2 satisfies BashReport ($(jq '.findings | length' "$2") findings)"
    ;;
  "" | -*)
    echo "usage: $0 <findings.json> | --bash <report.json> | --self-test" >&2
    exit 2
    ;;
  *)
    [[ -f "$1" ]] || { echo "ERROR: $1 not found" >&2; exit 2; }
    if ! validate Findings "$1" "$work/findings.log"; then
      cat "$work/findings.log"
      echo "ERROR: $1 does not satisfy schema/findings.ncl Findings" >&2
      exit 1
    fi
    echo "PASS: $1 satisfies Findings ($(jq 'length' "$1") findings; by level: $(jq -c 'group_by(.level) | map({(.[0].level): length}) | add // {}' "$1"))"
    ;;
esac
