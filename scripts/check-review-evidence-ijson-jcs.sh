#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Pinned, read-only differential gate for the restricted review-evidence
# envelope. Does not run any GitHub API call, review bot, merge or learning job.
set -euo pipefail

cd "$(dirname "$0")/.."

pin_file=".ci/review-evidence-ijson-jcs.pin"
[ -f "$pin_file" ] || { echo "::error::missing canonicaliser pin"; exit 2; }
pin="$(tr -d '\r\n' < "$pin_file")"
[[ "$pin" =~ ^[0-9a-f]{40}$ ]] || { echo "::error::invalid canonicaliser pin"; exit 2; }

source_dir="${HYPATIA_IJSON_JCS_SOURCE:?source checkout required}"
binary="${HYPATIA_IJSON_JCS_BIN:?canonicaliser executable required}"
[ -x "$binary" ] || { echo "::error::missing pinned canonicaliser executable"; exit 2; }

actual="$(git -C "$source_dir" rev-parse HEAD)" ||
  { echo "::error::canonicaliser checkout inaccessible"; exit 2; }
[ "$actual" = "$pin" ] ||
  { echo "::error::canonicaliser revision differs from declared pin"; exit 2; }

# Consume the entire help output before testing it. A quiet grep pipeline
# can exit early, SIGPIPE its producer under pipefail and mask a sound CLI.
help_text="$("$binary" --help)" ||
  { echo "::error::canonicaliser help probe failed"; exit 2; }
case "$help_text" in
  *--profile*) ;;
  *) echo "::error::pinned CLI lacks strict file profiles"; exit 2 ;;
esac
case "$help_text" in
  *check-jsonl*) ;;
  *) echo "::error::pinned CLI lacks JSONL framing check"; exit 2 ;;
esac

scratch="$(mktemp -d)"
trap 'rm -rf "$scratch"' EXIT

mix run --no-start scripts/review-evidence-jcs-fixtures.exs "$scratch"

expect_exit() {
  local expected="$1"
  shift
  local actual_status
  if "$@" >/dev/null 2>&1; then
    actual_status=0
  else
    actual_status="$?"
  fi

  if [ "$actual_status" -ne "$expected" ]; then
    echo "::error::canonicaliser exit $actual_status instead of $expected"
    return 1
  fi
}

count=0
for f in "$scratch"/library_*.json; do
  [ -f "$f" ] || { echo "::error::no library fixture generated"; exit 2; }

  # Canonicalising the raw byte stream must yield exactly the source bytes.
  "$binary" canon < "$f" | cmp -s - "$f" ||
    { echo "::error::JCS canonical bytes differ"; exit 1; }

  expect_exit 0 "$binary" check --profile library "$f"
  expect_exit 1 "$binary" check --profile estate-file "$f"

  estate="$scratch/${f##*/}"
  estate="$scratch/estate_${estate##*/library_}"
  [ -f "$estate" ] || { echo "::error::missing estate file profile fixture"; exit 2; }

  expect_exit 0 "$binary" check --profile estate-file "$estate"
  expect_exit 1 "$binary" check --profile library "$estate"

  count=$((count + 1))
done
[ "$count" -eq 6 ] || { echo "::error::unexpected fixture denominator: $count"; exit 1; }

expect_exit 0 "$binary" check-jsonl "$scratch/records_ok.jsonl"
expect_exit 1 "$binary" check-jsonl "$scratch/records_missing_lf.jsonl"
expect_exit 2 "$binary" check-jsonl "$scratch/records_blank.jsonl"
expect_exit 1 "$binary" check-jsonl "$scratch/records_not_sorted.jsonl"

invalid=0
for f in "$scratch"/invalid_*.json; do
  [ -f "$f" ] || { echo "::error::no invalid fixture generated"; exit 2; }

  # Parsing raw text is essential: decoding to a map first would remove
  # duplicated object keys and defeat the negative controls.
  expect_exit 2 "$binary" check --profile library "$f"
  expect_exit 2 "$binary" canon < "$f"
  invalid=$((invalid + 1))
done
[ "$invalid" -eq 5 ] ||
  { echo "::error::unexpected invalid fixture denominator: $invalid"; exit 1; }

echo "review evidence I-JSON/JCS differential: $count canonical examples, $invalid strict negative examples, four JSONL framing controls"
echo "ijson-jcs source pin: $pin"
echo "ijson-jcs built binary SHA-256: $(sha256sum "$binary" | cut -d' ' -f1)"
echo "qualification: restricted ASCII/no-JSON-number profile tested; no signatures, learning or merge authority"
