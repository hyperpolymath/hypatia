#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Offline test for scripts/fix-scripts/fix-pin-js-deps.sh: a package.json repo
# pinned by a Bun or pnpm lockfile is not reported as unpinned, an unpinned one
# is, and a Deno repo gets "lock": true.
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SCRIPT="$ROOT/scripts/fix-scripts/fix-pin-js-deps.sh"
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
fail() { echo "FAIL: $*" >&2; exit 1; }

for lock in bun.lock bun.lockb pnpm-lock.yaml; do
  d="$WORK/$lock"; mkdir -p "$d"; echo '{}' > "$d/package.json"; : > "$d/$lock"
  out=$(bash "$SCRIPT" "$d" 2>&1)
  grep -q "unpinned" <<<"$out" && fail "$lock repo reported as unpinned"
  grep -q "pinned by $lock" <<<"$out" || fail "$lock not recognised as the pinning lockfile"
  grep -qi "migrate to Deno" <<<"$out" && fail "$lock repo told to migrate to Deno"
done

d="$WORK/none"; mkdir -p "$d"; echo '{}' > "$d/package.json"
bash "$SCRIPT" "$d" 2>&1 | grep -q "unpinned" || fail "unpinned package.json repo not reported"

d="$WORK/deno"; mkdir -p "$d"; echo '{"tasks":{}}' > "$d/deno.json"
bash "$SCRIPT" "$d" >/dev/null 2>&1
jq -e '.lock == true' "$d/deno.json" >/dev/null || fail "deno.json did not get \"lock\": true"

echo "ok: fix-pin-js-deps"
