#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Offline test for the two shell permission fixers: a workflow with no
# permissions gets a least-privilege block (contents: read), never read-all;
# one that already declares top-level permissions is left alone; a second run
# changes nothing.
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
fail() { echo "FAIL: $*" >&2; exit 1; }

for script in scripts/fix-scripts/fix-workflow-permissions.sh scripts/fix-token-permissions.sh; do
  repo="$WORK/$(basename "$script" .sh)"; wf="$repo/.github/workflows"; mkdir -p "$wf"
  printf 'name: ci\non:\n  push:\njobs:\n  build:\n    runs-on: ubuntu-latest\n' > "$wf/ci.yml"
  printf 'name: rel\non: push\npermissions:\n  contents: write\njobs:\n  rel:\n    runs-on: ubuntu-latest\n' > "$wf/rel.yml"
  cp "$wf/rel.yml" "$WORK/rel.before"

  bash "$ROOT/$script" "$repo" >/dev/null

  grep -q "read-all" "$wf/ci.yml" && fail "$script wrote read-all"
  out=$(cat "$wf/ci.yml")
  grep -qx "permissions:" <<<"$out" || fail "$script added no top-level permissions block"
  grep -qx "  contents: read" <<<"$out" || fail "$script did not grant contents: read"
  cmp -s "$wf/rel.yml" "$WORK/rel.before" || fail "$script changed a workflow that already declares permissions"

  cp "$wf/ci.yml" "$WORK/ci.once"
  bash "$ROOT/$script" "$repo" >/dev/null
  cmp -s "$wf/ci.yml" "$WORK/ci.once" || fail "$script is not idempotent"
done

echo "ok: permission fixers"
