#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Mount-point resolution in scripts/ramdisk-orchestrator.sh: the default is
# found after argument parsing, so --help and --mount work with no HOME and
# no XDG directory, and a run with nowhere to mount says so and stops.
# Every run is --dry-run with a repo name that exists nowhere: nothing is
# cloned, mounted or scanned.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
script="$repo_root/scripts/ramdisk-orchestrator.sh"
fixture_root="$(mktemp -d /tmp/hypatia-ramdisk-test.XXXXXX)"
# cleanup removes the temporary fixture directory when its path matches the expected safe pattern.
cleanup() {
  case "$fixture_root" in
    /tmp/hypatia-ramdisk-test.*) rm -rf -- "$fixture_root" ;;
    *) echo "refusing unsafe cleanup target: $fixture_root" >&2 ;;
  esac
}
trap cleanup EXIT

mkdir -p "$fixture_root/repos" "$fixture_root/run" "$fixture_root/state"
failures=0

# run_bare <env...> -- <args...>: the script in an environment holding only
# PATH, the given variables, and an empty repos directory; output in $out,
# status in $status.
run_bare() {
  local vars=()
  while [[ "$1" != "--" ]]; do vars+=("$1"); shift; done
  shift
  status=0
  out="$(env -i PATH="$PATH" HYPATIA_REPOS_DIR="$fixture_root/repos" "${vars[@]}" \
    bash "$script" "$@" 2>&1)" || status=$?
}

check() {
  local label="$1" ok="$2"
  if [[ "$ok" == "true" ]]; then
    echo "PASS: $label"
  else
    echo "FAIL: $label"
    printf '  status %s, output:\n%s\n' "$status" "$out" | sed 's/^/  /'
    failures=$((failures + 1))
  fi
}

# --help needs no HOME.
run_bare -- --help
check "--help runs with no HOME or XDG directory" \
  "$([[ "$status" -eq 0 && "$out" == *"--mount PATH"* ]] && echo true || echo false)"

# Nowhere to mount: a clear refusal naming the way out, not an unbound HOME.
run_bare -- --dry-run --size 1M no-such-repo
check "no HOME or XDG directory: refused, naming --mount" \
  "$([[ "$status" -eq 1 && "$out" == *"pass --mount PATH"* && "$out" != *"unbound variable"* ]] && echo true || echo false)"

# --mount is enough on its own.
run_bare -- --dry-run --size 1M --mount "$fixture_root/m" no-such-repo
check "--mount with no HOME or XDG directory" \
  "$([[ "$status" -eq 0 && "$out" == *"Would mount tmpfs (1M) at $fixture_root/m"* ]] && echo true || echo false)"

# The default follows XDG_RUNTIME_DIR, then XDG_STATE_HOME, then HOME.
run_bare XDG_RUNTIME_DIR="$fixture_root/run" XDG_STATE_HOME="$fixture_root/state" HOME="$fixture_root" -- \
  --dry-run --size 1M no-such-repo
check "default under XDG_RUNTIME_DIR" \
  "$([[ "$status" -eq 0 && "$out" == *"at $fixture_root/run/hypatia/workspace"* ]] && echo true || echo false)"

run_bare XDG_STATE_HOME="$fixture_root/state" HOME="$fixture_root" -- --dry-run --size 1M no-such-repo
check "default under XDG_STATE_HOME" \
  "$([[ "$status" -eq 0 && "$out" == *"at $fixture_root/state/hypatia/workspace"* ]] && echo true || echo false)"

run_bare HOME="$fixture_root" -- --dry-run --size 1M no-such-repo
check "default under HOME/.local/state" \
  "$([[ "$status" -eq 0 && "$out" == *"at $fixture_root/.local/state/hypatia/workspace"* ]] && echo true || echo false)"

if [[ "$failures" -gt 0 ]]; then
  echo "ramdisk-orchestrator-test: $failures failure(s)"
  exit 1
fi
echo "ramdisk-orchestrator-test: all passed"
