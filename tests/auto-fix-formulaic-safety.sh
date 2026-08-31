#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
FIXER="${REPO_ROOT}/scripts/auto-fix-formulaic.sh"
TEST_ROOT="$(mktemp -d)"
trap 'rm -rf "${TEST_ROOT}"' EXIT

FAKE_HOME="${TEST_ROOT}/home"
FIXTURE="${TEST_ROOT}/fixture"
WORKFLOW="${FIXTURE}/.github/workflows/ci.yaml"
UNRELATED="${FIXTURE}/unrelated.txt"

mkdir -p "${FAKE_HOME}" "$(dirname "${WORKFLOW}")"
git -C "${FIXTURE}" init --quiet
git -C "${FIXTURE}" config user.email test@example.invalid
git -C "${FIXTURE}" config user.name "Hypatia safety test"

printf '%s\n' \
  'name: CI' \
  'on: push' \
  'jobs:' \
  '  test:' \
  '    runs-on: ubuntu-latest' \
  '    steps:' \
  '      - uses: actions/checkout@v7.0.1' >"${WORKFLOW}"
printf '%s\n' 'preserve this unrelated work' >"${UNRELATED}"
git -C "${FIXTURE}" add .github
git -C "${FIXTURE}" commit --quiet -m 'fixture workflow'

workflow_before="$(sha256sum "${WORKFLOW}")"
unrelated_before="$(sha256sum "${UNRELATED}")"
head_before="$(git -C "${FIXTURE}" rev-parse HEAD)"
status_before="$(git -C "${FIXTURE}" status --porcelain)"

HOME="${FAKE_HOME}" bash "${FIXER}" "${FIXTURE}" >/dev/null

[[ "$(sha256sum "${WORKFLOW}")" == "${workflow_before}" ]]
[[ "$(sha256sum "${UNRELATED}")" == "${unrelated_before}" ]]
[[ "$(git -C "${FIXTURE}" rev-parse HEAD)" == "${head_before}" ]]
[[ "$(git -C "${FIXTURE}" status --porcelain)" == "${status_before}" ]]

heartbeat="${FAKE_HOME}/.hypatia/kin/auto-fix.heartbeat.json"
[[ -f "${heartbeat}" ]]
grep -q '"status": "diagnostic_only"' "${heartbeat}"
grep -q '"mutation_enabled": false' "${heartbeat}"
if grep -q 'sha_pin_fix\|formulaic_fix' "${heartbeat}"; then
  echo "ERROR: heartbeat claims a disabled mutation capability" >&2
  exit 1
fi

if HOME="${FAKE_HOME}" bash "${FIXER}" --push "${FIXTURE}" >/dev/null 2>&1; then
  echo "ERROR: --push unexpectedly succeeded" >&2
  exit 1
fi

if HOME="${FAKE_HOME}" bash "${FIXER}" all >/dev/null 2>&1; then
  echo "ERROR: incomplete all-repository traversal unexpectedly succeeded" >&2
  exit 1
fi

# A failed tracked-file listing must fail the diagnostic instead of looking
# like a successful scan of an empty repository.
git_error_log="${TEST_ROOT}/git-ls-files-error.log"
printf '%s\n' 'deliberately invalid git index' >"${FIXTURE}/.git/index"
if HOME="${FAKE_HOME}" bash "${FIXER}" "${FIXTURE}" >/dev/null 2>"${git_error_log}"; then
  echo "ERROR: failed git ls-files unexpectedly produced a successful scan" >&2
  exit 1
fi
grep -q 'ERROR: git ls-files failed' "${git_error_log}"

echo "auto-fix-formulaic safety controls: PASS"
