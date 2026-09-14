#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# End-to-end escript-build soundness gate.
#
# The Elixir in-process soundness test (test/soundness_test.exs) catches
# rule-definition regressions: a rule whose regex stops matching its
# fixture fails `mix test` before merge. But it does NOT catch
# **packaging** regressions — exactly the bug class PR #278 documented,
# where a stale escript binary silently dropped entire pattern families
# even though the in-tree rule definitions were correct.
#
# This script closes that loop: builds the escript fresh from source,
# runs the built binary against every fixture in
# test/soundness/manifest.json, and asserts each rule fires at the
# expected severity. Exits non-zero on the first packaging regression.
#
# Run locally:
#     bash test/soundness/run-escript-soundness.sh
#
# In CI: .github/workflows/escript-soundness.yml, "Escript packaging
# soundness" step. NOT tests.yml — that workflow startup-dies
# (jobs.total_count == 0) and has never had a single successful run, so a
# gate placed in it is inert by construction.

set -euo pipefail

cd "$(dirname "$0")/../.."
REPO_ROOT=$(pwd)
MANIFEST="$REPO_ROOT/test/soundness/manifest.json"
ESCRIPT="$REPO_ROOT/hypatia"

if [[ ! -f "$MANIFEST" ]]; then
    echo "FATAL: manifest not found at $MANIFEST" >&2
    exit 1
fi

# Build the escript fresh. We deliberately rebuild every time — the
# stale-binary scenario PR #278 documented is the entire failure mode
# this script is designed to catch.
echo "[soundness] Building escript fresh..." >&2
rm -f "$ESCRIPT"
mix escript.build >&2
if [[ ! -x "$ESCRIPT" ]]; then
    echo "FATAL: mix escript.build did not produce $ESCRIPT" >&2
    exit 1
fi

if ! command -v jq >/dev/null 2>&1; then
    echo "FATAL: jq required to parse manifest" >&2
    exit 1
fi

entries_count=$(jq '.entries | length' "$MANIFEST")
echo "[soundness] Loaded $entries_count manifest entries; running escript against each..." >&2

failures=()
results=()

# Scan the entire fixtures tree once. The escript's CLI only accepts
# directories, and scanning the whole tree exercises the language-
# dispatch + file-walking code paths together — which is closer to how
# the scanner runs in production than scanning each fixture in
# isolation. Per-rule assertions then filter the resulting JSON.
echo "[soundness] Scanning fixtures tree against built escript..." >&2
# AGENTS.md §5: never 2>/dev/null the thing under test. Capture stderr to a
# file so a crash (e.g. the CondClauseError that took down 449 consumers on
# 2026-09-12) is DIAGNOSABLE from the gate output, not merely detected.
stderr_log=$(mktemp)
trap 'rm -f "$stderr_log"' EXIT
# Capture the exit code rather than discarding it with `|| true`. Under
# --exit-zero a non-zero status can only mean the scanner itself failed,
# which is a distinct and louder fact than "no findings".
set +e
output=$("$ESCRIPT" scan "$REPO_ROOT/test/soundness/fixtures" \
             --format json \
             --severity low \
             --exit-zero 2>"$stderr_log")
scan_rc=$?
set -e

if ! echo "$output" | jq -e 'type == "array"' >/dev/null 2>&1; then
    echo "FATAL: escript did not return a JSON array from the fixtures tree" >&2
    echo "--- stdout (first 20 lines) ---" >&2
    echo "$output" | head -20 >&2
    # Print the TAIL as well as the head: a toolchain shim (mise, asdf) can emit
    # dozens of warning lines to stderr, pushing the actual exception past a
    # head-only window. Measured 2026-09-14 — the CondClauseError was invisible
    # behind 40 lines of mise registry warnings.
    echo "--- stderr (first 20 lines) ---" >&2
    head -20 "$stderr_log" >&2
    echo "--- stderr (last 40 lines) ---" >&2
    tail -40 "$stderr_log" >&2
    exit 1
fi

total_findings=$(echo "$output" | jq 'length')
echo "[soundness] Escript produced $total_findings findings; checking manifest..." >&2

while IFS=$'\t' read -r rule_module rule_id language fixture expected; do
    [[ -z "$rule_id" ]] && continue

    if [[ ! -f "$fixture" ]]; then
        failures+=("$rule_module/$rule_id: fixture missing on disk: $fixture")
        results+=("FAIL  $rule_module/$rule_id  (fixture missing)")
        continue
    fi

    # The escript's `file` field may be either repo-relative or absolute
    # depending on how it walked the tree. Compare via a basename match
    # so the test is robust to either form. Also matches when the
    # fixture's directory was traversed but the file path is the only
    # location info we have.
    fixture_basename=$(basename "$fixture")

    matching=$(echo "$output" | jq --arg t "$rule_id" \
                                    --arg m "$rule_module" \
                                    --arg fb "$fixture_basename" \
        '[.[] | select(.type == $t and .rule_module == $m and (.file | endswith($fb)))]')

    found=$(echo "$matching" | jq 'length')

    if [[ "$found" -eq 0 ]]; then
        failures+=("$rule_module/$rule_id: rule did not fire on $fixture (PR #278 class regression)")
        results+=("FAIL  $rule_module/$rule_id  (rule silent)")
        continue
    fi

    actual_sev=$(echo "$matching" | jq -r 'first(.[] | .severity)')

    if [[ "$actual_sev" != "$expected" ]]; then
        failures+=("$rule_module/$rule_id: severity drift — fired at '$actual_sev', expected '$expected'")
        results+=("FAIL  $rule_module/$rule_id  (severity $actual_sev != $expected)")
        continue
    fi

    results+=("ok    $rule_module/$rule_id")
done < <(jq -r '.entries[] | [.rule_module, .rule_id, .language, .fixture, .expected_severity] | @tsv' "$MANIFEST")

# Report
printf '\n[soundness] Results:\n' >&2
for line in "${results[@]}"; do
    printf '  %s\n' "$line" >&2
done

if [[ ${#failures[@]} -gt 0 ]]; then
    printf '\n[soundness] %d packaging regression(s) detected:\n' "${#failures[@]}" >&2
    for f in "${failures[@]}"; do
        printf '  - %s\n' "$f" >&2
    done
    printf '\nThis is the PR #278 bug class: the in-tree rule sources may be\n' >&2
    printf 'correct, but the escript build is silently dropping the rule.\n' >&2
    printf 'Investigate the escript build (mix.exs:escript, hypatia-cli.sh)\n' >&2
    printf 'before merging.\n' >&2
    # Dump stderr on THIS path too, not only the non-JSON path. Measured
    # 2026-09-14: the same planted defect crashes locally (non-JSON output) but
    # in CI returns a VALID array with the rules silently absent. Silent rules
    # are the more dangerous symptom -- indistinguishable from a clean scan --
    # so the diagnostic belongs on the branch that detects them.
    printf '\n[soundness] escript exit status: %s\n' "$scan_rc" >&2
    echo "--- stderr (first 20 lines) ---" >&2
    head -20 "$stderr_log" >&2
    echo "--- stderr (last 40 lines) ---" >&2
    tail -40 "$stderr_log" >&2
    exit 1
fi

printf '\n[soundness] %d/%d rules fired at expected severity on the built escript.\n' \
       "$entries_count" "$entries_count" >&2
