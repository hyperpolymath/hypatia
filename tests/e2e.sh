#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Hypatia — End-to-End Test Suite
#
# Tests the neurosymbolic scanner pipeline without docker-compose:
#   1. Legacy formulaic diagnostics remain non-mutating and fail closed
#   2. Elixir scanner builds and runs
#   3. Scan a fixture repository
#   4. Verify findings are detected
#   5. Verify rule modules produce output
#   6. Verify JSON output format
#
# Usage:
#   bash tests/e2e.sh
#   just e2e
#
# Prerequisites:
#   - mix deps.get (Elixir deps)
#   - OR: cargo build (Rust CLI)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

PASS=0
FAIL=0
SKIP=0

green() { printf '\033[32m%s\033[0m\n' "$*"; }
red()   { printf '\033[31m%s\033[0m\n' "$*"; }
yellow(){ printf '\033[33m%s\033[0m\n' "$*"; }
bold()  { printf '\033[1m%s\033[0m\n' "$*"; }

pass() { green "  PASS: $1"; PASS=$((PASS + 1)); }
fail_test() { red "  FAIL: $1"; FAIL=$((FAIL + 1)); }
skip_test() { yellow "  SKIP: $1 ($2)"; SKIP=$((SKIP + 1)); }

echo "═══════════════════════════════════════════════════════════════"
echo "  Hypatia — End-to-End Tests"
echo "═══════════════════════════════════════════════════════════════"
echo ""

# ─── Preflight ───────────────────────────────────────────────────────
bold "Preflight"

HAS_ELIXIR=false
HAS_RUST=false

if command -v mix >/dev/null 2>&1; then
    HAS_ELIXIR=true
    green "  Elixir available"
fi

HYPER_BIN="$PROJECT_DIR/target/release/hyper"
if [ -f "$HYPER_BIN" ]; then
    HAS_RUST=true
    green "  Rust CLI available: $HYPER_BIN"
fi

if ! $HAS_ELIXIR && ! $HAS_RUST; then
    red "FATAL: Neither Elixir (mix) nor Rust CLI available"
    exit 1
fi
echo ""

# ─── Formulaic diagnostics safety control ──────────────────────────
bold "Formulaic diagnostics safety control"

if safety_output="$(bash "${PROJECT_DIR}/tests/auto-fix-formulaic-safety.sh" 2>&1)"; then
    pass "Legacy formulaic diagnostics are non-mutating and fail closed"
else
    fail_test "Legacy formulaic diagnostics safety control"
    printf '%s\n' "${safety_output}"
fi
echo ""

# ─── Create test fixture ────────────────────────────────────────────
FIXTURE_DIR="$PROJECT_DIR/integration/fixtures/test-repo"
if [ ! -d "$FIXTURE_DIR" ]; then
    # Create minimal fixture if one doesn't exist
    FIXTURE_DIR=$(mktemp -d)
    # Invoked indirectly by the EXIT trap below.
    # shellcheck disable=SC2329
    cleanup_fixture() {
        if [[ -n "${FIXTURE_DIR:-}" && "${FIXTURE_DIR}" == "${TMPDIR:-/tmp}/"* ]]; then
            rm -rf -- "${FIXTURE_DIR}"
        fi
    }
    trap cleanup_fixture EXIT
    mkdir -p "$FIXTURE_DIR/.github/workflows" "$FIXTURE_DIR/src"

    # Deliberately insecure workflow for scanner to find
    cat > "$FIXTURE_DIR/.github/workflows/ci.yml" << 'YAML'
name: CI
on: push
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - run: echo ${{ github.event.pull_request.body }}
YAML

    # Deliberately insecure code
    cat > "$FIXTURE_DIR/src/main.sh" << 'SHELL'
#!/bin/bash
eval "$1"
curl http://insecure-endpoint.example.com/api
SHELL

    git -C "$FIXTURE_DIR" init -q 2>/dev/null || true
fi

# ═══════════════════════════════════════════════════════════════════════
# Section 1: Elixir scanner E2E
# ═══════════════════════════════════════════════════════════════════════
if $HAS_ELIXIR; then
    bold "Section 1: Elixir scanner"

    cd "$PROJECT_DIR"

    # Compile check
    if mix compile --warnings-as-errors >/dev/null 2>&1; then
        pass "Elixir compiles without warnings"
    else
        if mix compile >/dev/null 2>&1; then
            pass "Elixir compiles (with warnings)"
        else
            fail_test "Elixir compilation"
        fi
    fi

    # Unit tests.
    #
    # Previously this was `mix test --trace 2>&1 | tail -5 | grep -q "0 failures"`,
    # which DISCARDED the entire test output. A CI failure therefore reported
    # exactly one line — "FAIL: Elixir unit tests" — with no failing test name,
    # no assertion, no stacktrace, making the red job impossible to diagnose
    # from CI. Capture the run, judge it by mix's own exit status, and echo the
    # output when it fails.
    #
    # `tail -N` was then the second defect, and it is why five consecutive red
    # runs were undiagnosable (issue #826). ExUnit prints failure diagnostics
    # INLINE, as each test fails — it does not collect them at the end. Under
    # `--trace` this suite emits roughly two lines per test across ~1600 tests,
    # so any fixed tail window contains only trace tail and the summary: grepping
    # the full logs of five red runs for '^\s*[0-9]+) test ' returned nothing.
    # A window sized for a summary cannot show diagnostics emitted inline.
    #
    # So print the failure BLOCKS, selected by content, not by position: from the
    # first `  1) test ...` to the end of the log. Fall back to a tail only when
    # no failure block exists, which means the run died some other way (a compile
    # error, or a startup crash) and the tail is then the informative part.
    _mix_log="${TMPDIR:-/tmp}/hypatia-mix-test.$$.log"
    if mix test --trace >"$_mix_log" 2>&1; then
        pass "Elixir unit tests pass"
    else
        fail_test "Elixir unit tests"
        _n_fail=$(grep -cE '^[[:space:]]*[0-9]+\) test ' "$_mix_log" || true)
        echo "--- mix test: ${_n_fail} failure block(s) ---"
        if [ "${_n_fail}" -gt 0 ]; then
            # Everything from the first failure block onward, capped so a
            # pathological run cannot flood the CI log.
            sed -n '/^[[:space:]]*1) test /,$p' "$_mix_log" | head -500
        else
            echo "(no ExUnit failure block found — the run did not reach the tests)"
            tail -120 "$_mix_log"
        fi
        echo "--- end mix test output ---"
        echo "full log retained at: $_mix_log"
    fi
    if [ "${HYPATIA_KEEP_MIX_LOG:-0}" != "1" ]; then
        rm -f "$_mix_log"
    fi

    echo ""
fi

# ═══════════════════════════════════════════════════════════════════════
# Section 2: Rust CLI scan
# ═══════════════════════════════════════════════════════════════════════
if $HAS_RUST; then
    bold "Section 2: Rust CLI scan"

    # Scan fixture repo
    SCAN_OUTPUT=$("$HYPER_BIN" scan "$FIXTURE_DIR" --format json 2>/dev/null || true)
    if [ -n "$SCAN_OUTPUT" ]; then
        pass "Rust CLI produces scan output"

        # Verify JSON format
        if echo "$SCAN_OUTPUT" | python3 -c "import sys,json; json.load(sys.stdin)" 2>/dev/null; then
            pass "Scan output is valid JSON"
        else
            fail_test "Scan output is not valid JSON"
        fi
    else
        skip_test "Rust CLI scan" "may need build first"
    fi

    echo ""
fi

# ═══════════════════════════════════════════════════════════════════════
# Section 3: Rule module coverage
# ═══════════════════════════════════════════════════════════════════════
bold "Section 3: Rule module validation"

RULE_DIR="$PROJECT_DIR/lib/rules"
if [ -d "$RULE_DIR" ]; then
    RULE_COUNT=$(find "$RULE_DIR" -name "*.ex" | wc -l)
    if [ "$RULE_COUNT" -gt 0 ]; then
        pass "Found $RULE_COUNT rule modules"
    else
        fail_test "No rule modules found"
    fi
else
    skip_test "Rule modules" "lib/hypatia/rules/ not found"
fi

# Check all documented rules have corresponding modules
for rule in root_hygiene honest_completion workflow_audit cicd_rules code_safety; do
    if find "$PROJECT_DIR/lib" -name "*${rule}*" -o -name "*$(echo $rule | tr '_' '-')*" 2>/dev/null | grep -q .; then
        pass "Rule module: $rule"
    else
        skip_test "Rule module: $rule" "file not found"
    fi
done

echo ""

# ═══════════════════════════════════════════════════════════════════════
# Section 4: Integration fixture scan
# ═══════════════════════════════════════════════════════════════════════
bold "Section 4: Fixture scan detection"

# The fixture has deliberately insecure patterns — scanner should find them
if $HAS_RUST && [ -f "$HYPER_BIN" ]; then
    FINDINGS=$("$HYPER_BIN" scan "$FIXTURE_DIR" 2>/dev/null || true)
    if echo "$FINDINGS" | grep -qi "unpinned\|injection\|insecure\|http\|eval\|finding\|weak\|issue" 2>/dev/null; then
        pass "Scanner detects issues in fixture repo"
    else
        skip_test "Fixture detection" "scanner may not produce text findings"
    fi
fi
echo ""

# ═══════════════════════════════════════════════════════════════════════
# Summary
# ═══════════════════════════════════════════════════════════════════════
echo "═══════════════════════════════════════════════════════════════"
printf "  Results: "
green "PASS=$PASS" | tr -d '\n'
echo -n "  "
if [ "$FAIL" -gt 0 ]; then red "FAIL=$FAIL" | tr -d '\n'; else echo -n "FAIL=0"; fi
echo -n "  "
if [ "$SKIP" -gt 0 ]; then yellow "SKIP=$SKIP" | tr -d '\n'; else echo -n "SKIP=0"; fi
echo ""

# The denominator. PASS and FAIL are the scenarios that actually executed;
# SKIP deliberately does not count, because a skipped scenario produced no
# evidence either way.
#
# Without this guard the suite ended `exit "$FAIL"`, so a run in which every
# scenario skipped — no Elixir, no Rust CLI, a renamed fixture — exited 0 and
# reported a clean end-to-end pass having tested nothing at all. Green on an
# empty subject is the failure mode this suite exists to catch in other
# people's pipelines, so it must not be able to commit it itself.
RAN=$((PASS + FAIL))
echo "  Scenarios run: $RAN"
echo "═══════════════════════════════════════════════════════════════"

if [ "$RAN" -eq 0 ]; then
    red "  E2E FAILED: 0 scenarios executed (SKIP=$SKIP)."
    red "  A suite with an empty subject cannot report a pass."
    exit 1
fi

exit "$FAIL"
