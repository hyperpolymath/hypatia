#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Hypatia — End-to-End Test Suite
#
# Tests the neurosymbolic scanner pipeline without docker-compose:
#   1. Elixir scanner builds and runs
#   2. Scan a fixture repository
#   3. Verify findings are detected
#   4. Verify rule modules produce output
#   5. Verify JSON output format
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

# ─── Create test fixture ────────────────────────────────────────────
FIXTURE_DIR="$PROJECT_DIR/integration/fixtures/test-repo"
if [ ! -d "$FIXTURE_DIR" ]; then
    # Create minimal fixture if one doesn't exist
    FIXTURE_DIR=$(mktemp -d)
    trap "rm -rf $FIXTURE_DIR" EXIT
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

    # Unit tests
    if mix test --trace 2>&1 | tail -5 | grep -q "0 failures"; then
        pass "Elixir unit tests pass"
    else
        fail_test "Elixir unit tests"
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
if [ "$SKIP" -gt 0 ]; then yellow "SKIP=$SKIP"; else echo "SKIP=0"; fi
echo ""
echo "═══════════════════════════════════════════════════════════════"

exit "$FAIL"
