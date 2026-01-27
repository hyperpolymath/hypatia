#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Test hypatia scanner on a single repo (svalinn)

set -euo pipefail

HYPATIA_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TARGET_REPO="${1:-/var/mnt/eclipse/repos/svalinn}"

echo "=== Hypatia Scanner Test ==="
echo "Target repo: $TARGET_REPO"
echo "Hypatia dir: $HYPATIA_DIR"
echo ""

# Check if target exists
if [ ! -d "$TARGET_REPO" ]; then
    echo "Error: Target repo not found: $TARGET_REPO"
    exit 1
fi

# Check if we have swipl (SWI-Prolog with Logtalk)
if ! command -v swipl &> /dev/null; then
    echo "Error: swipl not found. Install SWI-Prolog: dnf install pl"
    exit 1
fi

# Create temporary Logtalk loader
cat > /tmp/hypatia_test_loader.lgt << 'EOF'
% Load hypatia modules
:- initialization((
    % Load scanner first (provides implementations)
    logtalk_load('engine/scanner.lgt'),

    % Load rules that use scanner
    logtalk_load('code-safety-lessons.lgt'),
    logtalk_load('container-security-lessons.lgt'),
    logtalk_load('security-lessons.lgt'),

    writeln('=== Hypatia modules loaded ==='),
    writeln('')
)).
EOF

# Create test query script
cat > /tmp/hypatia_test_query.lgt << EOF
% Test scanner on svalinn
:- initialization((
    writeln('=== Scanning svalinn for safety issues ==='),
    writeln(''),

    % Test 1: Scan JWT.res for getExn issues
    writeln('Test 1: Scanning JWT.res for ReScript crashes...'),
    TargetFile = '$TARGET_REPO/src/auth/JWT.res',
    ( code_safety_lessons::scan_file_for_safety_issues(TargetFile, Issues1) ->
        ( Issues1 = [] ->
            writeln('  ✓ No issues found in JWT.res')
        ;   writeln('  ✗ Found issues:'),
            forall(member(Issue, Issues1),
                format('    - ~w~n', [Issue]))
        )
    ; writeln('  ! Scan failed or file not found')
    ),
    writeln(''),

    % Test 2: Scan Gateway.res for CORS issues
    writeln('Test 2: Scanning Gateway.res for CORS misconfigurations...'),
    GatewayFile = '$TARGET_REPO/src/gateway/Gateway.res',
    ( code_safety_lessons::scan_file_for_safety_issues(GatewayFile, Issues2) ->
        ( Issues2 = [] ->
            writeln('  ✓ No CORS issues found in Gateway.res')
        ;   writeln('  ✗ Found issues:'),
            forall(member(Issue, Issues2),
                format('    - ~w~n', [Issue]))
        )
    ; writeln('  ! Scan failed or file not found')
    ),
    writeln(''),

    % Test 3: Scan Middleware.res for auth bypass
    writeln('Test 3: Scanning Middleware.res for auth bypass...'),
    MiddlewareFile = '$TARGET_REPO/src/auth/Middleware.res',
    ( code_safety_lessons::scan_file_for_safety_issues(MiddlewareFile, Issues3) ->
        ( Issues3 = [] ->
            writeln('  ✓ No auth bypass issues found')
        ;   writeln('  ✗ Found issues:'),
            forall(member(Issue, Issues3),
                format('    - ~w~n', [Issue]))
        )
    ; writeln('  ! Scan failed or file not found')
    ),
    writeln(''),

    writeln('=== Test complete ==='),
    halt(0)
)).
EOF

# Run Logtalk with our test
cd "$HYPATIA_DIR"
echo "Running Logtalk scanner..."
echo ""

swipl -g "set_logtalk_flag(report, warnings)" \
      -g "logtalk_load('/tmp/hypatia_test_loader.lgt')" \
      -g "logtalk_load('/tmp/hypatia_test_query.lgt')" \
      -t "halt(1)" \
      2>&1 || true

# Cleanup
rm -f /tmp/hypatia_test_loader.lgt /tmp/hypatia_test_query.lgt

echo ""
echo "=== Scanner test finished ===\"
