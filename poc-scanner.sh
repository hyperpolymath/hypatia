#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Proof-of-concept scanner implementing hypatia rules in bash+ripgrep
# Demonstrates the patterns work before full Logtalk deployment

set -euo pipefail

TARGET_REPO="${1:-/var/mnt/eclipse/repos/svalinn}"
ISSUE_COUNT=0

echo "=== Hypatia POC Scanner ==="
echo "Target: $TARGET_REPO"
echo "Scanning for security patterns from code-safety-lessons.lgt"
echo ""

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

scan_pattern() {
    local severity="$1"
    local pattern="$2"
    local description="$3"
    local file_filter="$4"

    echo -e "${YELLOW}[${severity}]${NC} Scanning for: $description"

    if [ -n "$file_filter" ]; then
        results=$(rg -n "$pattern" "$TARGET_REPO" -g "$file_filter" 2>/dev/null || true)
    else
        results=$(rg -n "$pattern" "$TARGET_REPO" 2>/dev/null || true)
    fi

    if [ -n "$results" ]; then
        echo -e "${RED}  ✗ FOUND:${NC}"
        echo "$results" | while IFS= read -r line; do
            echo "    $line"
            ((ISSUE_COUNT++)) || true
        done
    else
        echo -e "${GREEN}  ✓ No issues found${NC}"
    fi
    echo ""
}

# Test Rule 1: ReScript getExn on external data (CRITICAL)
scan_pattern "CRITICAL" \
    "getExn" \
    "ReScript unsafe getExn() calls" \
    "*.res"

# Test Rule 2: CORS wildcard (CRITICAL)
scan_pattern "CRITICAL" \
    'Access-Control-Allow-Origin.*"\*"' \
    "CORS wildcard allowing any origin" \
    "*.{res,js,ts,rs}"

# Test Rule 3: Unverified JWT decode (CRITICAL)
scan_pattern "CRITICAL" \
    "decodeJWT.*Basic decode.*dev/testing" \
    "Unverified JWT decode in dev mode" \
    "*.{res,rs}"

# Test Rule 4: Rust unwrap() without checks (HIGH)
scan_pattern "HIGH" \
    "\.unwrap\(\)" \
    "Rust unwrap() calls (potential panic)" \
    "*.rs"

# Test Rule 5: Default to root UID (CRITICAL)
scan_pattern "CRITICAL" \
    "unwrap_or\(0\).*uid" \
    "Default to root UID (privilege escalation)" \
    "*.rs"

# Test Rule 6: Obj.magic type bypass (HIGH)
scan_pattern "HIGH" \
    "Obj\.magic" \
    "ReScript Obj.magic bypassing type safety" \
    "*.res"

# Test Rule 7: Path traversal risk (MEDIUM)
scan_pattern "MEDIUM" \
    "Path::new.*\.\." \
    "Potential path traversal" \
    "*.rs"

# Test Rule 8: Command injection risk (HIGH)
scan_pattern "HIGH" \
    "Command::new.*env::var" \
    "Command execution with env var (injection risk)" \
    "*.rs"

echo "================================================"
echo -e "Total issues found: ${RED}${ISSUE_COUNT}${NC}"
echo ""

if [ "$ISSUE_COUNT" -eq 0 ]; then
    echo -e "${GREEN}✓ No security issues detected${NC}"
    exit 0
else
    echo -e "${RED}✗ Security issues require attention${NC}"
    echo ""
    echo "These patterns map to hypatia rules:"
    echo "  - code-safety-lessons.lgt: has_unsafe_crash, has_unsafe_panic"
    echo "  - code-safety-lessons.lgt: has_cors_misconfiguration"
    echo "  - code-safety-lessons.lgt: has_auth_bypass, has_privilege_escalation"
    echo ""
    echo "Run with full Logtalk for detailed analysis and fix suggestions."
    exit 1
fi
