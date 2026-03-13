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

# ---------------------------------------------------------------------------
# RSR Compliance: Banned Language Detection
# ---------------------------------------------------------------------------

scan_banned_language() {
    local severity="$1"
    local extension="$2"
    local language="$3"
    local replacement="$4"

    echo -e "${YELLOW}[${severity}]${NC} Scanning for: Banned language files (${language})"

    results=$(find "$TARGET_REPO" -name "$extension" \
        -not -path '*/.git/*' \
        -not -path '*/node_modules/*' \
        -not -path '*/vendor/*' \
        -not -path '*/.lake/packages/*' 2>/dev/null || true)

    if [ -n "$results" ]; then
        echo -e "${RED}  ✗ FOUND: ${language} files (banned — use ${replacement}):${NC}"
        echo "$results" | while IFS= read -r line; do
            echo "    $line"
            ((ISSUE_COUNT++)) || true
        done
    else
        echo -e "${GREEN}  ✓ No issues found${NC}"
    fi
    echo ""
}

# Rule 9: Python files (CRITICAL — banned language)
scan_banned_language "CRITICAL" "*.py" "Python" "Julia/Rust/ReScript"

# Rule 10: TypeScript files (CRITICAL — banned language)
scan_banned_language "CRITICAL" "*.ts" "TypeScript" "ReScript"

# Rule 11: Go files (CRITICAL — banned language)
scan_banned_language "CRITICAL" "*.go" "Go" "Rust"

# ---------------------------------------------------------------------------
# RSR Compliance: SCM File Location Enforcement
# ---------------------------------------------------------------------------

echo -e "${YELLOW}[CRITICAL]${NC} Scanning for: SCM files outside .machine_readable/"

scm_misplaced=0
for scm_file in STATE.scm META.scm ECOSYSTEM.scm AGENTIC.scm NEUROSYM.scm PLAYBOOK.scm LANGUAGES.scm; do
    # Check root
    if [ -f "$TARGET_REPO/$scm_file" ]; then
        echo -e "${RED}  ✗ FOUND: $scm_file in repo root (must be in .machine_readable/)${NC}"
        ((scm_misplaced++)) || true
    fi
    # Check .meta/ (wrong location)
    if [ -f "$TARGET_REPO/.meta/$scm_file" ]; then
        echo -e "${RED}  ✗ FOUND: .meta/$scm_file (must be in .machine_readable/, not .meta/)${NC}"
        ((scm_misplaced++)) || true
    fi
done
if [ "$scm_misplaced" -eq 0 ]; then
    echo -e "${GREEN}  ✓ No issues found${NC}"
else
    ISSUE_COUNT=$((ISSUE_COUNT + scm_misplaced))
fi
echo ""

# ---------------------------------------------------------------------------
# RSR Compliance: Missing Required Files
# ---------------------------------------------------------------------------

echo -e "${YELLOW}[MEDIUM]${NC} Scanning for: Missing RSR required files"

missing_files=0
for required in "SECURITY.md" ".editorconfig" "LICENSE"; do
    if [ ! -f "$TARGET_REPO/$required" ]; then
        echo -e "${RED}  ✗ MISSING: $required${NC}"
        ((missing_files++)) || true
    fi
done
# AI manifest check
if [ ! -f "$TARGET_REPO/0-AI-MANIFEST.a2ml" ] && [ ! -f "$TARGET_REPO/AI.a2ml" ]; then
    echo -e "${RED}  ✗ MISSING: AI manifest (0-AI-MANIFEST.a2ml or AI.a2ml)${NC}"
    ((missing_files++)) || true
fi
if [ "$missing_files" -eq 0 ]; then
    echo -e "${GREEN}  ✓ No issues found${NC}"
else
    ISSUE_COUNT=$((ISSUE_COUNT + missing_files))
fi
echo ""

# ---------------------------------------------------------------------------
# RSR Compliance: Dockerfile vs Containerfile
# ---------------------------------------------------------------------------

echo -e "${YELLOW}[HIGH]${NC} Scanning for: Dockerfile instead of Containerfile"

dockerfile_results=$(find "$TARGET_REPO" -name "Dockerfile" -not -path '*/.git/*' 2>/dev/null || true)
if [ -n "$dockerfile_results" ]; then
    echo -e "${RED}  ✗ FOUND: Dockerfile (must be named Containerfile):${NC}"
    echo "$dockerfile_results" | while IFS= read -r line; do
        echo "    $line"
        ((ISSUE_COUNT++)) || true
    done
else
    echo -e "${GREEN}  ✓ No issues found${NC}"
fi
echo ""

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
