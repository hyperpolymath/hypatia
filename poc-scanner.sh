#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Proof-of-concept scanner implementing hypatia rules in bash+ripgrep
# Demonstrates the patterns work before full Logtalk deployment

set -euo pipefail

TARGET_REPO="${1:?Usage: $0 <path-to-repo>}"
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

# Rule 1 (formerly ReScript getExn scan) — dropped 2026-04 when ReScript
# was banned outright. The banned-language check below (Rule 12) now
# catches any .res/.resi file as a CRITICAL violation; pattern-specific
# rules for legacy ReScript are redundant.

# Rule 2: CORS wildcard (CRITICAL) — kept, JS/Rust still relevant
scan_pattern "CRITICAL" \
    'Access-Control-Allow-Origin.*"\*"' \
    "CORS wildcard allowing any origin" \
    "*.{js,ts,rs}"

# Rule 3: Unverified JWT decode (CRITICAL) — Rust-only now
scan_pattern "CRITICAL" \
    "decodeJWT.*Basic decode.*dev/testing" \
    "Unverified JWT decode in dev mode" \
    "*.rs"

# Rule 4: Rust unwrap() without checks (HIGH)
scan_pattern "HIGH" \
    "\.unwrap\(\)" \
    "Rust unwrap() calls (potential panic)" \
    "*.rs"

# Rule 5: Default to root UID (CRITICAL)
scan_pattern "CRITICAL" \
    "unwrap_or\(0\).*uid" \
    "Default to root UID (privilege escalation)" \
    "*.rs"

# Rule 6 (formerly ReScript Obj.magic) — dropped 2026-04 same reason
# as Rule 1.

# Rule 7: Path traversal risk (MEDIUM)
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
scan_banned_language "CRITICAL" "*.py" "Python" "Elixir/Rust"

# Rule 10: TypeScript files (CRITICAL — banned language)
scan_banned_language "CRITICAL" "*.ts" "TypeScript" "Ephapax (systems) / Gossamer (UI)"

# Rule 11: Go files (CRITICAL — banned language)
scan_banned_language "CRITICAL" "*.go" "Go" "Rust"

# Rule 12: ReScript files (CRITICAL — banned 2026-04)
scan_banned_language "CRITICAL" "*.res" "ReScript" "Ephapax (systems) / Gossamer (UI)"
scan_banned_language "CRITICAL" "*.resi" "ReScript (interface)" "Ephapax (systems) / Gossamer (UI)"

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

# === UX Patterns (UX001-UX010) ===
echo "--- UX001: Hardcoded Absolute Paths ---"
HARDCODED=$(rg -c '/home/hyper|/mnt/eclipse|/var/mnt/eclipse' \
    --glob '!.git' --glob '!target' --glob '!node_modules' --glob '!lib/bs' \
    "$TARGET_REPO" 2>/dev/null | wc -l)
if [ "$HARDCODED" -gt 0 ]; then
    echo -e "${RED}  [UX001] $HARDCODED files contain hardcoded absolute paths${NC}"
    ISSUE_COUNT=$((ISSUE_COUNT + HARDCODED))
else
    echo -e "${GREEN}  [OK] No hardcoded absolute paths${NC}"
fi

echo "--- UX002-UX010: Structure Checks ---"
[ ! -f "$TARGET_REPO/QUICKSTART-USER.adoc" ] && echo -e "${YELLOW}  [UX002] Missing QUICKSTART-USER.adoc${NC}" && ISSUE_COUNT=$((ISSUE_COUNT + 1))
if [ -f "$TARGET_REPO/Justfile" ]; then
    grep -q "^doctor:" "$TARGET_REPO/Justfile" 2>/dev/null || { echo -e "${YELLOW}  [UX003] Missing 'doctor' recipe in Justfile${NC}"; ISSUE_COUNT=$((ISSUE_COUNT + 1)); }
    grep -q "^heal:" "$TARGET_REPO/Justfile" 2>/dev/null || { echo -e "${YELLOW}  [UX004] Missing 'heal' recipe in Justfile${NC}"; ISSUE_COUNT=$((ISSUE_COUNT + 1)); }
fi
[ ! -f "$TARGET_REPO/.machine_readable/MUST.contractile" ] && echo -e "${YELLOW}  [UX005] Missing MUST.contractile${NC}" && ISSUE_COUNT=$((ISSUE_COUNT + 1))
[ ! -f "$TARGET_REPO/guix.scm" ] && [ ! -f "$TARGET_REPO/flake.nix" ] && echo -e "${YELLOW}  [UX006] Missing guix.scm AND flake.nix${NC}" && ISSUE_COUNT=$((ISSUE_COUNT + 1))
[ ! -f "$TARGET_REPO/llm-warmup-user.md" ] && echo -e "${YELLOW}  [UX007] Missing llm-warmup-user.md${NC}" && ISSUE_COUNT=$((ISSUE_COUNT + 1))
[ ! -f "$TARGET_REPO/EXPLAINME.adoc" ] && echo -e "${YELLOW}  [UX008] Missing EXPLAINME.adoc${NC}" && ISSUE_COUNT=$((ISSUE_COUNT + 1))
if [ -f "$TARGET_REPO/0-AI-MANIFEST.a2ml" ]; then
    MANIFEST_LINES=$(wc -l < "$TARGET_REPO/0-AI-MANIFEST.a2ml")
    [ "$MANIFEST_LINES" -gt 500 ] && echo -e "${YELLOW}  [UX009] AI manifest oversized ($MANIFEST_LINES lines > 500)${NC}" && ISSUE_COUNT=$((ISSUE_COUNT + 1))
fi
[ ! -f "$TARGET_REPO/.machine_readable/ADJUST.contractile" ] && echo -e "${YELLOW}  [UX010] Missing ADJUST.contractile (accessibility)${NC}" && ISSUE_COUNT=$((ISSUE_COUNT + 1))
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
