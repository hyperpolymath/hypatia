#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Hypatia CLI - Command-line interface for running scans
# This is what GitHub Actions and gitbot-fleet will call

set -euo pipefail

VERSION="1.0.0"
HYPATIA_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

usage() {
    cat << EOF
Hypatia ${VERSION} - Code Safety Scanner

USAGE:
    hypatia <command> [options]

COMMANDS:
    scan <path>         Scan directory or file for security issues
    report <path>       Generate detailed report with fix suggestions
    fix <path>          Auto-fix issues where possible
    check-staged        Check git staged files (for pre-commit hook)
    version             Show version
    help                Show this help

EXAMPLES:
    hypatia scan .                    # Scan current directory
    hypatia scan src/auth/JWT.res     # Scan specific file
    hypatia report . > report.txt     # Generate report
    hypatia check-staged              # Pre-commit hook

ENVIRONMENT:
    HYPATIA_SEVERITY    Minimum severity to report (critical|high|medium|low)
    HYPATIA_FORMAT      Output format (text|json|github)
    HYPATIA_FIX_MODE    Enable auto-fix (dry-run|apply)

INTEGRATION:
    - Called by gitbot-fleet coordination layer
    - Submits findings via bot_integration.lgt API
    - Triggers robot-repo-automaton for fixes
EOF
}

log_info() { echo -e "${BLUE}[INFO]${NC} $*"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $*"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $*"; }

scan_file() {
    local file="$1"
    local severity_filter="${HYPATIA_SEVERITY:-medium}"
    local format="${HYPATIA_FORMAT:-text}"

    # Use ripgrep-based scanner for now (POC)
    # TODO: Use SWI-Prolog with Logtalk when available

    local issues=0
    local findings_file="/tmp/hypatia-findings-$$.json"

    echo "[" > "$findings_file"
    local first=true

    # Pattern 1: ReScript getExn (CRITICAL)
    if [[ "$file" == *.res ]]; then
        while IFS=: read -r linenum line; do
            [[ "$first" == "false" ]] && echo "," >> "$findings_file"
            first=false

            # Use jq to properly construct JSON object
            jq -n \
                --arg sev "critical" \
                --arg type "unsafe_crash" \
                --arg pattern "getexn_on_external_data" \
                --arg file "$file" \
                --argjson line "$linenum" \
                --arg code "$line" \
                --arg cwe "CWE-754" \
                --arg fix "Replace getExn with switch/match or getWithDefault" \
                '{severity: $sev, type: $type, pattern: $pattern, file: $file, line: $line, code: $code, cwe: $cwe, fix: $fix}' \
                >> "$findings_file"
            ((issues++)) || true
        done < <(rg -n "getExn" "$file" 2>/dev/null || true)
    fi

    # Pattern 2: Rust unwrap (HIGH)
    if [[ "$file" == *.rs ]]; then
        while IFS=: read -r linenum line; do
            [[ "$first" == "false" ]] && echo "," >> "$findings_file"
            first=false

            jq -n \
                --arg sev "high" \
                --arg type "unsafe_panic" \
                --arg pattern "unwrap_without_check" \
                --arg file "$file" \
                --argjson line "$linenum" \
                --arg code "$line" \
                --arg cwe "CWE-754" \
                --arg fix "Replace unwrap() with ? operator or match" \
                '{severity: $sev, type: $type, pattern: $pattern, file: $file, line: $line, code: $code, cwe: $cwe, fix: $fix}' \
                >> "$findings_file"
            ((issues++)) || true
        done < <(rg -n "\.unwrap\(\)" "$file" 2>/dev/null || true)
    fi

    # Pattern 3: Obj.magic (HIGH)
    if [[ "$file" == *.res ]]; then
        while IFS=: read -r linenum line; do
            [[ "$first" == "false" ]] && echo "," >> "$findings_file"
            first=false

            jq -n \
                --arg sev "high" \
                --arg type "type_safety_bypass" \
                --arg pattern "obj_magic_bypass" \
                --arg file "$file" \
                --argjson line "$linenum" \
                --arg code "$line" \
                --arg cwe "CWE-704" \
                --arg fix "Remove Obj.magic and use proper type conversions" \
                '{severity: $sev, type: $type, pattern: $pattern, file: $file, line: $line, code: $code, cwe: $cwe, fix: $fix}' \
                >> "$findings_file"
            ((issues++)) || true
        done < <(rg -n "Obj\.magic" "$file" 2>/dev/null || true)
    fi

    # Pattern 4: CORS wildcard (CRITICAL)
    while IFS=: read -r linenum line; do
        [[ "$first" == "false" ]] && echo "," >> "$findings_file"
        first=false

        jq -n \
            --arg sev "critical" \
            --arg type "cors_misconfiguration" \
            --arg pattern "wildcard_cors" \
            --arg file "$file" \
            --argjson line "$linenum" \
            --arg code "$line" \
            --arg cwe "CWE-942" \
            --arg fix "Replace '*' with environment-based origin whitelist" \
            '{severity: $sev, type: $type, pattern: $pattern, file: $file, line: $line, code: $code, cwe: $cwe, fix: $fix}' \
            >> "$findings_file"
        ((issues++)) || true
    done < <(rg -n 'Access-Control-Allow-Origin.*"\*"' "$file" 2>/dev/null || true)

    # Close JSON array
    echo "]" >> "$findings_file"

    # Format output
    if [[ "$format" == "json" ]]; then
        cat "$findings_file"
    elif [[ "$format" == "github" ]]; then
        # GitHub Actions annotation format
        jq -r '.[] | select(. != {}) |
            "::\(.severity) file=\(.file),line=\(.line)::\(.pattern) - \(.fix)"' \
            "$findings_file"
    else
        # Human-readable text
        jq -r '.[] | select(. != {}) |
            "[\(.severity | ascii_upcase)] \(.file):\(.line)\n  Pattern: \(.pattern)\n  Fix: \(.fix)\n"' \
            "$findings_file"
    fi

    rm -f "$findings_file"

    return "$issues"
}

scan_directory() {
    local dir="$1"
    local total_issues=0
    local severity_filter="${HYPATIA_SEVERITY:-medium}"
    local format="${HYPATIA_FORMAT:-text}"

    log_info "Scanning directory: $dir" >&2

    # Find all source files
    local files=()
    while IFS= read -r -d '' file; do
        files+=("$file")
    done < <(find "$dir" -type f \( -name "*.rs" -o -name "*.res" -o -name "*.ml" \) -print0 2>/dev/null)

    log_info "Found ${#files[@]} source files" >&2

    # Collect all findings into one file
    local all_findings="/tmp/hypatia-all-findings-$$.json"
    echo "[" > "$all_findings"
    local first=true

    for file in "${files[@]}"; do
        local file_findings="/tmp/hypatia-file-$$.json"

        # Scan this file and collect findings
        local file_issues=0

        # Pattern 1: ReScript getExn (CRITICAL)
        if [[ "$file" == *.res ]]; then
            while IFS=: read -r linenum line; do
                [[ "$first" == "false" ]] && echo "," >> "$all_findings"
                first=false

                jq -n \
                    --arg sev "critical" \
                    --arg type "unsafe_crash" \
                    --arg pattern "getexn_on_external_data" \
                    --arg file "$file" \
                    --argjson line "$linenum" \
                    --arg code "$line" \
                    --arg cwe "CWE-754" \
                    --arg fix "Replace getExn with switch/match or getWithDefault" \
                    '{severity: $sev, type: $type, pattern: $pattern, file: $file, line: $line, code: $code, cwe: $cwe, fix: $fix, auto_fixable: false}' \
                    >> "$all_findings"
                ((file_issues++)) || true
            done < <(rg -n "getExn" "$file" 2>/dev/null || true)
        fi

        # Pattern 2: Rust unwrap (HIGH)
        if [[ "$file" == *.rs ]]; then
            while IFS=: read -r linenum line; do
                [[ "$first" == "false" ]] && echo "," >> "$all_findings"
                first=false

                jq -n \
                    --arg sev "high" \
                    --arg type "unsafe_panic" \
                    --arg pattern "unwrap_without_check" \
                    --arg file "$file" \
                    --argjson line "$linenum" \
                    --arg code "$line" \
                    --arg cwe "CWE-754" \
                    --arg fix "Replace unwrap() with ? operator or match" \
                    '{severity: $sev, type: $type, pattern: $pattern, file: $file, line: $line, code: $code, cwe: $cwe, fix: $fix, auto_fixable: false}' \
                    >> "$all_findings"
                ((file_issues++)) || true
            done < <(rg -n "\.unwrap\(\)" "$file" 2>/dev/null || true)
        fi

        # Pattern 3: Obj.magic (HIGH)
        if [[ "$file" == *.res ]]; then
            while IFS=: read -r linenum line; do
                [[ "$first" == "false" ]] && echo "," >> "$all_findings"
                first=false

                jq -n \
                    --arg sev "high" \
                    --arg type "type_safety_bypass" \
                    --arg pattern "obj_magic_bypass" \
                    --arg file "$file" \
                    --argjson line "$linenum" \
                    --arg code "$line" \
                    --arg cwe "CWE-704" \
                    --arg fix "Remove Obj.magic and use proper type conversions" \
                    '{severity: $sev, type: $type, pattern: $pattern, file: $file, line: $line, code: $code, cwe: $cwe, fix: $fix, auto_fixable: false}' \
                    >> "$all_findings"
                ((file_issues++)) || true
            done < <(rg -n "Obj\.magic" "$file" 2>/dev/null || true)
        fi

        # Pattern 4: CORS wildcard (CRITICAL)
        while IFS=: read -r linenum line; do
            [[ "$first" == "false" ]] && echo "," >> "$all_findings"
            first=false

            jq -n \
                --arg sev "critical" \
                --arg type "cors_misconfiguration" \
                --arg pattern "wildcard_cors" \
                --arg file "$file" \
                --argjson line "$linenum" \
                --arg code "$line" \
                --arg cwe "CWE-942" \
                --arg fix "Replace '*' with environment-based origin whitelist" \
                --arg fix_script "scripts/fix-cors-wildcard.sh" \
                '{severity: $sev, type: $type, pattern: $pattern, file: $file, line: $line, code: $code, cwe: $cwe, fix: $fix, auto_fixable: true, fix_suggestion: $fix_script}' \
                >> "$all_findings"
            ((file_issues++)) || true
        done < <(rg -n 'Access-Control-Allow-Origin.*"\*"' "$file" 2>/dev/null || true)

        # Pattern 5: Hardcoded secrets (CRITICAL)
        while IFS=: read -r linenum line; do
            # Skip if linenum is empty or non-numeric
            [[ -z "$linenum" || ! "$linenum" =~ ^[0-9]+$ ]] && continue

            [[ "$first" == "false" ]] && echo "," >> "$all_findings"
            first=false

            jq -n \
                --arg sev "critical" \
                --arg type "hardcoded_secret" \
                --arg pattern "hardcoded_credentials" \
                --arg file "$file" \
                --argjson line "$linenum" \
                --arg code "$line" \
                --arg cwe "CWE-798" \
                --arg fix "Move to environment variables or secret manager" \
                '{severity: $sev, type: $type, pattern: $pattern, file: $file, line: $line, code: $code, cwe: $cwe, fix: $fix, auto_fixable: false}' \
                >> "$all_findings"
            ((file_issues++)) || true
        done < <(rg -n '(?i)(api[_-]?key|password|secret|token|auth[_-]?key)\s*[:=]\s*["'\''][\\w-]{20,}' "$file" 2>/dev/null || true)

        # Pattern 6: eval() usage (CRITICAL)
        if [[ "$file" == *.js || "$file" == *.res ]]; then
            while IFS=: read -r linenum line; do
                # Skip if linenum is empty or non-numeric
                [[ -z "$linenum" || ! "$linenum" =~ ^[0-9]+$ ]] && continue

                [[ "$first" == "false" ]] && echo "," >> "$all_findings"
                first=false

                jq -n \
                    --arg sev "critical" \
                    --arg type "eval_usage" \
                    --arg pattern "dynamic_code_execution" \
                    --arg file "$file" \
                    --argjson line "$linenum" \
                    --arg code "$line" \
                    --arg cwe "CWE-95" \
                    --arg fix "Replace with safe alternatives (JSON.parse, switch statements)" \
                    '{severity: $sev, type: $type, pattern: $pattern, file: $file, line: $line, code: $code, cwe: $cwe, fix: $fix, auto_fixable: false}' \
                    >> "$all_findings"
                ((file_issues++)) || true
            done < <(rg -n '\beval\s*\(|Function\s*\(|setTimeout\s*\(["'\'']|setInterval\s*\(["'\'']' "$file" 2>/dev/null || true)
        fi

        # Pattern 7: unsafe blocks without doc comments (HIGH)
        if [[ "$file" == *.rs ]]; then
            while IFS=: read -r linenum line; do
                # Skip if linenum is empty or non-numeric
                [[ -z "$linenum" || ! "$linenum" =~ ^[0-9]+$ ]] && continue

                # Check if previous line has a comment explaining the unsafe block
                local prev_line=$((linenum - 1))
                local has_doc=false
                if [[ $prev_line -gt 0 ]]; then
                    if sed -n "${prev_line}p" "$file" | grep -q '^\s*//'; then
                        has_doc=true
                    fi
                fi

                if [[ "$has_doc" == "false" ]]; then
                    [[ "$first" == "false" ]] && echo "," >> "$all_findings"
                    first=false

                    jq -n \
                        --arg sev "high" \
                        --arg type "unsafe_without_doc" \
                        --arg pattern "undocumented_unsafe" \
                        --arg file "$file" \
                        --argjson line "$linenum" \
                        --arg code "$line" \
                        --arg cwe "CWE-1188" \
                        --arg fix "Add comment explaining why unsafe is needed and safety invariants" \
                        '{severity: $sev, type: $type, pattern: $pattern, file: $file, line: $line, code: $code, cwe: $cwe, fix: $fix, auto_fixable: false}' \
                        >> "$all_findings"
                    ((file_issues++)) || true
                fi
            done < <(rg -n '^\s*unsafe\s+\{' "$file" 2>/dev/null || true)
        fi

        # Pattern 8: TODO/FIXME/HACK markers (INFO)
        while IFS=: read -r linenum line; do
            # Skip if linenum is empty or non-numeric
            [[ -z "$linenum" || ! "$linenum" =~ ^[0-9]+$ ]] && continue

            [[ "$first" == "false" ]] && echo "," >> "$all_findings"
            first=false

            jq -n \
                --arg sev "info" \
                --arg type "technical_debt" \
                --arg pattern "debt_marker" \
                --arg file "$file" \
                --argjson line "$linenum" \
                --arg code "$line" \
                --arg cwe "CWE-1057" \
                --arg fix "Create issue for technical debt item and prioritize by age" \
                '{severity: $sev, type: $type, pattern: $pattern, file: $file, line: $line, code: $code, cwe: $cwe, fix: $fix, auto_fixable: false}' \
                >> "$all_findings"
            ((file_issues++)) || true
        done < <(rg -n '(?i)(TODO|FIXME|HACK|XXX|BUG):' "$file" 2>/dev/null || true)

        if [[ $file_issues -gt 0 ]]; then
            ((total_issues++)) || true
        fi
    done

    # Close JSON array
    echo "]" >> "$all_findings"

    # Format output
    if [[ "$format" == "json" ]]; then
        # Wrap findings in submission envelope for fleet coordination
        jq -n \
            --arg repo "$(basename "$dir")" \
            --arg timestamp "$(date -Iseconds)" \
            --arg scanner "hypatia" \
            --arg version "1.0.0" \
            --argjson findings "$(cat "$all_findings")" \
            '{
                submission_metadata: {
                    repo: $repo,
                    timestamp: $timestamp,
                    scanner: $scanner,
                    version: $version
                },
                findings: $findings
            }'
    elif [[ "$format" == "github" ]]; then
        # GitHub Actions annotation format
        jq -r '.[] | select(. != {}) |
            "::\(.severity) file=\(.file),line=\(.line)::\(.pattern) - \(.fix)"' \
            "$all_findings"
    else
        # Human-readable text
        jq -r '.[] | select(. != {}) |
            "[\(.severity | ascii_upcase)] \(.file):\(.line)\n  Pattern: \(.pattern)\n  Fix: \(.fix)\n"' \
            "$all_findings"
    fi

    rm -f "$all_findings"

    if [[ $total_issues -eq 0 ]]; then
        log_success "No security issues found" >&2
        return 0
    else
        log_error "Found issues in $total_issues files" >&2
        return 1
    fi
}

submit_findings_to_fleet() {
    local findings_json="$1"
    local gitbot_fleet_dir="${2:-/var/mnt/eclipse/repos/gitbot-fleet}"

    if [[ ! -d "$gitbot_fleet_dir" ]]; then
        log_warn "gitbot-fleet not found at $gitbot_fleet_dir, skipping submission"
        return 0
    fi

    # Submit to gitbot-fleet coordination
    local findings_dir="$gitbot_fleet_dir/shared-context/findings"
    mkdir -p "$findings_dir"

    local timestamp=$(date +%s)
    local findings_file="$findings_dir/hypatia-${timestamp}.json"

    echo "$findings_json" > "$findings_file"
    log_info "Submitted findings to gitbot-fleet: $findings_file"

    # Trigger robot-repo-automaton if critical issues found
    if echo "$findings_json" | jq -e '.[] | select(.severity == "critical")' >/dev/null 2>&1; then
        log_warn "CRITICAL issues detected - triggering robot-repo-automaton"
        # TODO: Call robot-repo-automaton executor
    fi
}

check_staged_files() {
    log_info "Checking git staged files..."

    # Get staged files
    local staged_files=()
    while IFS= read -r file; do
        if [[ -f "$file" && ("$file" == *.rs || "$file" == *.res || "$file" == *.ml) ]]; then
            staged_files+=("$file")
        fi
    done < <(git diff --cached --name-only --diff-filter=ACM)

    if [[ ${#staged_files[@]} -eq 0 ]]; then
        log_info "No source files staged"
        return 0
    fi

    log_info "Checking ${#staged_files[@]} staged files"

    local issues=0
    for file in "${staged_files[@]}"; do
        scan_file "$file" || ((issues++)) || true
    done

    if [[ $issues -gt 0 ]]; then
        log_error "Pre-commit check FAILED: $issues files have security issues"
        log_error "Fix issues or use 'git commit --no-verify' to bypass (not recommended)"
        return 1
    fi

    log_success "Pre-commit check PASSED"
    return 0
}

main() {
    local cmd="${1:-help}"

    case "$cmd" in
        scan)
            local target="${2:-.}"
            if [[ -f "$target" ]]; then
                scan_file "$target"
            elif [[ -d "$target" ]]; then
                scan_directory "$target"
            else
                log_error "Target not found: $target"
                exit 1
            fi
            ;;

        report)
            HYPATIA_FORMAT=json "$0" scan "${2:-.}"
            ;;

        check-staged)
            check_staged_files
            ;;

        version)
            echo "Hypatia ${VERSION}"
            ;;

        help|--help|-h)
            usage
            ;;

        *)
            log_error "Unknown command: $cmd"
            usage
            exit 1
            ;;
    esac
}

main "$@"
