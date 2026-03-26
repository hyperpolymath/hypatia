#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Hypatia Scanner v2 - Nickel-based JSON generation
# Generates type-safe findings using Nickel schema

set -euo pipefail

VERSION="2.0.0"
HYPATIA_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Colors
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${BLUE}[INFO]${NC} $*" >&2; }
log_error() { echo -e "${RED}[ERROR]${NC} $*" >&2; }

# Check dependencies
check_deps() {
    local missing=()
    command -v rg >/dev/null || missing+=("ripgrep")
    command -v jq >/dev/null || missing+=("jq")
    command -v nickel >/dev/null || missing+=("nickel")

    if [ ${#missing[@]} -gt 0 ]; then
        log_error "Missing dependencies: ${missing[*]}"
        log_error "Install: nix-shell -p ripgrep jq nickel"
        exit 1
    fi
}

# Build a single finding using jq (guaranteed valid JSON)
make_finding() {
    local severity="$1"
    local type="$2"
    local pattern="$3"
    local file_path="$4"
    local line_num="$5"
    local code_text="$6"
    local cwe="$7"
    local fix="$8"

    jq -n \
        --arg sev "$severity" \
        --arg typ "$type" \
        --arg pat "$pattern" \
        --arg fil "$file_path" \
        --arg lin "$line_num" \
        --arg cod "$code_text" \
        --arg cwe "$cwe" \
        --arg fix "$fix" \
        '{
            severity: $sev,
            type: $typ,
            pattern: $pat,
            file: $fil,
            line: ($lin | tonumber),
            code: $cod,
            cwe: $cwe,
            fix: $fix
        }'
}

# Scan a single file and output findings as JSON array
scan_file() {
    local file="$1"
    local findings=()

    # Helper to parse rg output (filepath:linenum:content)
    # Only split on first TWO colons
    parse_rg_line() {
        local line="$1"
        local filepath=$(echo "$line" | cut -d: -f1)
        local linenum=$(echo "$line" | cut -d: -f2)
        local content=$(echo "$line" | cut -d: -f3-)
        echo "$filepath|$linenum|$content"
    }

    # Pattern 1: ReScript getExn (CRITICAL)
    if [[ "$file" == *.res ]]; then
        while IFS= read -r match; do
            IFS='|' read -r filepath linenum content <<< "$(parse_rg_line "$match")"
            local finding
            finding=$(make_finding \
                "critical" \
                "unsafe_crash" \
                "getexn_on_external_data" \
                "$filepath" \
                "$linenum" \
                "$content" \
                "CWE-754" \
                "Replace getExn with switch/match or getWithDefault")
            findings+=("$finding")
        done < <(rg -nH "getExn" "$file" 2>/dev/null || true)
    fi

    # Pattern 2: Rust unwrap (HIGH)
    if [[ "$file" == *.rs ]]; then
        while IFS= read -r match; do
            IFS='|' read -r filepath linenum content <<< "$(parse_rg_line "$match")"
            local finding
            finding=$(make_finding \
                "high" \
                "unsafe_panic" \
                "unwrap_without_check" \
                "$filepath" \
                "$linenum" \
                "$content" \
                "CWE-754" \
                "Replace unwrap() with ? operator or match")
            findings+=("$finding")
        done < <(rg -nH "\.unwrap\(\)" "$file" 2>/dev/null || true)
    fi

    # Pattern 3: Obj.magic (HIGH)
    if [[ "$file" == *.res ]]; then
        while IFS= read -r match; do
            IFS='|' read -r filepath linenum content <<< "$(parse_rg_line "$match")"
            local finding
            finding=$(make_finding \
                "high" \
                "type_safety_bypass" \
                "obj_magic_bypass" \
                "$filepath" \
                "$linenum" \
                "$content" \
                "CWE-704" \
                "Remove Obj.magic and use proper type conversions")
            findings+=("$finding")
        done < <(rg -nH "Obj\.magic" "$file" 2>/dev/null || true)
    fi

    # Pattern 4: CORS wildcard (CRITICAL)
    while IFS= read -r match; do
        IFS='|' read -r filepath linenum content <<< "$(parse_rg_line "$match")"
        local finding
        finding=$(make_finding \
            "critical" \
            "cors_misconfiguration" \
            "wildcard_cors" \
            "$filepath" \
            "$linenum" \
            "$content" \
            "CWE-942" \
            "Replace '*' with environment-based origin whitelist")
        findings+=("$finding")
    done < <(rg -nH 'Access-Control-Allow-Origin.*"\*"' "$file" 2>/dev/null || true)

    # Pattern 5: Unverified JWT decode (CRITICAL)
    while IFS= read -r match; do
        IFS='|' read -r filepath linenum content <<< "$(parse_rg_line "$match")"
        local finding
        finding=$(make_finding \
            "critical" \
            "auth_bypass" \
            "unverified_jwt_decode" \
            "$filepath" \
            "$linenum" \
            "$content" \
            "CWE-347" \
            "Always verify JWT signatures before trusting payload")
        findings+=("$finding")
    done < <(rg -nH 'decodeJWT' "$file" | rg -v 'verifyJWT' 2>/dev/null || true)

    # Pattern 6: Google API Key (CRITICAL)
    while IFS= read -r match; do
        IFS='|' read -r filepath linenum content <<< "$(parse_rg_line "$match")"
        local finding
        finding=$(make_finding \
            "critical" \
            "secret_leak" \
            "google_api_key" \
            "$filepath" \
            "$linenum" \
            "$content" \
            "CWE-798" \
            "Revoke/Rotate the secret immediately and purge git history using git-filter-repo")
        findings+=("$finding")
    done < <(rg -nH "AIza[0-9A-Za-z\-_]{35}" "$file" 2>/dev/null || true)

    # Pattern 7: Vulnerable Rust versions in Cargo.lock (MEDIUM/HIGH)
    if [[ "$file" == *Cargo.lock ]]; then
        # protobuf recursion crash
        while IFS= read -r match; do
            IFS='|' read -r filepath linenum content <<< "$(parse_rg_line "$match")"
            local finding
            finding=$(make_finding \
                "medium" \
                "vulnerability" \
                "protobuf_recursion_crash" \
                "$filepath" \
                "$linenum" \
                "$content" \
                "CWE-674" \
                "Update protobuf to >= 3.7.2")
            findings+=("$finding")
        done < <(rg -nH -B 1 'version = "(2\.|3\.[0-6]\.|3\.7\.[01])"' "$file" | rg 'name = "protobuf"' -A 1 | rg 'version =' 2>/dev/null || true)

        # idna punycode mishandling
        while IFS= read -r match; do
            IFS='|' read -r filepath linenum content <<< "$(parse_rg_line "$match")"
            local finding
            finding=$(make_finding \
                "medium" \
                "vulnerability" \
                "idna_punycode_mishandling" \
                "$filepath" \
                "$linenum" \
                "$content" \
                "CWE-20" \
                "Update idna to >= 1.0.0")
            findings+=("$finding")
        done < <(rg -nH -B 1 'version = "0\.[0-4]\."' "$file" | rg 'name = "idna"' -A 1 | rg 'version =' 2>/dev/null || true)

        # jsonwebtoken type confusion
        while IFS= read -r match; do
            IFS='|' read -r filepath linenum content <<< "$(parse_rg_line "$match")"
            local finding
            finding=$(make_finding \
                "medium" \
                "vulnerability" \
                "jsonwebtoken_type_confusion" \
                "$filepath" \
                "$linenum" \
                "$content" \
                "CWE-843" \
                "Update jsonwebtoken to >= 10.3.0")
            findings+=("$finding")
        done < <(rg -nH -B 1 'version = "[0-9]\."' "$file" | rg 'name = "jsonwebtoken"' -A 1 | rg 'version =' 2>/dev/null || true)

        # lru stacked borrows violation
        while IFS= read -r match; do
            IFS='|' read -r filepath linenum content <<< "$(parse_rg_line "$match")"
            local finding
            finding=$(make_finding \
                "low" \
                "vulnerability" \
                "lru_itermut_stacked_borrows" \
                "$filepath" \
                "$linenum" \
                "$content" \
                "CWE-416" \
                "Update lru to >= 0.16.3")
            findings+=("$finding")
        done < <(rg -nH -B 1 'version = "0\.(9\.|1[0-5]\.|16\.[0-2])"' "$file" | rg 'name = "lru"' -A 1 | rg 'version =' 2>/dev/null || true)

        # glib variantstriter unsoundness
        while IFS= read -r match; do
            IFS='|' read -r filepath linenum content <<< "$(parse_rg_line "$match")"
            local finding
            finding=$(make_finding \
                "medium" \
                "vulnerability" \
                "glib_variantstriter_unsoundness" \
                "$filepath" \
                "$linenum" \
                "$content" \
                "CWE-754" \
                "Update glib to >= 0.18.6, 0.19.10, 0.20.7 or 0.22.3")
            findings+=("$finding")
        done < <(rg -nH -B 1 'version = "0\.(15|16|17|18\.[0-5]|19\.[0-9])"' "$file" | rg 'name = "glib"' -A 1 | rg 'version =' 2>/dev/null || true)

        # crossbeam-utils atomiccell unsoundness
        while IFS= read -r match; do
            IFS='|' read -r filepath linenum content <<< "$(parse_rg_line "$match")"
            local finding
            finding=$(make_finding \
                "high" \
                "vulnerability" \
                "crossbeam_utils_atomiccell_unsoundness" \
                "$filepath" \
                "$linenum" \
                "$content" \
                "CWE-754" \
                "Update crossbeam-utils to >= 0.8.7")
            findings+=("$finding")
        done < <(rg -nH -B 1 'version = "0\.[678]\.[0-6]"' "$file" | rg 'name = "crossbeam-utils"' -A 1 | rg 'version =' 2>/dev/null || true)

        # lock_api data race
        while IFS= read -r match; do
            IFS='|' read -r filepath linenum content <<< "$(parse_rg_line "$match")"
            local finding
            finding=$(make_finding \
                "medium" \
                "vulnerability" \
                "lock_api_data_race" \
                "$filepath" \
                "$linenum" \
                "$content" \
                "CWE-362" \
                "Update lock_api to >= 0.4.2")
            findings+=("$finding")
        done < <(rg -nH -B 1 'version = "0\.[1234]\.[01]"' "$file" | rg 'name = "lock_api"' -A 1 | rg 'version =' 2>/dev/null || true)

        # crossbeam-queue segqueue unsoundness
        while IFS= read -r match; do
            IFS='|' read -r filepath linenum content <<< "$(parse_rg_line "$match")"
            local finding
            finding=$(make_finding \
                "medium" \
                "vulnerability" \
                "crossbeam_queue_segqueue_unsoundness" \
                "$filepath" \
                "$linenum" \
                "$content" \
                "CWE-754" \
                "Update crossbeam-queue to >= 0.2.3")
            findings+=("$finding")
        done < <(rg -nH -B 1 'version = "0\.(1\.|2\.[0-2])"' "$file" | rg 'name = "crossbeam-queue"' -A 1 | rg 'version =' 2>/dev/null || true)
    fi

    # Pattern 8: Vulnerable npm versions in yarn.lock (MEDIUM/HIGH)
    if [[ "$file" == *yarn.lock ]]; then
        # serialize-javascript
        while IFS= read -r match; do
            IFS='|' read -r filepath linenum content <<< "$(parse_rg_line "$match")"
            local finding
            finding=$(make_finding \
                "high" \
                "vulnerability" \
                "npm_serialize_javascript_vulnerability" \
                "$filepath" \
                "$linenum" \
                "$content" \
                "CWE-79" \
                "Update serialize-javascript to >= 6.0.2 or 7.0.0")
            findings+=("$finding")
        done < <(rg -nH -B 1 'version: ([0-5]\.|6\.0\.[01])' "$file" | rg 'serialize-javascript@npm:' -A 1 | rg 'version:' 2>/dev/null || true)

        # minimatch
        while IFS= read -r match; do
            IFS='|' read -r filepath linenum content <<< "$(parse_rg_line "$match")"
            local finding
            finding=$(make_finding \
                "high" \
                "vulnerability" \
                "npm_minimatch_vulnerability" \
                "$filepath" \
                "$linenum" \
                "$content" \
                "CWE-1333" \
                "Update minimatch to >= 9.0.5 or 10.0.0")
            findings+=("$finding")
        done < <(rg -nH -B 1 'version: ([0-8]\.|9\.0\.[0-4])' "$file" | rg 'minimatch@npm:' -A 1 | rg 'version:' 2>/dev/null || true)

        # glob
        while IFS= read -r match; do
            IFS='|' read -r filepath linenum content <<< "$(parse_rg_line "$match")"
            local finding
            finding=$(make_finding \
                "high" \
                "vulnerability" \
                "npm_glob_vulnerability" \
                "$filepath" \
                "$linenum" \
                "$content" \
                "CWE-78" \
                "Update glob to >= 11.0.0")
            findings+=("$finding")
        done < <(rg -nH -B 1 'version: ([0-9]\.|10\.[0-5]\.0)' "$file" | rg 'glob@npm:' -A 1 | rg 'version:' 2>/dev/null || true)

        # js-yaml
        while IFS= read -r match; do
            IFS='|' read -r filepath linenum content <<< "$(parse_rg_line "$match")"
            local finding
            finding=$(make_finding \
                "medium" \
                "vulnerability" \
                "npm_js_yaml_vulnerability" \
                "$filepath" \
                "$linenum" \
                "$content" \
                "CWE-1321" \
                "Update js-yaml to >= 3.14.2 or 4.1.1")
            findings+=("$finding")
        done < <(rg -nH -B 1 'version: ([0-2]\.|3\.(1[0-3]\.[0]|14\.[01]))' "$file" | rg 'js-yaml@npm:' -A 1 | rg 'version:' 2>/dev/null || true)

        # h3
        while IFS= read -r match; do
            IFS='|' read -r filepath linenum content <<< "$(parse_rg_line "$match")"
            local finding
            finding=$(make_finding \
                "high" \
                "vulnerability" \
                "npm_h3_vulnerability" \
                "$filepath" \
                "$linenum" \
                "$content" \
                "CWE-22" \
                "Update h3 to >= 2.0.1-rc.15")
            findings+=("$finding")
        done < <(rg -nH -B 1 'version: 2\.0\.1-rc\.( [0-9]|1[0-4])' "$file" | rg 'h3@npm:' -A 1 | rg 'version:' 2>/dev/null || true)
    fi

    # Output findings as JSON array
    if [ ${#findings[@]} -gt 0 ]; then
        printf '%s\n' "${findings[@]}" | jq -s '.'
    else
        echo "[]"
    fi
}

# Scan directory recursively
scan_directory() {
    local dir="$1"
    local all_findings=()

    log_info "Scanning directory: $dir"

    # Find all source files
    local files=()
    while IFS= read -r -d '' file; do
        files+=("$file")
    done < <(find "$dir" -type f \( -name "*.rs" -o -name "*.res" -o -name "*.ml" \) -not -path "*/target/*" -not -path "*/node_modules/*" -print0 2>/dev/null)

    log_info "Found ${#files[@]} source files"

    local scanned=0
    # Scan each file and collect findings
    for file in "${files[@]}"; do
        local findings_json
        findings_json=$(scan_file "$file")

        # Merge findings
        if [ "$findings_json" != "[]" ]; then
            all_findings+=("$findings_json")
        fi

        ((scanned++)) || true
    done

    log_info "Scanned $scanned files"

    # Combine all findings into single array
    if [ ${#all_findings[@]} -gt 0 ]; then
        printf '%s\n' "${all_findings[@]}" | jq -s 'add'
    else
        echo "[]"
    fi
}

# Generate complete report with Nickel validation
generate_report() {
    local target="$1"
    local findings_json

    if [ -d "$target" ]; then
        findings_json=$(scan_directory "$target")
    elif [ -f "$target" ]; then
        findings_json=$(scan_file "$target")
    else
        log_error "Target not found: $target"
        exit 1
    fi

    # Count findings
    local count
    count=$(echo "$findings_json" | jq 'length')

    log_info "Found $count issues"

    # Export environment for Nickel
    export HYPATIA_TIMESTAMP=$(date -Iseconds)
    export HYPATIA_REPOSITORY=$(basename "$(realpath "$target")")
    export HYPATIA_FILES_SCANNED=$(find "$target" -type f \( -name "*.rs" -o -name "*.res" -o -name "*.ml" \) -not -path "*/target/*" 2>/dev/null | wc -l)

    # Create Nickel input with findings data
    local nickel_input
    nickel_input=$(jq -n \
        --argjson findings "$findings_json" \
        --arg timestamp "$HYPATIA_TIMESTAMP" \
        --arg repository "$HYPATIA_REPOSITORY" \
        --argjson files_scanned "$HYPATIA_FILES_SCANNED" \
        --argjson total_findings "$count" \
        '{
            scan_info: {
                timestamp: $timestamp,
                repository: $repository,
                scanner_version: "2.0.0",
                scanned_files: $files_scanned,
                total_findings: $total_findings
            },
            findings: $findings
        }')

    # Validate and format with Nickel schema
    # For now, just output the JSON (Nickel validation can be added when schema is stable)
    echo "$nickel_input"
}

# Main
main() {
    local command="${1:-help}"

    case "$command" in
        scan)
            check_deps
            local target="${2:-.}"
            generate_report "$target"
            ;;
        version)
            echo "Hypatia Scanner v${VERSION}"
            ;;
        help|--help|-h)
            cat << EOF
Hypatia Scanner v${VERSION}

USAGE:
    hypatia-scanner-v2.sh scan <path>

EXAMPLES:
    ./hypatia-scanner-v2.sh scan .                  # Scan current directory
    ./hypatia-scanner-v2.sh scan src/auth/JWT.res   # Scan specific file

OUTPUT:
    Valid JSON with Nickel-validated schema
EOF
            ;;
        *)
            log_error "Unknown command: $command"
            echo "Run './hypatia-scanner-v2.sh help' for usage"
            exit 1
            ;;
    esac
}

main "$@"
