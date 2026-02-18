#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# security-check.sh - Local security scanning script for cicd-hyper-a
#
# This script runs comprehensive security checks locally before committing
# or pushing code. It mirrors the CI security-audit workflow.
#
# Usage:
#   ./scripts/security-check.sh              # Run all checks
#   ./scripts/security-check.sh --quick      # Quick checks only (no container scan)
#   ./scripts/security-check.sh --rust       # Rust checks only
#   ./scripts/security-check.sh --haskell    # Haskell checks only
#   ./scripts/security-check.sh --secrets    # Secret detection only
#   ./scripts/security-check.sh --container  # Container scanning only
#   ./scripts/security-check.sh --help       # Show help
#
# Exit codes:
#   0 - All checks passed
#   1 - Security issues found
#   2 - Tool installation required
#   3 - Usage error

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Report directory
REPORT_DIR="${PROJECT_ROOT}/target/security-reports"
mkdir -p "$REPORT_DIR"

# Counters
CHECKS_RUN=0
CHECKS_PASSED=0
CHECKS_FAILED=0
CHECKS_WARNED=0

# Timestamp for reports
TIMESTAMP=$(date +%Y%m%d_%H%M%S)

# Default flags
RUN_RUST=true
RUN_HASKELL=true
RUN_SECRETS=true
RUN_CONTAINER=true
RUN_LICENSE=true
RUN_SBOM=true
QUICK_MODE=false
VERBOSE=false

# ============================================================================
# Utility Functions
# ============================================================================

print_header() {
    echo ""
    echo -e "${BLUE}================================================================${NC}"
    echo -e "${BLUE}  $1${NC}"
    echo -e "${BLUE}================================================================${NC}"
}

print_subheader() {
    echo ""
    echo -e "${BLUE}--- $1 ---${NC}"
}

print_success() {
    echo -e "${GREEN}[PASS]${NC} $1"
    CHECKS_PASSED=$((CHECKS_PASSED + 1))
}

print_failure() {
    echo -e "${RED}[FAIL]${NC} $1"
    CHECKS_FAILED=$((CHECKS_FAILED + 1))
}

print_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
    CHECKS_WARNED=$((CHECKS_WARNED + 1))
}

print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

check_command() {
    if ! command -v "$1" &> /dev/null; then
        echo -e "${RED}[ERROR]${NC} Required tool not found: $1"
        echo "  Install with: $2"
        return 1
    fi
    return 0
}

# ============================================================================
# Help
# ============================================================================

show_help() {
    cat << 'EOF'
security-check.sh - Local security scanning for cicd-hyper-a

USAGE:
    ./scripts/security-check.sh [OPTIONS]

OPTIONS:
    --quick       Run quick checks only (skip container scanning)
    --rust        Run Rust security checks only
    --haskell     Run Haskell security checks only
    --secrets     Run secret detection only
    --container   Run container scanning only
    --license     Run license compliance check only
    --sbom        Generate SBOM only
    --verbose     Show detailed output
    --help        Show this help message

EXAMPLES:
    # Run all security checks
    ./scripts/security-check.sh

    # Quick pre-commit check
    ./scripts/security-check.sh --quick

    # Check for secrets before push
    ./scripts/security-check.sh --secrets

REQUIRED TOOLS:
    - cargo-audit    : cargo install cargo-audit
    - cargo-deny     : cargo install cargo-deny
    - gitleaks       : brew install gitleaks (or download from GitHub)
    - trivy          : brew install aquasecurity/trivy/trivy
    - cabal          : ghcup install cabal

REPORTS:
    Reports are generated in: target/security-reports/
EOF
}

# ============================================================================
# Parse Arguments
# ============================================================================

parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            --quick)
                QUICK_MODE=true
                RUN_CONTAINER=false
                RUN_SBOM=false
                shift
                ;;
            --rust)
                RUN_RUST=true
                RUN_HASKELL=false
                RUN_SECRETS=false
                RUN_CONTAINER=false
                RUN_LICENSE=false
                RUN_SBOM=false
                shift
                ;;
            --haskell)
                RUN_RUST=false
                RUN_HASKELL=true
                RUN_SECRETS=false
                RUN_CONTAINER=false
                RUN_LICENSE=false
                RUN_SBOM=false
                shift
                ;;
            --secrets)
                RUN_RUST=false
                RUN_HASKELL=false
                RUN_SECRETS=true
                RUN_CONTAINER=false
                RUN_LICENSE=false
                RUN_SBOM=false
                shift
                ;;
            --container)
                RUN_RUST=false
                RUN_HASKELL=false
                RUN_SECRETS=false
                RUN_CONTAINER=true
                RUN_LICENSE=false
                RUN_SBOM=false
                shift
                ;;
            --license)
                RUN_RUST=false
                RUN_HASKELL=false
                RUN_SECRETS=false
                RUN_CONTAINER=false
                RUN_LICENSE=true
                RUN_SBOM=false
                shift
                ;;
            --sbom)
                RUN_RUST=false
                RUN_HASKELL=false
                RUN_SECRETS=false
                RUN_CONTAINER=false
                RUN_LICENSE=false
                RUN_SBOM=true
                shift
                ;;
            --verbose)
                VERBOSE=true
                shift
                ;;
            --help|-h)
                show_help
                exit 0
                ;;
            *)
                echo -e "${RED}Unknown option: $1${NC}"
                echo "Use --help for usage information"
                exit 3
                ;;
        esac
    done
}

# ============================================================================
# Rust Security Checks
# ============================================================================

check_rust_audit() {
    print_subheader "cargo-audit: Checking for known vulnerabilities"
    CHECKS_RUN=$((CHECKS_RUN + 1))

    if ! check_command "cargo" "rustup (https://rustup.rs)"; then
        return 2
    fi

    if ! cargo audit --version &> /dev/null; then
        print_warning "cargo-audit not installed. Installing..."
        cargo install cargo-audit --locked || return 2
    fi

    cd "$PROJECT_ROOT"

    local report_file="${REPORT_DIR}/cargo-audit-${TIMESTAMP}.json"

    if cargo audit --json > "$report_file" 2>&1; then
        print_success "No known vulnerabilities in Rust dependencies"
        return 0
    else
        print_failure "Vulnerabilities found in Rust dependencies"
        echo "  Report: $report_file"
        if $VERBOSE; then
            cargo audit
        fi
        return 1
    fi
}

check_rust_deny() {
    print_subheader "cargo-deny: Checking licenses and banned crates"
    CHECKS_RUN=$((CHECKS_RUN + 1))

    if ! cargo deny --version &> /dev/null; then
        print_warning "cargo-deny not installed. Installing..."
        cargo install cargo-deny --locked || return 2
    fi

    cd "$PROJECT_ROOT"

    local report_file="${REPORT_DIR}/cargo-deny-${TIMESTAMP}.txt"

    if cargo deny check > "$report_file" 2>&1; then
        print_success "License and ban checks passed"
        return 0
    else
        print_warning "cargo-deny found issues (may be warnings)"
        echo "  Report: $report_file"
        if $VERBOSE; then
            cat "$report_file"
        fi
        return 0  # Treat as warning, not failure
    fi
}

run_rust_checks() {
    print_header "Rust Security Checks"

    local rust_status=0

    check_rust_audit || rust_status=1
    check_rust_deny || rust_status=1

    return $rust_status
}

# ============================================================================
# Haskell Security Checks
# ============================================================================

check_haskell_outdated() {
    print_subheader "cabal outdated: Checking for outdated packages"
    CHECKS_RUN=$((CHECKS_RUN + 1))

    if ! check_command "cabal" "ghcup (https://www.haskell.org/ghcup/)"; then
        return 2
    fi

    cd "${PROJECT_ROOT}/registry"

    local report_file="${REPORT_DIR}/cabal-outdated-${TIMESTAMP}.txt"

    cabal outdated > "$report_file" 2>&1 || true

    if grep -q "All packages are up to date" "$report_file" 2>/dev/null; then
        print_success "All Haskell packages are up to date"
        return 0
    else
        print_warning "Some Haskell packages may be outdated"
        echo "  Report: $report_file"
        if $VERBOSE; then
            cat "$report_file"
        fi
        return 0  # Treat as warning, not failure
    fi
}

run_haskell_checks() {
    print_header "Haskell Security Checks"

    if [ ! -d "${PROJECT_ROOT}/registry" ]; then
        print_warning "Haskell registry directory not found, skipping"
        return 0
    fi

    local haskell_status=0

    check_haskell_outdated || haskell_status=1

    return $haskell_status
}

# ============================================================================
# Secret Detection
# ============================================================================

check_gitleaks() {
    print_subheader "gitleaks: Scanning for secrets in git history"
    CHECKS_RUN=$((CHECKS_RUN + 1))

    if ! check_command "gitleaks" "brew install gitleaks (or https://github.com/gitleaks/gitleaks)"; then
        return 2
    fi

    cd "$PROJECT_ROOT"

    local report_file="${REPORT_DIR}/gitleaks-${TIMESTAMP}.json"

    if gitleaks detect --source . --report-format json --report-path "$report_file" 2>/dev/null; then
        print_success "No secrets detected by gitleaks"
        return 0
    else
        print_failure "Potential secrets detected!"
        echo "  Report: $report_file"
        if $VERBOSE; then
            gitleaks detect --source . --verbose
        fi
        return 1
    fi
}

check_trufflehog() {
    print_subheader "trufflehog: Deep secret scanning"
    CHECKS_RUN=$((CHECKS_RUN + 1))

    if ! command -v trufflehog &> /dev/null; then
        print_warning "trufflehog not installed, skipping"
        echo "  Install: brew install trufflehog (or https://github.com/trufflesecurity/trufflehog)"
        return 0
    fi

    cd "$PROJECT_ROOT"

    local report_file="${REPORT_DIR}/trufflehog-${TIMESTAMP}.json"

    if trufflehog filesystem . --only-verified --json > "$report_file" 2>/dev/null; then
        if [ ! -s "$report_file" ]; then
            print_success "No verified secrets found by trufflehog"
            return 0
        else
            print_failure "Verified secrets detected!"
            echo "  Report: $report_file"
            return 1
        fi
    else
        print_warning "trufflehog scan completed with issues"
        return 0
    fi
}

run_secret_checks() {
    print_header "Secret Detection"

    local secret_status=0

    check_gitleaks || secret_status=1
    check_trufflehog || secret_status=1

    return $secret_status
}

# ============================================================================
# Container Security
# ============================================================================

check_container_trivy() {
    print_subheader "trivy: Scanning container images"
    CHECKS_RUN=$((CHECKS_RUN + 1))

    if ! check_command "trivy" "brew install aquasecurity/trivy/trivy"; then
        return 2
    fi

    if ! check_command "docker" "https://docs.docker.com/get-docker/"; then
        return 2
    fi

    cd "$PROJECT_ROOT"

    local container_status=0
    local dockerfiles=("deploy/Dockerfile" "deploy/Dockerfile.adapter" "deploy/Dockerfile.engine" "deploy/Dockerfile.registry")

    for dockerfile in "${dockerfiles[@]}"; do
        if [ ! -f "$dockerfile" ]; then
            print_warning "Dockerfile not found: $dockerfile"
            continue
        fi

        local image_name="cicd-hyper-a-$(basename "$dockerfile" | sed 's/Dockerfile//' | sed 's/^\./scan-/' | sed 's/^-//')"
        if [ "$image_name" = "cicd-hyper-a-scan-" ]; then
            image_name="cicd-hyper-a-main:scan"
        else
            image_name="${image_name}:scan"
        fi

        print_info "Building $dockerfile -> $image_name"
        if ! docker build -t "$image_name" -f "$dockerfile" . > /dev/null 2>&1; then
            print_warning "Failed to build $dockerfile, skipping"
            continue
        fi

        local report_file="${REPORT_DIR}/trivy-${image_name//[:\/ ]/-}-${TIMESTAMP}.json"

        if trivy image --format json --output "$report_file" --severity HIGH,CRITICAL "$image_name" 2>/dev/null; then
            local high_count
            high_count=$(jq '[.Results[]?.Vulnerabilities[]? | select(.Severity == "HIGH" or .Severity == "CRITICAL")] | length' "$report_file" 2>/dev/null || echo "0")

            if [ "$high_count" = "0" ] || [ "$high_count" = "null" ]; then
                print_success "No HIGH/CRITICAL vulnerabilities in $image_name"
            else
                print_warning "Found $high_count HIGH/CRITICAL vulnerabilities in $image_name"
                container_status=1
            fi
        else
            print_warning "Trivy scan failed for $image_name"
        fi
    done

    return $container_status
}

run_container_checks() {
    print_header "Container Security Scanning"

    local container_status=0

    check_container_trivy || container_status=1

    return $container_status
}

# ============================================================================
# License Compliance
# ============================================================================

check_spdx_headers() {
    print_subheader "SPDX Headers: Checking license headers"
    CHECKS_RUN=$((CHECKS_RUN + 1))

    cd "$PROJECT_ROOT"

    local missing_count=0
    local report_file="${REPORT_DIR}/spdx-headers-${TIMESTAMP}.txt"

    echo "Files missing SPDX-License-Identifier header:" > "$report_file"
    echo "" >> "$report_file"

    # Check Rust files
    echo "## Rust Files (.rs)" >> "$report_file"
    while IFS= read -r -d '' f; do
        if ! head -10 "$f" | grep -q "SPDX-License-Identifier"; then
            echo "  - $f" >> "$report_file"
            missing_count=$((missing_count + 1))
        fi
    done < <(find . -name "*.rs" -not -path "./target/*" -not -path "./security-fixes/*" -print0 2>/dev/null)

    # Check Haskell files
    echo "" >> "$report_file"
    echo "## Haskell Files (.hs)" >> "$report_file"
    while IFS= read -r -d '' f; do
        if ! head -10 "$f" | grep -q "SPDX-License-Identifier"; then
            echo "  - $f" >> "$report_file"
            missing_count=$((missing_count + 1))
        fi
    done < <(find . -name "*.hs" -not -path "./dist-newstyle/*" -print0 2>/dev/null)

    # Check Shell scripts
    echo "" >> "$report_file"
    echo "## Shell Scripts (.sh)" >> "$report_file"
    while IFS= read -r -d '' f; do
        if ! head -10 "$f" | grep -q "SPDX-License-Identifier"; then
            echo "  - $f" >> "$report_file"
            missing_count=$((missing_count + 1))
        fi
    done < <(find . -name "*.sh" -not -path "./target/*" -not -path "./security-fixes/*" -print0 2>/dev/null)

    # Check YAML workflows
    echo "" >> "$report_file"
    echo "## GitHub Workflows (.yml)" >> "$report_file"
    while IFS= read -r -d '' f; do
        if ! head -1 "$f" | grep -q "SPDX-License-Identifier"; then
            echo "  - $f" >> "$report_file"
            missing_count=$((missing_count + 1))
        fi
    done < <(find .github/workflows -name "*.yml" -print0 2>/dev/null)

    echo "" >> "$report_file"
    echo "Total files missing SPDX header: $missing_count" >> "$report_file"

    if [ $missing_count -eq 0 ]; then
        print_success "All source files have SPDX headers"
        return 0
    else
        print_warning "$missing_count files missing SPDX headers"
        echo "  Report: $report_file"
        return 0  # Treat as warning
    fi
}

run_license_checks() {
    print_header "License Compliance"

    local license_status=0

    check_spdx_headers || license_status=1

    return $license_status
}

# ============================================================================
# SBOM Generation
# ============================================================================

generate_sbom() {
    print_header "SBOM Generation"

    CHECKS_RUN=$((CHECKS_RUN + 1))

    cd "$PROJECT_ROOT"

    # Rust SBOM
    print_subheader "Generating Rust SBOM (CycloneDX)"

    if ! cargo cyclonedx --version &> /dev/null; then
        print_warning "cargo-cyclonedx not installed. Installing..."
        cargo install cargo-cyclonedx --locked || {
            print_warning "Failed to install cargo-cyclonedx"
            return 0
        }
    fi

    local rust_sbom="${REPORT_DIR}/sbom-rust-${TIMESTAMP}.cdx.json"

    if cargo cyclonedx --format json --output-cdx "$rust_sbom" 2>/dev/null; then
        print_success "Rust SBOM generated: $rust_sbom"
    else
        print_warning "Failed to generate Rust SBOM"
    fi

    # Haskell SBOM (basic)
    print_subheader "Generating Haskell SBOM"

    if [ -d "${PROJECT_ROOT}/registry" ]; then
        cd "${PROJECT_ROOT}/registry"
        local haskell_sbom="${REPORT_DIR}/sbom-haskell-${TIMESTAMP}.cdx.json"

        cat > "$haskell_sbom" << EOF
{
  "bomFormat": "CycloneDX",
  "specVersion": "1.5",
  "version": 1,
  "metadata": {
    "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
    "component": {
      "type": "application",
      "name": "cicd-hyper-a-registry",
      "version": "0.1.0.0"
    }
  },
  "components": []
}
EOF

        if cabal freeze 2>/dev/null; then
            print_success "Haskell SBOM generated: $haskell_sbom"
        else
            print_warning "Partial Haskell SBOM generated"
        fi
    fi

    return 0
}

# ============================================================================
# Main
# ============================================================================

main() {
    parse_args "$@"

    print_header "cicd-hyper-a Security Check"
    echo "Timestamp: $(date)"
    echo "Reports will be saved to: $REPORT_DIR"

    local overall_status=0

    if $RUN_RUST; then
        run_rust_checks || overall_status=1
    fi

    if $RUN_HASKELL; then
        run_haskell_checks || overall_status=1
    fi

    if $RUN_SECRETS; then
        run_secret_checks || overall_status=1
    fi

    if $RUN_CONTAINER; then
        run_container_checks || overall_status=1
    fi

    if $RUN_LICENSE; then
        run_license_checks || overall_status=1
    fi

    if $RUN_SBOM; then
        generate_sbom || overall_status=1
    fi

    # Summary
    print_header "Security Check Summary"
    echo ""
    echo -e "Checks run:    ${CHECKS_RUN}"
    echo -e "Passed:        ${GREEN}${CHECKS_PASSED}${NC}"
    echo -e "Failed:        ${RED}${CHECKS_FAILED}${NC}"
    echo -e "Warnings:      ${YELLOW}${CHECKS_WARNED}${NC}"
    echo ""
    echo "Reports saved to: $REPORT_DIR"
    echo ""

    if [ $CHECKS_FAILED -gt 0 ]; then
        echo -e "${RED}Security issues detected! Review the reports above.${NC}"
        exit 1
    elif [ $CHECKS_WARNED -gt 0 ]; then
        echo -e "${YELLOW}Security checks passed with warnings.${NC}"
        exit 0
    else
        echo -e "${GREEN}All security checks passed!${NC}"
        exit 0
    fi
}

main "$@"
