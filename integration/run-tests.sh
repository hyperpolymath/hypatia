#!/bin/bash
# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Integration Test Runner for cicd-hyper-a
#
# This script:
# 1. Starts test containers (ArangoDB, Dragonfly, mock servers)
# 2. Waits for services to be healthy
# 3. Runs integration tests
# 4. Collects coverage data
# 5. Cleans up containers
#
# Usage:
#   ./run-tests.sh                 # Run all tests
#   ./run-tests.sh --quick         # Skip slow tests
#   ./run-tests.sh --live          # Include tests against live services
#   ./run-tests.sh --coverage      # Generate coverage report
#   ./run-tests.sh --keep          # Keep containers running after tests
#   ./run-tests.sh --test NAME     # Run specific test file

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
COMPOSE_FILE="$SCRIPT_DIR/docker-compose.test.yml"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Default options
QUICK_MODE=false
LIVE_TESTS=false
COVERAGE=false
KEEP_CONTAINERS=false
SPECIFIC_TEST=""
VERBOSE=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --quick|-q)
            QUICK_MODE=true
            shift
            ;;
        --live|-l)
            LIVE_TESTS=true
            shift
            ;;
        --coverage|-c)
            COVERAGE=true
            shift
            ;;
        --keep|-k)
            KEEP_CONTAINERS=true
            shift
            ;;
        --test|-t)
            SPECIFIC_TEST="$2"
            shift 2
            ;;
        --verbose|-v)
            VERBOSE=true
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --quick, -q     Skip slow tests"
            echo "  --live, -l      Include tests against live services"
            echo "  --coverage, -c  Generate coverage report"
            echo "  --keep, -k      Keep containers running after tests"
            echo "  --test, -t NAME Run specific test file (e.g., fleet_test)"
            echo "  --verbose, -v   Verbose output"
            echo "  --help, -h      Show this help message"
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            exit 1
            ;;
    esac
done

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check dependencies
check_dependencies() {
    log_info "Checking dependencies..."

    local missing=()

    if ! command -v docker &> /dev/null; then
        missing+=("docker")
    fi

    if ! command -v docker-compose &> /dev/null && ! docker compose version &> /dev/null; then
        missing+=("docker-compose")
    fi

    if ! command -v cargo &> /dev/null; then
        missing+=("cargo")
    fi

    if [[ ${#missing[@]} -gt 0 ]]; then
        log_error "Missing dependencies: ${missing[*]}"
        exit 1
    fi

    log_success "All dependencies found"
}

# Start test containers
start_containers() {
    log_info "Starting test containers..."

    cd "$SCRIPT_DIR"

    # Use docker compose v2 if available, fall back to docker-compose
    if docker compose version &> /dev/null 2>&1; then
        COMPOSE_CMD="docker compose"
    else
        COMPOSE_CMD="docker-compose"
    fi

    $COMPOSE_CMD -f "$COMPOSE_FILE" up -d arangodb dragonfly

    log_info "Waiting for services to be healthy..."

    # Wait for ArangoDB
    local max_attempts=30
    local attempt=0
    while ! curl -sf http://localhost:8529/_api/version > /dev/null 2>&1; do
        attempt=$((attempt + 1))
        if [[ $attempt -ge $max_attempts ]]; then
            log_error "ArangoDB failed to start"
            exit 1
        fi
        echo -n "."
        sleep 2
    done
    echo ""
    log_success "ArangoDB is ready"

    # Wait for Dragonfly
    attempt=0
    while ! redis-cli -h localhost -p 6379 ping > /dev/null 2>&1; do
        attempt=$((attempt + 1))
        if [[ $attempt -ge $max_attempts ]]; then
            log_error "Dragonfly failed to start"
            exit 1
        fi
        echo -n "."
        sleep 2
    done
    echo ""
    log_success "Dragonfly is ready"
}

# Stop test containers
stop_containers() {
    if [[ "$KEEP_CONTAINERS" == true ]]; then
        log_info "Keeping containers running (--keep flag)"
        return
    fi

    log_info "Stopping test containers..."

    cd "$SCRIPT_DIR"

    if docker compose version &> /dev/null 2>&1; then
        docker compose -f "$COMPOSE_FILE" down -v
    else
        docker-compose -f "$COMPOSE_FILE" down -v
    fi

    log_success "Containers stopped and volumes removed"
}

# Initialize test database
init_database() {
    log_info "Initializing test database..."

    # Create database and collections using ArangoDB HTTP API
    local arango_url="http://localhost:8529"
    local auth="root:testpassword"

    # Create database
    curl -sf -X POST "$arango_url/_api/database" \
        -u "$auth" \
        -H "Content-Type: application/json" \
        -d '{"name": "cicd_hyper_a_test"}' > /dev/null 2>&1 || true

    # Create collections
    local db_url="$arango_url/_db/cicd_hyper_a_test/_api/collection"

    for collection in repositories alerts rules rulesets; do
        curl -sf -X POST "$db_url" \
            -u "$auth" \
            -H "Content-Type: application/json" \
            -d "{\"name\": \"$collection\"}" > /dev/null 2>&1 || true
    done

    # Create edge collections
    for edge in repo_has_alert rule_applies_to; do
        curl -sf -X POST "$db_url" \
            -u "$auth" \
            -H "Content-Type: application/json" \
            -d "{\"name\": \"$edge\", \"type\": 3}" > /dev/null 2>&1 || true
    done

    log_success "Test database initialized"
}

# Build tests
build_tests() {
    log_info "Building integration tests..."

    cd "$SCRIPT_DIR"

    local features=""
    if [[ "$LIVE_TESTS" == true ]]; then
        features="--features live-tests"
    fi
    if [[ "$QUICK_MODE" == false ]]; then
        features="$features --features slow-tests"
    fi

    if [[ "$COVERAGE" == true ]]; then
        # Use cargo-tarpaulin for coverage
        if ! command -v cargo-tarpaulin &> /dev/null; then
            log_warning "cargo-tarpaulin not installed, installing..."
            cargo install cargo-tarpaulin
        fi
    fi

    cargo build --release $features

    log_success "Tests built successfully"
}

# Run tests
run_tests() {
    log_info "Running integration tests..."

    cd "$SCRIPT_DIR"

    # Set environment variables
    export ARANGODB_URL="http://localhost:8529"
    export ARANGODB_DATABASE="cicd_hyper_a_test"
    export ARANGODB_USERNAME="root"
    export ARANGODB_PASSWORD="testpassword"
    export DRAGONFLY_URL="redis://localhost:6379"
    export RUST_LOG="${RUST_LOG:-info}"
    export RUST_BACKTRACE=1

    local test_args=""
    if [[ -n "$SPECIFIC_TEST" ]]; then
        test_args="--test $SPECIFIC_TEST"
    fi

    local features=""
    if [[ "$QUICK_MODE" == false ]]; then
        features="--features slow-tests"
    fi
    if [[ "$LIVE_TESTS" == true ]]; then
        features="$features --features live-tests"
    fi

    local verbose_flag=""
    if [[ "$VERBOSE" == true ]]; then
        verbose_flag="--nocapture"
    fi

    if [[ "$COVERAGE" == true ]]; then
        log_info "Running tests with coverage..."
        cargo tarpaulin \
            --out Html \
            --output-dir "$SCRIPT_DIR/coverage" \
            $features \
            $test_args \
            -- $verbose_flag
        log_success "Coverage report generated in $SCRIPT_DIR/coverage/"
    else
        cargo test \
            --release \
            $features \
            $test_args \
            -- $verbose_flag
    fi
}

# Print test summary
print_summary() {
    echo ""
    echo "============================================"
    echo "       Integration Test Summary"
    echo "============================================"
    echo ""
    echo "Tests completed at: $(date)"
    echo ""
    if [[ "$COVERAGE" == true ]]; then
        echo "Coverage report: $SCRIPT_DIR/coverage/tarpaulin-report.html"
    fi
    if [[ "$KEEP_CONTAINERS" == true ]]; then
        echo ""
        echo "Containers are still running:"
        echo "  ArangoDB: http://localhost:8529"
        echo "  Dragonfly: redis://localhost:6379"
        echo ""
        echo "To stop: docker-compose -f $COMPOSE_FILE down -v"
    fi
    echo "============================================"
}

# Cleanup handler
cleanup() {
    local exit_code=$?
    if [[ $exit_code -ne 0 ]]; then
        log_error "Tests failed with exit code $exit_code"
    fi
    stop_containers
    exit $exit_code
}

# Main execution
main() {
    echo ""
    echo "============================================"
    echo "  cicd-hyper-a Integration Test Runner"
    echo "============================================"
    echo ""

    # Set up trap for cleanup
    trap cleanup EXIT

    check_dependencies
    start_containers
    init_database
    build_tests
    run_tests
    print_summary

    log_success "All tests completed successfully!"
}

main "$@"
