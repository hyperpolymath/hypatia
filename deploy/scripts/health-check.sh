#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
# cicd-hyper-a Database Health Check Script
#
# Performs health checks on ArangoDB and Dragonfly databases.
# Returns exit code 0 if all checks pass, non-zero otherwise.
#
# Usage:
#   ./health-check.sh [OPTIONS]
#
# Options:
#   -h, --help          Show this help message
#   -v, --verbose       Enable verbose output
#   --db TYPE           Database type: arangodb, dragonfly, or all (default: all)
#   --json              Output results as JSON
#   --quiet             Only output errors
#   --timeout SECONDS   Connection timeout (default: 5)
#
# Environment Variables:
#   ARANGO_HOST         ArangoDB host (default: localhost)
#   ARANGO_PORT         ArangoDB port (default: 8529)
#   ARANGO_USER         ArangoDB username (default: root)
#   ARANGO_PASSWORD     ArangoDB password (required for ArangoDB)
#   ARANGO_DATABASE     Database name (default: cicd_hyper_a)
#   DRAGONFLY_HOST      Dragonfly host (default: localhost)
#   DRAGONFLY_PORT      Dragonfly port (default: 6379)
#   DRAGONFLY_PASSWORD  Dragonfly password (optional)
#
# Exit Codes:
#   0 - All health checks passed
#   1 - One or more health checks failed
#   2 - Configuration or dependency error
#
# Examples:
#   ./health-check.sh
#   ./health-check.sh --db arangodb --verbose
#   ./health-check.sh --json

set -e

# =============================================================================
# CONFIGURATION
# =============================================================================

ARANGO_HOST="${ARANGO_HOST:-localhost}"
ARANGO_PORT="${ARANGO_PORT:-8529}"
ARANGO_USER="${ARANGO_USER:-root}"
ARANGO_PASSWORD="${ARANGO_PASSWORD:-}"
ARANGO_DATABASE="${ARANGO_DATABASE:-cicd_hyper_a}"

DRAGONFLY_HOST="${DRAGONFLY_HOST:-localhost}"
DRAGONFLY_PORT="${DRAGONFLY_PORT:-6379}"
DRAGONFLY_PASSWORD="${DRAGONFLY_PASSWORD:-}"

VERBOSE=false
DB_TYPE="all"
JSON_OUTPUT=false
QUIET=false
TIMEOUT=5

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Health check results
ARANGO_STATUS="unknown"
ARANGO_DETAILS=""
DRAGONFLY_STATUS="unknown"
DRAGONFLY_DETAILS=""

# Thresholds
MEMORY_WARNING_PERCENT=80
MEMORY_CRITICAL_PERCENT=95
CONNECTION_WARNING=100
CONNECTION_CRITICAL=500

# =============================================================================
# FUNCTIONS
# =============================================================================

usage() {
    sed -n '3,35p' "$0" | sed 's/^# //' | sed 's/^#//'
    exit 0
}

log() {
    if [ "$QUIET" = false ]; then
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
    fi
}

log_verbose() {
    if [ "$VERBOSE" = true ]; then
        log "$1"
    fi
}

log_error() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] [ERROR] $1" >&2
}

check_dependencies() {
    log_verbose "Checking dependencies..."

    if [ "$DB_TYPE" = "all" ] || [ "$DB_TYPE" = "arangodb" ]; then
        if ! command -v curl >/dev/null 2>&1; then
            log_error "curl is required but not installed"
            exit 2
        fi
        if ! command -v jq >/dev/null 2>&1; then
            log_error "jq is required but not installed"
            exit 2
        fi
    fi

    if [ "$DB_TYPE" = "all" ] || [ "$DB_TYPE" = "dragonfly" ]; then
        if ! command -v redis-cli >/dev/null 2>&1; then
            log_error "redis-cli is required but not installed"
            exit 2
        fi
    fi
}

# ArangoDB API helper
arango_api() {
    local endpoint="$1"
    local db="${2:-$ARANGO_DATABASE}"

    local url="http://${ARANGO_HOST}:${ARANGO_PORT}/_db/${db}${endpoint}"

    curl -s -m "$TIMEOUT" \
        -u "${ARANGO_USER}:${ARANGO_PASSWORD}" \
        -H 'Content-Type: application/json' \
        "$url" 2>/dev/null
}

# Redis CLI helper
redis_cmd() {
    local auth_args=""
    if [ -n "$DRAGONFLY_PASSWORD" ]; then
        auth_args="-a $DRAGONFLY_PASSWORD"
    fi
    redis-cli -h "$DRAGONFLY_HOST" -p "$DRAGONFLY_PORT" $auth_args "$@" 2>/dev/null
}

check_arango_health() {
    log_verbose "Checking ArangoDB health..."

    local details=""
    local warnings=""
    local errors=""

    # Check connection and version
    local version_response
    version_response=$(arango_api "/_api/version" "_system")

    if [ -z "$version_response" ]; then
        ARANGO_STATUS="critical"
        ARANGO_DETAILS="Cannot connect to ArangoDB at ${ARANGO_HOST}:${ARANGO_PORT}"
        return 1
    fi

    local version
    version=$(echo "$version_response" | jq -r '.version // empty')

    if [ -z "$version" ]; then
        local error_msg
        error_msg=$(echo "$version_response" | jq -r '.errorMessage // "Unknown error"')
        ARANGO_STATUS="critical"
        ARANGO_DETAILS="Authentication failed: $error_msg"
        return 1
    fi

    details="version=$version"
    log_verbose "ArangoDB version: $version"

    # Check database exists
    local db_exists
    db_exists=$(arango_api "/_api/database" "_system" | jq -r ".result | index(\"$ARANGO_DATABASE\") // empty")

    if [ -z "$db_exists" ]; then
        ARANGO_STATUS="warning"
        ARANGO_DETAILS="Database '$ARANGO_DATABASE' does not exist"
        return 1
    fi

    details="$details, database=$ARANGO_DATABASE"
    log_verbose "Database exists: $ARANGO_DATABASE"

    # Check collection count
    local collection_count
    collection_count=$(arango_api "/_api/collection" "$ARANGO_DATABASE" | jq -r '.result | length // 0')
    details="$details, collections=$collection_count"
    log_verbose "Collection count: $collection_count"

    if [ "$collection_count" -eq 0 ]; then
        warnings="$warnings; No collections found"
    fi

    # Check server statistics
    local stats
    stats=$(arango_api "/_admin/statistics" "_system")

    if [ -n "$stats" ]; then
        # Memory usage
        local mem_resident
        mem_resident=$(echo "$stats" | jq -r '.server.physicalMemory // 0')
        local mem_virtual
        mem_virtual=$(echo "$stats" | jq -r '.server.v8Context.available // 0')

        # Connection count
        local connections
        connections=$(echo "$stats" | jq -r '.client.httpConnections // 0')

        details="$details, connections=$connections"
        log_verbose "HTTP connections: $connections"

        if [ "$connections" -gt "$CONNECTION_CRITICAL" ]; then
            errors="$errors; High connection count ($connections)"
        elif [ "$connections" -gt "$CONNECTION_WARNING" ]; then
            warnings="$warnings; Elevated connection count ($connections)"
        fi
    fi

    # Check engine status
    local engine
    engine=$(arango_api "/_api/engine" "_system" | jq -r '.name // "unknown"')
    details="$details, engine=$engine"
    log_verbose "Storage engine: $engine"

    # Determine final status
    if [ -n "$errors" ]; then
        ARANGO_STATUS="critical"
        ARANGO_DETAILS="$details$errors"
    elif [ -n "$warnings" ]; then
        ARANGO_STATUS="warning"
        ARANGO_DETAILS="$details$warnings"
    else
        ARANGO_STATUS="healthy"
        ARANGO_DETAILS="$details"
    fi

    return 0
}

check_dragonfly_health() {
    log_verbose "Checking Dragonfly health..."

    local details=""
    local warnings=""
    local errors=""

    # Check connection
    local ping
    ping=$(redis_cmd PING 2>/dev/null)

    if [ "$ping" != "PONG" ]; then
        DRAGONFLY_STATUS="critical"
        DRAGONFLY_DETAILS="Cannot connect to Dragonfly at ${DRAGONFLY_HOST}:${DRAGONFLY_PORT}"
        return 1
    fi

    log_verbose "Dragonfly PING: OK"

    # Get server info
    local info_server
    info_server=$(redis_cmd INFO server)

    local version
    version=$(echo "$info_server" | grep -E "^(redis_version|dragonfly_version):" | head -1 | cut -d: -f2 | tr -d '\r')
    details="version=$version"
    log_verbose "Dragonfly version: $version"

    local uptime
    uptime=$(echo "$info_server" | grep "^uptime_in_seconds:" | cut -d: -f2 | tr -d '\r')
    local uptime_human
    uptime_human=$((uptime / 86400))d$(( (uptime % 86400) / 3600))h
    details="$details, uptime=$uptime_human"
    log_verbose "Uptime: $uptime_human"

    # Get memory info
    local info_memory
    info_memory=$(redis_cmd INFO memory)

    local used_memory
    used_memory=$(echo "$info_memory" | grep "^used_memory:" | cut -d: -f2 | tr -d '\r')
    local used_memory_human
    used_memory_human=$(echo "$info_memory" | grep "^used_memory_human:" | cut -d: -f2 | tr -d '\r')
    local maxmemory
    maxmemory=$(echo "$info_memory" | grep "^maxmemory:" | cut -d: -f2 | tr -d '\r')

    details="$details, memory=$used_memory_human"
    log_verbose "Memory used: $used_memory_human"

    # Check memory usage percentage
    if [ -n "$maxmemory" ] && [ "$maxmemory" -gt 0 ]; then
        local mem_percent
        mem_percent=$((used_memory * 100 / maxmemory))
        details="$details, mem_percent=$mem_percent%"
        log_verbose "Memory usage: $mem_percent%"

        if [ "$mem_percent" -gt "$MEMORY_CRITICAL_PERCENT" ]; then
            errors="$errors; Critical memory usage ($mem_percent%)"
        elif [ "$mem_percent" -gt "$MEMORY_WARNING_PERCENT" ]; then
            warnings="$warnings; High memory usage ($mem_percent%)"
        fi
    fi

    # Get client info
    local info_clients
    info_clients=$(redis_cmd INFO clients)

    local connected_clients
    connected_clients=$(echo "$info_clients" | grep "^connected_clients:" | cut -d: -f2 | tr -d '\r')
    details="$details, clients=$connected_clients"
    log_verbose "Connected clients: $connected_clients"

    # Get keyspace info
    local info_keyspace
    info_keyspace=$(redis_cmd INFO keyspace)

    local db_keys
    db_keys=$(echo "$info_keyspace" | grep "^db0:" | sed 's/.*keys=\([0-9]*\).*/\1/' || echo "0")
    details="$details, keys=$db_keys"
    log_verbose "Keys: $db_keys"

    # Check critical keys exist
    local meta_init
    meta_init=$(redis_cmd EXISTS "meta:init")
    if [ "$meta_init" != "1" ]; then
        warnings="$warnings; Missing meta:init key (not initialized?)"
    fi

    # Check persistence
    local info_persistence
    info_persistence=$(redis_cmd INFO persistence)

    local rdb_status
    rdb_status=$(echo "$info_persistence" | grep "^rdb_bgsave_in_progress:" | cut -d: -f2 | tr -d '\r')
    if [ "$rdb_status" = "1" ]; then
        details="$details, bgsave=running"
    fi

    local last_save
    last_save=$(redis_cmd LASTSAVE)
    local save_age=$(($(date +%s) - last_save))
    if [ "$save_age" -gt 86400 ]; then
        warnings="$warnings; Last save was $(($save_age / 3600))h ago"
    fi

    # Determine final status
    if [ -n "$errors" ]; then
        DRAGONFLY_STATUS="critical"
        DRAGONFLY_DETAILS="$details$errors"
    elif [ -n "$warnings" ]; then
        DRAGONFLY_STATUS="warning"
        DRAGONFLY_DETAILS="$details$warnings"
    else
        DRAGONFLY_STATUS="healthy"
        DRAGONFLY_DETAILS="$details"
    fi

    return 0
}

output_results() {
    local overall_status="healthy"

    # Determine overall status
    if [ "$ARANGO_STATUS" = "critical" ] || [ "$DRAGONFLY_STATUS" = "critical" ]; then
        overall_status="critical"
    elif [ "$ARANGO_STATUS" = "warning" ] || [ "$DRAGONFLY_STATUS" = "warning" ]; then
        overall_status="warning"
    fi

    if [ "$JSON_OUTPUT" = true ]; then
        # JSON output
        cat <<EOF
{
  "status": "$overall_status",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "checks": {
    "arangodb": {
      "status": "$ARANGO_STATUS",
      "host": "$ARANGO_HOST:$ARANGO_PORT",
      "database": "$ARANGO_DATABASE",
      "details": "$ARANGO_DETAILS"
    },
    "dragonfly": {
      "status": "$DRAGONFLY_STATUS",
      "host": "$DRAGONFLY_HOST:$DRAGONFLY_PORT",
      "details": "$DRAGONFLY_DETAILS"
    }
  }
}
EOF
    else
        # Human-readable output
        echo ""
        echo "============================================================"
        echo "Health Check Results"
        echo "============================================================"
        echo "Timestamp: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
        echo "Overall Status: $overall_status"
        echo ""

        if [ "$DB_TYPE" = "all" ] || [ "$DB_TYPE" = "arangodb" ]; then
            local status_icon="[OK]"
            case "$ARANGO_STATUS" in
                healthy)  status_icon="[OK]" ;;
                warning)  status_icon="[WARN]" ;;
                critical) status_icon="[FAIL]" ;;
                *)        status_icon="[?]" ;;
            esac

            echo "ArangoDB: $status_icon $ARANGO_STATUS"
            echo "  Host: $ARANGO_HOST:$ARANGO_PORT"
            echo "  Database: $ARANGO_DATABASE"
            echo "  Details: $ARANGO_DETAILS"
            echo ""
        fi

        if [ "$DB_TYPE" = "all" ] || [ "$DB_TYPE" = "dragonfly" ]; then
            local status_icon="[OK]"
            case "$DRAGONFLY_STATUS" in
                healthy)  status_icon="[OK]" ;;
                warning)  status_icon="[WARN]" ;;
                critical) status_icon="[FAIL]" ;;
                *)        status_icon="[?]" ;;
            esac

            echo "Dragonfly: $status_icon $DRAGONFLY_STATUS"
            echo "  Host: $DRAGONFLY_HOST:$DRAGONFLY_PORT"
            echo "  Details: $DRAGONFLY_DETAILS"
            echo ""
        fi

        echo "============================================================"
    fi

    # Return appropriate exit code
    case "$overall_status" in
        healthy)  return 0 ;;
        warning)  return 0 ;;  # Warnings don't fail the check
        critical) return 1 ;;
        *)        return 1 ;;
    esac
}

# =============================================================================
# MAIN
# =============================================================================

main() {
    # Parse arguments
    while [ $# -gt 0 ]; do
        case "$1" in
            -h|--help)
                usage
                ;;
            -v|--verbose)
                VERBOSE=true
                ;;
            --db)
                shift
                DB_TYPE="$1"
                if [ "$DB_TYPE" != "all" ] && [ "$DB_TYPE" != "arangodb" ] && [ "$DB_TYPE" != "dragonfly" ]; then
                    log_error "Invalid database type: $DB_TYPE"
                    exit 2
                fi
                ;;
            --json)
                JSON_OUTPUT=true
                ;;
            --quiet)
                QUIET=true
                ;;
            --timeout)
                shift
                TIMEOUT="$1"
                ;;
            *)
                log_error "Unknown option: $1"
                exit 2
                ;;
        esac
        shift
    done

    check_dependencies

    # Perform health checks
    if [ "$DB_TYPE" = "all" ] || [ "$DB_TYPE" = "arangodb" ]; then
        if [ -z "$ARANGO_PASSWORD" ]; then
            ARANGO_STATUS="critical"
            ARANGO_DETAILS="ARANGO_PASSWORD not set"
        else
            check_arango_health || true
        fi
    fi

    if [ "$DB_TYPE" = "all" ] || [ "$DB_TYPE" = "dragonfly" ]; then
        check_dragonfly_health || true
    fi

    # Output results and return status
    output_results
}

main "$@"
