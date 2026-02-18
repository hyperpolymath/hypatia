#!/bin/sh
# SPDX-License-Identifier: PMPL-1.0-or-later
# cicd-hyper-a Dragonfly/Redis Initialization Script
#
# Initializes Dragonfly cache for the cicd-hyper-a CI/CD intelligence platform.
# Configures memory settings, key prefixes, and initial cache entries.
#
# Usage:
#   ./init-dragonfly.sh [OPTIONS]
#
# Options:
#   -h, --help          Show this help message
#   -n, --dry-run       Show what would be done without executing
#   -v, --verbose       Enable verbose output
#   --flush             Flush all data before initializing
#   --status-only       Only show current status
#
# Environment Variables:
#   DRAGONFLY_HOST      Dragonfly host (default: localhost)
#   DRAGONFLY_PORT      Dragonfly port (default: 6379)
#   DRAGONFLY_PASSWORD  Dragonfly password (optional)
#   DRAGONFLY_MAXMEM    Max memory (default: 2gb)
#
# Examples:
#   ./init-dragonfly.sh
#   ./init-dragonfly.sh --dry-run
#   ./init-dragonfly.sh --flush

set -e

# =============================================================================
# CONFIGURATION
# =============================================================================

DRAGONFLY_HOST="${DRAGONFLY_HOST:-localhost}"
DRAGONFLY_PORT="${DRAGONFLY_PORT:-6379}"
DRAGONFLY_PASSWORD="${DRAGONFLY_PASSWORD:-}"
DRAGONFLY_MAXMEM="${DRAGONFLY_MAXMEM:-2gb}"

DRY_RUN=false
VERBOSE=false
FLUSH=false
STATUS_ONLY=false

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Cache key prefixes and TTLs
PREFIX_ALERT="alert:"
PREFIX_RULE="rule:"
PREFIX_REPO="repo:"
PREFIX_FIX="fix:"
PREFIX_RATELIMIT="rl:"
PREFIX_JOB="job:"
PREFIX_SESSION="session:"
PREFIX_BOT="bot:"

TTL_ALERT=3600        # 1 hour
TTL_RULE=86400        # 24 hours
TTL_REPO=21600        # 6 hours
TTL_FIX=43200         # 12 hours
TTL_RATELIMIT=60      # 1 minute
TTL_SESSION=7200      # 2 hours

# =============================================================================
# FUNCTIONS
# =============================================================================

usage() {
    sed -n '3,24p' "$0" | sed 's/^# //' | sed 's/^#//'
    exit 0
}

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

log_verbose() {
    if [ "$VERBOSE" = true ]; then
        log "$1"
    fi
}

log_dry() {
    if [ "$DRY_RUN" = true ]; then
        echo "[DRY-RUN] $1"
    fi
}

error() {
    echo "[ERROR] $1" >&2
    exit 1
}

check_dependencies() {
    log_verbose "Checking dependencies..."

    if ! command -v redis-cli >/dev/null 2>&1; then
        error "redis-cli is required but not installed"
    fi
}

redis_cmd() {
    local cmd="$1"

    if [ "$DRY_RUN" = true ]; then
        log_dry "redis-cli: $cmd"
        return 0
    fi

    local auth_args=""
    if [ -n "$DRAGONFLY_PASSWORD" ]; then
        auth_args="-a $DRAGONFLY_PASSWORD"
    fi

    redis-cli -h "$DRAGONFLY_HOST" -p "$DRAGONFLY_PORT" $auth_args $cmd 2>/dev/null
}

check_connection() {
    log_verbose "Checking Dragonfly connection..."

    if [ "$DRY_RUN" = true ]; then
        log_dry "Would check connection to $DRAGONFLY_HOST:$DRAGONFLY_PORT"
        return 0
    fi

    local response
    response=$(redis_cmd "PING")

    if [ "$response" != "PONG" ]; then
        error "Failed to connect to Dragonfly at $DRAGONFLY_HOST:$DRAGONFLY_PORT"
    fi

    log "Connected to Dragonfly at $DRAGONFLY_HOST:$DRAGONFLY_PORT"
}

show_status() {
    log "Dragonfly Status"
    log "================"

    if [ "$DRY_RUN" = true ]; then
        log_dry "Would show status"
        return 0
    fi

    # Server info
    local info
    info=$(redis_cmd "INFO server" | grep -E "^(redis_version|dragonfly_version|uptime_in_seconds|tcp_port):" || true)
    echo "$info"

    # Memory info
    echo ""
    log "Memory:"
    info=$(redis_cmd "INFO memory" | grep -E "^(used_memory_human|used_memory_peak_human|maxmemory_human):" || true)
    echo "$info"

    # Key counts by prefix
    echo ""
    log "Key counts by prefix:"

    for prefix in "$PREFIX_ALERT" "$PREFIX_RULE" "$PREFIX_REPO" "$PREFIX_FIX" "$PREFIX_SESSION" "$PREFIX_BOT" "$PREFIX_JOB"; do
        local count
        count=$(redis_cmd "KEYS ${prefix}*" | wc -l)
        printf "  %s*: %d keys\n" "$prefix" "$count"
    done

    # Queue sizes
    echo ""
    log "Queue sizes:"
    local queue_alerts
    queue_alerts=$(redis_cmd "ZCARD queue:alerts" || echo "0")
    local queue_fixes
    queue_fixes=$(redis_cmd "ZCARD queue:fixes" || echo "0")
    local queue_scans
    queue_scans=$(redis_cmd "ZCARD queue:scans" || echo "0")

    printf "  queue:alerts: %s items\n" "$queue_alerts"
    printf "  queue:fixes: %s items\n" "$queue_fixes"
    printf "  queue:scans: %s items\n" "$queue_scans"
}

configure_memory() {
    log "Configuring memory settings..."

    # Set max memory
    redis_cmd "CONFIG SET maxmemory $DRAGONFLY_MAXMEM"
    log "Set maxmemory to $DRAGONFLY_MAXMEM"

    # Set eviction policy
    redis_cmd "CONFIG SET maxmemory-policy allkeys-lru"
    log "Set maxmemory-policy to allkeys-lru"
}

setup_queues() {
    log "Setting up priority queues..."

    # Initialize alert priority queue (sorted set)
    # Score represents priority (higher = more urgent)
    if [ "$DRY_RUN" = true ]; then
        log_dry "Would initialize queue:alerts sorted set"
    else
        # Queue is empty initially, just ensure key type is correct
        redis_cmd "ZADD queue:alerts NX 0 __placeholder__" >/dev/null
        redis_cmd "ZREM queue:alerts __placeholder__" >/dev/null
    fi
    log_verbose "Initialized queue:alerts"

    # Initialize fix execution queue
    if [ "$DRY_RUN" = true ]; then
        log_dry "Would initialize queue:fixes sorted set"
    else
        redis_cmd "ZADD queue:fixes NX 0 __placeholder__" >/dev/null
        redis_cmd "ZREM queue:fixes __placeholder__" >/dev/null
    fi
    log_verbose "Initialized queue:fixes"

    # Initialize scan queue
    if [ "$DRY_RUN" = true ]; then
        log_dry "Would initialize queue:scans sorted set"
    else
        redis_cmd "ZADD queue:scans NX 0 __placeholder__" >/dev/null
        redis_cmd "ZREM queue:scans __placeholder__" >/dev/null
    fi
    log_verbose "Initialized queue:scans"

    log "Priority queues initialized"
}

setup_metadata() {
    log "Setting up metadata..."

    local timestamp
    timestamp=$(date -u +%Y-%m-%dT%H:%M:%SZ)

    # Store initialization metadata
    redis_cmd "HSET meta:init version 1.0.0 initialized_at \"$timestamp\" schema_version 1"

    # Store TTL configuration
    redis_cmd "HSET meta:ttl alert $TTL_ALERT rule $TTL_RULE repo $TTL_REPO fix $TTL_FIX session $TTL_SESSION"

    # Store prefix configuration
    redis_cmd "HSET meta:prefixes alert \"$PREFIX_ALERT\" rule \"$PREFIX_RULE\" repo \"$PREFIX_REPO\" fix \"$PREFIX_FIX\" session \"$PREFIX_SESSION\" bot \"$PREFIX_BOT\" job \"$PREFIX_JOB\" ratelimit \"$PREFIX_RATELIMIT\""

    log "Metadata stored"
}

setup_pubsub_channels() {
    log "Setting up Pub/Sub channel documentation..."

    # Store channel documentation in a hash
    redis_cmd "HSET meta:channels alerts:new \"Published when new alert is found\" alerts:fixed \"Published when alert is auto-fixed\" rules:triggered \"Published when rule fires\" scans:complete \"Published when repo scan finishes\" bots:started \"Published when bot starts processing\" bots:completed \"Published when bot finishes processing\""

    log "Pub/Sub channels documented"
}

seed_initial_data() {
    log "Seeding initial cache entries..."

    local timestamp
    timestamp=$(date +%s)

    # Seed bot metadata cache
    for bot in rhodibot echidnabot oikos glambot seambot finishing_bot robot_repo_automaton cicd_hyper_a; do
        redis_cmd "HSET ${PREFIX_BOT}${bot} name $bot cached_at $timestamp enabled true"
        redis_cmd "EXPIRE ${PREFIX_BOT}${bot} $TTL_RULE"
        log_verbose "Cached bot: $bot"
    done

    # Seed default ruleset references
    for ruleset in openssf_scorecard rhodium_standard github_security; do
        redis_cmd "HSET ${PREFIX_RULE}set:${ruleset} name $ruleset cached_at $timestamp enabled true"
        redis_cmd "EXPIRE ${PREFIX_RULE}set:${ruleset} $TTL_RULE"
        log_verbose "Cached ruleset: $ruleset"
    done

    # Initialize counters
    redis_cmd "SET stats:scans_total 0"
    redis_cmd "SET stats:alerts_total 0"
    redis_cmd "SET stats:fixes_total 0"

    log "Initial cache entries seeded"
}

flush_all() {
    if [ "$FLUSH" = true ]; then
        log "Flushing all data..."

        if [ "$DRY_RUN" = true ]; then
            log_dry "Would execute FLUSHALL"
            return 0
        fi

        read -p "Are you sure you want to flush all data? (yes/no): " confirm
        if [ "$confirm" = "yes" ]; then
            redis_cmd "FLUSHALL"
            log "All data flushed"
        else
            log "Flush cancelled"
        fi
    fi
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
            -n|--dry-run)
                DRY_RUN=true
                ;;
            -v|--verbose)
                VERBOSE=true
                ;;
            --flush)
                FLUSH=true
                ;;
            --status-only)
                STATUS_ONLY=true
                ;;
            *)
                error "Unknown option: $1"
                ;;
        esac
        shift
    done

    log "============================================================"
    log "cicd-hyper-a Dragonfly Initialization"
    log "============================================================"
    log "Host: $DRAGONFLY_HOST:$DRAGONFLY_PORT"
    log "Max Memory: $DRAGONFLY_MAXMEM"
    log "Dry run: $DRY_RUN"
    log "============================================================"

    check_dependencies
    check_connection

    if [ "$STATUS_ONLY" = true ]; then
        show_status
        exit 0
    fi

    flush_all
    configure_memory
    setup_queues
    setup_metadata
    setup_pubsub_channels
    seed_initial_data

    log "============================================================"
    log "Initialization complete!"
    log "============================================================"

    # Show final status
    show_status
}

main "$@"
