#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
# cicd-hyper-a Database Migration Script
#
# Manages database schema migrations with version tracking and rollback support.
# Supports both ArangoDB and Dragonfly migrations.
#
# Usage:
#   ./migrate.sh [OPTIONS] COMMAND
#
# Commands:
#   status          Show current migration status
#   up              Apply all pending migrations
#   down            Rollback the last migration
#   rollback N      Rollback N migrations
#   version         Show current schema version
#   create NAME     Create a new migration file
#
# Options:
#   -h, --help          Show this help message
#   -n, --dry-run       Show what would be done without executing
#   -v, --verbose       Enable verbose output
#   --db TYPE           Database type: arangodb, dragonfly, or all (default: all)
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
#   MIGRATIONS_DIR      Migrations directory (default: ./migrations)
#
# Examples:
#   ./migrate.sh status
#   ./migrate.sh up
#   ./migrate.sh down
#   ./migrate.sh rollback 3
#   ./migrate.sh create add_analytics_collection

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

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
MIGRATIONS_DIR="${MIGRATIONS_DIR:-$SCRIPT_DIR/migrations}"

DRY_RUN=false
VERBOSE=false
DB_TYPE="all"

# =============================================================================
# FUNCTIONS
# =============================================================================

usage() {
    sed -n '3,36p' "$0" | sed 's/^# //' | sed 's/^#//'
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

    if [ "$DB_TYPE" = "all" ] || [ "$DB_TYPE" = "arangodb" ]; then
        if ! command -v curl >/dev/null 2>&1; then
            error "curl is required but not installed"
        fi
        if ! command -v jq >/dev/null 2>&1; then
            error "jq is required but not installed"
        fi
    fi

    if [ "$DB_TYPE" = "all" ] || [ "$DB_TYPE" = "dragonfly" ]; then
        if ! command -v redis-cli >/dev/null 2>&1; then
            error "redis-cli is required but not installed"
        fi
    fi
}

# ArangoDB API helper
arango_api() {
    local method="$1"
    local endpoint="$2"
    local data="$3"
    local db="${4:-$ARANGO_DATABASE}"

    local url="http://${ARANGO_HOST}:${ARANGO_PORT}/_db/${db}${endpoint}"

    if [ "$DRY_RUN" = true ]; then
        log_dry "ArangoDB $method $endpoint"
        return 0
    fi

    local curl_args="-s -X $method"
    curl_args="$curl_args -u ${ARANGO_USER}:${ARANGO_PASSWORD}"
    curl_args="$curl_args -H 'Content-Type: application/json'"

    if [ -n "$data" ]; then
        curl $curl_args -d "$data" "$url"
    else
        curl $curl_args "$url"
    fi
}

# Redis CLI helper
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

# Get current ArangoDB schema version
get_arango_version() {
    local result
    result=$(arango_api GET "/_api/collection/_migrations" "" "$ARANGO_DATABASE" 2>/dev/null) || true

    if echo "$result" | grep -q '"error":true'; then
        echo "0"
        return
    fi

    result=$(arango_api POST "/_api/cursor" '{"query": "FOR m IN _migrations SORT m.version DESC LIMIT 1 RETURN m.version"}' "$ARANGO_DATABASE" 2>/dev/null) || echo '{"result":[]}'
    echo "$result" | jq -r '.result[0] // 0'
}

# Get current Dragonfly schema version
get_dragonfly_version() {
    redis_cmd "GET migration:version" 2>/dev/null || echo "0"
}

# Record migration in ArangoDB
record_arango_migration() {
    local version="$1"
    local name="$2"
    local direction="$3"  # up or down

    local timestamp
    timestamp=$(date -u +%Y-%m-%dT%H:%M:%SZ)

    # Ensure migrations collection exists
    arango_api POST "/_api/collection" '{"name": "_migrations"}' "$ARANGO_DATABASE" >/dev/null 2>&1 || true

    local data
    data=$(cat <<EOF
{
    "_key": "${version}_${direction}_$(date +%s)",
    "version": $version,
    "name": "$name",
    "direction": "$direction",
    "applied_at": "$timestamp"
}
EOF
)

    arango_api POST "/_api/document/_migrations" "$data" "$ARANGO_DATABASE"
}

# Record migration in Dragonfly
record_dragonfly_migration() {
    local version="$1"
    local name="$2"
    local direction="$3"

    local timestamp
    timestamp=$(date -u +%Y-%m-%dT%H:%M:%SZ)

    redis_cmd "SET migration:version $version"
    redis_cmd "LPUSH migration:history \"$timestamp|$version|$name|$direction\""
    redis_cmd "LTRIM migration:history 0 99"  # Keep last 100 entries
}

# Create migrations directory if needed
ensure_migrations_dir() {
    if [ ! -d "$MIGRATIONS_DIR" ]; then
        mkdir -p "$MIGRATIONS_DIR"
        log "Created migrations directory: $MIGRATIONS_DIR"
    fi
}

# Get list of pending migrations
get_pending_migrations() {
    local current_version="$1"
    local db="$2"

    ensure_migrations_dir

    # Find all migration files sorted by version
    find "$MIGRATIONS_DIR" -name "*.sh" -type f 2>/dev/null | \
        grep -E "^$MIGRATIONS_DIR/[0-9]+_.*_${db}\.sh$" | \
        sort | \
        while read -r file; do
            local version
            version=$(basename "$file" | cut -d'_' -f1)
            if [ "$version" -gt "$current_version" ]; then
                echo "$file"
            fi
        done
}

# Show migration status
cmd_status() {
    log "Migration Status"
    log "================"

    if [ "$DB_TYPE" = "all" ] || [ "$DB_TYPE" = "arangodb" ]; then
        local arango_version
        arango_version=$(get_arango_version)
        log "ArangoDB version: $arango_version"

        local pending
        pending=$(get_pending_migrations "$arango_version" "arangodb" | wc -l)
        log "ArangoDB pending migrations: $pending"
    fi

    if [ "$DB_TYPE" = "all" ] || [ "$DB_TYPE" = "dragonfly" ]; then
        local dragonfly_version
        dragonfly_version=$(get_dragonfly_version)
        log "Dragonfly version: $dragonfly_version"

        local pending
        pending=$(get_pending_migrations "$dragonfly_version" "dragonfly" | wc -l)
        log "Dragonfly pending migrations: $pending"
    fi
}

# Apply pending migrations
cmd_up() {
    log "Applying pending migrations..."

    if [ "$DB_TYPE" = "all" ] || [ "$DB_TYPE" = "arangodb" ]; then
        local arango_version
        arango_version=$(get_arango_version)
        log "ArangoDB current version: $arango_version"

        get_pending_migrations "$arango_version" "arangodb" | while read -r file; do
            if [ -n "$file" ]; then
                local name
                name=$(basename "$file" .sh)
                local version
                version=$(echo "$name" | cut -d'_' -f1)

                log "Applying ArangoDB migration: $name"

                if [ "$DRY_RUN" = true ]; then
                    log_dry "Would execute: $file up"
                else
                    # Source and run the migration
                    export ARANGO_HOST ARANGO_PORT ARANGO_USER ARANGO_PASSWORD ARANGO_DATABASE
                    sh "$file" up

                    record_arango_migration "$version" "$name" "up"
                    log "Applied: $name"
                fi
            fi
        done
    fi

    if [ "$DB_TYPE" = "all" ] || [ "$DB_TYPE" = "dragonfly" ]; then
        local dragonfly_version
        dragonfly_version=$(get_dragonfly_version)
        log "Dragonfly current version: $dragonfly_version"

        get_pending_migrations "$dragonfly_version" "dragonfly" | while read -r file; do
            if [ -n "$file" ]; then
                local name
                name=$(basename "$file" .sh)
                local version
                version=$(echo "$name" | cut -d'_' -f1)

                log "Applying Dragonfly migration: $name"

                if [ "$DRY_RUN" = true ]; then
                    log_dry "Would execute: $file up"
                else
                    export DRAGONFLY_HOST DRAGONFLY_PORT DRAGONFLY_PASSWORD
                    sh "$file" up

                    record_dragonfly_migration "$version" "$name" "up"
                    log "Applied: $name"
                fi
            fi
        done
    fi

    log "Migrations complete"
}

# Rollback last migration
cmd_down() {
    log "Rolling back last migration..."

    if [ "$DB_TYPE" = "all" ] || [ "$DB_TYPE" = "arangodb" ]; then
        local arango_version
        arango_version=$(get_arango_version)

        if [ "$arango_version" = "0" ]; then
            log "ArangoDB: No migrations to rollback"
        else
            # Find migration file for current version
            local file
            file=$(find "$MIGRATIONS_DIR" -name "${arango_version}_*_arangodb.sh" -type f 2>/dev/null | head -1)

            if [ -n "$file" ]; then
                local name
                name=$(basename "$file" .sh)

                log "Rolling back ArangoDB migration: $name"

                if [ "$DRY_RUN" = true ]; then
                    log_dry "Would execute: $file down"
                else
                    export ARANGO_HOST ARANGO_PORT ARANGO_USER ARANGO_PASSWORD ARANGO_DATABASE
                    sh "$file" down

                    # Find previous version
                    local prev_version=$((arango_version - 1))
                    record_arango_migration "$prev_version" "$name" "down"
                    log "Rolled back: $name"
                fi
            else
                log "ArangoDB: Migration file not found for version $arango_version"
            fi
        fi
    fi

    if [ "$DB_TYPE" = "all" ] || [ "$DB_TYPE" = "dragonfly" ]; then
        local dragonfly_version
        dragonfly_version=$(get_dragonfly_version)

        if [ "$dragonfly_version" = "0" ]; then
            log "Dragonfly: No migrations to rollback"
        else
            local file
            file=$(find "$MIGRATIONS_DIR" -name "${dragonfly_version}_*_dragonfly.sh" -type f 2>/dev/null | head -1)

            if [ -n "$file" ]; then
                local name
                name=$(basename "$file" .sh)

                log "Rolling back Dragonfly migration: $name"

                if [ "$DRY_RUN" = true ]; then
                    log_dry "Would execute: $file down"
                else
                    export DRAGONFLY_HOST DRAGONFLY_PORT DRAGONFLY_PASSWORD
                    sh "$file" down

                    local prev_version=$((dragonfly_version - 1))
                    record_dragonfly_migration "$prev_version" "$name" "down"
                    log "Rolled back: $name"
                fi
            else
                log "Dragonfly: Migration file not found for version $dragonfly_version"
            fi
        fi
    fi
}

# Rollback N migrations
cmd_rollback() {
    local count="$1"

    if [ -z "$count" ] || [ "$count" -lt 1 ]; then
        error "Invalid rollback count: $count"
    fi

    log "Rolling back $count migrations..."

    for i in $(seq 1 "$count"); do
        cmd_down
    done
}

# Show current version
cmd_version() {
    if [ "$DB_TYPE" = "all" ] || [ "$DB_TYPE" = "arangodb" ]; then
        local arango_version
        arango_version=$(get_arango_version)
        echo "ArangoDB: $arango_version"
    fi

    if [ "$DB_TYPE" = "all" ] || [ "$DB_TYPE" = "dragonfly" ]; then
        local dragonfly_version
        dragonfly_version=$(get_dragonfly_version)
        echo "Dragonfly: $dragonfly_version"
    fi
}

# Create new migration file
cmd_create() {
    local name="$1"

    if [ -z "$name" ]; then
        error "Migration name is required"
    fi

    ensure_migrations_dir

    # Generate version number (timestamp-based)
    local version
    version=$(date +%Y%m%d%H%M%S)

    # Create ArangoDB migration
    local arango_file="$MIGRATIONS_DIR/${version}_${name}_arangodb.sh"
    cat > "$arango_file" <<'MIGRATION'
#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
# Migration: MIGRATION_NAME
# Type: ArangoDB

set -e

DIRECTION="$1"

arango_api() {
    local method="$1"
    local endpoint="$2"
    local data="$3"

    local url="http://${ARANGO_HOST}:${ARANGO_PORT}/_db/${ARANGO_DATABASE}${endpoint}"
    local curl_args="-s -X $method -u ${ARANGO_USER}:${ARANGO_PASSWORD} -H 'Content-Type: application/json'"

    if [ -n "$data" ]; then
        curl $curl_args -d "$data" "$url"
    else
        curl $curl_args "$url"
    fi
}

up() {
    echo "Applying migration: MIGRATION_NAME"
    # TODO: Add up migration logic
    # Example: arango_api POST "/_api/collection" '{"name": "new_collection"}'
}

down() {
    echo "Rolling back migration: MIGRATION_NAME"
    # TODO: Add down migration logic
    # Example: arango_api DELETE "/_api/collection/new_collection"
}

case "$DIRECTION" in
    up)   up   ;;
    down) down ;;
    *)    echo "Usage: $0 {up|down}" >&2; exit 1 ;;
esac
MIGRATION

    sed -i "s/MIGRATION_NAME/$name/g" "$arango_file"
    chmod +x "$arango_file"
    log "Created: $arango_file"

    # Create Dragonfly migration
    local dragonfly_file="$MIGRATIONS_DIR/${version}_${name}_dragonfly.sh"
    cat > "$dragonfly_file" <<'MIGRATION'
#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
# Migration: MIGRATION_NAME
# Type: Dragonfly

set -e

DIRECTION="$1"

redis_cmd() {
    local auth_args=""
    if [ -n "$DRAGONFLY_PASSWORD" ]; then
        auth_args="-a $DRAGONFLY_PASSWORD"
    fi
    redis-cli -h "$DRAGONFLY_HOST" -p "$DRAGONFLY_PORT" $auth_args "$@" 2>/dev/null
}

up() {
    echo "Applying migration: MIGRATION_NAME"
    # TODO: Add up migration logic
    # Example: redis_cmd SET "config:feature_x" "enabled"
}

down() {
    echo "Rolling back migration: MIGRATION_NAME"
    # TODO: Add down migration logic
    # Example: redis_cmd DEL "config:feature_x"
}

case "$DIRECTION" in
    up)   up   ;;
    down) down ;;
    *)    echo "Usage: $0 {up|down}" >&2; exit 1 ;;
esac
MIGRATION

    sed -i "s/MIGRATION_NAME/$name/g" "$dragonfly_file"
    chmod +x "$dragonfly_file"
    log "Created: $dragonfly_file"

    log "Migration files created with version: $version"
}

# =============================================================================
# MAIN
# =============================================================================

main() {
    local command=""
    local command_arg=""

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
            --db)
                shift
                DB_TYPE="$1"
                if [ "$DB_TYPE" != "all" ] && [ "$DB_TYPE" != "arangodb" ] && [ "$DB_TYPE" != "dragonfly" ]; then
                    error "Invalid database type: $DB_TYPE"
                fi
                ;;
            status|up|down|version)
                command="$1"
                ;;
            rollback|create)
                command="$1"
                shift
                command_arg="$1"
                ;;
            *)
                if [ -z "$command" ]; then
                    error "Unknown command: $1"
                fi
                ;;
        esac
        shift
    done

    if [ -z "$command" ]; then
        error "No command specified. Use --help for usage."
    fi

    log "============================================================"
    log "cicd-hyper-a Database Migration"
    log "============================================================"
    log "Database: $DB_TYPE"
    log "Command: $command"
    log "Dry run: $DRY_RUN"
    log "============================================================"

    check_dependencies

    case "$command" in
        status)
            cmd_status
            ;;
        up)
            cmd_up
            ;;
        down)
            cmd_down
            ;;
        rollback)
            cmd_rollback "$command_arg"
            ;;
        version)
            cmd_version
            ;;
        create)
            cmd_create "$command_arg"
            ;;
    esac
}

main "$@"
