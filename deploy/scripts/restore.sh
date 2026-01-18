#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
# cicd-hyper-a Database Restore Script
#
# Restores ArangoDB and Dragonfly databases from backups.
# Supports local and remote backup sources.
#
# Usage:
#   ./restore.sh [OPTIONS] BACKUP_FILE
#
# Options:
#   -h, --help          Show this help message
#   -n, --dry-run       Show what would be done without executing
#   -v, --verbose       Enable verbose output
#   --db TYPE           Database type: arangodb, dragonfly, or auto (default: auto)
#   --drop              Drop existing data before restore
#   --no-confirm        Skip confirmation prompt
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
#   BACKUP_DIR          Backup directory (default: /var/backups/cicd-hyper-a)
#
# Examples:
#   ./restore.sh cicd-hyper-a_arangodb_20240115_120000.tar.gz
#   ./restore.sh --db dragonfly backup.rdb.gz
#   ./restore.sh --drop --no-confirm latest_backup.tar.gz

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

BACKUP_DIR="${BACKUP_DIR:-/var/backups/cicd-hyper-a}"

DRY_RUN=false
VERBOSE=false
DB_TYPE="auto"
DROP_EXISTING=false
NO_CONFIRM=false
BACKUP_FILE=""

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TEMP_DIR=""

# =============================================================================
# FUNCTIONS
# =============================================================================

usage() {
    sed -n '3,32p' "$0" | sed 's/^# //' | sed 's/^#//'
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
    cleanup
    exit 1
}

cleanup() {
    if [ -n "$TEMP_DIR" ] && [ -d "$TEMP_DIR" ]; then
        log_verbose "Cleaning up temporary directory: $TEMP_DIR"
        rm -rf "$TEMP_DIR"
    fi
}

trap cleanup EXIT

check_dependencies() {
    log_verbose "Checking dependencies..."

    if [ "$DB_TYPE" = "auto" ] || [ "$DB_TYPE" = "arangodb" ]; then
        if ! command -v curl >/dev/null 2>&1; then
            error "curl is required but not installed"
        fi
        if ! command -v jq >/dev/null 2>&1; then
            error "jq is required but not installed"
        fi
    fi

    if [ "$DB_TYPE" = "auto" ] || [ "$DB_TYPE" = "dragonfly" ]; then
        if ! command -v redis-cli >/dev/null 2>&1; then
            error "redis-cli is required but not installed"
        fi
    fi

    if ! command -v gzip >/dev/null 2>&1; then
        error "gzip is required but not installed"
    fi

    if ! command -v tar >/dev/null 2>&1; then
        error "tar is required but not installed"
    fi
}

detect_backup_type() {
    local file="$1"
    local filename
    filename=$(basename "$file")

    if echo "$filename" | grep -q "arangodb"; then
        echo "arangodb"
    elif echo "$filename" | grep -q "dragonfly"; then
        echo "dragonfly"
    elif echo "$filename" | grep -qE "\.rdb"; then
        echo "dragonfly"
    elif echo "$filename" | grep -qE "\.dump"; then
        echo "dragonfly"
    else
        echo "unknown"
    fi
}

confirm_restore() {
    if [ "$NO_CONFIRM" = true ]; then
        return 0
    fi

    if [ "$DRY_RUN" = true ]; then
        return 0
    fi

    echo ""
    echo "WARNING: This will restore data to the database."
    if [ "$DROP_EXISTING" = true ]; then
        echo "DANGER: Existing data will be DROPPED before restore!"
    fi
    echo ""
    echo "Backup file: $BACKUP_FILE"
    echo "Database: $DB_TYPE"
    echo ""

    printf "Are you sure you want to continue? (yes/no): "
    read -r confirm

    if [ "$confirm" != "yes" ]; then
        log "Restore cancelled"
        exit 0
    fi
}

# ArangoDB API helper
arango_api() {
    local method="$1"
    local endpoint="$2"
    local data="$3"
    local db="${4:-$ARANGO_DATABASE}"

    local url="http://${ARANGO_HOST}:${ARANGO_PORT}/_db/${db}${endpoint}"

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
    local auth_args=""
    if [ -n "$DRAGONFLY_PASSWORD" ]; then
        auth_args="-a $DRAGONFLY_PASSWORD"
    fi
    redis-cli -h "$DRAGONFLY_HOST" -p "$DRAGONFLY_PORT" $auth_args "$@" 2>/dev/null
}

extract_backup() {
    local backup_file="$1"

    TEMP_DIR=$(mktemp -d)
    log_verbose "Extracting to temporary directory: $TEMP_DIR"

    # Determine extraction method
    if echo "$backup_file" | grep -qE "\.tar\.gz$"; then
        tar -xzf "$backup_file" -C "$TEMP_DIR"
    elif echo "$backup_file" | grep -qE "\.tar$"; then
        tar -xf "$backup_file" -C "$TEMP_DIR"
    elif echo "$backup_file" | grep -qE "\.gz$"; then
        gunzip -c "$backup_file" > "$TEMP_DIR/$(basename "$backup_file" .gz)"
    else
        cp "$backup_file" "$TEMP_DIR/"
    fi

    echo "$TEMP_DIR"
}

restore_arangodb() {
    local backup_path="$1"

    log "Restoring ArangoDB..."

    if [ "$DRY_RUN" = true ]; then
        log_dry "Would restore ArangoDB from: $backup_path"
        return 0
    fi

    # Check connection
    local version
    version=$(arango_api GET "/_api/version" "" "_system" | jq -r '.version // empty')
    if [ -z "$version" ]; then
        error "Cannot connect to ArangoDB"
    fi
    log_verbose "Connected to ArangoDB version $version"

    # Ensure database exists
    local db_exists
    db_exists=$(arango_api GET "/_api/database" "" "_system" | jq -r ".result | index(\"$ARANGO_DATABASE\") // empty")
    if [ -z "$db_exists" ]; then
        log "Creating database: $ARANGO_DATABASE"
        arango_api POST "/_api/database" "{\"name\": \"$ARANGO_DATABASE\"}" "_system"
    fi

    # Try arangorestore first
    if command -v arangorestore >/dev/null 2>&1 && [ -d "$backup_path" ]; then
        log "Using arangorestore..."

        local restore_args="--server.endpoint tcp://${ARANGO_HOST}:${ARANGO_PORT}"
        restore_args="$restore_args --server.username $ARANGO_USER"
        restore_args="$restore_args --server.password $ARANGO_PASSWORD"
        restore_args="$restore_args --server.database $ARANGO_DATABASE"
        restore_args="$restore_args --input-directory $backup_path"
        restore_args="$restore_args --create-collection true"

        if [ "$DROP_EXISTING" = true ]; then
            restore_args="$restore_args --overwrite true"
        fi

        arangorestore $restore_args

        log "ArangoDB restore complete"
        return 0
    fi

    # Fallback: Restore via HTTP API
    log "Using HTTP API for restore..."

    # Find JSON files in backup
    local json_files
    json_files=$(find "$backup_path" -name "*.json" ! -name "*.meta.json" ! -name "_*.json" 2>/dev/null || true)

    if [ -z "$json_files" ]; then
        error "No data files found in backup"
    fi

    # Drop collections if requested
    if [ "$DROP_EXISTING" = true ]; then
        log "Dropping existing collections..."
        local collections
        collections=$(arango_api GET "/_api/collection" "" "$ARANGO_DATABASE" | jq -r '.result[] | select(.isSystem == false) | .name')
        for collection in $collections; do
            log_verbose "Dropping collection: $collection"
            arango_api DELETE "/_api/collection/$collection" "" "$ARANGO_DATABASE"
        done
    fi

    # Restore each collection
    for json_file in $json_files; do
        local collection
        collection=$(basename "$json_file" .json)
        local meta_file="$backup_path/${collection}.meta.json"

        log_verbose "Restoring collection: $collection"

        # Create collection if metadata exists
        if [ -f "$meta_file" ]; then
            local type
            type=$(jq -r '.type // 2' "$meta_file")

            # Create collection
            arango_api POST "/_api/collection" "{\"name\": \"$collection\", \"type\": $type}" "$ARANGO_DATABASE" >/dev/null 2>&1 || true
        else
            # Default to document collection
            arango_api POST "/_api/collection" "{\"name\": \"$collection\", \"type\": 2}" "$ARANGO_DATABASE" >/dev/null 2>&1 || true
        fi

        # Import documents
        local doc_count=0
        while IFS= read -r doc; do
            if [ -n "$doc" ]; then
                arango_api POST "/_api/document/$collection" "$doc" "$ARANGO_DATABASE" >/dev/null
                doc_count=$((doc_count + 1))
            fi
        done < "$json_file"

        log_verbose "Restored $doc_count documents to $collection"
    done

    # Restore graphs
    local graphs_file="$backup_path/_graphs.json"
    if [ -f "$graphs_file" ]; then
        log "Restoring graphs..."
        local graphs
        graphs=$(jq -c '.[]' "$graphs_file" 2>/dev/null || true)
        for graph in $graphs; do
            local graph_name
            graph_name=$(echo "$graph" | jq -r '.name')
            log_verbose "Restoring graph: $graph_name"

            # Delete existing graph if present
            arango_api DELETE "/_api/gharial/$graph_name" "" "$ARANGO_DATABASE" >/dev/null 2>&1 || true

            # Create graph
            arango_api POST "/_api/gharial" "$graph" "$ARANGO_DATABASE" >/dev/null 2>&1 || true
        done
    fi

    # Restore views
    local view_files
    view_files=$(find "$backup_path" -name "_view_*.json" 2>/dev/null || true)
    for view_file in $view_files; do
        local view_name
        view_name=$(basename "$view_file" .json | sed 's/^_view_//')
        log_verbose "Restoring view: $view_name"

        local view_def
        view_def=$(cat "$view_file")

        # Delete existing view if present
        arango_api DELETE "/_api/view/$view_name" "" "$ARANGO_DATABASE" >/dev/null 2>&1 || true

        # Create view
        arango_api POST "/_api/view" "$view_def" "$ARANGO_DATABASE" >/dev/null 2>&1 || true
    done

    log "ArangoDB restore complete"
}

restore_dragonfly() {
    local backup_path="$1"

    log "Restoring Dragonfly..."

    if [ "$DRY_RUN" = true ]; then
        log_dry "Would restore Dragonfly from: $backup_path"
        return 0
    fi

    # Check connection
    local ping
    ping=$(redis_cmd PING)
    if [ "$ping" != "PONG" ]; then
        error "Cannot connect to Dragonfly"
    fi
    log_verbose "Connected to Dragonfly"

    # Drop existing if requested
    if [ "$DROP_EXISTING" = true ]; then
        log "Flushing existing data..."
        redis_cmd FLUSHALL
    fi

    # Find backup file
    local backup_file
    if [ -f "$backup_path" ]; then
        backup_file="$backup_path"
    else
        backup_file=$(find "$backup_path" -name "*.rdb" -o -name "*.dump" 2>/dev/null | head -1)
    fi

    if [ -z "$backup_file" ]; then
        error "No Dragonfly backup file found"
    fi

    # Determine restore method
    if echo "$backup_file" | grep -qE "\.rdb"; then
        log "Restoring from RDB file..."

        # Get Dragonfly data directory
        local rdb_dir
        rdb_dir=$(redis_cmd CONFIG GET dir | sed -n '2p')
        local rdb_name
        rdb_name=$(redis_cmd CONFIG GET dbfilename | sed -n '2p')

        # Stop Dragonfly, copy RDB, restart
        # This requires direct access or Docker exec
        log "RDB restore requires manual intervention:"
        log "1. Stop Dragonfly"
        log "2. Copy $backup_file to $rdb_dir/$rdb_name"
        log "3. Start Dragonfly"
        error "Automatic RDB restore not supported. Please restore manually."
    elif echo "$backup_file" | grep -qE "\.dump"; then
        log "Restoring from dump file..."

        # Execute Redis commands from dump file
        local line_count=0
        local error_count=0

        while IFS= read -r line; do
            # Skip comments
            if echo "$line" | grep -q "^#"; then
                continue
            fi

            if [ -n "$line" ]; then
                redis_cmd $line >/dev/null 2>&1 || error_count=$((error_count + 1))
                line_count=$((line_count + 1))
            fi
        done < "$backup_file"

        log "Executed $line_count commands ($error_count errors)"
    else
        error "Unknown backup format: $backup_file"
    fi

    log "Dragonfly restore complete"
}

list_backups() {
    log "Available backups in $BACKUP_DIR:"
    echo ""

    if [ -d "$BACKUP_DIR" ]; then
        ls -lh "$BACKUP_DIR"/ 2>/dev/null | grep -v "^total" || echo "No backups found"
    else
        echo "Backup directory does not exist"
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
            --db)
                shift
                DB_TYPE="$1"
                if [ "$DB_TYPE" != "auto" ] && [ "$DB_TYPE" != "arangodb" ] && [ "$DB_TYPE" != "dragonfly" ]; then
                    error "Invalid database type: $DB_TYPE"
                fi
                ;;
            --drop)
                DROP_EXISTING=true
                ;;
            --no-confirm)
                NO_CONFIRM=true
                ;;
            --list)
                list_backups
                exit 0
                ;;
            -*)
                error "Unknown option: $1"
                ;;
            *)
                BACKUP_FILE="$1"
                ;;
        esac
        shift
    done

    if [ -z "$BACKUP_FILE" ]; then
        error "Backup file is required. Use --list to see available backups."
    fi

    # Resolve backup file path
    if [ ! -f "$BACKUP_FILE" ]; then
        if [ -f "$BACKUP_DIR/$BACKUP_FILE" ]; then
            BACKUP_FILE="$BACKUP_DIR/$BACKUP_FILE"
        else
            error "Backup file not found: $BACKUP_FILE"
        fi
    fi

    # Auto-detect database type if needed
    if [ "$DB_TYPE" = "auto" ]; then
        DB_TYPE=$(detect_backup_type "$BACKUP_FILE")
        if [ "$DB_TYPE" = "unknown" ]; then
            error "Cannot auto-detect backup type. Please specify with --db"
        fi
        log "Auto-detected backup type: $DB_TYPE"
    fi

    log "============================================================"
    log "cicd-hyper-a Database Restore"
    log "============================================================"
    log "Database: $DB_TYPE"
    log "Backup file: $BACKUP_FILE"
    log "Drop existing: $DROP_EXISTING"
    log "Dry run: $DRY_RUN"
    log "============================================================"

    check_dependencies
    confirm_restore

    # Extract backup
    local backup_path
    backup_path=$(extract_backup "$BACKUP_FILE")

    # Find actual data directory (might be nested)
    local data_dir
    data_dir=$(find "$backup_path" -type d -name "*_${DB_TYPE}_*" 2>/dev/null | head -1)
    if [ -z "$data_dir" ]; then
        data_dir="$backup_path"
    fi

    # Perform restore
    case "$DB_TYPE" in
        arangodb)
            if [ -z "$ARANGO_PASSWORD" ]; then
                error "ARANGO_PASSWORD is required for ArangoDB restore"
            fi
            restore_arangodb "$data_dir"
            ;;
        dragonfly)
            restore_dragonfly "$data_dir"
            ;;
    esac

    log "============================================================"
    log "Restore complete!"
    log "============================================================"
}

main "$@"
