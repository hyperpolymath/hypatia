#!/bin/sh
# SPDX-License-Identifier: PLMP-1.0-or-later
# cicd-hyper-a Database Backup Script
#
# Creates backups of ArangoDB and Dragonfly databases with rotation.
# Supports local storage, compression, and optional remote upload.
#
# Usage:
#   ./backup.sh [OPTIONS]
#
# Options:
#   -h, --help          Show this help message
#   -n, --dry-run       Show what would be done without executing
#   -v, --verbose       Enable verbose output
#   --db TYPE           Database type: arangodb, dragonfly, or all (default: all)
#   --no-compress       Skip compression
#   --keep N            Number of backups to keep (default: 7)
#   --prefix PREFIX     Backup filename prefix (default: cicd-hyper-a)
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
#   BACKUP_REMOTE       Remote backup destination (e.g., s3://bucket/path)
#
# Examples:
#   ./backup.sh
#   ./backup.sh --db arangodb
#   ./backup.sh --keep 14 --prefix daily
#   BACKUP_DIR=/mnt/backups ./backup.sh

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
BACKUP_REMOTE="${BACKUP_REMOTE:-}"

DRY_RUN=false
VERBOSE=false
DB_TYPE="all"
COMPRESS=true
KEEP_COUNT=7
PREFIX="cicd-hyper-a"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)

# =============================================================================
# FUNCTIONS
# =============================================================================

usage() {
    sed -n '3,33p' "$0" | sed 's/^# //' | sed 's/^#//'
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
        if ! command -v arangodump >/dev/null 2>&1; then
            log "Warning: arangodump not found, will use HTTP API for backup"
        fi
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

    if [ "$COMPRESS" = true ]; then
        if ! command -v gzip >/dev/null 2>&1; then
            error "gzip is required but not installed"
        fi
    fi

    if [ -n "$BACKUP_REMOTE" ]; then
        case "$BACKUP_REMOTE" in
            s3://*)
                if ! command -v aws >/dev/null 2>&1; then
                    error "aws CLI is required for S3 backup but not installed"
                fi
                ;;
            gs://*)
                if ! command -v gsutil >/dev/null 2>&1; then
                    error "gsutil is required for GCS backup but not installed"
                fi
                ;;
        esac
    fi
}

ensure_backup_dir() {
    if [ ! -d "$BACKUP_DIR" ]; then
        if [ "$DRY_RUN" = true ]; then
            log_dry "Would create backup directory: $BACKUP_DIR"
        else
            mkdir -p "$BACKUP_DIR"
            log "Created backup directory: $BACKUP_DIR"
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

backup_arangodb() {
    log "Backing up ArangoDB..."

    local backup_name="${PREFIX}_arangodb_${TIMESTAMP}"
    local backup_path="$BACKUP_DIR/$backup_name"

    if [ "$DRY_RUN" = true ]; then
        log_dry "Would create ArangoDB backup: $backup_path"
        return 0
    fi

    # Check connection
    local version
    version=$(arango_api GET "/_api/version" "" "_system" | jq -r '.version // empty')
    if [ -z "$version" ]; then
        error "Cannot connect to ArangoDB"
    fi
    log_verbose "Connected to ArangoDB version $version"

    # Use arangodump if available
    if command -v arangodump >/dev/null 2>&1; then
        log "Using arangodump for backup..."

        arangodump \
            --server.endpoint "tcp://${ARANGO_HOST}:${ARANGO_PORT}" \
            --server.username "$ARANGO_USER" \
            --server.password "$ARANGO_PASSWORD" \
            --server.database "$ARANGO_DATABASE" \
            --output-directory "$backup_path" \
            --overwrite true \
            --compress-output true

        log "ArangoDB dump complete: $backup_path"
    else
        # Fallback: Export each collection via HTTP API
        log "Using HTTP API for backup (arangodump not available)..."

        mkdir -p "$backup_path"

        # Get list of collections
        local collections
        collections=$(arango_api GET "/_api/collection" "" "$ARANGO_DATABASE" | jq -r '.result[] | select(.isSystem == false) | .name')

        for collection in $collections; do
            log_verbose "Exporting collection: $collection"

            local export_file="$backup_path/${collection}.json"

            # Export collection data
            arango_api POST "/_api/cursor" "{\"query\": \"FOR doc IN $collection RETURN doc\", \"batchSize\": 10000}" "$ARANGO_DATABASE" | jq -c '.result[]' > "$export_file"

            # Export collection metadata
            arango_api GET "/_api/collection/${collection}/properties" "" "$ARANGO_DATABASE" > "$backup_path/${collection}.meta.json"
        done

        # Export graph definitions
        arango_api GET "/_api/gharial" "" "$ARANGO_DATABASE" | jq '.graphs' > "$backup_path/_graphs.json"

        # Export views
        local views
        views=$(arango_api GET "/_api/view" "" "$ARANGO_DATABASE" | jq -r '.result[].name')
        for view in $views; do
            arango_api GET "/_api/view/${view}/properties" "" "$ARANGO_DATABASE" > "$backup_path/_view_${view}.json"
        done

        log "HTTP API backup complete: $backup_path"
    fi

    # Compress if requested
    if [ "$COMPRESS" = true ]; then
        log "Compressing backup..."
        tar -czf "${backup_path}.tar.gz" -C "$BACKUP_DIR" "$backup_name"
        rm -rf "$backup_path"
        backup_path="${backup_path}.tar.gz"
        log "Compressed: $backup_path"
    fi

    # Calculate size
    local size
    size=$(du -sh "$backup_path" | cut -f1)
    log "ArangoDB backup complete: $backup_path ($size)"

    echo "$backup_path"
}

backup_dragonfly() {
    log "Backing up Dragonfly..."

    local backup_name="${PREFIX}_dragonfly_${TIMESTAMP}"
    local backup_path="$BACKUP_DIR/${backup_name}.rdb"

    if [ "$DRY_RUN" = true ]; then
        log_dry "Would create Dragonfly backup: $backup_path"
        return 0
    fi

    # Check connection
    local ping
    ping=$(redis_cmd PING)
    if [ "$ping" != "PONG" ]; then
        error "Cannot connect to Dragonfly"
    fi
    log_verbose "Connected to Dragonfly"

    # Trigger BGSAVE
    log "Triggering background save..."
    redis_cmd BGSAVE

    # Wait for save to complete
    local max_wait=300  # 5 minutes
    local waited=0
    while [ $waited -lt $max_wait ]; do
        local lastsave
        lastsave=$(redis_cmd LASTSAVE)
        local bgsave_status
        bgsave_status=$(redis_cmd INFO persistence | grep -E "^rdb_bgsave_in_progress:" | cut -d: -f2 | tr -d '\r')

        if [ "$bgsave_status" = "0" ]; then
            break
        fi

        sleep 2
        waited=$((waited + 2))
        log_verbose "Waiting for BGSAVE to complete... ($waited seconds)"
    done

    if [ $waited -ge $max_wait ]; then
        error "BGSAVE timed out after $max_wait seconds"
    fi

    # Get RDB file location and copy
    local rdb_dir
    rdb_dir=$(redis_cmd CONFIG GET dir | sed -n '2p')
    local rdb_file
    rdb_file=$(redis_cmd CONFIG GET dbfilename | sed -n '2p')

    if [ -f "$rdb_dir/$rdb_file" ]; then
        cp "$rdb_dir/$rdb_file" "$backup_path"
        log "Copied RDB file to: $backup_path"
    else
        # Fallback: dump keys manually
        log "RDB file not accessible, using DUMP command..."

        local dump_path="$BACKUP_DIR/${backup_name}.dump"
        mkdir -p "$(dirname "$dump_path")"

        # Get all keys and dump them
        local keys
        keys=$(redis_cmd KEYS "*")
        echo "# Dragonfly backup $TIMESTAMP" > "$dump_path"
        echo "# Keys: $(echo "$keys" | wc -w)" >> "$dump_path"

        for key in $keys; do
            local type
            type=$(redis_cmd TYPE "$key")
            local ttl
            ttl=$(redis_cmd TTL "$key")

            echo "# Key: $key, Type: $type, TTL: $ttl" >> "$dump_path"

            case "$type" in
                string)
                    local val
                    val=$(redis_cmd GET "$key")
                    echo "SET \"$key\" \"$val\"" >> "$dump_path"
                    ;;
                hash)
                    redis_cmd HGETALL "$key" | while read -r field && read -r value; do
                        echo "HSET \"$key\" \"$field\" \"$value\"" >> "$dump_path"
                    done
                    ;;
                list)
                    local vals
                    vals=$(redis_cmd LRANGE "$key" 0 -1)
                    for val in $vals; do
                        echo "RPUSH \"$key\" \"$val\"" >> "$dump_path"
                    done
                    ;;
                set)
                    local members
                    members=$(redis_cmd SMEMBERS "$key")
                    for member in $members; do
                        echo "SADD \"$key\" \"$member\"" >> "$dump_path"
                    done
                    ;;
                zset)
                    redis_cmd ZRANGE "$key" 0 -1 WITHSCORES | while read -r member && read -r score; do
                        echo "ZADD \"$key\" $score \"$member\"" >> "$dump_path"
                    done
                    ;;
            esac

            if [ "$ttl" -gt 0 ]; then
                echo "EXPIRE \"$key\" $ttl" >> "$dump_path"
            fi
        done

        backup_path="$dump_path"
    fi

    # Compress if requested
    if [ "$COMPRESS" = true ]; then
        log "Compressing backup..."
        gzip -f "$backup_path"
        backup_path="${backup_path}.gz"
        log "Compressed: $backup_path"
    fi

    # Calculate size
    local size
    size=$(du -sh "$backup_path" | cut -f1)
    log "Dragonfly backup complete: $backup_path ($size)"

    echo "$backup_path"
}

upload_remote() {
    local backup_file="$1"

    if [ -z "$BACKUP_REMOTE" ]; then
        return 0
    fi

    log "Uploading to remote: $BACKUP_REMOTE"

    if [ "$DRY_RUN" = true ]; then
        log_dry "Would upload $backup_file to $BACKUP_REMOTE"
        return 0
    fi

    local filename
    filename=$(basename "$backup_file")

    case "$BACKUP_REMOTE" in
        s3://*)
            aws s3 cp "$backup_file" "${BACKUP_REMOTE}/${filename}"
            ;;
        gs://*)
            gsutil cp "$backup_file" "${BACKUP_REMOTE}/${filename}"
            ;;
        rsync://*)
            local dest
            dest=$(echo "$BACKUP_REMOTE" | sed 's/rsync:\/\///')
            rsync -avz "$backup_file" "$dest/"
            ;;
        *)
            # Assume it's an SSH/SCP destination
            scp "$backup_file" "${BACKUP_REMOTE}/"
            ;;
    esac

    log "Uploaded: $filename"
}

rotate_backups() {
    log "Rotating old backups (keeping last $KEEP_COUNT)..."

    if [ "$DRY_RUN" = true ]; then
        log_dry "Would rotate backups in $BACKUP_DIR"
        return 0
    fi

    # Rotate ArangoDB backups
    local arango_backups
    arango_backups=$(ls -1t "$BACKUP_DIR"/${PREFIX}_arangodb_*.tar.gz 2>/dev/null || true)
    local count=0
    for backup in $arango_backups; do
        count=$((count + 1))
        if [ $count -gt $KEEP_COUNT ]; then
            log_verbose "Removing old backup: $backup"
            rm -f "$backup"
        fi
    done

    # Rotate Dragonfly backups
    local dragonfly_backups
    dragonfly_backups=$(ls -1t "$BACKUP_DIR"/${PREFIX}_dragonfly_*.* 2>/dev/null || true)
    count=0
    for backup in $dragonfly_backups; do
        count=$((count + 1))
        if [ $count -gt $KEEP_COUNT ]; then
            log_verbose "Removing old backup: $backup"
            rm -f "$backup"
        fi
    done

    log "Rotation complete"
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
                if [ "$DB_TYPE" != "all" ] && [ "$DB_TYPE" != "arangodb" ] && [ "$DB_TYPE" != "dragonfly" ]; then
                    error "Invalid database type: $DB_TYPE"
                fi
                ;;
            --no-compress)
                COMPRESS=false
                ;;
            --keep)
                shift
                KEEP_COUNT="$1"
                ;;
            --prefix)
                shift
                PREFIX="$1"
                ;;
            *)
                error "Unknown option: $1"
                ;;
        esac
        shift
    done

    log "============================================================"
    log "cicd-hyper-a Database Backup"
    log "============================================================"
    log "Database: $DB_TYPE"
    log "Backup directory: $BACKUP_DIR"
    log "Compress: $COMPRESS"
    log "Keep count: $KEEP_COUNT"
    log "Remote: ${BACKUP_REMOTE:-none}"
    log "Dry run: $DRY_RUN"
    log "============================================================"

    check_dependencies
    ensure_backup_dir

    local backups=""

    if [ "$DB_TYPE" = "all" ] || [ "$DB_TYPE" = "arangodb" ]; then
        if [ -z "$ARANGO_PASSWORD" ]; then
            error "ARANGO_PASSWORD is required for ArangoDB backup"
        fi
        local arango_backup
        arango_backup=$(backup_arangodb)
        backups="$backups $arango_backup"

        if [ -n "$arango_backup" ]; then
            upload_remote "$arango_backup"
        fi
    fi

    if [ "$DB_TYPE" = "all" ] || [ "$DB_TYPE" = "dragonfly" ]; then
        local dragonfly_backup
        dragonfly_backup=$(backup_dragonfly)
        backups="$backups $dragonfly_backup"

        if [ -n "$dragonfly_backup" ]; then
            upload_remote "$dragonfly_backup"
        fi
    fi

    rotate_backups

    log "============================================================"
    log "Backup complete!"
    log "============================================================"

    # List backups
    if [ "$DRY_RUN" = false ]; then
        log "Current backups:"
        ls -lh "$BACKUP_DIR"/${PREFIX}_* 2>/dev/null | tail -10 || true
    fi
}

main "$@"
