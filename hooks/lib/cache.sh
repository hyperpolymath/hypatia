#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
# cicd-hyper-a Git Hooks - Cache Functions
# Dragonfly/Redis cache integration for ruleset caching
#
# This file is designed to be sourced by hook scripts:
#   . "$(dirname "$0")/lib/cache.sh"

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Dragonfly/Redis connection settings
CICD_CACHE_HOST="${CICD_CACHE_HOST:-localhost}"
CICD_CACHE_PORT="${CICD_CACHE_PORT:-6379}"
CICD_CACHE_DB="${CICD_CACHE_DB:-0}"
CICD_CACHE_PASSWORD="${CICD_CACHE_PASSWORD:-}"
CICD_CACHE_TTL="${CICD_CACHE_TTL:-3600}"  # 1 hour default TTL

# Cache key prefix
CICD_CACHE_PREFIX="cicd-hyper-a:hooks"

# Fallback to local file cache if Redis unavailable
CICD_FILE_CACHE_DIR="${CICD_CACHE_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/cicd-hyper-a}/rulesets"

# ==============================================================================
# REDIS/DRAGONFLY CLIENT
# ==============================================================================

# Check if redis-cli is available
has_redis_cli() {
    command -v redis-cli >/dev/null 2>&1
}

# Check if cache server is reachable
is_cache_available() {
    if ! has_redis_cli; then
        return 1
    fi

    _auth=""
    [ -n "$CICD_CACHE_PASSWORD" ] && _auth="-a $CICD_CACHE_PASSWORD"

    redis-cli -h "$CICD_CACHE_HOST" -p "$CICD_CACHE_PORT" $_auth PING 2>/dev/null | grep -q "PONG"
}

# Execute Redis command
# Usage: result=$(redis_cmd "GET" "key")
redis_cmd() {
    if ! has_redis_cli; then
        return 1
    fi

    _auth=""
    [ -n "$CICD_CACHE_PASSWORD" ] && _auth="-a $CICD_CACHE_PASSWORD"

    redis-cli -h "$CICD_CACHE_HOST" -p "$CICD_CACHE_PORT" -n "$CICD_CACHE_DB" $_auth "$@" 2>/dev/null
}

# ==============================================================================
# CACHE OPERATIONS
# ==============================================================================

# Get value from cache (tries Redis first, then file cache)
# Usage: value=$(cache_get "ruleset:security")
cache_get() {
    _key="$1"
    _full_key="$CICD_CACHE_PREFIX:$_key"

    # Try Redis/Dragonfly first
    if is_cache_available; then
        _value=$(redis_cmd GET "$_full_key")
        if [ -n "$_value" ] && [ "$_value" != "(nil)" ]; then
            log_debug "Cache hit (redis): $_key"
            echo "$_value"
            return 0
        fi
    fi

    # Fallback to file cache
    _cache_file="$CICD_FILE_CACHE_DIR/$(echo "$_key" | sed 's/[:/]/_/g')"
    if [ -f "$_cache_file" ]; then
        # Check if cache is still valid (not older than TTL)
        _now=$(date +%s)
        _mtime=$(stat -c %Y "$_cache_file" 2>/dev/null || stat -f %m "$_cache_file" 2>/dev/null)
        _age=$((_now - _mtime))
        if [ "$_age" -lt "$CICD_CACHE_TTL" ]; then
            log_debug "Cache hit (file): $_key"
            cat "$_cache_file"
            return 0
        else
            log_debug "Cache expired (file): $_key"
            rm -f "$_cache_file"
        fi
    fi

    log_debug "Cache miss: $_key"
    return 1
}

# Set value in cache (writes to both Redis and file)
# Usage: cache_set "ruleset:security" "$value" [ttl_seconds]
cache_set() {
    _key="$1"
    _value="$2"
    _ttl="${3:-$CICD_CACHE_TTL}"
    _full_key="$CICD_CACHE_PREFIX:$_key"

    # Set in Redis/Dragonfly if available
    if is_cache_available; then
        redis_cmd SETEX "$_full_key" "$_ttl" "$_value" >/dev/null 2>&1
        log_debug "Cache set (redis): $_key (TTL: ${_ttl}s)"
    fi

    # Also write to file cache for fallback
    mkdir -p "$CICD_FILE_CACHE_DIR"
    _cache_file="$CICD_FILE_CACHE_DIR/$(echo "$_key" | sed 's/[:/]/_/g')"
    echo "$_value" > "$_cache_file"
    log_debug "Cache set (file): $_key"
}

# Delete value from cache
# Usage: cache_delete "ruleset:security"
cache_delete() {
    _key="$1"
    _full_key="$CICD_CACHE_PREFIX:$_key"

    # Delete from Redis
    if is_cache_available; then
        redis_cmd DEL "$_full_key" >/dev/null 2>&1
    fi

    # Delete from file cache
    _cache_file="$CICD_FILE_CACHE_DIR/$(echo "$_key" | sed 's/[:/]/_/g')"
    rm -f "$_cache_file" 2>/dev/null || true

    log_debug "Cache delete: $_key"
}

# Clear all cached rulesets
# Usage: cache_clear_rulesets
cache_clear_rulesets() {
    # Clear from Redis
    if is_cache_available; then
        _keys=$(redis_cmd KEYS "$CICD_CACHE_PREFIX:ruleset:*")
        for _key in $_keys; do
            redis_cmd DEL "$_key" >/dev/null 2>&1
        done
        log_debug "Cleared ruleset cache (redis)"
    fi

    # Clear file cache
    if [ -d "$CICD_FILE_CACHE_DIR" ]; then
        rm -rf "$CICD_FILE_CACHE_DIR"/*
        log_debug "Cleared ruleset cache (file)"
    fi
}

# ==============================================================================
# RULESET CACHING
# ==============================================================================

# Get ruleset from cache or fetch from registry
# Usage: ruleset=$(get_cached_ruleset "security/secrets" "1.0.0")
get_cached_ruleset() {
    _ruleset_name="$1"
    _version="${2:-latest}"
    _cache_key="ruleset:$_ruleset_name:$_version"

    # Try cache first
    _cached=$(cache_get "$_cache_key") && {
        echo "$_cached"
        return 0
    }

    # Fetch from API if not cached
    _ruleset=$(fetch_ruleset_from_registry "$_ruleset_name" "$_version") || return 1

    # Cache the result
    cache_set "$_cache_key" "$_ruleset"

    echo "$_ruleset"
}

# Fetch ruleset from cicd-hyper-a registry
# Usage: ruleset=$(fetch_ruleset_from_registry "security/secrets" "1.0.0")
fetch_ruleset_from_registry() {
    _ruleset_name="$1"
    _version="${2:-latest}"

    # Source api.sh for API functions
    _script_dir=$(dirname "$0")
    if [ -f "$_script_dir/lib/api.sh" ]; then
        . "$_script_dir/lib/api.sh"
    fi

    # Try API fetch
    if [ -n "${CICD_API_TOKEN:-}" ]; then
        api_get "/registry/rulesets/$_ruleset_name/versions/$_version"
        return $?
    fi

    # Fallback to local registry file
    _local_file="${CICD_REGISTRY_PATH:-/var/lib/cicd-hyper-a/registry}/rulesets/$_ruleset_name/$_version.json"
    if [ -f "$_local_file" ]; then
        cat "$_local_file"
        return 0
    fi

    log_warn "Ruleset not found: $_ruleset_name@$_version"
    return 1
}

# Prefetch multiple rulesets (for batch operations)
# Usage: prefetch_rulesets "security/secrets,security/keys,format/spdx"
prefetch_rulesets() {
    _rulesets="$1"

    echo "$_rulesets" | tr ',' '\n' | while read -r _rs; do
        [ -z "$_rs" ] && continue
        get_cached_ruleset "$_rs" >/dev/null 2>&1 || true
    done
}

# ==============================================================================
# CACHE STATISTICS
# ==============================================================================

# Get cache statistics
# Usage: stats=$(cache_stats)
cache_stats() {
    _stats="{"

    if is_cache_available; then
        _keys=$(redis_cmd KEYS "$CICD_CACHE_PREFIX:*" | wc -l)
        _memory=$(redis_cmd MEMORY USAGE "$CICD_CACHE_PREFIX:*" 2>/dev/null | head -1 || echo "unknown")
        _stats="$_stats\"redis_available\":true,\"redis_keys\":$_keys,\"redis_memory\":\"$_memory\","
    else
        _stats="$_stats\"redis_available\":false,"
    fi

    _file_count=0
    _file_size=0
    if [ -d "$CICD_FILE_CACHE_DIR" ]; then
        _file_count=$(find "$CICD_FILE_CACHE_DIR" -type f 2>/dev/null | wc -l)
        _file_size=$(du -sh "$CICD_FILE_CACHE_DIR" 2>/dev/null | cut -f1 || echo "0")
    fi
    _stats="$_stats\"file_cache_count\":$_file_count,\"file_cache_size\":\"$_file_size\"}"

    echo "$_stats"
}

# ==============================================================================
# INITIALIZATION
# ==============================================================================

# Initialize cache subsystem
# Usage: init_cache
init_cache() {
    # Ensure file cache directory exists
    mkdir -p "$CICD_FILE_CACHE_DIR"

    # Test Redis connection
    if is_cache_available; then
        log_debug "Cache backend: Dragonfly/Redis at $CICD_CACHE_HOST:$CICD_CACHE_PORT"
    else
        log_debug "Cache backend: File cache at $CICD_FILE_CACHE_DIR"
    fi
}
