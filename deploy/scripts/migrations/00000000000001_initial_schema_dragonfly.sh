#!/bin/sh
# SPDX-License-Identifier: PMPL-1.0-or-later
# Migration: initial_schema
# Version: 00000000000001
# Type: Dragonfly
# Description: Initial cache configuration

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
    echo "Applying migration: initial_schema"

    # Store schema metadata
    redis_cmd HSET meta:init \
        version "1.0.0" \
        schema_version "1" \
        initialized_at "$(date -u +%Y-%m-%dT%H:%M:%SZ)"

    # Configure TTL settings
    redis_cmd HSET meta:ttl \
        alert 3600 \
        rule 86400 \
        repo 21600 \
        fix 43200 \
        session 7200

    # Configure key prefixes
    redis_cmd HSET meta:prefixes \
        alert "alert:" \
        rule "rule:" \
        repo "repo:" \
        fix "fix:" \
        session "session:" \
        bot "bot:" \
        job "job:" \
        ratelimit "rl:"

    # Initialize queues
    redis_cmd ZADD queue:alerts NX 0 __init__ >/dev/null
    redis_cmd ZREM queue:alerts __init__ >/dev/null
    redis_cmd ZADD queue:fixes NX 0 __init__ >/dev/null
    redis_cmd ZREM queue:fixes __init__ >/dev/null
    redis_cmd ZADD queue:scans NX 0 __init__ >/dev/null
    redis_cmd ZREM queue:scans __init__ >/dev/null

    # Initialize counters
    redis_cmd SET stats:scans_total 0
    redis_cmd SET stats:alerts_total 0
    redis_cmd SET stats:fixes_total 0

    echo "Initial schema applied"
}

down() {
    echo "Rolling back migration: initial_schema"

    # Remove metadata
    redis_cmd DEL meta:init meta:ttl meta:prefixes meta:channels

    # Remove queues
    redis_cmd DEL queue:alerts queue:fixes queue:scans

    # Remove counters
    redis_cmd DEL stats:scans_total stats:alerts_total stats:fixes_total

    echo "Initial schema rolled back"
}

case "$DIRECTION" in
    up)   up   ;;
    down) down ;;
    *)    echo "Usage: $0 {up|down}" >&2; exit 1 ;;
esac
