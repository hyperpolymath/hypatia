#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
# Migration: initial_schema
# Version: 00000000000001
# Type: ArangoDB
# Description: Initial database schema setup

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
    echo "Applying migration: initial_schema"

    # Create core document collections
    for collection in repos bots sessions findings rules fixes workflows learningData owners rulesets bot_results alerts; do
        echo "Creating collection: $collection"
        arango_api POST "/_api/collection" "{\"name\": \"$collection\", \"type\": 2}" >/dev/null 2>&1 || true
    done

    # Create edge collections
    for edge in repo_has_workflow repo_owned_by session_scans_repo session_ran_bot bot_found_finding finding_in_session finding_triggered_by finding_in_file finding_fixed_by fix_applies_rule bot_owns_rule bot_depends_on rule_learned_from finding_related_to workflow_triggers_rule repo_similar_to rule_applies_to alert_in_repo bot_analyzed ruleset_contains repo_uses_ruleset; do
        echo "Creating edge collection: $edge"
        arango_api POST "/_api/collection" "{\"name\": \"$edge\", \"type\": 3}" >/dev/null 2>&1 || true
    done

    echo "Initial schema applied"
}

down() {
    echo "Rolling back migration: initial_schema"

    # Note: This will destroy all data!
    echo "WARNING: This will drop all collections!"

    # Drop edge collections
    for edge in repo_has_workflow repo_owned_by session_scans_repo session_ran_bot bot_found_finding finding_in_session finding_triggered_by finding_in_file finding_fixed_by fix_applies_rule bot_owns_rule bot_depends_on rule_learned_from finding_related_to workflow_triggers_rule repo_similar_to rule_applies_to alert_in_repo bot_analyzed ruleset_contains repo_uses_ruleset; do
        echo "Dropping edge collection: $edge"
        arango_api DELETE "/_api/collection/$edge" >/dev/null 2>&1 || true
    done

    # Drop document collections
    for collection in repos bots sessions findings rules fixes workflows learningData owners rulesets bot_results alerts; do
        echo "Dropping collection: $collection"
        arango_api DELETE "/_api/collection/$collection" >/dev/null 2>&1 || true
    done

    echo "Initial schema rolled back"
}

case "$DIRECTION" in
    up)   up   ;;
    down) down ;;
    *)    echo "Usage: $0 {up|down}" >&2; exit 1 ;;
esac
