#!/bin/sh
# SPDX-License-Identifier: PLMP-1.0-or-later
# cicd-hyper-a ArangoDB Initialization Script
#
# Initializes ArangoDB for the cicd-hyper-a CI/CD intelligence platform.
# Creates database, collections, edges, indexes, graphs, and seed data.
#
# Usage:
#   ./init-arangodb.sh [OPTIONS]
#
# Options:
#   -h, --help          Show this help message
#   -n, --dry-run       Show what would be done without executing
#   -v, --verbose       Enable verbose output
#   -f, --force         Drop existing collections before creating
#   --skip-seed         Skip seeding initial data
#   --seed-only         Only seed data (skip schema creation)
#
# Environment Variables:
#   ARANGO_HOST         ArangoDB host (default: localhost)
#   ARANGO_PORT         ArangoDB port (default: 8529)
#   ARANGO_USER         ArangoDB username (default: root)
#   ARANGO_PASSWORD     ArangoDB password (required)
#   ARANGO_DATABASE     Database name (default: cicd_hyper_a)
#
# Examples:
#   ARANGO_PASSWORD=secret ./init-arangodb.sh
#   ARANGO_PASSWORD=secret ./init-arangodb.sh --dry-run
#   ARANGO_PASSWORD=secret ./init-arangodb.sh --force

set -e

# =============================================================================
# CONFIGURATION
# =============================================================================

ARANGO_HOST="${ARANGO_HOST:-localhost}"
ARANGO_PORT="${ARANGO_PORT:-8529}"
ARANGO_USER="${ARANGO_USER:-root}"
ARANGO_PASSWORD="${ARANGO_PASSWORD:-}"
ARANGO_DATABASE="${ARANGO_DATABASE:-cicd_hyper_a}"

DRY_RUN=false
VERBOSE=false
FORCE=false
SKIP_SEED=false
SEED_ONLY=false

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# =============================================================================
# FUNCTIONS
# =============================================================================

usage() {
    sed -n '3,25p' "$0" | sed 's/^# //' | sed 's/^#//'
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

    if ! command -v curl >/dev/null 2>&1; then
        error "curl is required but not installed"
    fi

    if ! command -v jq >/dev/null 2>&1; then
        error "jq is required but not installed"
    fi
}

check_connection() {
    log_verbose "Checking ArangoDB connection..."

    local url="http://${ARANGO_HOST}:${ARANGO_PORT}/_api/version"

    if [ "$DRY_RUN" = true ]; then
        log_dry "Would check connection to $url"
        return 0
    fi

    local response
    response=$(curl -s -u "${ARANGO_USER}:${ARANGO_PASSWORD}" "$url" 2>&1) || {
        error "Failed to connect to ArangoDB at $url"
    }

    local version
    version=$(echo "$response" | jq -r '.version // empty' 2>/dev/null)

    if [ -z "$version" ]; then
        error "Failed to get ArangoDB version. Response: $response"
    fi

    log "Connected to ArangoDB version $version"
}

arango_api() {
    local method="$1"
    local endpoint="$2"
    local data="$3"
    local db="${4:-_system}"

    local url="http://${ARANGO_HOST}:${ARANGO_PORT}/_db/${db}${endpoint}"

    if [ "$DRY_RUN" = true ]; then
        log_dry "$method $url"
        if [ -n "$data" ]; then
            log_dry "  Data: $data"
        fi
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

create_database() {
    log "Creating database: $ARANGO_DATABASE"

    # Check if database exists
    local exists
    exists=$(arango_api GET "/_api/database" "" "_system" | jq -r ".result | index(\"$ARANGO_DATABASE\") // empty")

    if [ -n "$exists" ]; then
        log "Database $ARANGO_DATABASE already exists"
        return 0
    fi

    local result
    result=$(arango_api POST "/_api/database" "{\"name\": \"$ARANGO_DATABASE\"}" "_system")

    if [ "$DRY_RUN" = true ]; then
        return 0
    fi

    local error_msg
    error_msg=$(echo "$result" | jq -r '.errorMessage // empty')

    if [ -n "$error_msg" ]; then
        error "Failed to create database: $error_msg"
    fi

    log "Database $ARANGO_DATABASE created successfully"
}

create_collection() {
    local name="$1"
    local type="${2:-document}"  # document or edge

    log_verbose "Creating $type collection: $name"

    local collection_type=2
    if [ "$type" = "edge" ]; then
        collection_type=3
    fi

    # Check if collection exists
    local exists
    exists=$(arango_api GET "/_api/collection/$name" "" "$ARANGO_DATABASE" | jq -r '.name // empty')

    if [ -n "$exists" ]; then
        if [ "$FORCE" = true ]; then
            log "Dropping existing collection: $name"
            arango_api DELETE "/_api/collection/$name" "" "$ARANGO_DATABASE" >/dev/null
        else
            log_verbose "Collection $name already exists, skipping"
            return 0
        fi
    fi

    local result
    result=$(arango_api POST "/_api/collection" "{\"name\": \"$name\", \"type\": $collection_type}" "$ARANGO_DATABASE")

    if [ "$DRY_RUN" = true ]; then
        return 0
    fi

    local error_msg
    error_msg=$(echo "$result" | jq -r '.errorMessage // empty')

    if [ -n "$error_msg" ]; then
        error "Failed to create collection $name: $error_msg"
    fi
}

create_index() {
    local collection="$1"
    local index_type="$2"
    local fields="$3"
    local unique="${4:-false}"

    log_verbose "Creating $index_type index on $collection: $fields"

    local data
    data=$(cat <<EOF
{
    "type": "$index_type",
    "fields": $fields,
    "unique": $unique
}
EOF
)

    local result
    result=$(arango_api POST "/_api/index?collection=$collection" "$data" "$ARANGO_DATABASE")

    if [ "$DRY_RUN" = true ]; then
        return 0
    fi

    local error_msg
    error_msg=$(echo "$result" | jq -r '.errorMessage // empty')

    # Ignore "index already exists" errors
    if [ -n "$error_msg" ] && ! echo "$error_msg" | grep -q "already exists"; then
        log "Warning: Index creation returned: $error_msg"
    fi
}

create_graph() {
    local name="$1"
    local edge_definitions="$2"

    log "Creating graph: $name"

    # Check if graph exists
    local exists
    exists=$(arango_api GET "/_api/gharial/$name" "" "$ARANGO_DATABASE" | jq -r '.graph.name // empty')

    if [ -n "$exists" ]; then
        if [ "$FORCE" = true ]; then
            log "Dropping existing graph: $name"
            arango_api DELETE "/_api/gharial/$name?dropCollections=false" "" "$ARANGO_DATABASE" >/dev/null
        else
            log_verbose "Graph $name already exists, skipping"
            return 0
        fi
    fi

    local data
    data=$(cat <<EOF
{
    "name": "$name",
    "edgeDefinitions": $edge_definitions
}
EOF
)

    local result
    result=$(arango_api POST "/_api/gharial" "$data" "$ARANGO_DATABASE")

    if [ "$DRY_RUN" = true ]; then
        return 0
    fi

    local error_msg
    error_msg=$(echo "$result" | jq -r '.errorMessage // empty')

    if [ -n "$error_msg" ]; then
        error "Failed to create graph $name: $error_msg"
    fi
}

create_analyzer() {
    local name="$1"
    local analyzer_type="$2"
    local properties="$3"
    local features="$4"

    log_verbose "Creating analyzer: $name"

    local data
    data=$(cat <<EOF
{
    "name": "$name",
    "type": "$analyzer_type",
    "properties": $properties,
    "features": $features
}
EOF
)

    local result
    result=$(arango_api POST "/_api/analyzer" "$data" "$ARANGO_DATABASE")

    if [ "$DRY_RUN" = true ]; then
        return 0
    fi

    # Ignore if already exists
    local error_num
    error_num=$(echo "$result" | jq -r '.errorNum // 0')

    if [ "$error_num" = "1207" ]; then
        log_verbose "Analyzer $name already exists"
    fi
}

create_view() {
    local name="$1"
    local links="$2"

    log_verbose "Creating ArangoSearch view: $name"

    # Check if view exists
    local exists
    exists=$(arango_api GET "/_api/view/$name" "" "$ARANGO_DATABASE" | jq -r '.name // empty')

    if [ -n "$exists" ]; then
        log_verbose "View $name already exists, skipping"
        return 0
    fi

    local data
    data=$(cat <<EOF
{
    "name": "$name",
    "type": "arangosearch",
    "links": $links
}
EOF
)

    local result
    result=$(arango_api POST "/_api/view" "$data" "$ARANGO_DATABASE")

    if [ "$DRY_RUN" = true ]; then
        return 0
    fi

    local error_msg
    error_msg=$(echo "$result" | jq -r '.errorMessage // empty')

    if [ -n "$error_msg" ]; then
        log "Warning: View creation returned: $error_msg"
    fi
}

insert_document() {
    local collection="$1"
    local document="$2"

    local result
    result=$(arango_api POST "/_api/document/$collection?overwrite=true" "$document" "$ARANGO_DATABASE")

    if [ "$DRY_RUN" = true ]; then
        return 0
    fi

    local error_msg
    error_msg=$(echo "$result" | jq -r '.errorMessage // empty')

    if [ -n "$error_msg" ]; then
        log_verbose "Warning inserting document: $error_msg"
    fi
}

# =============================================================================
# SCHEMA CREATION
# =============================================================================

create_schema() {
    log "Creating database schema..."

    # Create database
    create_database

    # Document collections
    log "Creating document collections..."
    create_collection "repos" "document"
    create_collection "bots" "document"
    create_collection "sessions" "document"
    create_collection "findings" "document"
    create_collection "rules" "document"
    create_collection "fixes" "document"
    create_collection "workflows" "document"
    create_collection "learningData" "document"
    create_collection "owners" "document"
    create_collection "rulesets" "document"
    create_collection "bot_results" "document"

    # Edge collections
    log "Creating edge collections..."
    create_collection "repo_has_workflow" "edge"
    create_collection "repo_owned_by" "edge"
    create_collection "session_scans_repo" "edge"
    create_collection "session_ran_bot" "edge"
    create_collection "bot_found_finding" "edge"
    create_collection "finding_in_session" "edge"
    create_collection "finding_triggered_by" "edge"
    create_collection "finding_in_file" "edge"
    create_collection "finding_fixed_by" "edge"
    create_collection "fix_applies_rule" "edge"
    create_collection "bot_owns_rule" "edge"
    create_collection "bot_depends_on" "edge"
    create_collection "rule_learned_from" "edge"
    create_collection "finding_related_to" "edge"
    create_collection "workflow_triggers_rule" "edge"
    create_collection "repo_similar_to" "edge"
    create_collection "rule_applies_to" "edge"
    create_collection "alert_in_repo" "edge"
    create_collection "bot_analyzed" "edge"
    create_collection "ruleset_contains" "edge"
    create_collection "repo_uses_ruleset" "edge"

    # Indexes
    log "Creating indexes..."

    # Repos indexes
    create_index "repos" "persistent" '["forge", "owner"]'
    create_index "repos" "persistent" '["scorecardScore"]'
    create_index "repos" "persistent" '["lastScanned"]'

    # Findings indexes
    create_index "findings" "persistent" '["severity"]'
    create_index "findings" "persistent" '["category"]'
    create_index "findings" "persistent" '["sessionId"]'
    create_index "findings" "persistent" '["botId"]'
    create_index "findings" "persistent" '["fixed"]'
    create_index "findings" "persistent" '["createdAt"]'

    # Sessions indexes
    create_index "sessions" "persistent" '["repoKey"]'
    create_index "sessions" "persistent" '["status"]'
    create_index "sessions" "persistent" '["startedAt"]'

    # Rules indexes
    create_index "rules" "persistent" '["type"]'
    create_index "rules" "persistent" '["category"]'
    create_index "rules" "persistent" '["enabled"]'
    create_index "rules" "persistent" '["effect"]'
    create_index "rules" "persistent" '["triggerCount"]'

    # Workflows indexes
    create_index "workflows" "persistent" '["repoKey"]'
    create_index "workflows" "persistent" '["allActionsPinned"]'

    # Rulesets indexes
    create_index "rulesets" "persistent" '["name"]' "true"
    create_index "rulesets" "persistent" '["effect"]'
    create_index "rulesets" "persistent" '["verified"]'

    # Bot results indexes
    create_index "bot_results" "persistent" '["botId", "repoKey"]'
    create_index "bot_results" "persistent" '["runAt"]'

    # Text analyzer for full-text search
    log "Creating analyzers..."
    create_analyzer "text_en" "text" '{"locale": "en", "case": "lower", "accent": false, "stemming": true, "stopwords": []}' '["position", "frequency", "norm"]'

    # ArangoSearch views
    log "Creating search views..."

    create_view "findings_search" '{
        "findings": {
            "includeAllFields": false,
            "fields": {
                "message": {"analyzers": ["text_en"]},
                "description": {"analyzers": ["text_en"]},
                "file": {"analyzers": ["identity"]}
            }
        }
    }'

    create_view "rules_search" '{
        "rules": {
            "includeAllFields": false,
            "fields": {
                "name": {"analyzers": ["text_en"]},
                "description": {"analyzers": ["text_en"]}
            }
        },
        "rulesets": {
            "includeAllFields": false,
            "fields": {
                "name": {"analyzers": ["text_en"]},
                "description": {"analyzers": ["text_en"]}
            }
        }
    }'

    # Graphs
    log "Creating graphs..."

    create_graph "cicd_intelligence" '[
        {"collection": "repo_has_workflow", "from": ["repos"], "to": ["workflows"]},
        {"collection": "repo_owned_by", "from": ["repos"], "to": ["owners"]},
        {"collection": "session_scans_repo", "from": ["sessions"], "to": ["repos"]},
        {"collection": "session_ran_bot", "from": ["sessions"], "to": ["bots"]},
        {"collection": "bot_found_finding", "from": ["bots"], "to": ["findings"]},
        {"collection": "finding_in_session", "from": ["findings"], "to": ["sessions"]},
        {"collection": "finding_triggered_by", "from": ["findings"], "to": ["rules"]},
        {"collection": "finding_in_file", "from": ["findings"], "to": ["workflows"]},
        {"collection": "finding_fixed_by", "from": ["findings"], "to": ["fixes"]},
        {"collection": "fix_applies_rule", "from": ["fixes"], "to": ["rules"]},
        {"collection": "bot_owns_rule", "from": ["bots"], "to": ["rules"]},
        {"collection": "workflow_triggers_rule", "from": ["workflows"], "to": ["rules"]}
    ]'

    create_graph "bot_fleet" '[
        {"collection": "bot_depends_on", "from": ["bots"], "to": ["bots"]},
        {"collection": "session_ran_bot", "from": ["sessions"], "to": ["bots"]},
        {"collection": "bot_owns_rule", "from": ["bots"], "to": ["rules"]}
    ]'

    create_graph "learning_graph" '[
        {"collection": "rule_learned_from", "from": ["rules"], "to": ["learningData"]},
        {"collection": "finding_related_to", "from": ["findings"], "to": ["findings"]},
        {"collection": "repo_similar_to", "from": ["repos"], "to": ["repos"]}
    ]'

    log "Schema creation complete"
}

# =============================================================================
# SEED DATA
# =============================================================================

seed_data() {
    log "Seeding initial data..."

    # Seed bots
    log "Seeding gitbot-fleet bots..."

    insert_document "bots" '{
        "_key": "rhodibot",
        "name": "rhodibot",
        "displayName": "Rhodibot",
        "version": "0.1.0",
        "tier": "verifier",
        "categories": ["structure", "policy", "licensing"],
        "canFix": true,
        "enabled": true,
        "createdAt": "'"$(date -u +%Y-%m-%dT%H:%M:%SZ)"'"
    }'

    insert_document "bots" '{
        "_key": "echidnabot",
        "name": "echidnabot",
        "displayName": "Echidnabot",
        "version": "0.1.0",
        "tier": "verifier",
        "categories": ["verification", "fuzzing"],
        "canFix": false,
        "enabled": true,
        "createdAt": "'"$(date -u +%Y-%m-%dT%H:%M:%SZ)"'"
    }'

    insert_document "bots" '{
        "_key": "oikos",
        "name": "oikos",
        "displayName": "Oikos",
        "version": "0.1.0",
        "tier": "verifier",
        "categories": ["sustainability", "efficiency"],
        "canFix": false,
        "enabled": true,
        "createdAt": "'"$(date -u +%Y-%m-%dT%H:%M:%SZ)"'"
    }'

    insert_document "bots" '{
        "_key": "glambot",
        "name": "glambot",
        "displayName": "Glambot",
        "version": "0.1.0",
        "tier": "finisher",
        "categories": ["accessibility", "seo", "documentation"],
        "canFix": true,
        "enabled": true,
        "createdAt": "'"$(date -u +%Y-%m-%dT%H:%M:%SZ)"'"
    }'

    insert_document "bots" '{
        "_key": "seambot",
        "name": "seambot",
        "displayName": "Seambot",
        "version": "0.1.0",
        "tier": "finisher",
        "categories": ["integration", "api"],
        "canFix": false,
        "enabled": true,
        "createdAt": "'"$(date -u +%Y-%m-%dT%H:%M:%SZ)"'"
    }'

    insert_document "bots" '{
        "_key": "finishing_bot",
        "name": "finishing_bot",
        "displayName": "Finishing Bot",
        "version": "0.1.0",
        "tier": "finisher",
        "categories": ["release", "licensing", "code_quality"],
        "canFix": true,
        "enabled": true,
        "createdAt": "'"$(date -u +%Y-%m-%dT%H:%M:%SZ)"'"
    }'

    insert_document "bots" '{
        "_key": "robot_repo_automaton",
        "name": "robot_repo_automaton",
        "displayName": "Robot Repo Automaton",
        "version": "0.1.0",
        "tier": "executor",
        "categories": ["security", "workflow", "structure"],
        "canFix": true,
        "enabled": true,
        "createdAt": "'"$(date -u +%Y-%m-%dT%H:%M:%SZ)"'"
    }'

    insert_document "bots" '{
        "_key": "cicd_hyper_a",
        "name": "cicd_hyper_a",
        "displayName": "CICD Hyper-A",
        "version": "0.1.0",
        "tier": "engine",
        "categories": ["all"],
        "canFix": false,
        "enabled": true,
        "createdAt": "'"$(date -u +%Y-%m-%dT%H:%M:%SZ)"'"
    }'

    # Seed bot dependencies
    log "Seeding bot dependencies..."

    insert_document "bot_depends_on" '{
        "_from": "bots/glambot",
        "_to": "bots/rhodibot",
        "dependencyType": "required"
    }'

    insert_document "bot_depends_on" '{
        "_from": "bots/seambot",
        "_to": "bots/rhodibot",
        "dependencyType": "required"
    }'

    insert_document "bot_depends_on" '{
        "_from": "bots/seambot",
        "_to": "bots/echidnabot",
        "dependencyType": "required"
    }'

    insert_document "bot_depends_on" '{
        "_from": "bots/finishing_bot",
        "_to": "bots/rhodibot",
        "dependencyType": "required"
    }'

    insert_document "bot_depends_on" '{
        "_from": "bots/finishing_bot",
        "_to": "bots/glambot",
        "dependencyType": "required"
    }'

    # Seed default rulesets
    log "Seeding default rulesets..."

    insert_document "rulesets" '{
        "_key": "openssf_scorecard",
        "name": "OpenSSF Scorecard",
        "description": "Rules for OpenSSF Scorecard compliance",
        "version": "1.0.0",
        "effect": "diagnostic",
        "verified": true,
        "enabled": true,
        "createdAt": "'"$(date -u +%Y-%m-%dT%H:%M:%SZ)"'"
    }'

    insert_document "rulesets" '{
        "_key": "rhodium_standard",
        "name": "Rhodium Standard",
        "description": "RSR repository structure and policy rules",
        "version": "1.0.0",
        "effect": "preventive",
        "verified": true,
        "enabled": true,
        "createdAt": "'"$(date -u +%Y-%m-%dT%H:%M:%SZ)"'"
    }'

    insert_document "rulesets" '{
        "_key": "github_security",
        "name": "GitHub Security",
        "description": "GitHub Actions security best practices",
        "version": "1.0.0",
        "effect": "curative",
        "verified": true,
        "enabled": true,
        "createdAt": "'"$(date -u +%Y-%m-%dT%H:%M:%SZ)"'"
    }'

    log "Seed data complete"
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
            -f|--force)
                FORCE=true
                ;;
            --skip-seed)
                SKIP_SEED=true
                ;;
            --seed-only)
                SEED_ONLY=true
                ;;
            *)
                error "Unknown option: $1"
                ;;
        esac
        shift
    done

    # Validate environment
    if [ -z "$ARANGO_PASSWORD" ]; then
        error "ARANGO_PASSWORD environment variable is required"
    fi

    log "============================================================"
    log "cicd-hyper-a ArangoDB Initialization"
    log "============================================================"
    log "Host: $ARANGO_HOST:$ARANGO_PORT"
    log "Database: $ARANGO_DATABASE"
    log "User: $ARANGO_USER"
    log "Dry run: $DRY_RUN"
    log "Force: $FORCE"
    log "============================================================"

    check_dependencies
    check_connection

    if [ "$SEED_ONLY" = true ]; then
        seed_data
    else
        create_schema

        if [ "$SKIP_SEED" = false ]; then
            seed_data
        fi
    fi

    log "============================================================"
    log "Initialization complete!"
    log "============================================================"
}

main "$@"
