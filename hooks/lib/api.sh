#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
# cicd-hyper-a Git Hooks - API Client Functions
# HTTP client for cicd-hyper-a API integration
#
# This file is designed to be sourced by hook scripts:
#   . "$(dirname "$0")/lib/api.sh"

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# API connection settings
CICD_API_URL="${CICD_HYPER_A_API:-http://localhost:8080}"
CICD_API_TOKEN="${CICD_HYPER_A_TOKEN:-}"
CICD_API_TIMEOUT="${CICD_API_TIMEOUT:-10}"
CICD_API_RETRIES="${CICD_API_RETRIES:-3}"

# User agent for requests
CICD_USER_AGENT="cicd-hyper-a-hooks/1.0"

# ==============================================================================
# HTTP CLIENT DETECTION
# ==============================================================================

# Detect available HTTP client
get_http_client() {
    if command -v curl >/dev/null 2>&1; then
        echo "curl"
    elif command -v wget >/dev/null 2>&1; then
        echo "wget"
    else
        echo "none"
    fi
}

# ==============================================================================
# HTTP REQUEST FUNCTIONS
# ==============================================================================

# Make HTTP GET request
# Usage: response=$(api_get "/path/to/endpoint")
api_get() {
    _path="$1"
    _url="$CICD_API_URL$_path"
    _client=$(get_http_client)

    case "$_client" in
        curl)
            _auth=""
            [ -n "$CICD_API_TOKEN" ] && _auth="-H \"Authorization: Bearer $CICD_API_TOKEN\""
            curl -s -f -X GET \
                --connect-timeout "$CICD_API_TIMEOUT" \
                -H "User-Agent: $CICD_USER_AGENT" \
                -H "Accept: application/json" \
                $_auth \
                "$_url" 2>/dev/null
            ;;
        wget)
            _auth=""
            [ -n "$CICD_API_TOKEN" ] && _auth="--header=\"Authorization: Bearer $CICD_API_TOKEN\""
            wget -q -O - \
                --timeout="$CICD_API_TIMEOUT" \
                --header="User-Agent: $CICD_USER_AGENT" \
                --header="Accept: application/json" \
                $_auth \
                "$_url" 2>/dev/null
            ;;
        *)
            log_error "No HTTP client available (curl or wget required)"
            return 1
            ;;
    esac
}

# Make HTTP POST request with JSON body
# Usage: response=$(api_post "/path/to/endpoint" '{"key": "value"}')
api_post() {
    _path="$1"
    _body="$2"
    _url="$CICD_API_URL$_path"
    _client=$(get_http_client)

    case "$_client" in
        curl)
            _auth=""
            [ -n "$CICD_API_TOKEN" ] && _auth="-H \"Authorization: Bearer $CICD_API_TOKEN\""
            curl -s -f -X POST \
                --connect-timeout "$CICD_API_TIMEOUT" \
                -H "User-Agent: $CICD_USER_AGENT" \
                -H "Content-Type: application/json" \
                -H "Accept: application/json" \
                $_auth \
                -d "$_body" \
                "$_url" 2>/dev/null
            ;;
        wget)
            _auth=""
            [ -n "$CICD_API_TOKEN" ] && _auth="--header=\"Authorization: Bearer $CICD_API_TOKEN\""
            echo "$_body" | wget -q -O - \
                --timeout="$CICD_API_TIMEOUT" \
                --header="User-Agent: $CICD_USER_AGENT" \
                --header="Content-Type: application/json" \
                --header="Accept: application/json" \
                --post-data="$_body" \
                $_auth \
                "$_url" 2>/dev/null
            ;;
        *)
            log_error "No HTTP client available (curl or wget required)"
            return 1
            ;;
    esac
}

# Make HTTP PUT request with JSON body
# Usage: response=$(api_put "/path/to/endpoint" '{"key": "value"}')
api_put() {
    _path="$1"
    _body="$2"
    _url="$CICD_API_URL$_path"
    _client=$(get_http_client)

    case "$_client" in
        curl)
            _auth=""
            [ -n "$CICD_API_TOKEN" ] && _auth="-H \"Authorization: Bearer $CICD_API_TOKEN\""
            curl -s -f -X PUT \
                --connect-timeout "$CICD_API_TIMEOUT" \
                -H "User-Agent: $CICD_USER_AGENT" \
                -H "Content-Type: application/json" \
                -H "Accept: application/json" \
                $_auth \
                -d "$_body" \
                "$_url" 2>/dev/null
            ;;
        wget)
            log_warn "wget does not support PUT method natively"
            return 1
            ;;
        *)
            log_error "No HTTP client available"
            return 1
            ;;
    esac
}

# Make HTTP request with retry logic
# Usage: response=$(api_request_with_retry "GET" "/path" "" 3)
api_request_with_retry() {
    _method="$1"
    _path="$2"
    _body="${3:-}"
    _retries="${4:-$CICD_API_RETRIES}"
    _attempt=1

    while [ "$_attempt" -le "$_retries" ]; do
        case "$_method" in
            GET)  _result=$(api_get "$_path") ;;
            POST) _result=$(api_post "$_path" "$_body") ;;
            PUT)  _result=$(api_put "$_path" "$_body") ;;
            *)    log_error "Unsupported HTTP method: $_method"; return 1 ;;
        esac

        if [ -n "$_result" ]; then
            echo "$_result"
            return 0
        fi

        log_debug "API request failed (attempt $_attempt/$_retries)"
        _attempt=$((_attempt + 1))
        sleep 1
    done

    log_error "API request failed after $_retries attempts: $_method $_path"
    return 1
}

# ==============================================================================
# CICD-HYPER-A API ENDPOINTS
# ==============================================================================

# Report hook execution to cicd-hyper-a
# Usage: report_hook_execution "pre-commit" "success" "..."
report_hook_execution() {
    _hook_name="$1"
    _status="$2"
    _details="${3:-}"
    _repo=$(get_repo_name 2>/dev/null || echo "unknown")
    _branch=$(get_current_branch 2>/dev/null || echo "unknown")
    _commit=$(git rev-parse HEAD 2>/dev/null || echo "unknown")

    _payload=$(cat <<EOF
{
    "hook": "$_hook_name",
    "status": "$_status",
    "repository": "$_repo",
    "branch": "$_branch",
    "commit": "$_commit",
    "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
    "details": "$_details"
}
EOF
)

    api_post "/hooks/report" "$_payload" >/dev/null 2>&1 || \
        log_debug "Failed to report hook execution (API may be unavailable)"
}

# Fetch rules for a specific hook
# Usage: rules=$(fetch_hook_rules "pre-commit")
fetch_hook_rules() {
    _hook_name="$1"
    _repo=$(get_repo_name 2>/dev/null || echo "unknown")

    api_get "/hooks/$_hook_name/rules?repo=$_repo" 2>/dev/null
}

# Submit file for analysis
# Usage: result=$(analyze_file "path/to/file" "pre-commit")
analyze_file() {
    _file="$1"
    _hook="$2"

    [ -f "$_file" ] || return 1

    _content=$(base64 < "$_file" | tr -d '\n')
    _payload=$(cat <<EOF
{
    "filename": "$_file",
    "hook": "$_hook",
    "content_base64": "$_content"
}
EOF
)

    api_post "/analyze/file" "$_payload" 2>/dev/null
}

# Fetch SHA pin for a GitHub Action
# Usage: sha=$(get_action_sha "actions/checkout" "v4")
get_action_sha() {
    _action="$1"
    _version="$2"

    api_get "/registry/actions/$_action/sha?version=$_version" 2>/dev/null
}

# ==============================================================================
# WEBHOOK NOTIFICATIONS
# ==============================================================================

# Send webhook notification
# Usage: send_webhook "https://webhook.url" '{"event": "push"}'
send_webhook() {
    _url="$1"
    _payload="$2"
    _client=$(get_http_client)

    case "$_client" in
        curl)
            curl -s -f -X POST \
                --connect-timeout "$CICD_API_TIMEOUT" \
                -H "User-Agent: $CICD_USER_AGENT" \
                -H "Content-Type: application/json" \
                -d "$_payload" \
                "$_url" >/dev/null 2>&1
            ;;
        wget)
            echo "$_payload" | wget -q -O - \
                --timeout="$CICD_API_TIMEOUT" \
                --header="User-Agent: $CICD_USER_AGENT" \
                --header="Content-Type: application/json" \
                --post-data="$_payload" \
                "$_url" >/dev/null 2>&1
            ;;
        *)
            log_warn "No HTTP client available for webhook"
            return 1
            ;;
    esac
}

# ==============================================================================
# HEALTH CHECK
# ==============================================================================

# Check if API is available
# Usage: if is_api_available; then ...
is_api_available() {
    _response=$(api_get "/health" 2>/dev/null)
    [ -n "$_response" ]
}

# Get API version
# Usage: version=$(get_api_version)
get_api_version() {
    api_get "/version" 2>/dev/null
}

# ==============================================================================
# INITIALIZATION
# ==============================================================================

# Initialize API subsystem
# Usage: init_api
init_api() {
    _client=$(get_http_client)
    if [ "$_client" = "none" ]; then
        log_warn "No HTTP client available - API features disabled"
        return 1
    fi

    log_debug "API client: $_client"
    log_debug "API URL: $CICD_API_URL"

    if is_api_available; then
        log_debug "API is available"
        return 0
    else
        log_debug "API is unavailable - falling back to local mode"
        return 1
    fi
}
