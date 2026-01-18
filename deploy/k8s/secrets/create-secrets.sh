#!/bin/bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# cicd-hyper-a - Create Kubernetes Secrets from Environment Variables
#
# This script creates Kubernetes secrets from environment variables.
# It's designed for local development and CI/CD pipelines.
#
# Usage:
#   ./create-secrets.sh                    # Create all secrets
#   ./create-secrets.sh --dry-run          # Preview without applying
#   ./create-secrets.sh --namespace dev    # Use different namespace
#   ./create-secrets.sh --delete           # Delete existing secrets first
#
# Required environment variables (see .env.example):
#   ARANGODB_ROOT_PASSWORD, DRAGONFLY_PASSWORD, JWT_SECRET, etc.
#
# For production, use Sealed Secrets, External Secrets, or SOPS instead.

set -euo pipefail

# Configuration
NAMESPACE="${NAMESPACE:-cicd-hyper-a}"
DRY_RUN="${DRY_RUN:-false}"
DELETE_FIRST="${DELETE_FIRST:-false}"
KUBECTL="${KUBECTL:-kubectl}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Helper functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --dry-run)
            DRY_RUN="true"
            shift
            ;;
        --delete)
            DELETE_FIRST="true"
            shift
            ;;
        --namespace)
            NAMESPACE="$2"
            shift 2
            ;;
        -h|--help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --dry-run           Preview changes without applying"
            echo "  --delete            Delete existing secrets before creating"
            echo "  --namespace NAME    Use specified namespace (default: cicd-hyper-a)"
            echo "  -h, --help          Show this help message"
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Check for required tools
if ! command -v "${KUBECTL}" &> /dev/null; then
    log_error "kubectl is not installed or not in PATH"
    exit 1
fi

# Load environment variables from .env file if it exists
if [[ -f ".env" ]]; then
    log_info "Loading environment variables from .env file"
    set -a
    # shellcheck source=/dev/null
    source .env
    set +a
elif [[ -f "../../../.env" ]]; then
    log_info "Loading environment variables from project root .env file"
    set -a
    # shellcheck source=/dev/null
    source ../../../.env
    set +a
fi

# Generate random secrets if not provided
generate_secret() {
    openssl rand -base64 32 | tr -d '\n'
}

generate_hex_secret() {
    openssl rand -hex 32 | tr -d '\n'
}

# Set defaults for optional secrets
ARANGODB_ROOT_USER="${ARANGODB_ROOT_USER:-root}"
ARANGODB_APP_USER="${ARANGODB_APP_USER:-cicd_app}"
GRAFANA_ADMIN_USER="${GRAFANA_ADMIN_USER:-admin}"

# Validate required secrets
validate_secrets() {
    local missing_required=()

    # Core required secrets (fail if not set)
    if [[ -z "${ARANGODB_ROOT_PASSWORD:-}" ]]; then
        missing_required+=("ARANGODB_ROOT_PASSWORD")
    fi
    if [[ -z "${DRAGONFLY_PASSWORD:-}" ]]; then
        missing_required+=("DRAGONFLY_PASSWORD")
    fi

    if [[ ${#missing_required[@]} -gt 0 ]]; then
        log_error "Missing required environment variables:"
        for var in "${missing_required[@]}"; do
            echo "  - $var"
        done
        echo ""
        echo "Set these variables or create a .env file."
        echo "You can generate secrets with: openssl rand -base64 32"
        exit 1
    fi

    # Warn about optional secrets
    local missing_optional=()
    [[ -z "${JWT_SECRET:-}" ]] && missing_optional+=("JWT_SECRET")
    [[ -z "${ENCRYPTION_KEY:-}" ]] && missing_optional+=("ENCRYPTION_KEY")
    [[ -z "${GITHUB_TOKEN:-}" ]] && missing_optional+=("GITHUB_TOKEN")
    [[ -z "${GITLAB_TOKEN:-}" ]] && missing_optional+=("GITLAB_TOKEN")

    if [[ ${#missing_optional[@]} -gt 0 ]]; then
        log_warning "Missing optional environment variables (will use empty/generated values):"
        for var in "${missing_optional[@]}"; do
            echo "  - $var"
        done
        echo ""
    fi
}

# Generate secrets that weren't provided
auto_generate_secrets() {
    if [[ -z "${JWT_SECRET:-}" ]]; then
        JWT_SECRET=$(generate_secret)
        log_info "Auto-generated JWT_SECRET"
    fi
    if [[ -z "${ENCRYPTION_KEY:-}" ]]; then
        ENCRYPTION_KEY=$(generate_secret)
        log_info "Auto-generated ENCRYPTION_KEY"
    fi
    if [[ -z "${WEBHOOK_SECRET:-}" ]]; then
        WEBHOOK_SECRET=$(generate_hex_secret)
        log_info "Auto-generated WEBHOOK_SECRET"
    fi
    if [[ -z "${GRAFANA_ADMIN_PASSWORD:-}" ]]; then
        GRAFANA_ADMIN_PASSWORD=$(generate_secret)
        log_info "Auto-generated GRAFANA_ADMIN_PASSWORD"
    fi
    if [[ -z "${ARANGODB_APP_PASSWORD:-}" ]]; then
        ARANGODB_APP_PASSWORD=$(generate_secret)
        log_info "Auto-generated ARANGODB_APP_PASSWORD"
    fi
}

# Build kubectl command with dry-run if needed
kubectl_apply() {
    if [[ "${DRY_RUN}" == "true" ]]; then
        ${KUBECTL} "$@" --dry-run=client -o yaml
    else
        ${KUBECTL} "$@"
    fi
}

# Create namespace if it doesn't exist
create_namespace() {
    log_info "Ensuring namespace '${NAMESPACE}' exists"
    if [[ "${DRY_RUN}" == "true" ]]; then
        ${KUBECTL} create namespace "${NAMESPACE}" --dry-run=client -o yaml
    else
        ${KUBECTL} create namespace "${NAMESPACE}" --dry-run=client -o yaml | ${KUBECTL} apply -f -
    fi
}

# Delete existing secrets
delete_secrets() {
    if [[ "${DELETE_FIRST}" == "true" ]]; then
        log_info "Deleting existing secrets in namespace '${NAMESPACE}'"
        local secrets=(
            "cicd-hyper-a-core-secrets"
            "forge-credentials"
            "external-api-keys"
            "monitoring-secrets"
            "registry-credentials"
        )
        for secret in "${secrets[@]}"; do
            if ${KUBECTL} get secret "${secret}" -n "${NAMESPACE}" &> /dev/null; then
                if [[ "${DRY_RUN}" == "true" ]]; then
                    log_info "Would delete secret: ${secret}"
                else
                    ${KUBECTL} delete secret "${secret}" -n "${NAMESPACE}" || true
                    log_info "Deleted secret: ${secret}"
                fi
            fi
        done
    fi
}

# Create core application secrets
create_core_secrets() {
    log_info "Creating core application secrets"

    kubectl_apply create secret generic cicd-hyper-a-core-secrets \
        --namespace="${NAMESPACE}" \
        --from-literal="arangodb-root-user=${ARANGODB_ROOT_USER}" \
        --from-literal="arangodb-root-password=${ARANGODB_ROOT_PASSWORD}" \
        --from-literal="arangodb-app-user=${ARANGODB_APP_USER}" \
        --from-literal="arangodb-app-password=${ARANGODB_APP_PASSWORD:-}" \
        --from-literal="dragonfly-password=${DRAGONFLY_PASSWORD}" \
        --from-literal="jwt-secret=${JWT_SECRET}" \
        --from-literal="encryption-key=${ENCRYPTION_KEY}"
}

# Create forge credentials
create_forge_credentials() {
    log_info "Creating forge credentials"

    kubectl_apply create secret generic forge-credentials \
        --namespace="${NAMESPACE}" \
        --from-literal="github-token=${GITHUB_TOKEN:-}" \
        --from-literal="github-username=${GITHUB_USERNAME:-}" \
        --from-literal="gitlab-token=${GITLAB_TOKEN:-}" \
        --from-literal="gitlab-username=${GITLAB_USERNAME:-}" \
        --from-literal="bitbucket-username=${BITBUCKET_USERNAME:-}" \
        --from-literal="bitbucket-app-password=${BITBUCKET_APP_PASSWORD:-}" \
        --from-literal="codeberg-token=${CODEBERG_TOKEN:-}" \
        --from-literal="codeberg-username=${CODEBERG_USERNAME:-}"
}

# Create external API keys
create_api_keys() {
    log_info "Creating external API keys"

    kubectl_apply create secret generic external-api-keys \
        --namespace="${NAMESPACE}" \
        --from-literal="webhook-secret=${WEBHOOK_SECRET:-}" \
        --from-literal="slack-webhook-url=${SLACK_WEBHOOK_URL:-}" \
        --from-literal="discord-webhook-url=${DISCORD_WEBHOOK_URL:-}" \
        --from-literal="datadog-api-key=${DATADOG_API_KEY:-}" \
        --from-literal="datadog-app-key=${DATADOG_APP_KEY:-}" \
        --from-literal="sentry-dsn=${SENTRY_DSN:-}"
}

# Create monitoring secrets
create_monitoring_secrets() {
    log_info "Creating monitoring secrets"

    kubectl_apply create secret generic monitoring-secrets \
        --namespace="${NAMESPACE}" \
        --from-literal="grafana-admin-user=${GRAFANA_ADMIN_USER}" \
        --from-literal="grafana-admin-password=${GRAFANA_ADMIN_PASSWORD:-}" \
        --from-literal="prometheus-remote-write-user=${PROMETHEUS_REMOTE_WRITE_USER:-}" \
        --from-literal="prometheus-remote-write-password=${PROMETHEUS_REMOTE_WRITE_PASSWORD:-}"
}

# Create docker registry credentials
create_registry_credentials() {
    if [[ -n "${DOCKER_CONFIG_JSON:-}" ]]; then
        log_info "Creating registry credentials from DOCKER_CONFIG_JSON"
        kubectl_apply create secret docker-registry registry-credentials \
            --namespace="${NAMESPACE}" \
            --from-file=.dockerconfigjson=<(echo "${DOCKER_CONFIG_JSON}")
    elif [[ -n "${DOCKER_USERNAME:-}" ]] && [[ -n "${DOCKER_PASSWORD:-}" ]]; then
        log_info "Creating registry credentials from DOCKER_USERNAME/PASSWORD"
        kubectl_apply create secret docker-registry registry-credentials \
            --namespace="${NAMESPACE}" \
            --docker-server="${DOCKER_SERVER:-ghcr.io}" \
            --docker-username="${DOCKER_USERNAME}" \
            --docker-password="${DOCKER_PASSWORD}" \
            --docker-email="${DOCKER_EMAIL:-}"
    else
        log_warning "Skipping registry credentials (no DOCKER_CONFIG_JSON or DOCKER_USERNAME/PASSWORD)"
    fi
}

# Label secrets for easier management
label_secrets() {
    if [[ "${DRY_RUN}" != "true" ]]; then
        log_info "Labeling secrets"
        local secrets=(
            "cicd-hyper-a-core-secrets"
            "forge-credentials"
            "external-api-keys"
            "monitoring-secrets"
        )
        for secret in "${secrets[@]}"; do
            ${KUBECTL} label secret "${secret}" \
                --namespace="${NAMESPACE}" \
                --overwrite \
                app.kubernetes.io/name=cicd-hyper-a \
                app.kubernetes.io/part-of=cicd-hyper-a \
                app.kubernetes.io/managed-by=create-secrets-script
        done
    fi
}

# Main execution
main() {
    echo "=============================================="
    echo "cicd-hyper-a Secret Creation Script"
    echo "=============================================="
    echo ""
    echo "Namespace: ${NAMESPACE}"
    echo "Dry Run: ${DRY_RUN}"
    echo "Delete First: ${DELETE_FIRST}"
    echo ""

    validate_secrets
    auto_generate_secrets

    create_namespace
    delete_secrets

    create_core_secrets
    create_forge_credentials
    create_api_keys
    create_monitoring_secrets
    create_registry_credentials

    label_secrets

    echo ""
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_success "Dry run completed. No changes were made."
    else
        log_success "All secrets created successfully!"
        echo ""
        echo "To verify:"
        echo "  kubectl get secrets -n ${NAMESPACE}"
        echo ""
        echo "To view a secret:"
        echo "  kubectl get secret cicd-hyper-a-core-secrets -n ${NAMESPACE} -o jsonpath='{.data}'"
    fi
}

main "$@"
