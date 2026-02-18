#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# cicd-hyper-a - Rotate Kubernetes Secrets Safely
#
# This script rotates secrets with zero-downtime using rolling updates.
# It creates new secrets, triggers pod restarts, and cleans up old versions.
#
# Usage:
#   ./rotate-secrets.sh                           # Interactive mode
#   ./rotate-secrets.sh --secret core             # Rotate core secrets only
#   ./rotate-secrets.sh --secret forge            # Rotate forge credentials only
#   ./rotate-secrets.sh --all                     # Rotate all secrets
#   ./rotate-secrets.sh --dry-run                 # Preview without changes
#   ./rotate-secrets.sh --rollback                # Rollback to previous version
#
# Best Practices:
#   1. Always test with --dry-run first
#   2. Rotate one secret at a time in production
#   3. Monitor application health during rotation
#   4. Keep backup of old secrets (done automatically)
#
# For production, consider:
#   - Using External Secrets Operator with automatic rotation
#   - HashiCorp Vault dynamic secrets
#   - AWS Secrets Manager rotation lambdas

set -euo pipefail

# Configuration
NAMESPACE="${NAMESPACE:-cicd-hyper-a}"
DRY_RUN="${DRY_RUN:-false}"
ROLLBACK="${ROLLBACK:-false}"
SECRET_TYPE="${SECRET_TYPE:-}"
ROTATE_ALL="${ROTATE_ALL:-false}"
BACKUP_DIR="${BACKUP_DIR:-/tmp/cicd-hyper-a-secrets-backup}"
KUBECTL="${KUBECTL:-kubectl}"
RESTART_DEPLOYMENTS="${RESTART_DEPLOYMENTS:-true}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Secret definitions
declare -A SECRET_NAMES=(
    ["core"]="cicd-hyper-a-core-secrets"
    ["forge"]="forge-credentials"
    ["api"]="external-api-keys"
    ["monitoring"]="monitoring-secrets"
    ["registry"]="registry-credentials"
)

declare -A SECRET_FIELDS=(
    ["core"]="arangodb-root-password arangodb-app-password dragonfly-password jwt-secret encryption-key"
    ["forge"]="github-token gitlab-token bitbucket-app-password codeberg-token"
    ["api"]="webhook-secret"
    ["monitoring"]="grafana-admin-password prometheus-remote-write-password"
)

declare -A AFFECTED_DEPLOYMENTS=(
    ["core"]="cicd-api cicd-engine cicd-registry"
    ["forge"]="cicd-api cicd-engine"
    ["api"]="cicd-api"
    ["monitoring"]="grafana prometheus"
)

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

log_step() {
    echo -e "${CYAN}[STEP]${NC} $1"
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --dry-run)
            DRY_RUN="true"
            shift
            ;;
        --rollback)
            ROLLBACK="true"
            shift
            ;;
        --all)
            ROTATE_ALL="true"
            shift
            ;;
        --secret)
            SECRET_TYPE="$2"
            shift 2
            ;;
        --namespace)
            NAMESPACE="$2"
            shift 2
            ;;
        --no-restart)
            RESTART_DEPLOYMENTS="false"
            shift
            ;;
        -h|--help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --secret TYPE       Rotate specific secret type:"
            echo "                        core      - Database and app secrets"
            echo "                        forge     - Git forge tokens"
            echo "                        api       - External API keys"
            echo "                        monitoring - Monitoring credentials"
            echo "                        registry  - Docker registry credentials"
            echo "  --all               Rotate all secrets"
            echo "  --dry-run           Preview changes without applying"
            echo "  --rollback          Rollback to previous secret version"
            echo "  --no-restart        Don't restart deployments"
            echo "  --namespace NAME    Use specified namespace"
            echo "  -h, --help          Show this help message"
            echo ""
            echo "Examples:"
            echo "  $0 --secret core --dry-run    # Preview core secret rotation"
            echo "  $0 --secret forge             # Rotate forge credentials"
            echo "  $0 --all                      # Rotate all secrets"
            echo "  $0 --rollback --secret core   # Rollback core secrets"
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Check prerequisites
check_prerequisites() {
    if ! command -v "${KUBECTL}" &> /dev/null; then
        log_error "kubectl is not installed or not in PATH"
        exit 1
    fi

    if ! ${KUBECTL} auth can-i get secrets -n "${NAMESPACE}" &> /dev/null; then
        log_error "No permission to read secrets in namespace ${NAMESPACE}"
        exit 1
    fi

    if ! ${KUBECTL} auth can-i create secrets -n "${NAMESPACE}" &> /dev/null; then
        log_error "No permission to create secrets in namespace ${NAMESPACE}"
        exit 1
    fi
}

# Generate a new random secret value
generate_secret() {
    local length="${1:-32}"
    openssl rand -base64 "${length}" | tr -d '\n'
}

generate_hex_secret() {
    local length="${1:-32}"
    openssl rand -hex "${length}" | tr -d '\n'
}

# Backup a secret before modification
backup_secret() {
    local secret_name="$1"
    local timestamp
    timestamp=$(date +%Y%m%d-%H%M%S)
    local backup_file="${BACKUP_DIR}/${secret_name}-${timestamp}.yaml"

    mkdir -p "${BACKUP_DIR}"

    log_info "Backing up ${secret_name} to ${backup_file}"

    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY-RUN] Would backup to ${backup_file}"
    else
        ${KUBECTL} get secret "${secret_name}" -n "${NAMESPACE}" -o yaml > "${backup_file}"
        # Remove cluster-specific fields for cleaner backup
        sed -i '/uid:/d; /resourceVersion:/d; /creationTimestamp:/d' "${backup_file}"
        log_success "Backup saved to ${backup_file}"
    fi
}

# Find the most recent backup for rollback
find_latest_backup() {
    local secret_name="$1"
    local backup_pattern="${BACKUP_DIR}/${secret_name}-*.yaml"

    # shellcheck disable=SC2012
    ls -t ${backup_pattern} 2>/dev/null | head -1
}

# Rollback to previous secret version
rollback_secret() {
    local secret_type="$1"
    local secret_name="${SECRET_NAMES[$secret_type]}"
    local backup_file

    backup_file=$(find_latest_backup "${secret_name}")

    if [[ -z "${backup_file}" ]]; then
        log_error "No backup found for ${secret_name}"
        return 1
    fi

    log_info "Rolling back ${secret_name} from ${backup_file}"

    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY-RUN] Would restore from ${backup_file}"
        cat "${backup_file}"
    else
        ${KUBECTL} apply -f "${backup_file}"
        log_success "Rolled back ${secret_name} from backup"
    fi
}

# Rotate a single secret field
rotate_secret_field() {
    local secret_name="$1"
    local field="$2"
    local new_value

    # Generate appropriate secret based on field name
    case "${field}" in
        *-password|*-secret|*-key)
            new_value=$(generate_secret 32)
            ;;
        webhook-secret)
            new_value=$(generate_hex_secret 32)
            ;;
        *-token)
            log_warning "Token fields require manual rotation from the provider"
            log_info "Skipping ${field} - update manually and re-run"
            return 0
            ;;
        *)
            new_value=$(generate_secret 24)
            ;;
    esac

    log_info "Rotating field: ${field}"

    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY-RUN] Would update ${field} with new value"
    else
        # Patch the secret with the new value
        local encoded_value
        encoded_value=$(echo -n "${new_value}" | base64)

        ${KUBECTL} patch secret "${secret_name}" -n "${NAMESPACE}" \
            --type='json' \
            -p="[{\"op\": \"replace\", \"path\": \"/data/${field}\", \"value\": \"${encoded_value}\"}]"

        log_success "Rotated ${field}"
    fi
}

# Rotate all fields in a secret
rotate_secret() {
    local secret_type="$1"
    local secret_name="${SECRET_NAMES[$secret_type]}"
    local fields="${SECRET_FIELDS[$secret_type]:-}"

    log_step "Rotating secret: ${secret_name}"

    # Check if secret exists
    if ! ${KUBECTL} get secret "${secret_name}" -n "${NAMESPACE}" &> /dev/null; then
        log_error "Secret ${secret_name} not found in namespace ${NAMESPACE}"
        return 1
    fi

    # Backup first
    backup_secret "${secret_name}"

    # Rotate each field
    for field in ${fields}; do
        rotate_secret_field "${secret_name}" "${field}"
    done

    # Add rotation timestamp annotation
    if [[ "${DRY_RUN}" != "true" ]]; then
        local timestamp
        timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
        ${KUBECTL} annotate secret "${secret_name}" -n "${NAMESPACE}" \
            --overwrite \
            "secrets.cicd-hyper-a/last-rotated=${timestamp}"
    fi

    log_success "Completed rotation for ${secret_name}"
}

# Restart affected deployments to pick up new secrets
restart_deployments() {
    local secret_type="$1"
    local deployments="${AFFECTED_DEPLOYMENTS[$secret_type]:-}"

    if [[ -z "${deployments}" ]]; then
        log_info "No deployments to restart for ${secret_type}"
        return 0
    fi

    if [[ "${RESTART_DEPLOYMENTS}" != "true" ]]; then
        log_warning "Deployment restart skipped (--no-restart flag)"
        log_info "Affected deployments: ${deployments}"
        return 0
    fi

    log_step "Restarting affected deployments"

    for deployment in ${deployments}; do
        if ${KUBECTL} get deployment "${deployment}" -n "${NAMESPACE}" &> /dev/null; then
            if [[ "${DRY_RUN}" == "true" ]]; then
                log_info "[DRY-RUN] Would restart deployment: ${deployment}"
            else
                ${KUBECTL} rollout restart deployment "${deployment}" -n "${NAMESPACE}"
                log_info "Triggered restart for deployment: ${deployment}"
            fi
        else
            log_warning "Deployment ${deployment} not found, skipping"
        fi
    done
}

# Wait for deployments to be ready
wait_for_deployments() {
    local secret_type="$1"
    local deployments="${AFFECTED_DEPLOYMENTS[$secret_type]:-}"
    local timeout="${DEPLOYMENT_TIMEOUT:-300}"

    if [[ -z "${deployments}" ]] || [[ "${DRY_RUN}" == "true" ]]; then
        return 0
    fi

    log_step "Waiting for deployments to be ready"

    for deployment in ${deployments}; do
        if ${KUBECTL} get deployment "${deployment}" -n "${NAMESPACE}" &> /dev/null; then
            log_info "Waiting for ${deployment} (timeout: ${timeout}s)"
            if ${KUBECTL} rollout status deployment "${deployment}" -n "${NAMESPACE}" --timeout="${timeout}s"; then
                log_success "${deployment} is ready"
            else
                log_error "${deployment} failed to become ready"
                log_warning "Consider running: $0 --rollback --secret ${secret_type}"
                return 1
            fi
        fi
    done
}

# Interactive mode - prompt user for which secrets to rotate
interactive_mode() {
    echo ""
    echo "Available secret types to rotate:"
    echo "  1) core       - Database and application secrets"
    echo "  2) forge      - Git forge credentials (GitHub, GitLab, etc.)"
    echo "  3) api        - External API keys"
    echo "  4) monitoring - Monitoring credentials (Grafana, Prometheus)"
    echo "  5) all        - Rotate all secrets"
    echo "  q) Quit"
    echo ""

    read -rp "Select option (1-5, or q to quit): " choice

    case "${choice}" in
        1) SECRET_TYPE="core" ;;
        2) SECRET_TYPE="forge" ;;
        3) SECRET_TYPE="api" ;;
        4) SECRET_TYPE="monitoring" ;;
        5) ROTATE_ALL="true" ;;
        q|Q) exit 0 ;;
        *)
            log_error "Invalid choice: ${choice}"
            exit 1
            ;;
    esac
}

# Main execution
main() {
    echo "=============================================="
    echo "cicd-hyper-a Secret Rotation Script"
    echo "=============================================="
    echo ""
    echo "Namespace: ${NAMESPACE}"
    echo "Dry Run: ${DRY_RUN}"
    echo "Rollback: ${ROLLBACK}"
    echo ""

    check_prerequisites

    # Determine which secrets to rotate
    if [[ "${ROLLBACK}" == "true" ]]; then
        if [[ -z "${SECRET_TYPE}" ]]; then
            log_error "--rollback requires --secret TYPE"
            exit 1
        fi
        rollback_secret "${SECRET_TYPE}"
        restart_deployments "${SECRET_TYPE}"
        wait_for_deployments "${SECRET_TYPE}"
        exit 0
    fi

    if [[ -z "${SECRET_TYPE}" ]] && [[ "${ROTATE_ALL}" != "true" ]]; then
        interactive_mode
    fi

    if [[ "${ROTATE_ALL}" == "true" ]]; then
        for secret_type in "${!SECRET_NAMES[@]}"; do
            rotate_secret "${secret_type}"
            restart_deployments "${secret_type}"
            wait_for_deployments "${secret_type}"
            echo ""
        done
    else
        if [[ -z "${SECRET_NAMES[$SECRET_TYPE]:-}" ]]; then
            log_error "Unknown secret type: ${SECRET_TYPE}"
            log_info "Valid types: ${!SECRET_NAMES[*]}"
            exit 1
        fi
        rotate_secret "${SECRET_TYPE}"
        restart_deployments "${SECRET_TYPE}"
        wait_for_deployments "${SECRET_TYPE}"
    fi

    echo ""
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_success "Dry run completed. No changes were made."
    else
        log_success "Secret rotation completed!"
        echo ""
        echo "Backups saved to: ${BACKUP_DIR}"
        echo ""
        echo "To verify:"
        echo "  kubectl get secrets -n ${NAMESPACE}"
        echo ""
        echo "To rollback:"
        echo "  $0 --rollback --secret ${SECRET_TYPE:-<type>}"
    fi
}

main "$@"
