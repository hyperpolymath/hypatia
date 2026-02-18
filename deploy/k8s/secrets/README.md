# SPDX-License-Identifier: PMPL-1.0-or-later

# cicd-hyper-a Kubernetes Secrets Management

This directory contains templates and scripts for managing Kubernetes secrets for the cicd-hyper-a CI/CD platform.

## Quick Start

### Option 1: Manual Secrets (Development Only)

```bash
# 1. Set environment variables
export ARANGODB_ROOT_PASSWORD=$(openssl rand -base64 32)
export DRAGONFLY_PASSWORD=$(openssl rand -base64 32)
export JWT_SECRET=$(openssl rand -base64 64)
export GITHUB_TOKEN=ghp_your_token_here

# 2. Create secrets
./create-secrets.sh

# 3. Verify
kubectl get secrets -n cicd-hyper-a
```

### Option 2: Sealed Secrets (Recommended for GitOps)

```bash
# 1. Install Sealed Secrets controller
helm repo add sealed-secrets https://bitnami-labs.github.io/sealed-secrets
helm install sealed-secrets sealed-secrets/sealed-secrets -n kube-system

# 2. Create and seal secrets
kubeseal --format yaml < secrets.yaml > sealed-secrets.yaml

# 3. Commit sealed-secrets.yaml to Git (safe!)
git add sealed-secrets.yaml && git commit -m "Add sealed secrets"
```

### Option 3: External Secrets Operator (Recommended for Cloud)

```bash
# 1. Install External Secrets Operator
helm repo add external-secrets https://charts.external-secrets.io
helm install external-secrets external-secrets/external-secrets -n external-secrets --create-namespace

# 2. Configure your secret store (AWS SM, GCP SM, Vault, etc.)
kubectl apply -f external-secrets.yaml.template

# 3. Secrets are automatically synced from your provider
```

## Files Overview

| File | Purpose |
|------|---------|
| `secrets.yaml.template` | Base template with all secret definitions and placeholders |
| `sealed-secrets.yaml.template` | Bitnami Sealed Secrets format (GitOps-safe) |
| `external-secrets.yaml.template` | External Secrets Operator (AWS SM, GCP SM, Vault, Azure KV) |
| `sops-secrets.yaml.template` | Mozilla SOPS encrypted secrets |
| `create-secrets.sh` | Script to create secrets from environment variables |
| `rotate-secrets.sh` | Script to safely rotate secrets with zero-downtime |

## Secret Categories

### Core Application Secrets (`cicd-hyper-a-core-secrets`)

| Key | Description | Auto-Generated |
|-----|-------------|----------------|
| `arangodb-root-user` | ArangoDB root username | No (default: root) |
| `arangodb-root-password` | ArangoDB root password | No (required) |
| `arangodb-app-user` | ArangoDB application username | No (default: cicd_app) |
| `arangodb-app-password` | ArangoDB application password | Yes |
| `dragonfly-password` | Dragonfly (Redis) password | No (required) |
| `jwt-secret` | JWT signing secret | Yes |
| `encryption-key` | Data encryption key | Yes |

### Git Forge Credentials (`forge-credentials`)

| Key | Description | Where to Get |
|-----|-------------|--------------|
| `github-token` | GitHub PAT or App token | [GitHub Settings](https://github.com/settings/tokens) |
| `github-username` | GitHub username | - |
| `gitlab-token` | GitLab PAT | [GitLab Settings](https://gitlab.com/-/profile/personal_access_tokens) |
| `gitlab-username` | GitLab username | - |
| `bitbucket-username` | Bitbucket username | - |
| `bitbucket-app-password` | Bitbucket App Password | [Bitbucket Settings](https://bitbucket.org/account/settings/app-passwords/) |
| `codeberg-token` | Codeberg token | [Codeberg Settings](https://codeberg.org/user/settings/applications) |

### External API Keys (`external-api-keys`)

| Key | Description |
|-----|-------------|
| `webhook-secret` | Secret for validating incoming webhooks |
| `slack-webhook-url` | Slack notification webhook URL |
| `discord-webhook-url` | Discord notification webhook URL |
| `datadog-api-key` | Datadog API key for metrics |
| `datadog-app-key` | Datadog application key |
| `sentry-dsn` | Sentry DSN for error tracking |

### Monitoring Secrets (`monitoring-secrets`)

| Key | Description |
|-----|-------------|
| `grafana-admin-user` | Grafana admin username (default: admin) |
| `grafana-admin-password` | Grafana admin password |
| `prometheus-remote-write-user` | Remote write auth username |
| `prometheus-remote-write-password` | Remote write auth password |

## Secrets Management Options

### 1. Bitnami Sealed Secrets

**Best for:** GitOps workflows, teams that want encrypted secrets in Git.

**How it works:**
- Secrets are encrypted with the cluster's public key
- Only the Sealed Secrets controller can decrypt them
- Encrypted secrets are safe to commit to Git

**Setup:**
```bash
# Install controller
helm install sealed-secrets sealed-secrets/sealed-secrets -n kube-system

# Install CLI
brew install kubeseal  # macOS

# Seal a secret
kubeseal --format yaml < secrets.yaml > sealed-secrets.yaml
```

**Limitations:**
- Secrets are cluster-specific (re-seal for new clusters)
- No automatic rotation

### 2. External Secrets Operator

**Best for:** Cloud-native deployments, centralized secret management.

**Supported Providers:**
- AWS Secrets Manager
- GCP Secret Manager
- Azure Key Vault
- HashiCorp Vault
- Kubernetes Secrets (for migration)

**Setup:**
```bash
# Install operator
helm install external-secrets external-secrets/external-secrets \
  -n external-secrets --create-namespace

# Create ClusterSecretStore for your provider
kubectl apply -f external-secrets.yaml.template

# Create ExternalSecret to sync from provider
kubectl apply -f external-secrets.yaml
```

**Benefits:**
- Automatic secret rotation
- Centralized management
- Audit logging via provider
- No secrets in Git

### 3. Mozilla SOPS

**Best for:** Teams using Kustomize, FluxCD, or ArgoCD.

**Encryption Options:**
- age (recommended, simple)
- PGP (legacy)
- AWS KMS
- GCP KMS
- Azure Key Vault
- HashiCorp Vault Transit

**Setup with age:**
```bash
# Install tools
brew install sops age

# Generate key
age-keygen -o key.txt

# Create .sops.yaml config
cat > .sops.yaml << 'EOF'
creation_rules:
  - path_regex: .*\.enc\.yaml$
    age: age1your_public_key_here
EOF

# Encrypt
sops --encrypt secrets.yaml > secrets.enc.yaml

# Edit encrypted file
sops secrets.enc.yaml

# Decrypt
sops --decrypt secrets.enc.yaml > secrets.yaml
```

**GitOps Integration:**
- **FluxCD:** Native SOPS support via `decryption.provider: sops`
- **ArgoCD:** Use KSOPS plugin or helm-secrets
- **Kustomize:** Use KSOPS generator

### 4. HashiCorp Vault (Direct Integration)

**Best for:** Enterprise environments with existing Vault infrastructure.

**Features:**
- Dynamic secrets (database credentials, etc.)
- Automatic rotation
- Fine-grained access control
- Audit logging

**Kubernetes Integration Options:**
1. **Vault Agent Injector** - Sidecar that injects secrets
2. **Vault CSI Provider** - Mount secrets as files
3. **External Secrets Operator** - Sync to K8s secrets

## Secret Rotation

### Using the Rotation Script

```bash
# Preview rotation (dry run)
./rotate-secrets.sh --secret core --dry-run

# Rotate core secrets
./rotate-secrets.sh --secret core

# Rotate all secrets
./rotate-secrets.sh --all

# Rollback if needed
./rotate-secrets.sh --rollback --secret core
```

### Automated Rotation with External Secrets

Configure `refreshInterval` in ExternalSecret to auto-sync:
```yaml
spec:
  refreshInterval: 1h  # Sync every hour
```

### Rotation Best Practices

1. **Test in staging first** - Always validate rotation procedure
2. **One secret at a time** - Easier to rollback if issues occur
3. **Monitor during rotation** - Watch for authentication failures
4. **Keep backups** - The rotation script creates automatic backups
5. **Update dependent services** - Some secrets require coordinated updates

## TLS Certificates

### Using cert-manager (Recommended)

The `secrets.yaml.template` includes a `Certificate` resource for cert-manager:

```bash
# Install cert-manager
kubectl apply -f https://github.com/cert-manager/cert-manager/releases/download/v1.14.0/cert-manager.yaml

# Create ClusterIssuer for Let's Encrypt
kubectl apply -f - << 'EOF'
apiVersion: cert-manager.io/v1
kind: ClusterIssuer
metadata:
  name: letsencrypt-prod
spec:
  acme:
    server: https://acme-v02.api.letsencrypt.org/directory
    email: your-email@example.com
    privateKeySecretRef:
      name: letsencrypt-prod-account-key
    solvers:
      - http01:
          ingress:
            class: nginx
EOF
```

### Manual TLS Certificates

If not using cert-manager:

```bash
# Create TLS secret from files
kubectl create secret tls cicd-hyper-a-tls \
  --cert=path/to/tls.crt \
  --key=path/to/tls.key \
  -n cicd-hyper-a
```

## Environment Variables Reference

Create a `.env` file (never commit!) with these variables:

```bash
# Required
ARANGODB_ROOT_PASSWORD=your_strong_password
DRAGONFLY_PASSWORD=your_strong_password

# Optional (auto-generated if not set)
ARANGODB_APP_PASSWORD=
JWT_SECRET=
ENCRYPTION_KEY=
WEBHOOK_SECRET=
GRAFANA_ADMIN_PASSWORD=

# Git forges (required for sync features)
GITHUB_TOKEN=ghp_xxxx
GITHUB_USERNAME=hyperpolymath
GITLAB_TOKEN=glpat-xxxx
GITLAB_USERNAME=hyperpolymath
BITBUCKET_USERNAME=hyperpolymath
BITBUCKET_APP_PASSWORD=xxxx
CODEBERG_TOKEN=xxxx
CODEBERG_USERNAME=hyperpolymath

# Optional integrations
SLACK_WEBHOOK_URL=
DISCORD_WEBHOOK_URL=
DATADOG_API_KEY=
SENTRY_DSN=

# Docker registry
DOCKER_SERVER=ghcr.io
DOCKER_USERNAME=hyperpolymath
DOCKER_PASSWORD=ghp_xxxx
```

## Security Best Practices

1. **Never commit plaintext secrets** - Use Sealed Secrets, SOPS, or External Secrets
2. **Use strong passwords** - Generate with `openssl rand -base64 32`
3. **Rotate regularly** - At least quarterly, or after any exposure
4. **Limit access** - Use RBAC to restrict who can read secrets
5. **Audit access** - Enable audit logging in your cluster
6. **Use namespaces** - Isolate secrets per environment
7. **Encrypt etcd** - Enable encryption at rest for Kubernetes
8. **Use short-lived tokens** - Where possible, use dynamic credentials

## Troubleshooting

### Secret not found
```bash
kubectl get secrets -n cicd-hyper-a
kubectl describe secret cicd-hyper-a-core-secrets -n cicd-hyper-a
```

### Decode a secret value
```bash
kubectl get secret cicd-hyper-a-core-secrets -n cicd-hyper-a \
  -o jsonpath='{.data.jwt-secret}' | base64 -d
```

### Check External Secrets sync status
```bash
kubectl get externalsecrets -n cicd-hyper-a
kubectl describe externalsecret cicd-hyper-a-core-secrets -n cicd-hyper-a
```

### Sealed Secrets not decrypting
```bash
# Check controller logs
kubectl logs -n kube-system -l app.kubernetes.io/name=sealed-secrets

# Verify certificate
kubeseal --fetch-cert --controller-name=sealed-secrets --controller-namespace=kube-system
```

## Related Documentation

- [Kubernetes Secrets](https://kubernetes.io/docs/concepts/configuration/secret/)
- [Sealed Secrets](https://github.com/bitnami-labs/sealed-secrets)
- [External Secrets Operator](https://external-secrets.io/)
- [Mozilla SOPS](https://github.com/getsops/sops)
- [cert-manager](https://cert-manager.io/)
- [HashiCorp Vault](https://www.vaultproject.io/)
