# cicd-hyper-a Terraform Infrastructure

This directory contains Terraform/OpenTofu infrastructure-as-code configurations for deploying cicd-hyper-a to AWS, GCP, or Azure.

## Prerequisites

- [Terraform](https://www.terraform.io/downloads) >= 1.6.0 or [OpenTofu](https://opentofu.org/) >= 1.6.0
- Cloud provider CLI configured:
  - AWS: `aws configure`
  - GCP: `gcloud auth application-default login`
  - Azure: `az login`
- kubectl for cluster access
- Helm for chart verification (optional)

## Quick Start

### 1. Clone and Configure

```bash
cd deploy/terraform

# Copy the example variables file
cp terraform.tfvars.example terraform.tfvars

# Edit with your configuration
# IMPORTANT: Replace YOUR-DOMAIN.example.com with your actual domain
vim terraform.tfvars
```

### 2. Initialize Terraform

```bash
terraform init
```

### 3. Review the Plan

```bash
terraform plan
```

### 4. Apply the Configuration

```bash
terraform apply
```

### 5. Configure kubectl

After deployment, configure kubectl to access the cluster:

```bash
# AWS
aws eks update-kubeconfig --region <region> --name cicd-hyper-a-<environment>

# GCP
gcloud container clusters get-credentials cicd-hyper-a-<environment> \
  --region <region> --project <project-id>

# Azure
az aks get-credentials --resource-group <resource-group> \
  --name cicd-hyper-a-<environment>
```

## Architecture

```
                                   Internet
                                      |
                              [Load Balancer]
                                      |
                    +-----------------+-----------------+
                    |                 |                 |
                [API Pod]       [API Pod]         [API Pod]
                    |                 |                 |
                    +-----------------+-----------------+
                              |                 |
                    [Registry Pods]     [Engine Pods]
                              |                 |
                    +---------+-----------------+---------+
                    |                                     |
              [ArangoDB Cluster]               [Dragonfly Cache]
                    |                                     |
              [Persistent Storage]           [Persistent Storage]
                    |
              [Prometheus] --> [Grafana]
```

## Module Structure

```
terraform/
├── main.tf                 # Main configuration with providers
├── variables.tf            # Input variable definitions
├── outputs.tf              # Output value definitions
├── terraform.tfvars.example # Example variable values
├── helm-values.yaml.tpl    # Helm values template
└── modules/
    ├── kubernetes/         # K8s cluster (EKS/GKE/AKS)
    ├── database/           # ArangoDB deployment
    ├── cache/              # Dragonfly/Redis cache
    ├── monitoring/         # Prometheus/Grafana stack
    └── networking/         # VPC/VNet, subnets, NAT
```

## Configuration Options

### Cloud Provider Selection

Set `cloud_provider` to one of:
- `aws` - Amazon Web Services (EKS)
- `gcp` - Google Cloud Platform (GKE)
- `azure` - Microsoft Azure (AKS)

### Deployment Modes

#### Self-Hosted (Default)

Deploy all components on Kubernetes:
```hcl
use_managed_database = false
use_managed_cache    = false
```

#### Hybrid (Managed Database/Cache)

Use cloud-managed services for database and cache:
```hcl
use_managed_database = true  # Uses AWS DocumentDB/GCP equivalent
use_managed_cache    = true  # Uses ElastiCache/Memorystore/Azure Cache
```

### Environment Configurations

| Environment | API Replicas | DB Replicas | Cache | Autoscaling |
|-------------|--------------|-------------|-------|-------------|
| dev         | 1            | 1           | 1     | Disabled    |
| staging     | 2            | 2           | 1     | Enabled     |
| production  | 3            | 3           | 1+    | Enabled     |

## Outputs

After deployment, Terraform provides:

| Output | Description |
|--------|-------------|
| `cluster_endpoint` | Kubernetes API server endpoint |
| `api_url` | External API URL |
| `grafana_url` | Grafana dashboard URL |
| `arangodb_endpoint` | ArangoDB connection endpoint |
| `cache_endpoint` | Dragonfly/Redis endpoint |
| `kubeconfig_command` | Command to configure kubectl |

## Security Considerations

### Secrets Management

1. **Never commit `terraform.tfvars`** - Add to `.gitignore`
2. **Use secret managers** for production:
   - AWS: Secrets Manager or Parameter Store
   - GCP: Secret Manager
   - Azure: Key Vault

3. **Auto-generated passwords**: Leave password variables empty to auto-generate:
   ```hcl
   arangodb_root_password = ""  # Auto-generated
   cache_password = ""          # Auto-generated
   grafana_admin_password = ""  # Auto-generated
   ```

   Retrieve generated passwords:
   ```bash
   terraform output -json generated_passwords
   ```

### Network Security

- Private subnets for all workloads
- NAT gateways for outbound internet access
- VPC endpoints for AWS services (ECR, S3)
- Network policies enabled on the cluster

### Cluster Security

- RBAC enabled
- Pod Security Standards enforced
- Secrets encryption at rest
- Node auto-upgrade enabled

## Customization

### Domain Configuration

Replace the placeholder domain in your `terraform.tfvars`:

```hcl
domain = "cicd.yourdomain.com"
```

This will configure:
- API endpoint: `api.cicd.yourdomain.com`
- Grafana: `grafana.cicd.yourdomain.com`

### DNS Setup

After deployment, create DNS records pointing to the load balancer:

```bash
terraform output dns_records_required
```

### Custom Helm Values

Override Helm values by editing `helm-values.yaml.tpl` or providing additional values:

```hcl
# In variables.tf, add custom values
variable "custom_helm_values" {
  type    = map(any)
  default = {}
}
```

## Maintenance

### Upgrading

1. Update Terraform version in `main.tf`
2. Update provider versions
3. Run `terraform init -upgrade`
4. Review and apply: `terraform plan && terraform apply`

### Scaling

Adjust node counts in `terraform.tfvars`:
```hcl
min_node_count     = 5
max_node_count     = 20
desired_node_count = 5
```

### Destroying

```bash
# WARNING: This destroys all resources including data
terraform destroy
```

## Troubleshooting

### Common Issues

1. **EKS cluster creation timeout**
   - Increase timeout: `timeout = 30m` in provider config
   - Check IAM permissions

2. **GKE private cluster access**
   - Ensure authorized networks include your IP
   - Use Cloud Shell or bastion host

3. **AKS identity issues**
   - Verify Azure CLI is logged in
   - Check service principal permissions

### Debugging

```bash
# Enable debug logging
TF_LOG=DEBUG terraform apply

# Verify cluster connectivity
kubectl cluster-info

# Check pod status
kubectl get pods -n cicd-hyper-a
```

## State Management

### Remote State (Recommended for Production)

Uncomment the appropriate backend in `main.tf`:

**AWS S3:**
```hcl
backend "s3" {
  bucket         = "cicd-hyper-a-terraform-state"
  key            = "terraform.tfstate"
  region         = "us-east-1"
  encrypt        = true
  dynamodb_table = "cicd-hyper-a-terraform-locks"
}
```

**GCP GCS:**
```hcl
backend "gcs" {
  bucket = "cicd-hyper-a-terraform-state"
  prefix = "terraform/state"
}
```

**Azure Blob:**
```hcl
backend "azurerm" {
  resource_group_name  = "cicd-hyper-a-terraform"
  storage_account_name = "cicdtfstate"
  container_name       = "tfstate"
  key                  = "terraform.tfstate"
}
```

## License

SPDX-License-Identifier: PLMP-1.0-or-later
