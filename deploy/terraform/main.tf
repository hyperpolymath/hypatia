# SPDX-License-Identifier: PLMP-1.0-or-later
# hypatia Terraform Main Configuration
#
# This configuration supports multi-cloud deployment to AWS, GCP, or Azure.
# Set the `cloud_provider` variable to select your target platform.

terraform {
  required_version = ">= 1.6.0"

  required_providers {
    # AWS Provider
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.31"
    }
    # Google Cloud Provider
    google = {
      source  = "hashicorp/google"
      version = "~> 5.12"
    }
    # Azure Provider
    azurerm = {
      source  = "hashicorp/azurerm"
      version = "~> 3.85"
    }
    # Kubernetes Provider (for deploying workloads)
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.24"
    }
    # Helm Provider (for chart deployments)
    helm = {
      source  = "hashicorp/helm"
      version = "~> 2.12"
    }
    # Random Provider (for generating passwords/tokens)
    random = {
      source  = "hashicorp/random"
      version = "~> 3.6"
    }
    # TLS Provider (for certificate generation)
    tls = {
      source  = "hashicorp/tls"
      version = "~> 4.0"
    }
  }

  # Backend configuration - uncomment and configure for remote state
  # backend "s3" {
  #   bucket         = "hypatia-terraform-state"
  #   key            = "terraform.tfstate"
  #   region         = "us-east-1"
  #   encrypt        = true
  #   dynamodb_table = "hypatia-terraform-locks"
  # }

  # backend "gcs" {
  #   bucket = "hypatia-terraform-state"
  #   prefix = "terraform/state"
  # }

  # backend "azurerm" {
  #   resource_group_name  = "hypatia-terraform"
  #   storage_account_name = "cicdtfstate"
  #   container_name       = "tfstate"
  #   key                  = "terraform.tfstate"
  # }
}

# =============================================================================
# Provider Configurations
# =============================================================================

# AWS Provider - only configured when cloud_provider is "aws"
provider "aws" {
  region = var.aws_region

  default_tags {
    tags = local.common_tags
  }

  # Skip provider configuration if not using AWS
  skip_credentials_validation = var.cloud_provider != "aws"
  skip_requesting_account_id  = var.cloud_provider != "aws"
  skip_metadata_api_check     = var.cloud_provider != "aws"
}

# Google Cloud Provider - only configured when cloud_provider is "gcp"
provider "google" {
  project = var.gcp_project_id
  region  = var.gcp_region
}

# Azure Provider - only configured when cloud_provider is "azure"
provider "azurerm" {
  features {
    resource_group {
      prevent_deletion_if_contains_resources = true
    }
    key_vault {
      purge_soft_delete_on_destroy    = false
      recover_soft_deleted_key_vaults = true
    }
  }

  subscription_id = var.azure_subscription_id

  # Skip provider configuration if not using Azure
  skip_provider_registration = var.cloud_provider != "azure"
}

# Kubernetes Provider - configured after cluster creation
provider "kubernetes" {
  host                   = local.kubernetes_host
  cluster_ca_certificate = local.kubernetes_ca_cert
  token                  = local.kubernetes_token

  # For EKS authentication
  dynamic "exec" {
    for_each = var.cloud_provider == "aws" ? [1] : []
    content {
      api_version = "client.authentication.k8s.io/v1beta1"
      command     = "aws"
      args        = ["eks", "get-token", "--cluster-name", local.cluster_name]
    }
  }
}

# Helm Provider - configured after cluster creation
provider "helm" {
  kubernetes {
    host                   = local.kubernetes_host
    cluster_ca_certificate = local.kubernetes_ca_cert
    token                  = local.kubernetes_token

    dynamic "exec" {
      for_each = var.cloud_provider == "aws" ? [1] : []
      content {
        api_version = "client.authentication.k8s.io/v1beta1"
        command     = "aws"
        args        = ["eks", "get-token", "--cluster-name", local.cluster_name]
      }
    }
  }
}

# =============================================================================
# Local Values
# =============================================================================

locals {
  # Common tags applied to all resources
  common_tags = merge(var.tags, {
    Project     = "hypatia"
    Environment = var.environment
    ManagedBy   = "terraform"
    Repository  = "github.com/hyperpolymath/hypatia"
  })

  # Cluster name based on environment
  cluster_name = "${var.project_name}-${var.environment}"

  # Domain configuration
  base_domain    = var.domain
  api_domain     = "api.${local.base_domain}"
  grafana_domain = "grafana.${local.base_domain}"

  # Kubernetes connection details (set after cluster creation)
  kubernetes_host = (
    var.cloud_provider == "aws" ? module.kubernetes[0].cluster_endpoint :
    var.cloud_provider == "gcp" ? module.kubernetes[0].cluster_endpoint :
    var.cloud_provider == "azure" ? module.kubernetes[0].cluster_endpoint :
    ""
  )

  kubernetes_ca_cert = (
    var.cloud_provider == "aws" ? base64decode(module.kubernetes[0].cluster_ca_certificate) :
    var.cloud_provider == "gcp" ? base64decode(module.kubernetes[0].cluster_ca_certificate) :
    var.cloud_provider == "azure" ? base64decode(module.kubernetes[0].cluster_ca_certificate) :
    ""
  )

  kubernetes_token = (
    var.cloud_provider == "aws" ? null :
    var.cloud_provider == "gcp" ? module.kubernetes[0].cluster_token :
    var.cloud_provider == "azure" ? module.kubernetes[0].cluster_token :
    ""
  )
}

# =============================================================================
# Networking Module
# =============================================================================

module "networking" {
  source = "./modules/networking"
  count  = var.create_networking ? 1 : 0

  cloud_provider = var.cloud_provider
  environment    = var.environment
  project_name   = var.project_name

  # VPC/Network configuration
  vpc_cidr           = var.vpc_cidr
  availability_zones = var.availability_zones

  # AWS-specific
  aws_region = var.aws_region

  # GCP-specific
  gcp_project_id = var.gcp_project_id
  gcp_region     = var.gcp_region

  # Azure-specific
  azure_resource_group_name = var.azure_resource_group_name
  azure_location            = var.azure_location

  tags = local.common_tags
}

# =============================================================================
# Kubernetes Cluster Module
# =============================================================================

module "kubernetes" {
  source = "./modules/kubernetes"
  count  = 1

  cloud_provider = var.cloud_provider
  environment    = var.environment
  project_name   = var.project_name
  cluster_name   = local.cluster_name

  # Cluster configuration
  kubernetes_version  = var.kubernetes_version
  node_instance_types = var.node_instance_types
  min_node_count      = var.min_node_count
  max_node_count      = var.max_node_count
  desired_node_count  = var.desired_node_count
  node_disk_size_gb   = var.node_disk_size_gb

  # Networking (from networking module or existing)
  vpc_id             = var.create_networking ? module.networking[0].vpc_id : var.existing_vpc_id
  subnet_ids         = var.create_networking ? module.networking[0].private_subnet_ids : var.existing_subnet_ids

  # AWS-specific
  aws_region = var.aws_region

  # GCP-specific
  gcp_project_id = var.gcp_project_id
  gcp_region     = var.gcp_region
  gcp_zone       = var.gcp_zone

  # Azure-specific
  azure_resource_group_name = var.azure_resource_group_name
  azure_location            = var.azure_location

  tags = local.common_tags

  depends_on = [module.networking]
}

# =============================================================================
# Database Module (ArangoDB)
# =============================================================================

module "database" {
  source = "./modules/database"
  count  = var.enable_database ? 1 : 0

  cloud_provider = var.cloud_provider
  environment    = var.environment
  project_name   = var.project_name

  # Deployment mode
  use_managed_database = var.use_managed_database
  arangodb_version     = var.arangodb_version

  # ArangoDB configuration
  arangodb_replica_count = var.arangodb_replica_count
  arangodb_storage_size  = var.arangodb_storage_size
  arangodb_memory_limit  = var.arangodb_memory_limit
  arangodb_cpu_limit     = var.arangodb_cpu_limit

  # Credentials
  arangodb_root_password = var.arangodb_root_password

  # Kubernetes namespace
  kubernetes_namespace = var.kubernetes_namespace

  tags = local.common_tags

  depends_on = [module.kubernetes]
}

# =============================================================================
# Cache Module (Dragonfly/Redis)
# =============================================================================

module "cache" {
  source = "./modules/cache"
  count  = var.enable_cache ? 1 : 0

  cloud_provider = var.cloud_provider
  environment    = var.environment
  project_name   = var.project_name

  # Deployment mode
  use_managed_cache = var.use_managed_cache
  cache_engine      = var.cache_engine # "dragonfly" or "redis"

  # Cache configuration
  cache_node_type   = var.cache_node_type
  cache_replica_count = var.cache_replica_count
  cache_storage_size  = var.cache_storage_size
  cache_memory_limit  = var.cache_memory_limit

  # Dragonfly-specific
  dragonfly_version         = var.dragonfly_version
  dragonfly_proactor_threads = var.dragonfly_proactor_threads

  # Credentials
  cache_password = var.cache_password

  # Networking
  vpc_id     = var.create_networking ? module.networking[0].vpc_id : var.existing_vpc_id
  subnet_ids = var.create_networking ? module.networking[0].private_subnet_ids : var.existing_subnet_ids

  # Kubernetes namespace
  kubernetes_namespace = var.kubernetes_namespace

  tags = local.common_tags

  depends_on = [module.kubernetes]
}

# =============================================================================
# Monitoring Module (Prometheus/Grafana)
# =============================================================================

module "monitoring" {
  source = "./modules/monitoring"
  count  = var.enable_monitoring ? 1 : 0

  environment  = var.environment
  project_name = var.project_name

  # Prometheus configuration
  prometheus_retention      = var.prometheus_retention
  prometheus_retention_size = var.prometheus_retention_size
  prometheus_storage_size   = var.prometheus_storage_size

  # Grafana configuration
  grafana_admin_password = var.grafana_admin_password
  grafana_storage_size   = var.grafana_storage_size
  grafana_domain         = local.grafana_domain

  # Alertmanager configuration
  enable_alertmanager   = var.enable_alertmanager
  alertmanager_replicas = var.alertmanager_replicas

  # Kubernetes namespace
  kubernetes_namespace = var.kubernetes_namespace

  # Ingress configuration
  enable_monitoring_ingress = var.enable_monitoring_ingress
  ingress_class_name        = var.ingress_class_name
  cluster_issuer            = var.cluster_issuer

  tags = local.common_tags

  depends_on = [module.kubernetes]
}

# =============================================================================
# Application Deployment (via Helm)
# =============================================================================

resource "helm_release" "cicd_hyper_a" {
  count = var.deploy_application ? 1 : 0

  name       = "hypatia"
  namespace  = var.kubernetes_namespace
  chart      = "${path.module}/../helm"

  create_namespace = true
  wait             = true
  timeout          = 600

  values = [
    templatefile("${path.module}/helm-values.yaml.tpl", {
      environment         = var.environment
      domain              = local.base_domain
      api_domain          = local.api_domain
      grafana_domain      = local.grafana_domain

      # Replica counts
      api_replicas      = var.api_replicas
      registry_replicas = var.registry_replicas
      engine_replicas   = var.engine_replicas

      # Database configuration
      arangodb_enabled     = var.enable_database
      arangodb_external    = var.use_managed_database
      arangodb_url         = var.enable_database && var.use_managed_database ? module.database[0].arangodb_endpoint : ""

      # Cache configuration
      dragonfly_enabled  = var.enable_cache
      dragonfly_external = var.use_managed_cache
      dragonfly_url      = var.enable_cache && var.use_managed_cache ? module.cache[0].cache_endpoint : ""

      # Monitoring
      monitoring_enabled = var.enable_monitoring

      # Ingress
      ingress_class_name = var.ingress_class_name
      cluster_issuer     = var.cluster_issuer

      # Feature flags
      neural_learning_enabled = var.enable_neural_learning
      p2p_federation_enabled  = var.enable_p2p_federation
    })
  ]

  # Set sensitive values
  set_sensitive {
    name  = "secrets.arangodb.password"
    value = var.arangodb_root_password
  }

  set_sensitive {
    name  = "secrets.dragonfly.password"
    value = var.cache_password
  }

  set_sensitive {
    name  = "secrets.github.token"
    value = var.github_token
  }

  set_sensitive {
    name  = "secrets.gitlab.token"
    value = var.gitlab_token
  }

  depends_on = [
    module.kubernetes,
    module.database,
    module.cache,
    module.monitoring
  ]
}

# =============================================================================
# Random Password Generation (for defaults)
# =============================================================================

resource "random_password" "arangodb" {
  count   = var.arangodb_root_password == "" ? 1 : 0
  length  = 32
  special = true
}

resource "random_password" "cache" {
  count   = var.cache_password == "" ? 1 : 0
  length  = 32
  special = true
}

resource "random_password" "grafana" {
  count   = var.grafana_admin_password == "" ? 1 : 0
  length  = 24
  special = true
}
