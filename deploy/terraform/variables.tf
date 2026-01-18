# SPDX-License-Identifier: PLMP-1.0-or-later
# hypatia Terraform Variables
#
# This file defines all input variables for the infrastructure configuration.
# See terraform.tfvars.example for example values.

# =============================================================================
# General Configuration
# =============================================================================

variable "project_name" {
  description = "Name of the project, used for resource naming"
  type        = string
  default     = "hypatia"
}

variable "environment" {
  description = "Deployment environment (dev, staging, production)"
  type        = string

  validation {
    condition     = contains(["dev", "staging", "production"], var.environment)
    error_message = "Environment must be one of: dev, staging, production."
  }
}

variable "domain" {
  description = "Base domain for the deployment (e.g., YOUR-DOMAIN.example.com)"
  type        = string
  default     = "YOUR-DOMAIN.example.com"
}

variable "tags" {
  description = "Additional tags to apply to all resources"
  type        = map(string)
  default     = {}
}

# =============================================================================
# Cloud Provider Selection
# =============================================================================

variable "cloud_provider" {
  description = "Cloud provider to deploy to (aws, gcp, azure)"
  type        = string

  validation {
    condition     = contains(["aws", "gcp", "azure"], var.cloud_provider)
    error_message = "Cloud provider must be one of: aws, gcp, azure."
  }
}

# =============================================================================
# AWS Configuration
# =============================================================================

variable "aws_region" {
  description = "AWS region for deployment"
  type        = string
  default     = "us-east-1"
}

variable "aws_availability_zones" {
  description = "AWS availability zones (leave empty for auto-selection)"
  type        = list(string)
  default     = []
}

# =============================================================================
# GCP Configuration
# =============================================================================

variable "gcp_project_id" {
  description = "GCP project ID"
  type        = string
  default     = ""
}

variable "gcp_region" {
  description = "GCP region for deployment"
  type        = string
  default     = "us-central1"
}

variable "gcp_zone" {
  description = "GCP zone for zonal resources"
  type        = string
  default     = "us-central1-a"
}

# =============================================================================
# Azure Configuration
# =============================================================================

variable "azure_subscription_id" {
  description = "Azure subscription ID"
  type        = string
  default     = ""
}

variable "azure_resource_group_name" {
  description = "Azure resource group name"
  type        = string
  default     = "hypatia-rg"
}

variable "azure_location" {
  description = "Azure location for deployment"
  type        = string
  default     = "East US"
}

# =============================================================================
# Networking Configuration
# =============================================================================

variable "create_networking" {
  description = "Whether to create new VPC/networking resources"
  type        = bool
  default     = true
}

variable "vpc_cidr" {
  description = "CIDR block for the VPC"
  type        = string
  default     = "10.0.0.0/16"
}

variable "availability_zones" {
  description = "List of availability zones to use"
  type        = list(string)
  default     = []
}

variable "existing_vpc_id" {
  description = "ID of existing VPC (if create_networking is false)"
  type        = string
  default     = ""
}

variable "existing_subnet_ids" {
  description = "IDs of existing subnets (if create_networking is false)"
  type        = list(string)
  default     = []
}

# =============================================================================
# Kubernetes Cluster Configuration
# =============================================================================

variable "kubernetes_version" {
  description = "Kubernetes version to deploy"
  type        = string
  default     = "1.29"
}

variable "node_instance_types" {
  description = "Instance types for Kubernetes nodes"
  type        = list(string)
  default     = ["m6i.xlarge", "m6a.xlarge"]
}

variable "min_node_count" {
  description = "Minimum number of nodes in the cluster"
  type        = number
  default     = 3
}

variable "max_node_count" {
  description = "Maximum number of nodes in the cluster"
  type        = number
  default     = 10
}

variable "desired_node_count" {
  description = "Desired number of nodes in the cluster"
  type        = number
  default     = 3
}

variable "node_disk_size_gb" {
  description = "Disk size for each node in GB"
  type        = number
  default     = 100
}

variable "kubernetes_namespace" {
  description = "Kubernetes namespace for the application"
  type        = string
  default     = "hypatia"
}

# =============================================================================
# Database Configuration (ArangoDB)
# =============================================================================

variable "enable_database" {
  description = "Whether to deploy ArangoDB"
  type        = bool
  default     = true
}

variable "use_managed_database" {
  description = "Use cloud-managed database service instead of self-hosted"
  type        = bool
  default     = false
}

variable "arangodb_version" {
  description = "ArangoDB version to deploy"
  type        = string
  default     = "3.11"
}

variable "arangodb_replica_count" {
  description = "Number of ArangoDB replicas"
  type        = number
  default     = 3
}

variable "arangodb_storage_size" {
  description = "Storage size for ArangoDB in GB"
  type        = string
  default     = "50Gi"
}

variable "arangodb_memory_limit" {
  description = "Memory limit for ArangoDB pods"
  type        = string
  default     = "8Gi"
}

variable "arangodb_cpu_limit" {
  description = "CPU limit for ArangoDB pods"
  type        = string
  default     = "2"
}

variable "arangodb_root_password" {
  description = "Root password for ArangoDB (leave empty to auto-generate)"
  type        = string
  default     = ""
  sensitive   = true
}

# =============================================================================
# Cache Configuration (Dragonfly/Redis)
# =============================================================================

variable "enable_cache" {
  description = "Whether to deploy cache (Dragonfly/Redis)"
  type        = bool
  default     = true
}

variable "use_managed_cache" {
  description = "Use cloud-managed cache service instead of self-hosted"
  type        = bool
  default     = false
}

variable "cache_engine" {
  description = "Cache engine to use (dragonfly or redis)"
  type        = string
  default     = "dragonfly"

  validation {
    condition     = contains(["dragonfly", "redis"], var.cache_engine)
    error_message = "Cache engine must be one of: dragonfly, redis."
  }
}

variable "cache_node_type" {
  description = "Instance type for managed cache nodes"
  type        = string
  default     = "cache.r6g.large"
}

variable "cache_replica_count" {
  description = "Number of cache replicas"
  type        = number
  default     = 1
}

variable "cache_storage_size" {
  description = "Storage size for cache in GB"
  type        = string
  default     = "20Gi"
}

variable "cache_memory_limit" {
  description = "Memory limit for cache pods"
  type        = string
  default     = "4Gi"
}

variable "dragonfly_version" {
  description = "Dragonfly version to deploy"
  type        = string
  default     = "v1.14.1"
}

variable "dragonfly_proactor_threads" {
  description = "Number of Dragonfly proactor threads"
  type        = number
  default     = 4
}

variable "cache_password" {
  description = "Password for cache authentication (leave empty to auto-generate)"
  type        = string
  default     = ""
  sensitive   = true
}

# =============================================================================
# Monitoring Configuration
# =============================================================================

variable "enable_monitoring" {
  description = "Whether to deploy monitoring stack (Prometheus/Grafana)"
  type        = bool
  default     = true
}

variable "prometheus_retention" {
  description = "Prometheus data retention period"
  type        = string
  default     = "30d"
}

variable "prometheus_retention_size" {
  description = "Prometheus retention size limit"
  type        = string
  default     = "10GB"
}

variable "prometheus_storage_size" {
  description = "Storage size for Prometheus"
  type        = string
  default     = "50Gi"
}

variable "grafana_admin_password" {
  description = "Admin password for Grafana (leave empty to auto-generate)"
  type        = string
  default     = ""
  sensitive   = true
}

variable "grafana_storage_size" {
  description = "Storage size for Grafana"
  type        = string
  default     = "10Gi"
}

variable "enable_alertmanager" {
  description = "Whether to deploy Alertmanager"
  type        = bool
  default     = true
}

variable "alertmanager_replicas" {
  description = "Number of Alertmanager replicas"
  type        = number
  default     = 2
}

variable "enable_monitoring_ingress" {
  description = "Whether to create ingress for monitoring services"
  type        = bool
  default     = true
}

# =============================================================================
# Ingress Configuration
# =============================================================================

variable "ingress_class_name" {
  description = "Ingress class name to use"
  type        = string
  default     = "nginx"
}

variable "cluster_issuer" {
  description = "cert-manager cluster issuer for TLS certificates"
  type        = string
  default     = "letsencrypt-prod"
}

# =============================================================================
# Application Configuration
# =============================================================================

variable "deploy_application" {
  description = "Whether to deploy the application via Helm"
  type        = bool
  default     = true
}

variable "api_replicas" {
  description = "Number of API service replicas"
  type        = number
  default     = 3
}

variable "registry_replicas" {
  description = "Number of Registry service replicas"
  type        = number
  default     = 2
}

variable "engine_replicas" {
  description = "Number of Engine service replicas"
  type        = number
  default     = 2
}

# =============================================================================
# Feature Flags
# =============================================================================

variable "enable_neural_learning" {
  description = "Enable neural learning features"
  type        = bool
  default     = true
}

variable "enable_p2p_federation" {
  description = "Enable P2P federation features"
  type        = bool
  default     = false
}

# =============================================================================
# Forge Credentials
# =============================================================================

variable "github_token" {
  description = "GitHub personal access token for API access"
  type        = string
  default     = ""
  sensitive   = true
}

variable "gitlab_token" {
  description = "GitLab personal access token for API access"
  type        = string
  default     = ""
  sensitive   = true
}

variable "bitbucket_user" {
  description = "Bitbucket username"
  type        = string
  default     = ""
}

variable "bitbucket_app_password" {
  description = "Bitbucket app password for API access"
  type        = string
  default     = ""
  sensitive   = true
}
