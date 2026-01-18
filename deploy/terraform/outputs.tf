# SPDX-License-Identifier: PLMP-1.0-or-later
# cicd-hyper-a Terraform Outputs
#
# This file defines all output values from the infrastructure deployment.

# =============================================================================
# Cluster Information
# =============================================================================

output "cluster_name" {
  description = "Name of the Kubernetes cluster"
  value       = local.cluster_name
}

output "cluster_endpoint" {
  description = "Endpoint URL for the Kubernetes cluster API server"
  value       = module.kubernetes[0].cluster_endpoint
  sensitive   = true
}

output "cluster_ca_certificate" {
  description = "Base64 encoded CA certificate for cluster authentication"
  value       = module.kubernetes[0].cluster_ca_certificate
  sensitive   = true
}

output "kubeconfig_command" {
  description = "Command to configure kubectl for cluster access"
  value = (
    var.cloud_provider == "aws" ? "aws eks update-kubeconfig --region ${var.aws_region} --name ${local.cluster_name}" :
    var.cloud_provider == "gcp" ? "gcloud container clusters get-credentials ${local.cluster_name} --region ${var.gcp_region} --project ${var.gcp_project_id}" :
    var.cloud_provider == "azure" ? "az aks get-credentials --resource-group ${var.azure_resource_group_name} --name ${local.cluster_name}" :
    "Unknown cloud provider"
  )
}

# =============================================================================
# Networking Information
# =============================================================================

output "vpc_id" {
  description = "ID of the VPC"
  value       = var.create_networking ? module.networking[0].vpc_id : var.existing_vpc_id
}

output "private_subnet_ids" {
  description = "IDs of the private subnets"
  value       = var.create_networking ? module.networking[0].private_subnet_ids : var.existing_subnet_ids
}

output "public_subnet_ids" {
  description = "IDs of the public subnets"
  value       = var.create_networking ? module.networking[0].public_subnet_ids : []
}

output "load_balancer_dns" {
  description = "DNS name of the load balancer"
  value       = var.create_networking ? module.networking[0].load_balancer_dns : ""
}

output "nat_gateway_ips" {
  description = "Public IP addresses of NAT gateways"
  value       = var.create_networking ? module.networking[0].nat_gateway_ips : []
}

# =============================================================================
# Database Information
# =============================================================================

output "arangodb_endpoint" {
  description = "Endpoint for ArangoDB connection"
  value       = var.enable_database ? module.database[0].arangodb_endpoint : ""
  sensitive   = true
}

output "arangodb_internal_endpoint" {
  description = "Internal Kubernetes service endpoint for ArangoDB"
  value       = var.enable_database ? "arangodb.${var.kubernetes_namespace}.svc.cluster.local:8529" : ""
}

output "arangodb_connection_string" {
  description = "Connection string for ArangoDB"
  value       = var.enable_database ? "http://root:${local.arangodb_password}@${module.database[0].arangodb_endpoint}" : ""
  sensitive   = true
}

# =============================================================================
# Cache Information
# =============================================================================

output "cache_endpoint" {
  description = "Endpoint for cache (Dragonfly/Redis) connection"
  value       = var.enable_cache ? module.cache[0].cache_endpoint : ""
  sensitive   = true
}

output "cache_internal_endpoint" {
  description = "Internal Kubernetes service endpoint for cache"
  value       = var.enable_cache ? "dragonfly.${var.kubernetes_namespace}.svc.cluster.local:6379" : ""
}

output "cache_connection_string" {
  description = "Connection string for cache"
  value       = var.enable_cache ? "redis://:${local.cache_password}@${module.cache[0].cache_endpoint}" : ""
  sensitive   = true
}

# =============================================================================
# Monitoring Information
# =============================================================================

output "prometheus_endpoint" {
  description = "Prometheus endpoint"
  value       = var.enable_monitoring ? "http://prometheus.${var.kubernetes_namespace}.svc.cluster.local:9090" : ""
}

output "grafana_endpoint" {
  description = "Grafana endpoint"
  value       = var.enable_monitoring ? "http://grafana.${var.kubernetes_namespace}.svc.cluster.local:3000" : ""
}

output "grafana_url" {
  description = "External URL for Grafana (if ingress enabled)"
  value       = var.enable_monitoring && var.enable_monitoring_ingress ? "https://${local.grafana_domain}" : ""
}

output "grafana_admin_password" {
  description = "Grafana admin password"
  value       = local.grafana_password
  sensitive   = true
}

# =============================================================================
# Application Endpoints
# =============================================================================

output "api_url" {
  description = "External URL for the API service"
  value       = "https://${local.api_domain}"
}

output "api_internal_endpoint" {
  description = "Internal Kubernetes service endpoint for API"
  value       = "api.${var.kubernetes_namespace}.svc.cluster.local:8000"
}

output "registry_internal_endpoint" {
  description = "Internal Kubernetes service endpoint for Registry"
  value       = "registry.${var.kubernetes_namespace}.svc.cluster.local:8080"
}

output "engine_internal_endpoint" {
  description = "Internal Kubernetes service endpoint for Engine"
  value       = "engine.${var.kubernetes_namespace}.svc.cluster.local:8082"
}

# =============================================================================
# DNS Configuration
# =============================================================================

output "dns_records_required" {
  description = "DNS records that need to be configured"
  value = {
    api_record = {
      name  = "api"
      type  = "CNAME"
      value = var.create_networking ? module.networking[0].load_balancer_dns : "CONFIGURE_MANUALLY"
    }
    grafana_record = var.enable_monitoring ? {
      name  = "grafana"
      type  = "CNAME"
      value = var.create_networking ? module.networking[0].load_balancer_dns : "CONFIGURE_MANUALLY"
    } : null
  }
}

# =============================================================================
# Security Information
# =============================================================================

output "cluster_security_group_id" {
  description = "Security group ID for the Kubernetes cluster"
  value       = module.kubernetes[0].cluster_security_group_id
}

output "node_security_group_id" {
  description = "Security group ID for the Kubernetes nodes"
  value       = module.kubernetes[0].node_security_group_id
}

# =============================================================================
# Generated Credentials
# =============================================================================

output "generated_passwords" {
  description = "Auto-generated passwords (if not provided)"
  value = {
    arangodb_password = var.arangodb_root_password == "" ? random_password.arangodb[0].result : "USER_PROVIDED"
    cache_password    = var.cache_password == "" ? random_password.cache[0].result : "USER_PROVIDED"
    grafana_password  = var.grafana_admin_password == "" ? random_password.grafana[0].result : "USER_PROVIDED"
  }
  sensitive = true
}

# =============================================================================
# Helper Locals for Outputs
# =============================================================================

locals {
  arangodb_password = var.arangodb_root_password != "" ? var.arangodb_root_password : (
    length(random_password.arangodb) > 0 ? random_password.arangodb[0].result : ""
  )

  cache_password = var.cache_password != "" ? var.cache_password : (
    length(random_password.cache) > 0 ? random_password.cache[0].result : ""
  )

  grafana_password = var.grafana_admin_password != "" ? var.grafana_admin_password : (
    length(random_password.grafana) > 0 ? random_password.grafana[0].result : ""
  )
}

# =============================================================================
# Deployment Summary
# =============================================================================

output "deployment_summary" {
  description = "Summary of deployed resources"
  value = {
    cloud_provider     = var.cloud_provider
    environment        = var.environment
    cluster_name       = local.cluster_name
    kubernetes_version = var.kubernetes_version
    node_count         = "${var.min_node_count}-${var.max_node_count}"

    components = {
      database   = var.enable_database ? (var.use_managed_database ? "managed" : "self-hosted") : "disabled"
      cache      = var.enable_cache ? (var.use_managed_cache ? "managed" : "self-hosted") : "disabled"
      monitoring = var.enable_monitoring ? "enabled" : "disabled"
    }

    endpoints = {
      api     = "https://${local.api_domain}"
      grafana = var.enable_monitoring ? "https://${local.grafana_domain}" : "disabled"
    }
  }
}
