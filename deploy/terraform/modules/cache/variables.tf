# SPDX-License-Identifier: PLMP-1.0-or-later
# cicd-hyper-a Cache Module Variables

variable "cloud_provider" {
  description = "Cloud provider (aws, gcp, azure)"
  type        = string
}

variable "environment" {
  description = "Deployment environment"
  type        = string
}

variable "project_name" {
  description = "Project name"
  type        = string
}

variable "use_managed_cache" {
  description = "Use cloud-managed cache service"
  type        = bool
  default     = false
}

variable "cache_engine" {
  description = "Cache engine (dragonfly or redis)"
  type        = string
  default     = "dragonfly"
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
  description = "Storage size for cache"
  type        = string
  default     = "20Gi"
}

variable "cache_memory_limit" {
  description = "Memory limit for cache"
  type        = string
  default     = "4Gi"
}

variable "dragonfly_version" {
  description = "Dragonfly version"
  type        = string
  default     = "v1.14.1"
}

variable "dragonfly_proactor_threads" {
  description = "Number of Dragonfly proactor threads"
  type        = number
  default     = 4
}

variable "cache_password" {
  description = "Password for cache authentication"
  type        = string
  sensitive   = true
}

variable "vpc_id" {
  description = "VPC ID for managed cache"
  type        = string
  default     = ""
}

variable "subnet_ids" {
  description = "Subnet IDs for managed cache"
  type        = list(string)
  default     = []
}

variable "kubernetes_namespace" {
  description = "Kubernetes namespace"
  type        = string
}

variable "storage_class" {
  description = "Storage class for persistent volumes"
  type        = string
  default     = ""
}

# GCP-specific
variable "gcp_region" {
  description = "GCP region"
  type        = string
  default     = ""
}

# Azure-specific
variable "azure_location" {
  description = "Azure location"
  type        = string
  default     = ""
}

variable "azure_resource_group_name" {
  description = "Azure resource group name"
  type        = string
  default     = ""
}

variable "tags" {
  description = "Tags to apply to resources"
  type        = map(string)
  default     = {}
}
