# SPDX-License-Identifier: PMPL-1.0-or-later
# cicd-hyper-a Database Module Variables

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

variable "use_managed_database" {
  description = "Use cloud-managed database service"
  type        = bool
  default     = false
}

variable "arangodb_version" {
  description = "ArangoDB version"
  type        = string
  default     = "3.11"
}

variable "arangodb_replica_count" {
  description = "Number of ArangoDB replicas"
  type        = number
  default     = 3
}

variable "arangodb_storage_size" {
  description = "Storage size for ArangoDB"
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
  description = "Root password for ArangoDB"
  type        = string
  sensitive   = true
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

variable "tags" {
  description = "Tags to apply to resources"
  type        = map(string)
  default     = {}
}
