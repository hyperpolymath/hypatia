# SPDX-License-Identifier: AGPL-3.0-or-later
# cicd-hyper-a Kubernetes Module Variables

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

variable "cluster_name" {
  description = "Name of the Kubernetes cluster"
  type        = string
}

variable "kubernetes_version" {
  description = "Kubernetes version"
  type        = string
}

variable "node_instance_types" {
  description = "Instance types for nodes"
  type        = list(string)
}

variable "min_node_count" {
  description = "Minimum number of nodes"
  type        = number
}

variable "max_node_count" {
  description = "Maximum number of nodes"
  type        = number
}

variable "desired_node_count" {
  description = "Desired number of nodes"
  type        = number
}

variable "node_disk_size_gb" {
  description = "Disk size for nodes in GB"
  type        = number
}

variable "vpc_id" {
  description = "VPC/Network ID"
  type        = string
}

variable "subnet_ids" {
  description = "Subnet IDs for the cluster"
  type        = list(string)
}

# AWS-specific
variable "aws_region" {
  description = "AWS region"
  type        = string
  default     = ""
}

# GCP-specific
variable "gcp_project_id" {
  description = "GCP project ID"
  type        = string
  default     = ""
}

variable "gcp_region" {
  description = "GCP region"
  type        = string
  default     = ""
}

variable "gcp_zone" {
  description = "GCP zone"
  type        = string
  default     = ""
}

# Azure-specific
variable "azure_resource_group_name" {
  description = "Azure resource group name"
  type        = string
  default     = ""
}

variable "azure_location" {
  description = "Azure location"
  type        = string
  default     = ""
}

variable "tags" {
  description = "Tags to apply to resources"
  type        = map(string)
  default     = {}
}
