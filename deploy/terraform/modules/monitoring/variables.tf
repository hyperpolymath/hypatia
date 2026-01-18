# SPDX-License-Identifier: PLMP-1.0-or-later
# cicd-hyper-a Monitoring Module Variables

variable "environment" {
  description = "Deployment environment"
  type        = string
}

variable "project_name" {
  description = "Project name"
  type        = string
}

variable "kubernetes_namespace" {
  description = "Kubernetes namespace"
  type        = string
}

# Prometheus configuration
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

# Grafana configuration
variable "grafana_admin_password" {
  description = "Admin password for Grafana"
  type        = string
  sensitive   = true
}

variable "grafana_storage_size" {
  description = "Storage size for Grafana"
  type        = string
  default     = "10Gi"
}

variable "grafana_domain" {
  description = "Domain for Grafana ingress"
  type        = string
}

# Alertmanager configuration
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

# Ingress configuration
variable "enable_monitoring_ingress" {
  description = "Whether to create ingress for monitoring services"
  type        = bool
  default     = true
}

variable "ingress_class_name" {
  description = "Ingress class name"
  type        = string
  default     = "nginx"
}

variable "cluster_issuer" {
  description = "cert-manager cluster issuer"
  type        = string
  default     = "letsencrypt-prod"
}

variable "tags" {
  description = "Tags to apply to resources"
  type        = map(string)
  default     = {}
}
