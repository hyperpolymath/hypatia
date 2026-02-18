# SPDX-License-Identifier: PMPL-1.0-or-later
# cicd-hyper-a Database Module Outputs

output "arangodb_endpoint" {
  description = "ArangoDB endpoint"
  value       = var.use_managed_database ? "" : "arangodb.${var.kubernetes_namespace}.svc.cluster.local:8529"
}

output "arangodb_service_name" {
  description = "ArangoDB Kubernetes service name"
  value       = var.use_managed_database ? "" : "arangodb"
}

output "arangodb_secret_name" {
  description = "Name of the Kubernetes secret containing ArangoDB credentials"
  value       = var.use_managed_database ? "" : kubernetes_secret.arangodb[0].metadata[0].name
}

output "arangodb_headless_service" {
  description = "ArangoDB headless service for StatefulSet"
  value       = var.use_managed_database ? "" : "${var.project_name}-arangodb-headless"
}
