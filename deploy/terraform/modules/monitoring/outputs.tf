# SPDX-License-Identifier: PMPL-1.0-or-later
# cicd-hyper-a Monitoring Module Outputs

output "prometheus_endpoint" {
  description = "Prometheus endpoint"
  value       = "http://kube-prometheus-stack-prometheus.${var.kubernetes_namespace}.svc.cluster.local:9090"
}

output "grafana_endpoint" {
  description = "Grafana endpoint"
  value       = "http://kube-prometheus-stack-grafana.${var.kubernetes_namespace}.svc.cluster.local:80"
}

output "grafana_url" {
  description = "External Grafana URL"
  value       = var.enable_monitoring_ingress ? "https://${var.grafana_domain}" : ""
}

output "alertmanager_endpoint" {
  description = "Alertmanager endpoint"
  value       = var.enable_alertmanager ? "http://kube-prometheus-stack-alertmanager.${var.kubernetes_namespace}.svc.cluster.local:9093" : ""
}

output "helm_release_name" {
  description = "Name of the Helm release"
  value       = helm_release.kube_prometheus_stack.name
}

output "helm_release_version" {
  description = "Version of the Helm chart"
  value       = helm_release.kube_prometheus_stack.version
}
