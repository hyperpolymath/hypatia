# SPDX-License-Identifier: PLMP-1.0-or-later
# cicd-hyper-a Kubernetes Module Outputs

output "cluster_endpoint" {
  description = "Kubernetes cluster API endpoint"
  value = (
    var.cloud_provider == "aws" ? aws_eks_cluster.main[0].endpoint :
    var.cloud_provider == "gcp" ? "https://${google_container_cluster.main[0].endpoint}" :
    var.cloud_provider == "azure" ? azurerm_kubernetes_cluster.main[0].kube_config[0].host :
    ""
  )
}

output "cluster_ca_certificate" {
  description = "Base64 encoded cluster CA certificate"
  value = (
    var.cloud_provider == "aws" ? aws_eks_cluster.main[0].certificate_authority[0].data :
    var.cloud_provider == "gcp" ? google_container_cluster.main[0].master_auth[0].cluster_ca_certificate :
    var.cloud_provider == "azure" ? azurerm_kubernetes_cluster.main[0].kube_config[0].cluster_ca_certificate :
    ""
  )
  sensitive = true
}

output "cluster_token" {
  description = "Cluster authentication token (GCP/Azure only)"
  value = (
    var.cloud_provider == "gcp" ? data.google_client_config.current[0].access_token :
    var.cloud_provider == "azure" ? azurerm_kubernetes_cluster.main[0].kube_config[0].password :
    ""
  )
  sensitive = true
}

output "cluster_name" {
  description = "Name of the Kubernetes cluster"
  value = (
    var.cloud_provider == "aws" ? aws_eks_cluster.main[0].name :
    var.cloud_provider == "gcp" ? google_container_cluster.main[0].name :
    var.cloud_provider == "azure" ? azurerm_kubernetes_cluster.main[0].name :
    ""
  )
}

output "cluster_security_group_id" {
  description = "Security group ID for the cluster"
  value = (
    var.cloud_provider == "aws" ? aws_security_group.eks_cluster[0].id :
    var.cloud_provider == "gcp" ? "" :
    var.cloud_provider == "azure" ? "" :
    ""
  )
}

output "node_security_group_id" {
  description = "Security group ID for the nodes"
  value = (
    var.cloud_provider == "aws" ? aws_eks_cluster.main[0].vpc_config[0].cluster_security_group_id :
    var.cloud_provider == "gcp" ? "" :
    var.cloud_provider == "azure" ? "" :
    ""
  )
}

output "cluster_oidc_issuer_url" {
  description = "OIDC issuer URL for the cluster (for IAM integration)"
  value = (
    var.cloud_provider == "aws" ? aws_eks_cluster.main[0].identity[0].oidc[0].issuer :
    var.cloud_provider == "gcp" ? "" :
    var.cloud_provider == "azure" ? azurerm_kubernetes_cluster.main[0].oidc_issuer_url :
    ""
  )
}

# Data source for GCP client config (needed for access token)
data "google_client_config" "current" {
  count = var.cloud_provider == "gcp" ? 1 : 0
}
