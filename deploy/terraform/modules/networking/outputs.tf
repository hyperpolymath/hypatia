# SPDX-License-Identifier: PMPL-1.0-or-later
# cicd-hyper-a Networking Module Outputs

output "vpc_id" {
  description = "VPC/VNet ID"
  value = (
    var.cloud_provider == "aws" ? aws_vpc.main[0].id :
    var.cloud_provider == "gcp" ? google_compute_network.main[0].id :
    var.cloud_provider == "azure" ? azurerm_virtual_network.main[0].id :
    ""
  )
}

output "vpc_cidr" {
  description = "VPC CIDR block"
  value       = var.vpc_cidr
}

output "public_subnet_ids" {
  description = "IDs of public subnets"
  value = (
    var.cloud_provider == "aws" ? aws_subnet.public[*].id :
    var.cloud_provider == "gcp" ? [google_compute_subnetwork.main[0].id] :
    var.cloud_provider == "azure" ? [azurerm_subnet.services[0].id] :
    []
  )
}

output "private_subnet_ids" {
  description = "IDs of private subnets"
  value = (
    var.cloud_provider == "aws" ? aws_subnet.private[*].id :
    var.cloud_provider == "gcp" ? [google_compute_subnetwork.main[0].id] :
    var.cloud_provider == "azure" ? [azurerm_subnet.nodes[0].id] :
    []
  )
}

output "nat_gateway_ips" {
  description = "NAT Gateway public IPs"
  value = (
    var.cloud_provider == "aws" ? aws_eip.nat[*].public_ip :
    var.cloud_provider == "gcp" ? [] :  # GCP NAT uses auto-allocated IPs
    var.cloud_provider == "azure" ? [azurerm_public_ip.nat[0].ip_address] :
    []
  )
}

output "load_balancer_dns" {
  description = "Load balancer DNS name (if created)"
  value       = ""  # Load balancer is created by Kubernetes Ingress Controller
}

output "network_name" {
  description = "Network name"
  value = (
    var.cloud_provider == "aws" ? aws_vpc.main[0].tags["Name"] :
    var.cloud_provider == "gcp" ? google_compute_network.main[0].name :
    var.cloud_provider == "azure" ? azurerm_virtual_network.main[0].name :
    ""
  )
}

output "availability_zones" {
  description = "Availability zones used"
  value = (
    var.cloud_provider == "aws" ? local.aws_azs :
    var.cloud_provider == "gcp" ? [var.gcp_region] :
    var.cloud_provider == "azure" ? [var.azure_location] :
    []
  )
}
