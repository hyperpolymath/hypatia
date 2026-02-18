# SPDX-License-Identifier: PMPL-1.0-or-later
# cicd-hyper-a Networking Module
#
# This module creates VPC/VNet, subnets, NAT gateways, and load balancers
# for AWS, GCP, or Azure.

# =============================================================================
# Local Values
# =============================================================================

locals {
  network_name = "${var.project_name}-${var.environment}"

  # Subnet CIDR calculations
  # Split VPC CIDR into public and private subnets
  public_subnet_cidrs = [
    for i, az in var.availability_zones :
    cidrsubnet(var.vpc_cidr, 4, i)
  ]

  private_subnet_cidrs = [
    for i, az in var.availability_zones :
    cidrsubnet(var.vpc_cidr, 4, i + length(var.availability_zones))
  ]

  # Common tags for networking resources
  network_tags = merge(var.tags, {
    Component = "networking"
  })
}

# =============================================================================
# AWS Networking
# =============================================================================

# VPC
resource "aws_vpc" "main" {
  count = var.cloud_provider == "aws" ? 1 : 0

  cidr_block           = var.vpc_cidr
  enable_dns_hostnames = true
  enable_dns_support   = true

  tags = merge(local.network_tags, {
    Name = local.network_name
  })
}

# Internet Gateway
resource "aws_internet_gateway" "main" {
  count = var.cloud_provider == "aws" ? 1 : 0

  vpc_id = aws_vpc.main[0].id

  tags = merge(local.network_tags, {
    Name = "${local.network_name}-igw"
  })
}

# Availability Zones Data Source
data "aws_availability_zones" "available" {
  count = var.cloud_provider == "aws" ? 1 : 0

  state = "available"
}

locals {
  aws_azs = var.cloud_provider == "aws" ? (
    length(var.availability_zones) > 0 ? var.availability_zones :
    slice(data.aws_availability_zones.available[0].names, 0, 3)
  ) : []
}

# Public Subnets
resource "aws_subnet" "public" {
  count = var.cloud_provider == "aws" ? length(local.aws_azs) : 0

  vpc_id                  = aws_vpc.main[0].id
  cidr_block              = cidrsubnet(var.vpc_cidr, 4, count.index)
  availability_zone       = local.aws_azs[count.index]
  map_public_ip_on_launch = true

  tags = merge(local.network_tags, {
    Name                                           = "${local.network_name}-public-${count.index + 1}"
    "kubernetes.io/role/elb"                       = "1"
    "kubernetes.io/cluster/${local.network_name}" = "shared"
  })
}

# Private Subnets
resource "aws_subnet" "private" {
  count = var.cloud_provider == "aws" ? length(local.aws_azs) : 0

  vpc_id            = aws_vpc.main[0].id
  cidr_block        = cidrsubnet(var.vpc_cidr, 4, count.index + length(local.aws_azs))
  availability_zone = local.aws_azs[count.index]

  tags = merge(local.network_tags, {
    Name                                           = "${local.network_name}-private-${count.index + 1}"
    "kubernetes.io/role/internal-elb"              = "1"
    "kubernetes.io/cluster/${local.network_name}" = "shared"
  })
}

# Elastic IPs for NAT Gateways
resource "aws_eip" "nat" {
  count = var.cloud_provider == "aws" ? length(local.aws_azs) : 0

  domain = "vpc"

  tags = merge(local.network_tags, {
    Name = "${local.network_name}-nat-${count.index + 1}"
  })

  depends_on = [aws_internet_gateway.main]
}

# NAT Gateways
resource "aws_nat_gateway" "main" {
  count = var.cloud_provider == "aws" ? length(local.aws_azs) : 0

  allocation_id = aws_eip.nat[count.index].id
  subnet_id     = aws_subnet.public[count.index].id

  tags = merge(local.network_tags, {
    Name = "${local.network_name}-nat-${count.index + 1}"
  })

  depends_on = [aws_internet_gateway.main]
}

# Public Route Table
resource "aws_route_table" "public" {
  count = var.cloud_provider == "aws" ? 1 : 0

  vpc_id = aws_vpc.main[0].id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.main[0].id
  }

  tags = merge(local.network_tags, {
    Name = "${local.network_name}-public"
  })
}

# Public Route Table Associations
resource "aws_route_table_association" "public" {
  count = var.cloud_provider == "aws" ? length(local.aws_azs) : 0

  subnet_id      = aws_subnet.public[count.index].id
  route_table_id = aws_route_table.public[0].id
}

# Private Route Tables
resource "aws_route_table" "private" {
  count = var.cloud_provider == "aws" ? length(local.aws_azs) : 0

  vpc_id = aws_vpc.main[0].id

  route {
    cidr_block     = "0.0.0.0/0"
    nat_gateway_id = aws_nat_gateway.main[count.index].id
  }

  tags = merge(local.network_tags, {
    Name = "${local.network_name}-private-${count.index + 1}"
  })
}

# Private Route Table Associations
resource "aws_route_table_association" "private" {
  count = var.cloud_provider == "aws" ? length(local.aws_azs) : 0

  subnet_id      = aws_subnet.private[count.index].id
  route_table_id = aws_route_table.private[count.index].id
}

# VPC Endpoints for AWS Services
resource "aws_vpc_endpoint" "s3" {
  count = var.cloud_provider == "aws" ? 1 : 0

  vpc_id       = aws_vpc.main[0].id
  service_name = "com.amazonaws.${var.aws_region}.s3"

  tags = merge(local.network_tags, {
    Name = "${local.network_name}-s3-endpoint"
  })
}

resource "aws_vpc_endpoint" "ecr_api" {
  count = var.cloud_provider == "aws" ? 1 : 0

  vpc_id              = aws_vpc.main[0].id
  service_name        = "com.amazonaws.${var.aws_region}.ecr.api"
  vpc_endpoint_type   = "Interface"
  subnet_ids          = aws_subnet.private[*].id
  security_group_ids  = [aws_security_group.vpc_endpoints[0].id]
  private_dns_enabled = true

  tags = merge(local.network_tags, {
    Name = "${local.network_name}-ecr-api-endpoint"
  })
}

resource "aws_vpc_endpoint" "ecr_dkr" {
  count = var.cloud_provider == "aws" ? 1 : 0

  vpc_id              = aws_vpc.main[0].id
  service_name        = "com.amazonaws.${var.aws_region}.ecr.dkr"
  vpc_endpoint_type   = "Interface"
  subnet_ids          = aws_subnet.private[*].id
  security_group_ids  = [aws_security_group.vpc_endpoints[0].id]
  private_dns_enabled = true

  tags = merge(local.network_tags, {
    Name = "${local.network_name}-ecr-dkr-endpoint"
  })
}

# Security Group for VPC Endpoints
resource "aws_security_group" "vpc_endpoints" {
  count = var.cloud_provider == "aws" ? 1 : 0

  name        = "${local.network_name}-vpc-endpoints"
  description = "Security group for VPC endpoints"
  vpc_id      = aws_vpc.main[0].id

  ingress {
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = [var.vpc_cidr]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = merge(local.network_tags, {
    Name = "${local.network_name}-vpc-endpoints"
  })
}

# =============================================================================
# GCP Networking
# =============================================================================

# VPC Network
resource "google_compute_network" "main" {
  count = var.cloud_provider == "gcp" ? 1 : 0

  name                    = local.network_name
  project                 = var.gcp_project_id
  auto_create_subnetworks = false
  routing_mode            = "REGIONAL"
}

# Subnetwork
resource "google_compute_subnetwork" "main" {
  count = var.cloud_provider == "gcp" ? 1 : 0

  name          = "${local.network_name}-subnet"
  project       = var.gcp_project_id
  region        = var.gcp_region
  network       = google_compute_network.main[0].id
  ip_cidr_range = var.vpc_cidr

  secondary_ip_range {
    range_name    = "pods"
    ip_cidr_range = "10.1.0.0/16"
  }

  secondary_ip_range {
    range_name    = "services"
    ip_cidr_range = "10.2.0.0/20"
  }

  private_ip_google_access = true
}

# Cloud Router
resource "google_compute_router" "main" {
  count = var.cloud_provider == "gcp" ? 1 : 0

  name    = "${local.network_name}-router"
  project = var.gcp_project_id
  region  = var.gcp_region
  network = google_compute_network.main[0].id
}

# Cloud NAT
resource "google_compute_router_nat" "main" {
  count = var.cloud_provider == "gcp" ? 1 : 0

  name                               = "${local.network_name}-nat"
  project                            = var.gcp_project_id
  router                             = google_compute_router.main[0].name
  region                             = var.gcp_region
  nat_ip_allocate_option             = "AUTO_ONLY"
  source_subnetwork_ip_ranges_to_nat = "ALL_SUBNETWORKS_ALL_IP_RANGES"

  log_config {
    enable = true
    filter = "ERRORS_ONLY"
  }
}

# Firewall Rules
resource "google_compute_firewall" "allow_internal" {
  count = var.cloud_provider == "gcp" ? 1 : 0

  name    = "${local.network_name}-allow-internal"
  project = var.gcp_project_id
  network = google_compute_network.main[0].name

  allow {
    protocol = "icmp"
  }

  allow {
    protocol = "tcp"
    ports    = ["0-65535"]
  }

  allow {
    protocol = "udp"
    ports    = ["0-65535"]
  }

  source_ranges = [var.vpc_cidr]
}

resource "google_compute_firewall" "allow_health_checks" {
  count = var.cloud_provider == "gcp" ? 1 : 0

  name    = "${local.network_name}-allow-health-checks"
  project = var.gcp_project_id
  network = google_compute_network.main[0].name

  allow {
    protocol = "tcp"
  }

  source_ranges = ["130.211.0.0/22", "35.191.0.0/16"]
}

# =============================================================================
# Azure Networking
# =============================================================================

# Resource Group
resource "azurerm_resource_group" "main" {
  count = var.cloud_provider == "azure" ? 1 : 0

  name     = var.azure_resource_group_name
  location = var.azure_location

  tags = local.network_tags
}

# Virtual Network
resource "azurerm_virtual_network" "main" {
  count = var.cloud_provider == "azure" ? 1 : 0

  name                = local.network_name
  location            = azurerm_resource_group.main[0].location
  resource_group_name = azurerm_resource_group.main[0].name
  address_space       = [var.vpc_cidr]

  tags = local.network_tags
}

# Subnets
resource "azurerm_subnet" "nodes" {
  count = var.cloud_provider == "azure" ? 1 : 0

  name                 = "${local.network_name}-nodes"
  resource_group_name  = azurerm_resource_group.main[0].name
  virtual_network_name = azurerm_virtual_network.main[0].name
  address_prefixes     = [cidrsubnet(var.vpc_cidr, 2, 0)]
}

resource "azurerm_subnet" "pods" {
  count = var.cloud_provider == "azure" ? 1 : 0

  name                 = "${local.network_name}-pods"
  resource_group_name  = azurerm_resource_group.main[0].name
  virtual_network_name = azurerm_virtual_network.main[0].name
  address_prefixes     = [cidrsubnet(var.vpc_cidr, 2, 1)]

  delegation {
    name = "aks-delegation"
    service_delegation {
      name    = "Microsoft.ContainerService/managedClusters"
      actions = ["Microsoft.Network/virtualNetworks/subnets/join/action"]
    }
  }
}

resource "azurerm_subnet" "services" {
  count = var.cloud_provider == "azure" ? 1 : 0

  name                 = "${local.network_name}-services"
  resource_group_name  = azurerm_resource_group.main[0].name
  virtual_network_name = azurerm_virtual_network.main[0].name
  address_prefixes     = [cidrsubnet(var.vpc_cidr, 2, 2)]
}

# Network Security Group
resource "azurerm_network_security_group" "main" {
  count = var.cloud_provider == "azure" ? 1 : 0

  name                = "${local.network_name}-nsg"
  location            = azurerm_resource_group.main[0].location
  resource_group_name = azurerm_resource_group.main[0].name

  security_rule {
    name                       = "AllowHTTPS"
    priority                   = 100
    direction                  = "Inbound"
    access                     = "Allow"
    protocol                   = "Tcp"
    source_port_range          = "*"
    destination_port_range     = "443"
    source_address_prefix      = "*"
    destination_address_prefix = "*"
  }

  security_rule {
    name                       = "AllowHTTP"
    priority                   = 101
    direction                  = "Inbound"
    access                     = "Allow"
    protocol                   = "Tcp"
    source_port_range          = "*"
    destination_port_range     = "80"
    source_address_prefix      = "*"
    destination_address_prefix = "*"
  }

  tags = local.network_tags
}

# NAT Gateway
resource "azurerm_public_ip" "nat" {
  count = var.cloud_provider == "azure" ? 1 : 0

  name                = "${local.network_name}-nat-ip"
  location            = azurerm_resource_group.main[0].location
  resource_group_name = azurerm_resource_group.main[0].name
  allocation_method   = "Static"
  sku                 = "Standard"

  tags = local.network_tags
}

resource "azurerm_nat_gateway" "main" {
  count = var.cloud_provider == "azure" ? 1 : 0

  name                = "${local.network_name}-nat"
  location            = azurerm_resource_group.main[0].location
  resource_group_name = azurerm_resource_group.main[0].name
  sku_name            = "Standard"

  tags = local.network_tags
}

resource "azurerm_nat_gateway_public_ip_association" "main" {
  count = var.cloud_provider == "azure" ? 1 : 0

  nat_gateway_id       = azurerm_nat_gateway.main[0].id
  public_ip_address_id = azurerm_public_ip.nat[0].id
}

resource "azurerm_subnet_nat_gateway_association" "nodes" {
  count = var.cloud_provider == "azure" ? 1 : 0

  subnet_id      = azurerm_subnet.nodes[0].id
  nat_gateway_id = azurerm_nat_gateway.main[0].id
}
