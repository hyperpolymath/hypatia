# SPDX-License-Identifier: PLMP-1.0-or-later
# cicd-hyper-a Kubernetes Module
#
# This module provisions Kubernetes clusters on AWS (EKS), GCP (GKE), or Azure (AKS).

# =============================================================================
# Local Values
# =============================================================================

locals {
  # Standardize instance types across providers
  node_config = {
    aws = {
      instance_types = var.node_instance_types
      disk_size      = var.node_disk_size_gb
    }
    gcp = {
      machine_type = length(var.node_instance_types) > 0 ? (
        # Map AWS instance types to GCP equivalents
        replace(replace(var.node_instance_types[0], "m6i.", "n2-standard-"), "m6a.", "n2-standard-")
      ) : "n2-standard-4"
      disk_size = var.node_disk_size_gb
    }
    azure = {
      vm_size   = length(var.node_instance_types) > 0 ? "Standard_D4s_v3" : "Standard_D4s_v3"
      disk_size = var.node_disk_size_gb
    }
  }
}

# =============================================================================
# AWS EKS Cluster
# =============================================================================

# EKS Cluster IAM Role
resource "aws_iam_role" "eks_cluster" {
  count = var.cloud_provider == "aws" ? 1 : 0

  name = "${var.cluster_name}-eks-cluster-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Principal = {
          Service = "eks.amazonaws.com"
        }
      }
    ]
  })

  tags = var.tags
}

resource "aws_iam_role_policy_attachment" "eks_cluster_policy" {
  count = var.cloud_provider == "aws" ? 1 : 0

  policy_arn = "arn:aws:iam::aws:policy/AmazonEKSClusterPolicy"
  role       = aws_iam_role.eks_cluster[0].name
}

resource "aws_iam_role_policy_attachment" "eks_vpc_resource_controller" {
  count = var.cloud_provider == "aws" ? 1 : 0

  policy_arn = "arn:aws:iam::aws:policy/AmazonEKSVPCResourceController"
  role       = aws_iam_role.eks_cluster[0].name
}

# EKS Cluster
resource "aws_eks_cluster" "main" {
  count = var.cloud_provider == "aws" ? 1 : 0

  name     = var.cluster_name
  role_arn = aws_iam_role.eks_cluster[0].arn
  version  = var.kubernetes_version

  vpc_config {
    subnet_ids              = var.subnet_ids
    endpoint_private_access = true
    endpoint_public_access  = true
    security_group_ids      = [aws_security_group.eks_cluster[0].id]
  }

  enabled_cluster_log_types = [
    "api",
    "audit",
    "authenticator",
    "controllerManager",
    "scheduler"
  ]

  encryption_config {
    provider {
      key_arn = aws_kms_key.eks[0].arn
    }
    resources = ["secrets"]
  }

  tags = var.tags

  depends_on = [
    aws_iam_role_policy_attachment.eks_cluster_policy,
    aws_iam_role_policy_attachment.eks_vpc_resource_controller
  ]
}

# EKS Node Group IAM Role
resource "aws_iam_role" "eks_nodes" {
  count = var.cloud_provider == "aws" ? 1 : 0

  name = "${var.cluster_name}-eks-node-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Principal = {
          Service = "ec2.amazonaws.com"
        }
      }
    ]
  })

  tags = var.tags
}

resource "aws_iam_role_policy_attachment" "eks_worker_node_policy" {
  count = var.cloud_provider == "aws" ? 1 : 0

  policy_arn = "arn:aws:iam::aws:policy/AmazonEKSWorkerNodePolicy"
  role       = aws_iam_role.eks_nodes[0].name
}

resource "aws_iam_role_policy_attachment" "eks_cni_policy" {
  count = var.cloud_provider == "aws" ? 1 : 0

  policy_arn = "arn:aws:iam::aws:policy/AmazonEKS_CNI_Policy"
  role       = aws_iam_role.eks_nodes[0].name
}

resource "aws_iam_role_policy_attachment" "eks_container_registry" {
  count = var.cloud_provider == "aws" ? 1 : 0

  policy_arn = "arn:aws:iam::aws:policy/AmazonEC2ContainerRegistryReadOnly"
  role       = aws_iam_role.eks_nodes[0].name
}

# EKS Node Group
resource "aws_eks_node_group" "main" {
  count = var.cloud_provider == "aws" ? 1 : 0

  cluster_name    = aws_eks_cluster.main[0].name
  node_group_name = "${var.cluster_name}-nodes"
  node_role_arn   = aws_iam_role.eks_nodes[0].arn
  subnet_ids      = var.subnet_ids

  instance_types = local.node_config.aws.instance_types

  scaling_config {
    min_size     = var.min_node_count
    max_size     = var.max_node_count
    desired_size = var.desired_node_count
  }

  update_config {
    max_unavailable = 1
  }

  disk_size = local.node_config.aws.disk_size

  labels = {
    Environment = var.environment
    Project     = var.project_name
  }

  tags = var.tags

  depends_on = [
    aws_iam_role_policy_attachment.eks_worker_node_policy,
    aws_iam_role_policy_attachment.eks_cni_policy,
    aws_iam_role_policy_attachment.eks_container_registry
  ]
}

# EKS Security Group
resource "aws_security_group" "eks_cluster" {
  count = var.cloud_provider == "aws" ? 1 : 0

  name        = "${var.cluster_name}-eks-cluster-sg"
  description = "Security group for EKS cluster"
  vpc_id      = var.vpc_id

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = merge(var.tags, {
    Name = "${var.cluster_name}-eks-cluster-sg"
  })
}

# KMS Key for EKS encryption
resource "aws_kms_key" "eks" {
  count = var.cloud_provider == "aws" ? 1 : 0

  description             = "KMS key for EKS cluster ${var.cluster_name}"
  deletion_window_in_days = 7
  enable_key_rotation     = true

  tags = var.tags
}

resource "aws_kms_alias" "eks" {
  count = var.cloud_provider == "aws" ? 1 : 0

  name          = "alias/${var.cluster_name}-eks"
  target_key_id = aws_kms_key.eks[0].key_id
}

# =============================================================================
# GCP GKE Cluster
# =============================================================================

resource "google_container_cluster" "main" {
  count = var.cloud_provider == "gcp" ? 1 : 0

  name     = var.cluster_name
  location = var.gcp_region
  project  = var.gcp_project_id

  # We can't create a cluster with no node pool defined, but we want to only use
  # separately managed node pools. So we create the smallest possible default
  # node pool and immediately delete it.
  remove_default_node_pool = true
  initial_node_count       = 1

  min_master_version = var.kubernetes_version

  network    = var.vpc_id
  subnetwork = length(var.subnet_ids) > 0 ? var.subnet_ids[0] : null

  # Enable Workload Identity
  workload_identity_config {
    workload_pool = "${var.gcp_project_id}.svc.id.goog"
  }

  # Enable private cluster
  private_cluster_config {
    enable_private_nodes    = true
    enable_private_endpoint = false
    master_ipv4_cidr_block  = "172.16.0.0/28"
  }

  # Network policy
  network_policy {
    enabled  = true
    provider = "CALICO"
  }

  # Binary Authorization
  binary_authorization {
    evaluation_mode = "PROJECT_SINGLETON_POLICY_ENFORCE"
  }

  # Addons
  addons_config {
    http_load_balancing {
      disabled = false
    }
    horizontal_pod_autoscaling {
      disabled = false
    }
    network_policy_config {
      disabled = false
    }
    gcs_fuse_csi_driver_config {
      enabled = true
    }
  }

  # Maintenance window
  maintenance_policy {
    daily_maintenance_window {
      start_time = "03:00"
    }
  }

  # Enable shielded nodes
  release_channel {
    channel = "REGULAR"
  }

  resource_labels = var.tags
}

# GKE Node Pool
resource "google_container_node_pool" "main" {
  count = var.cloud_provider == "gcp" ? 1 : 0

  name       = "${var.cluster_name}-node-pool"
  location   = var.gcp_region
  project    = var.gcp_project_id
  cluster    = google_container_cluster.main[0].name

  initial_node_count = var.desired_node_count

  autoscaling {
    min_node_count = var.min_node_count
    max_node_count = var.max_node_count
  }

  management {
    auto_repair  = true
    auto_upgrade = true
  }

  node_config {
    machine_type = local.node_config.gcp.machine_type
    disk_size_gb = local.node_config.gcp.disk_size
    disk_type    = "pd-ssd"

    oauth_scopes = [
      "https://www.googleapis.com/auth/cloud-platform"
    ]

    labels = {
      environment = var.environment
      project     = var.project_name
    }

    # Shielded instance config
    shielded_instance_config {
      enable_secure_boot          = true
      enable_integrity_monitoring = true
    }

    # Workload metadata
    workload_metadata_config {
      mode = "GKE_METADATA"
    }
  }

  upgrade_settings {
    max_surge       = 1
    max_unavailable = 0
  }
}

# =============================================================================
# Azure AKS Cluster
# =============================================================================

resource "azurerm_kubernetes_cluster" "main" {
  count = var.cloud_provider == "azure" ? 1 : 0

  name                = var.cluster_name
  location            = var.azure_location
  resource_group_name = var.azure_resource_group_name
  dns_prefix          = var.cluster_name
  kubernetes_version  = var.kubernetes_version

  default_node_pool {
    name                = "default"
    node_count          = var.desired_node_count
    vm_size             = local.node_config.azure.vm_size
    os_disk_size_gb     = local.node_config.azure.disk_size
    vnet_subnet_id      = length(var.subnet_ids) > 0 ? var.subnet_ids[0] : null
    enable_auto_scaling = true
    min_count           = var.min_node_count
    max_count           = var.max_node_count

    node_labels = {
      environment = var.environment
      project     = var.project_name
    }
  }

  identity {
    type = "SystemAssigned"
  }

  network_profile {
    network_plugin    = "azure"
    network_policy    = "calico"
    load_balancer_sku = "standard"
    outbound_type     = "loadBalancer"
  }

  # Azure AD integration
  azure_active_directory_role_based_access_control {
    managed                = true
    azure_rbac_enabled     = true
  }

  # Enable Azure Monitor
  oms_agent {
    log_analytics_workspace_id = azurerm_log_analytics_workspace.aks[0].id
  }

  # Enable Azure Defender
  microsoft_defender {
    log_analytics_workspace_id = azurerm_log_analytics_workspace.aks[0].id
  }

  # Auto-upgrade
  automatic_channel_upgrade = "patch"

  # Maintenance window
  maintenance_window {
    allowed {
      day   = "Sunday"
      hours = [0, 1, 2, 3, 4, 5]
    }
  }

  tags = var.tags
}

# Azure Log Analytics Workspace for AKS
resource "azurerm_log_analytics_workspace" "aks" {
  count = var.cloud_provider == "azure" ? 1 : 0

  name                = "${var.cluster_name}-logs"
  location            = var.azure_location
  resource_group_name = var.azure_resource_group_name
  sku                 = "PerGB2018"
  retention_in_days   = 30

  tags = var.tags
}

# =============================================================================
# Common Add-ons (deployed via Helm after cluster creation)
# =============================================================================

# Note: The following add-ons should be installed via Helm after the cluster
# is created. They are commented out here but can be uncommented if you want
# Terraform to manage them:
#
# - nginx-ingress-controller
# - cert-manager
# - external-dns
# - cluster-autoscaler (for AWS)
# - metrics-server
