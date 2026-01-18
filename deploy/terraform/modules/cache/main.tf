# SPDX-License-Identifier: AGPL-3.0-or-later
# cicd-hyper-a Cache Module (Dragonfly/Redis)
#
# This module deploys a cache layer using either:
# - Dragonfly (default, Redis-compatible with better performance)
# - Redis (traditional)
# - Managed services (ElastiCache, Memorystore, Azure Cache)

# =============================================================================
# Local Values
# =============================================================================

locals {
  cache_name = "${var.project_name}-${var.cache_engine}"

  # Resource presets based on environment
  resource_presets = {
    dev = {
      cpu_request    = "100m"
      cpu_limit      = "500m"
      memory_request = "512Mi"
      memory_limit   = "1Gi"
      storage_size   = "5Gi"
      replicas       = 1
    }
    staging = {
      cpu_request    = "250m"
      cpu_limit      = "1"
      memory_request = "1Gi"
      memory_limit   = "2Gi"
      storage_size   = "10Gi"
      replicas       = 1
    }
    production = {
      cpu_request    = "500m"
      cpu_limit      = "2"
      memory_request = "2Gi"
      memory_limit   = var.cache_memory_limit
      storage_size   = var.cache_storage_size
      replicas       = var.cache_replica_count
    }
  }

  resources = lookup(local.resource_presets, var.environment, local.resource_presets.production)

  # Dragonfly image
  dragonfly_image = "docker.dragonflydb.io/dragonflydb/dragonfly:${var.dragonfly_version}"
  redis_image     = "redis:7-alpine"
}

# =============================================================================
# Kubernetes Secret for Cache Credentials
# =============================================================================

resource "kubernetes_secret" "cache" {
  count = !var.use_managed_cache ? 1 : 0

  metadata {
    name      = "${local.cache_name}-credentials"
    namespace = var.kubernetes_namespace

    labels = {
      "app.kubernetes.io/name"      = var.cache_engine
      "app.kubernetes.io/component" = "cache"
      "app.kubernetes.io/part-of"   = var.project_name
    }
  }

  data = {
    "password" = var.cache_password
  }

  type = "Opaque"
}

# =============================================================================
# Kubernetes ConfigMap for Dragonfly Configuration
# =============================================================================

resource "kubernetes_config_map" "dragonfly" {
  count = !var.use_managed_cache && var.cache_engine == "dragonfly" ? 1 : 0

  metadata {
    name      = "${local.cache_name}-config"
    namespace = var.kubernetes_namespace

    labels = {
      "app.kubernetes.io/name"      = "dragonfly"
      "app.kubernetes.io/component" = "cache"
      "app.kubernetes.io/part-of"   = var.project_name
    }
  }

  data = {
    "dragonfly.conf" = <<-EOT
      # Dragonfly configuration for cicd-hyper-a
      # Memory management
      maxmemory ${replace(local.resources.memory_limit, "Gi", "gb")}
      maxmemory-policy allkeys-lru

      # Performance tuning
      proactor_threads ${var.dragonfly_proactor_threads}

      # Persistence
      dir /data
      dbfilename dump.rdb

      # Snapshot schedule (every 6 hours)
      save 21600 1

      # Logging
      loglevel notice

      # Security
      requirepass $DRAGONFLY_PASSWORD

      # Network
      bind 0.0.0.0
      port 6379

      # Cache mode optimizations
      cache_mode true

      # TCP keepalive
      tcp-keepalive 300
    EOT
  }
}

# =============================================================================
# Kubernetes Deployment for Dragonfly
# =============================================================================

resource "kubernetes_deployment" "dragonfly" {
  count = !var.use_managed_cache && var.cache_engine == "dragonfly" ? 1 : 0

  metadata {
    name      = local.cache_name
    namespace = var.kubernetes_namespace

    labels = {
      "app.kubernetes.io/name"      = "dragonfly"
      "app.kubernetes.io/component" = "cache"
      "app.kubernetes.io/part-of"   = var.project_name
      "app.kubernetes.io/version"   = var.dragonfly_version
    }
  }

  spec {
    replicas = local.resources.replicas

    selector {
      match_labels = {
        "app.kubernetes.io/name"      = "dragonfly"
        "app.kubernetes.io/component" = "cache"
      }
    }

    template {
      metadata {
        labels = {
          "app.kubernetes.io/name"      = "dragonfly"
          "app.kubernetes.io/component" = "cache"
          "app.kubernetes.io/part-of"   = var.project_name
        }

        annotations = {
          "prometheus.io/scrape" = "true"
          "prometheus.io/port"   = "6379"
        }
      }

      spec {
        security_context {
          run_as_non_root = true
          run_as_user     = 999
          run_as_group    = 999
          fs_group        = 999
        }

        container {
          name  = "dragonfly"
          image = local.dragonfly_image

          args = [
            "--logtostderr",
            "--cache_mode=true",
            "--maxmemory=${replace(local.resources.memory_limit, "Gi", "gb")}",
            "--proactor_threads=${var.dragonfly_proactor_threads}",
            "--requirepass=$(DRAGONFLY_PASSWORD)",
            "--dir=/data",
            "--dbfilename=dump.rdb",
            "--snapshot_cron=0 */6 * * *"
          ]

          port {
            name           = "redis"
            container_port = 6379
            protocol       = "TCP"
          }

          env {
            name = "DRAGONFLY_PASSWORD"
            value_from {
              secret_key_ref {
                name = kubernetes_secret.cache[0].metadata[0].name
                key  = "password"
              }
            }
          }

          resources {
            requests = {
              cpu    = local.resources.cpu_request
              memory = local.resources.memory_request
            }
            limits = {
              cpu    = local.resources.cpu_limit
              memory = local.resources.memory_limit
            }
          }

          volume_mount {
            name       = "data"
            mount_path = "/data"
          }

          liveness_probe {
            exec {
              command = ["redis-cli", "-a", "$(DRAGONFLY_PASSWORD)", "ping"]
            }
            initial_delay_seconds = 30
            period_seconds        = 10
            timeout_seconds       = 5
            failure_threshold     = 3
          }

          readiness_probe {
            exec {
              command = ["redis-cli", "-a", "$(DRAGONFLY_PASSWORD)", "ping"]
            }
            initial_delay_seconds = 10
            period_seconds        = 5
            timeout_seconds       = 3
            failure_threshold     = 3
          }

          security_context {
            allow_privilege_escalation = false
            read_only_root_filesystem  = false
            capabilities {
              drop = ["ALL"]
            }
          }
        }

        volume {
          name = "data"
          persistent_volume_claim {
            claim_name = "${local.cache_name}-data"
          }
        }
      }
    }
  }

  lifecycle {
    ignore_changes = [
      spec[0].template[0].metadata[0].annotations["kubectl.kubernetes.io/restartedAt"]
    ]
  }
}

# =============================================================================
# Kubernetes Deployment for Redis (alternative)
# =============================================================================

resource "kubernetes_deployment" "redis" {
  count = !var.use_managed_cache && var.cache_engine == "redis" ? 1 : 0

  metadata {
    name      = local.cache_name
    namespace = var.kubernetes_namespace

    labels = {
      "app.kubernetes.io/name"      = "redis"
      "app.kubernetes.io/component" = "cache"
      "app.kubernetes.io/part-of"   = var.project_name
    }
  }

  spec {
    replicas = local.resources.replicas

    selector {
      match_labels = {
        "app.kubernetes.io/name"      = "redis"
        "app.kubernetes.io/component" = "cache"
      }
    }

    template {
      metadata {
        labels = {
          "app.kubernetes.io/name"      = "redis"
          "app.kubernetes.io/component" = "cache"
          "app.kubernetes.io/part-of"   = var.project_name
        }

        annotations = {
          "prometheus.io/scrape" = "true"
          "prometheus.io/port"   = "6379"
        }
      }

      spec {
        security_context {
          run_as_non_root = true
          run_as_user     = 999
          run_as_group    = 999
          fs_group        = 999
        }

        container {
          name  = "redis"
          image = local.redis_image

          args = [
            "redis-server",
            "--requirepass", "$(REDIS_PASSWORD)",
            "--maxmemory", replace(local.resources.memory_limit, "Gi", "gb"),
            "--maxmemory-policy", "allkeys-lru",
            "--appendonly", "yes",
            "--dir", "/data"
          ]

          port {
            name           = "redis"
            container_port = 6379
            protocol       = "TCP"
          }

          env {
            name = "REDIS_PASSWORD"
            value_from {
              secret_key_ref {
                name = kubernetes_secret.cache[0].metadata[0].name
                key  = "password"
              }
            }
          }

          resources {
            requests = {
              cpu    = local.resources.cpu_request
              memory = local.resources.memory_request
            }
            limits = {
              cpu    = local.resources.cpu_limit
              memory = local.resources.memory_limit
            }
          }

          volume_mount {
            name       = "data"
            mount_path = "/data"
          }

          liveness_probe {
            exec {
              command = ["redis-cli", "-a", "$(REDIS_PASSWORD)", "ping"]
            }
            initial_delay_seconds = 30
            period_seconds        = 10
            timeout_seconds       = 5
            failure_threshold     = 3
          }

          readiness_probe {
            exec {
              command = ["redis-cli", "-a", "$(REDIS_PASSWORD)", "ping"]
            }
            initial_delay_seconds = 10
            period_seconds        = 5
            timeout_seconds       = 3
            failure_threshold     = 3
          }

          security_context {
            allow_privilege_escalation = false
            read_only_root_filesystem  = false
            capabilities {
              drop = ["ALL"]
            }
          }
        }

        volume {
          name = "data"
          persistent_volume_claim {
            claim_name = "${local.cache_name}-data"
          }
        }
      }
    }
  }
}

# =============================================================================
# Persistent Volume Claim
# =============================================================================

resource "kubernetes_persistent_volume_claim" "cache" {
  count = !var.use_managed_cache ? 1 : 0

  metadata {
    name      = "${local.cache_name}-data"
    namespace = var.kubernetes_namespace

    labels = {
      "app.kubernetes.io/name"      = var.cache_engine
      "app.kubernetes.io/component" = "cache"
      "app.kubernetes.io/part-of"   = var.project_name
    }
  }

  spec {
    access_modes       = ["ReadWriteOnce"]
    storage_class_name = var.storage_class

    resources {
      requests = {
        storage = local.resources.storage_size
      }
    }
  }
}

# =============================================================================
# Kubernetes Service
# =============================================================================

resource "kubernetes_service" "cache" {
  count = !var.use_managed_cache ? 1 : 0

  metadata {
    name      = "dragonfly"  # Keep consistent name for app compatibility
    namespace = var.kubernetes_namespace

    labels = {
      "app.kubernetes.io/name"      = var.cache_engine
      "app.kubernetes.io/component" = "cache"
      "app.kubernetes.io/part-of"   = var.project_name
    }
  }

  spec {
    type = "ClusterIP"

    selector = {
      "app.kubernetes.io/name"      = var.cache_engine
      "app.kubernetes.io/component" = "cache"
    }

    port {
      name        = "redis"
      port        = 6379
      target_port = 6379
      protocol    = "TCP"
    }
  }
}

# =============================================================================
# AWS ElastiCache (Managed Redis)
# =============================================================================

resource "aws_elasticache_subnet_group" "main" {
  count = var.use_managed_cache && var.cloud_provider == "aws" ? 1 : 0

  name       = "${var.project_name}-cache-subnet"
  subnet_ids = var.subnet_ids

  tags = var.tags
}

resource "aws_security_group" "elasticache" {
  count = var.use_managed_cache && var.cloud_provider == "aws" ? 1 : 0

  name        = "${var.project_name}-elasticache-sg"
  description = "Security group for ElastiCache"
  vpc_id      = var.vpc_id

  ingress {
    from_port   = 6379
    to_port     = 6379
    protocol    = "tcp"
    cidr_blocks = ["10.0.0.0/8"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = var.tags
}

resource "aws_elasticache_replication_group" "main" {
  count = var.use_managed_cache && var.cloud_provider == "aws" ? 1 : 0

  replication_group_id       = "${var.project_name}-cache"
  description                = "Redis cache for ${var.project_name}"
  node_type                  = var.cache_node_type
  num_cache_clusters         = var.cache_replica_count + 1
  port                       = 6379
  parameter_group_name       = "default.redis7"
  engine_version             = "7.0"

  subnet_group_name  = aws_elasticache_subnet_group.main[0].name
  security_group_ids = [aws_security_group.elasticache[0].id]

  automatic_failover_enabled = var.cache_replica_count > 0
  multi_az_enabled           = var.cache_replica_count > 0

  at_rest_encryption_enabled = true
  transit_encryption_enabled = true
  auth_token                 = var.cache_password

  snapshot_retention_limit = 7
  snapshot_window          = "05:00-06:00"
  maintenance_window       = "sun:06:00-sun:07:00"

  tags = var.tags
}

# =============================================================================
# GCP Memorystore (Managed Redis)
# =============================================================================

resource "google_redis_instance" "main" {
  count = var.use_managed_cache && var.cloud_provider == "gcp" ? 1 : 0

  name           = "${var.project_name}-cache"
  tier           = "STANDARD_HA"
  memory_size_gb = tonumber(replace(var.cache_memory_limit, "Gi", ""))
  region         = var.gcp_region

  redis_version     = "REDIS_7_0"
  display_name      = "${var.project_name} Cache"
  auth_enabled      = true
  transit_encryption_mode = "SERVER_AUTHENTICATION"

  labels = var.tags
}

# =============================================================================
# Azure Cache for Redis (Managed)
# =============================================================================

resource "azurerm_redis_cache" "main" {
  count = var.use_managed_cache && var.cloud_provider == "azure" ? 1 : 0

  name                = "${var.project_name}-cache"
  location            = var.azure_location
  resource_group_name = var.azure_resource_group_name
  capacity            = 2
  family              = "C"
  sku_name            = "Standard"
  enable_non_ssl_port = false
  minimum_tls_version = "1.2"

  redis_configuration {
    maxmemory_policy = "allkeys-lru"
  }

  tags = var.tags
}
