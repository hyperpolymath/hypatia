# SPDX-License-Identifier: PMPL-1.0-or-later
# cicd-hyper-a Database Module (ArangoDB)
#
# This module deploys ArangoDB either as:
# - Self-hosted StatefulSet on Kubernetes
# - Managed database service (when available)

# =============================================================================
# Local Values
# =============================================================================

locals {
  arangodb_name = "${var.project_name}-arangodb"

  # Resource limits based on environment
  resource_presets = {
    dev = {
      cpu_request    = "250m"
      cpu_limit      = "1"
      memory_request = "1Gi"
      memory_limit   = "2Gi"
      storage_size   = "10Gi"
      replicas       = 1
    }
    staging = {
      cpu_request    = "500m"
      cpu_limit      = "2"
      memory_request = "2Gi"
      memory_limit   = "4Gi"
      storage_size   = "25Gi"
      replicas       = 2
    }
    production = {
      cpu_request    = "1"
      cpu_limit      = var.arangodb_cpu_limit
      memory_request = "4Gi"
      memory_limit   = var.arangodb_memory_limit
      storage_size   = var.arangodb_storage_size
      replicas       = var.arangodb_replica_count
    }
  }

  # Use preset based on environment, or production settings
  resources = lookup(local.resource_presets, var.environment, local.resource_presets.production)
}

# =============================================================================
# Kubernetes Secret for ArangoDB Credentials
# =============================================================================

resource "kubernetes_secret" "arangodb" {
  count = !var.use_managed_database ? 1 : 0

  metadata {
    name      = "${local.arangodb_name}-credentials"
    namespace = var.kubernetes_namespace

    labels = {
      "app.kubernetes.io/name"      = "arangodb"
      "app.kubernetes.io/component" = "database"
      "app.kubernetes.io/part-of"   = var.project_name
    }
  }

  data = {
    "root-password" = var.arangodb_root_password
    "username"      = "root"
  }

  type = "Opaque"
}

# =============================================================================
# Kubernetes ConfigMap for ArangoDB Configuration
# =============================================================================

resource "kubernetes_config_map" "arangodb" {
  count = !var.use_managed_database ? 1 : 0

  metadata {
    name      = "${local.arangodb_name}-config"
    namespace = var.kubernetes_namespace

    labels = {
      "app.kubernetes.io/name"      = "arangodb"
      "app.kubernetes.io/component" = "database"
      "app.kubernetes.io/part-of"   = var.project_name
    }
  }

  data = {
    "arangod.conf" = <<-EOT
      [server]
      endpoint = tcp://0.0.0.0:8529
      authentication = true
      statistics = true

      [log]
      level = info

      [query]
      cache-mode = demand
      cache-entries = 128

      [javascript]
      enabled = true

      [foxx]
      queues = true

      [rocksdb]
      encryption-keyfile =

      [cluster]
      agency-endpoint =
    EOT

    "init-database.js" = <<-EOT
      // Initialize cicd-hyper-a database
      const db = require('@arangodb').db;

      // Create main database if not exists
      if (!db._databases().includes('cicd_hyper_a')) {
        db._createDatabase('cicd_hyper_a');
      }

      db._useDatabase('cicd_hyper_a');

      // Create collections
      const collections = [
        { name: 'bots', type: 'document' },
        { name: 'rules', type: 'document' },
        { name: 'findings', type: 'document' },
        { name: 'repos', type: 'document' },
        { name: 'executions', type: 'document' },
        { name: 'metrics', type: 'document' },
        { name: 'bot_repo_edges', type: 'edge' },
        { name: 'rule_bot_edges', type: 'edge' },
        { name: 'finding_repo_edges', type: 'edge' }
      ];

      for (const col of collections) {
        if (!db._collection(col.name)) {
          if (col.type === 'edge') {
            db._createEdgeCollection(col.name);
          } else {
            db._create(col.name);
          }
          console.log('Created collection: ' + col.name);
        }
      }

      // Create indexes
      const indexes = [
        { collection: 'bots', fields: ['name'], type: 'persistent', unique: true },
        { collection: 'rules', fields: ['id'], type: 'persistent', unique: true },
        { collection: 'repos', fields: ['full_name'], type: 'persistent', unique: true },
        { collection: 'findings', fields: ['created_at'], type: 'persistent' },
        { collection: 'findings', fields: ['severity'], type: 'persistent' },
        { collection: 'executions', fields: ['started_at'], type: 'persistent' }
      ];

      for (const idx of indexes) {
        const col = db._collection(idx.collection);
        if (col) {
          try {
            col.ensureIndex({
              type: idx.type,
              fields: idx.fields,
              unique: idx.unique || false
            });
          } catch (e) {
            console.log('Index exists or error: ' + e.message);
          }
        }
      }

      console.log('Database initialization complete');
    EOT
  }
}

# =============================================================================
# Kubernetes StatefulSet for ArangoDB
# =============================================================================

resource "kubernetes_stateful_set" "arangodb" {
  count = !var.use_managed_database ? 1 : 0

  metadata {
    name      = local.arangodb_name
    namespace = var.kubernetes_namespace

    labels = {
      "app.kubernetes.io/name"      = "arangodb"
      "app.kubernetes.io/component" = "database"
      "app.kubernetes.io/part-of"   = var.project_name
      "app.kubernetes.io/version"   = var.arangodb_version
    }
  }

  spec {
    service_name = local.arangodb_name
    replicas     = local.resources.replicas

    selector {
      match_labels = {
        "app.kubernetes.io/name"      = "arangodb"
        "app.kubernetes.io/component" = "database"
      }
    }

    template {
      metadata {
        labels = {
          "app.kubernetes.io/name"      = "arangodb"
          "app.kubernetes.io/component" = "database"
          "app.kubernetes.io/part-of"   = var.project_name
        }

        annotations = {
          "prometheus.io/scrape" = "true"
          "prometheus.io/port"   = "8529"
          "prometheus.io/path"   = "/_admin/metrics"
        }
      }

      spec {
        security_context {
          run_as_non_root = true
          run_as_user     = 1000
          run_as_group    = 1000
          fs_group        = 1000
        }

        container {
          name  = "arangodb"
          image = "arangodb:${var.arangodb_version}"

          port {
            name           = "http"
            container_port = 8529
            protocol       = "TCP"
          }

          env {
            name = "ARANGO_ROOT_PASSWORD"
            value_from {
              secret_key_ref {
                name = kubernetes_secret.arangodb[0].metadata[0].name
                key  = "root-password"
              }
            }
          }

          env {
            name  = "ARANGO_NO_AUTH"
            value = "0"
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
            mount_path = "/var/lib/arangodb3"
          }

          volume_mount {
            name       = "config"
            mount_path = "/etc/arangodb3"
          }

          liveness_probe {
            http_get {
              path = "/_api/version"
              port = 8529
            }
            initial_delay_seconds = 30
            period_seconds        = 10
            timeout_seconds       = 5
            failure_threshold     = 3
          }

          readiness_probe {
            http_get {
              path = "/_api/version"
              port = 8529
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
          name = "config"
          config_map {
            name = kubernetes_config_map.arangodb[0].metadata[0].name
          }
        }
      }
    }

    volume_claim_template {
      metadata {
        name = "data"
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
  }

  lifecycle {
    ignore_changes = [
      spec[0].template[0].metadata[0].annotations["kubectl.kubernetes.io/restartedAt"]
    ]
  }
}

# =============================================================================
# Kubernetes Service for ArangoDB
# =============================================================================

resource "kubernetes_service" "arangodb" {
  count = !var.use_managed_database ? 1 : 0

  metadata {
    name      = "arangodb"
    namespace = var.kubernetes_namespace

    labels = {
      "app.kubernetes.io/name"      = "arangodb"
      "app.kubernetes.io/component" = "database"
      "app.kubernetes.io/part-of"   = var.project_name
    }
  }

  spec {
    type = "ClusterIP"

    selector = {
      "app.kubernetes.io/name"      = "arangodb"
      "app.kubernetes.io/component" = "database"
    }

    port {
      name        = "http"
      port        = 8529
      target_port = 8529
      protocol    = "TCP"
    }
  }
}

# =============================================================================
# Kubernetes Service (Headless) for StatefulSet
# =============================================================================

resource "kubernetes_service" "arangodb_headless" {
  count = !var.use_managed_database ? 1 : 0

  metadata {
    name      = "${local.arangodb_name}-headless"
    namespace = var.kubernetes_namespace

    labels = {
      "app.kubernetes.io/name"      = "arangodb"
      "app.kubernetes.io/component" = "database"
      "app.kubernetes.io/part-of"   = var.project_name
    }
  }

  spec {
    type       = "ClusterIP"
    cluster_ip = "None"

    selector = {
      "app.kubernetes.io/name"      = "arangodb"
      "app.kubernetes.io/component" = "database"
    }

    port {
      name        = "http"
      port        = 8529
      target_port = 8529
      protocol    = "TCP"
    }
  }
}

# =============================================================================
# Pod Disruption Budget
# =============================================================================

resource "kubernetes_pod_disruption_budget" "arangodb" {
  count = !var.use_managed_database && local.resources.replicas > 1 ? 1 : 0

  metadata {
    name      = "${local.arangodb_name}-pdb"
    namespace = var.kubernetes_namespace
  }

  spec {
    min_available = local.resources.replicas > 2 ? 2 : 1

    selector {
      match_labels = {
        "app.kubernetes.io/name"      = "arangodb"
        "app.kubernetes.io/component" = "database"
      }
    }
  }
}

# =============================================================================
# Initialization Job (runs once after deployment)
# =============================================================================

resource "kubernetes_job" "arangodb_init" {
  count = !var.use_managed_database ? 1 : 0

  metadata {
    name      = "${local.arangodb_name}-init"
    namespace = var.kubernetes_namespace

    labels = {
      "app.kubernetes.io/name"      = "arangodb-init"
      "app.kubernetes.io/component" = "database"
      "app.kubernetes.io/part-of"   = var.project_name
    }
  }

  spec {
    ttl_seconds_after_finished = 300

    template {
      metadata {
        labels = {
          "app.kubernetes.io/name" = "arangodb-init"
        }
      }

      spec {
        restart_policy = "OnFailure"

        container {
          name  = "init"
          image = "arangodb:${var.arangodb_version}"

          command = [
            "/bin/sh",
            "-c",
            <<-EOT
              sleep 30  # Wait for ArangoDB to be ready
              arangosh --server.endpoint tcp://arangodb:8529 \
                       --server.username root \
                       --server.password "$ARANGO_ROOT_PASSWORD" \
                       --javascript.execute /init/init-database.js
            EOT
          ]

          env {
            name = "ARANGO_ROOT_PASSWORD"
            value_from {
              secret_key_ref {
                name = kubernetes_secret.arangodb[0].metadata[0].name
                key  = "root-password"
              }
            }
          }

          volume_mount {
            name       = "init-script"
            mount_path = "/init"
          }
        }

        volume {
          name = "init-script"
          config_map {
            name = kubernetes_config_map.arangodb[0].metadata[0].name
          }
        }
      }
    }
  }

  wait_for_completion = false

  depends_on = [
    kubernetes_stateful_set.arangodb,
    kubernetes_service.arangodb
  ]
}
