# SPDX-License-Identifier: AGPL-3.0-or-later
# cicd-hyper-a Monitoring Module (Prometheus/Grafana)
#
# This module deploys a complete monitoring stack using the kube-prometheus-stack
# Helm chart, which includes Prometheus, Grafana, and Alertmanager.

# =============================================================================
# Local Values
# =============================================================================

locals {
  monitoring_name = "${var.project_name}-monitoring"

  # Prometheus operator CRD versions
  prometheus_operator_version = "0.71.2"

  # Grafana dashboards ConfigMap name
  dashboards_configmap = "${var.project_name}-grafana-dashboards"
}

# =============================================================================
# Prometheus Operator CRDs (installed separately for safety)
# =============================================================================

# Note: CRDs should be installed before the Helm chart.
# The kube-prometheus-stack chart can install them, but for production
# it's recommended to manage CRDs separately.

# =============================================================================
# Kube-Prometheus-Stack Helm Release
# =============================================================================

resource "helm_release" "kube_prometheus_stack" {
  name       = "kube-prometheus-stack"
  namespace  = var.kubernetes_namespace
  repository = "https://prometheus-community.github.io/helm-charts"
  chart      = "kube-prometheus-stack"
  version    = "55.5.0"  # Pin to specific version for stability

  create_namespace = true
  timeout          = 600
  wait             = true

  values = [
    yamlencode({
      # Global settings
      commonLabels = {
        "app.kubernetes.io/part-of" = var.project_name
      }

      # Prometheus configuration
      prometheus = {
        enabled = true

        prometheusSpec = {
          replicas = var.environment == "production" ? 2 : 1

          retention        = var.prometheus_retention
          retentionSize    = var.prometheus_retention_size
          scrapeInterval   = "30s"
          evaluationInterval = "30s"

          resources = {
            requests = {
              cpu    = var.environment == "production" ? "500m" : "250m"
              memory = var.environment == "production" ? "1Gi" : "512Mi"
            }
            limits = {
              cpu    = var.environment == "production" ? "2" : "1"
              memory = var.environment == "production" ? "4Gi" : "2Gi"
            }
          }

          storageSpec = {
            volumeClaimTemplate = {
              spec = {
                accessModes = ["ReadWriteOnce"]
                resources = {
                  requests = {
                    storage = var.prometheus_storage_size
                  }
                }
              }
            }
          }

          # Service discovery for cicd-hyper-a components
          additionalScrapeConfigs = [
            {
              job_name = "cicd-hyper-a-api"
              kubernetes_sd_configs = [{
                role = "endpoints"
                namespaces = {
                  names = [var.kubernetes_namespace]
                }
              }]
              relabel_configs = [
                {
                  source_labels = ["__meta_kubernetes_service_name"]
                  regex         = "api"
                  action        = "keep"
                },
                {
                  source_labels = ["__meta_kubernetes_endpoint_port_name"]
                  regex         = "http"
                  action        = "keep"
                }
              ]
            },
            {
              job_name = "cicd-hyper-a-registry"
              kubernetes_sd_configs = [{
                role = "endpoints"
                namespaces = {
                  names = [var.kubernetes_namespace]
                }
              }]
              relabel_configs = [
                {
                  source_labels = ["__meta_kubernetes_service_name"]
                  regex         = "registry"
                  action        = "keep"
                }
              ]
            },
            {
              job_name = "cicd-hyper-a-engine"
              kubernetes_sd_configs = [{
                role = "endpoints"
                namespaces = {
                  names = [var.kubernetes_namespace]
                }
              }]
              relabel_configs = [
                {
                  source_labels = ["__meta_kubernetes_service_name"]
                  regex         = "engine"
                  action        = "keep"
                }
              ]
            },
            {
              job_name = "arangodb"
              kubernetes_sd_configs = [{
                role = "endpoints"
                namespaces = {
                  names = [var.kubernetes_namespace]
                }
              }]
              relabel_configs = [
                {
                  source_labels = ["__meta_kubernetes_service_name"]
                  regex         = "arangodb"
                  action        = "keep"
                }
              ]
              metrics_path = "/_admin/metrics"
            },
            {
              job_name = "dragonfly"
              kubernetes_sd_configs = [{
                role = "endpoints"
                namespaces = {
                  names = [var.kubernetes_namespace]
                }
              }]
              relabel_configs = [
                {
                  source_labels = ["__meta_kubernetes_service_name"]
                  regex         = "dragonfly"
                  action        = "keep"
                }
              ]
            }
          ]

          # Pod security
          securityContext = {
            runAsNonRoot = true
            runAsUser    = 1000
            runAsGroup   = 1000
            fsGroup      = 1000
          }
        }

        # Prometheus service
        service = {
          type = "ClusterIP"
        }
      }

      # Grafana configuration
      grafana = {
        enabled = true

        replicas = 1

        adminPassword = var.grafana_admin_password

        resources = {
          requests = {
            cpu    = "250m"
            memory = "256Mi"
          }
          limits = {
            cpu    = "1"
            memory = "1Gi"
          }
        }

        persistence = {
          enabled = true
          size    = var.grafana_storage_size
        }

        # Ingress for Grafana
        ingress = {
          enabled = var.enable_monitoring_ingress
          ingressClassName = var.ingress_class_name
          annotations = var.enable_monitoring_ingress ? {
            "cert-manager.io/cluster-issuer"           = var.cluster_issuer
            "nginx.ingress.kubernetes.io/ssl-redirect" = "true"
          } : {}
          hosts = var.enable_monitoring_ingress ? [var.grafana_domain] : []
          tls = var.enable_monitoring_ingress ? [{
            secretName = "${var.project_name}-grafana-tls"
            hosts      = [var.grafana_domain]
          }] : []
        }

        # Additional data sources
        additionalDataSources = []

        # Dashboard providers
        dashboardProviders = {
          "dashboardproviders.yaml" = {
            apiVersion = 1
            providers = [{
              name            = "cicd-hyper-a"
              orgId           = 1
              folder          = "cicd-hyper-a"
              type            = "file"
              disableDeletion = false
              editable        = true
              options = {
                path = "/var/lib/grafana/dashboards/cicd-hyper-a"
              }
            }]
          }
        }

        # Dashboard ConfigMaps
        dashboardsConfigMaps = {
          cicd-hyper-a = local.dashboards_configmap
        }

        # Grafana plugins
        plugins = [
          "grafana-clock-panel",
          "grafana-piechart-panel",
          "grafana-worldmap-panel"
        ]

        # Grafana configuration
        "grafana.ini" = {
          server = {
            root_url = var.enable_monitoring_ingress ? "https://${var.grafana_domain}" : ""
          }
          security = {
            admin_user     = "admin"
            admin_password = var.grafana_admin_password
          }
          auth = {
            disable_login_form = false
          }
          "auth.anonymous" = {
            enabled = false
          }
        }

        # Pod security
        securityContext = {
          runAsNonRoot = true
          runAsUser    = 472
          runAsGroup   = 472
          fsGroup      = 472
        }
      }

      # Alertmanager configuration
      alertmanager = {
        enabled = var.enable_alertmanager

        alertmanagerSpec = {
          replicas = var.alertmanager_replicas

          resources = {
            requests = {
              cpu    = "100m"
              memory = "128Mi"
            }
            limits = {
              cpu    = "500m"
              memory = "512Mi"
            }
          }

          storage = {
            volumeClaimTemplate = {
              spec = {
                accessModes = ["ReadWriteOnce"]
                resources = {
                  requests = {
                    storage = "5Gi"
                  }
                }
              }
            }
          }

          securityContext = {
            runAsNonRoot = true
            runAsUser    = 1000
            runAsGroup   = 1000
            fsGroup      = 1000
          }
        }
      }

      # Node exporter (for node metrics)
      nodeExporter = {
        enabled = true
      }

      # Kube-state-metrics (for Kubernetes object metrics)
      kubeStateMetrics = {
        enabled = true
      }

      # Prometheus rules for cicd-hyper-a
      additionalPrometheusRulesMap = {
        "cicd-hyper-a-rules" = {
          groups = [
            {
              name = "cicd-hyper-a.rules"
              rules = [
                {
                  alert = "CicdHyperAApiHighErrorRate"
                  expr  = "sum(rate(http_requests_total{job=\"cicd-hyper-a-api\",status=~\"5..\"}[5m])) / sum(rate(http_requests_total{job=\"cicd-hyper-a-api\"}[5m])) > 0.05"
                  for   = "5m"
                  labels = {
                    severity = "critical"
                  }
                  annotations = {
                    summary     = "High error rate on cicd-hyper-a API"
                    description = "Error rate is {{ $value | humanizePercentage }} over the last 5 minutes"
                  }
                },
                {
                  alert = "CicdHyperAApiHighLatency"
                  expr  = "histogram_quantile(0.99, sum(rate(http_request_duration_seconds_bucket{job=\"cicd-hyper-a-api\"}[5m])) by (le)) > 1"
                  for   = "5m"
                  labels = {
                    severity = "warning"
                  }
                  annotations = {
                    summary     = "High latency on cicd-hyper-a API"
                    description = "P99 latency is {{ $value | humanizeDuration }}"
                  }
                },
                {
                  alert = "ArangoDBDown"
                  expr  = "up{job=\"arangodb\"} == 0"
                  for   = "5m"
                  labels = {
                    severity = "critical"
                  }
                  annotations = {
                    summary     = "ArangoDB is down"
                    description = "ArangoDB has been down for more than 5 minutes"
                  }
                },
                {
                  alert = "DragonflyDown"
                  expr  = "up{job=\"dragonfly\"} == 0"
                  for   = "5m"
                  labels = {
                    severity = "critical"
                  }
                  annotations = {
                    summary     = "Dragonfly cache is down"
                    description = "Dragonfly has been down for more than 5 minutes"
                  }
                },
                {
                  alert = "HighMemoryUsage"
                  expr  = "container_memory_usage_bytes{namespace=\"${var.kubernetes_namespace}\"} / container_spec_memory_limit_bytes{namespace=\"${var.kubernetes_namespace}\"} > 0.9"
                  for   = "5m"
                  labels = {
                    severity = "warning"
                  }
                  annotations = {
                    summary     = "High memory usage"
                    description = "Container {{ $labels.container }} in pod {{ $labels.pod }} is using {{ $value | humanizePercentage }} of its memory limit"
                  }
                }
              ]
            },
            {
              name = "cicd-hyper-a.recording"
              rules = [
                {
                  record = "cicd_hyper_a:api:request_rate_5m"
                  expr   = "sum(rate(http_requests_total{job=\"cicd-hyper-a-api\"}[5m]))"
                },
                {
                  record = "cicd_hyper_a:api:error_rate_5m"
                  expr   = "sum(rate(http_requests_total{job=\"cicd-hyper-a-api\",status=~\"5..\"}[5m])) / sum(rate(http_requests_total{job=\"cicd-hyper-a-api\"}[5m]))"
                },
                {
                  record = "cicd_hyper_a:api:latency_p99_5m"
                  expr   = "histogram_quantile(0.99, sum(rate(http_request_duration_seconds_bucket{job=\"cicd-hyper-a-api\"}[5m])) by (le))"
                }
              ]
            }
          ]
        }
      }

      # Disable components not needed
      kubeApiServer = {
        enabled = true
      }

      kubelet = {
        enabled = true
      }

      kubeControllerManager = {
        enabled = false  # Usually not accessible in managed clusters
      }

      coreDns = {
        enabled = true
      }

      kubeEtcd = {
        enabled = false  # Usually not accessible in managed clusters
      }

      kubeScheduler = {
        enabled = false  # Usually not accessible in managed clusters
      }

      kubeProxy = {
        enabled = true
      }
    })
  ]
}

# =============================================================================
# Custom Grafana Dashboards ConfigMap
# =============================================================================

resource "kubernetes_config_map" "grafana_dashboards" {
  metadata {
    name      = local.dashboards_configmap
    namespace = var.kubernetes_namespace

    labels = {
      "grafana_dashboard" = "1"
      "app.kubernetes.io/part-of" = var.project_name
    }
  }

  data = {
    "overview.json" = file("${path.module}/dashboards/overview.json")
    "api.json"      = file("${path.module}/dashboards/api.json")
    "bots.json"     = file("${path.module}/dashboards/bots.json")
  }

  depends_on = [helm_release.kube_prometheus_stack]
}

# =============================================================================
# ServiceMonitor for cicd-hyper-a components
# =============================================================================

resource "kubernetes_manifest" "api_service_monitor" {
  manifest = {
    apiVersion = "monitoring.coreos.com/v1"
    kind       = "ServiceMonitor"
    metadata = {
      name      = "${var.project_name}-api"
      namespace = var.kubernetes_namespace
      labels = {
        "app.kubernetes.io/part-of" = var.project_name
      }
    }
    spec = {
      selector = {
        matchLabels = {
          "app.kubernetes.io/name"      = "api"
          "app.kubernetes.io/component" = "api"
        }
      }
      endpoints = [{
        port     = "http"
        interval = "30s"
        path     = "/metrics"
      }]
    }
  }

  depends_on = [helm_release.kube_prometheus_stack]
}

resource "kubernetes_manifest" "registry_service_monitor" {
  manifest = {
    apiVersion = "monitoring.coreos.com/v1"
    kind       = "ServiceMonitor"
    metadata = {
      name      = "${var.project_name}-registry"
      namespace = var.kubernetes_namespace
      labels = {
        "app.kubernetes.io/part-of" = var.project_name
      }
    }
    spec = {
      selector = {
        matchLabels = {
          "app.kubernetes.io/name"      = "registry"
          "app.kubernetes.io/component" = "registry"
        }
      }
      endpoints = [{
        port     = "http"
        interval = "30s"
        path     = "/metrics"
      }]
    }
  }

  depends_on = [helm_release.kube_prometheus_stack]
}

resource "kubernetes_manifest" "engine_service_monitor" {
  manifest = {
    apiVersion = "monitoring.coreos.com/v1"
    kind       = "ServiceMonitor"
    metadata = {
      name      = "${var.project_name}-engine"
      namespace = var.kubernetes_namespace
      labels = {
        "app.kubernetes.io/part-of" = var.project_name
      }
    }
    spec = {
      selector = {
        matchLabels = {
          "app.kubernetes.io/name"      = "engine"
          "app.kubernetes.io/component" = "engine"
        }
      }
      endpoints = [{
        port     = "http"
        interval = "30s"
        path     = "/metrics"
      }]
    }
  }

  depends_on = [helm_release.kube_prometheus_stack]
}
