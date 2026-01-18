# SPDX-License-Identifier: PLMP-1.0-or-later
# cicd-hyper-a Helm Values Template
# This file is processed by Terraform to generate Helm values

# Global settings
global:
  environment: ${environment}
  domain: ${domain}

# Namespace configuration
namespace:
  create: true
  name: cicd-hyper-a

# API Service
api:
  enabled: true
  replicaCount: ${api_replicas}

  ingress:
    enabled: true
    className: ${ingress_class_name}
    annotations:
      cert-manager.io/cluster-issuer: ${cluster_issuer}
      nginx.ingress.kubernetes.io/ssl-redirect: "true"
    hosts:
      - host: ${api_domain}
        paths:
          - path: /
            pathType: Prefix
    tls:
      - secretName: cicd-hyper-a-api-tls
        hosts:
          - ${api_domain}

  autoscaling:
    enabled: true
    minReplicas: ${api_replicas}
    maxReplicas: 10
    targetCPUUtilizationPercentage: 70

# Registry Service
registry:
  enabled: true
  replicaCount: ${registry_replicas}

  autoscaling:
    enabled: true
    minReplicas: ${registry_replicas}
    maxReplicas: 6

# Engine Service
engine:
  enabled: true
  replicaCount: ${engine_replicas}

  autoscaling:
    enabled: true
    minReplicas: ${engine_replicas}
    maxReplicas: 8

# ArangoDB Configuration
arangodb:
  enabled: ${arangodb_enabled}
  external: ${arangodb_external}
%{ if arangodb_external ~}
  externalUrl: "${arangodb_url}"
%{ endif ~}
  replicaCount: 3

# Dragonfly Configuration
dragonfly:
  enabled: ${dragonfly_enabled}
  external: ${dragonfly_external}
%{ if dragonfly_external ~}
  externalUrl: "${dragonfly_url}"
%{ endif ~}

# Monitoring Configuration
monitoring:
  prometheus:
    enabled: ${monitoring_enabled}
  grafana:
    enabled: ${monitoring_enabled}
    ingress:
      enabled: ${monitoring_enabled}
      host: ${grafana_domain}

# Feature Flags
features:
  neuralLearning: ${neural_learning_enabled}
  p2pFederation: ${p2p_federation_enabled}
  metrics: true

# Configuration
config:
  rustLog: info
  cacheTtlRules: 3600
  cacheTtlRepos: 300
  cacheTtlRegistry: 86400
