# SPDX-License-Identifier: PLMP-1.0-or-later
# cicd-hyper-a Cache Module Outputs

output "cache_endpoint" {
  description = "Cache endpoint"
  value = (
    var.use_managed_cache ? (
      var.cloud_provider == "aws" ? aws_elasticache_replication_group.main[0].primary_endpoint_address :
      var.cloud_provider == "gcp" ? google_redis_instance.main[0].host :
      var.cloud_provider == "azure" ? azurerm_redis_cache.main[0].hostname :
      ""
    ) : "dragonfly.${var.kubernetes_namespace}.svc.cluster.local"
  )
}

output "cache_port" {
  description = "Cache port"
  value = (
    var.use_managed_cache && var.cloud_provider == "azure" ? 6380 : 6379
  )
}

output "cache_service_name" {
  description = "Cache Kubernetes service name"
  value       = var.use_managed_cache ? "" : "dragonfly"
}

output "cache_secret_name" {
  description = "Name of the Kubernetes secret containing cache credentials"
  value       = var.use_managed_cache ? "" : kubernetes_secret.cache[0].metadata[0].name
}

output "cache_connection_string" {
  description = "Redis-compatible connection string"
  value = var.use_managed_cache ? (
    var.cloud_provider == "aws" ? "rediss://:${var.cache_password}@${aws_elasticache_replication_group.main[0].primary_endpoint_address}:6379" :
    var.cloud_provider == "gcp" ? "redis://:${google_redis_instance.main[0].auth_string}@${google_redis_instance.main[0].host}:6379" :
    var.cloud_provider == "azure" ? "rediss://:${azurerm_redis_cache.main[0].primary_access_key}@${azurerm_redis_cache.main[0].hostname}:6380" :
    ""
  ) : "redis://:${var.cache_password}@dragonfly.${var.kubernetes_namespace}.svc.cluster.local:6379"
  sensitive = true
}
