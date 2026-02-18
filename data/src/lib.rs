// SPDX-License-Identifier: PMPL-1.0-or-later
//! Data layer clients for cicd-hyper-a
//!
//! Provides unified access to:
//! - ArangoDB: Graph database for repos, alerts, rules, relationships
//! - Dragonfly: Redis-compatible cache for fast rule lookups
//! - Cache: High-level caching abstraction with connection pooling

pub mod arangodb;
pub mod cache;
pub mod dragonfly;
pub mod error;
pub mod models;

pub use arangodb::ArangoClient;
pub use cache::{CacheHandle, CachePrefix, CacheStats, DragonflyCache, InvalidationEvent};
pub use dragonfly::DragonflyClient;
pub use error::{DataError, Result};
pub use models::*;

/// Configuration for data layer connections
#[derive(Debug, Clone)]
pub struct DataConfig {
    pub arangodb: ArangoConfig,
    pub dragonfly: DragonflyConfig,
}

/// ArangoDB configuration
#[derive(Debug, Clone)]
pub struct ArangoConfig {
    pub url: String,
    pub database: String,
    pub username: String,
    pub password: String,
    pub pool_size: u32,
}

impl Default for ArangoConfig {
    fn default() -> Self {
        Self {
            url: "http://localhost:8529".to_string(),
            database: "cicd_hyper_a".to_string(),
            username: "root".to_string(),
            password: String::new(),
            pool_size: 10,
        }
    }
}

/// Dragonfly configuration
#[derive(Debug, Clone)]
pub struct DragonflyConfig {
    pub url: String,
    pub pool_size: u32,
    pub default_ttl: u64,
}

impl Default for DragonflyConfig {
    fn default() -> Self {
        Self {
            url: "redis://localhost:6379".to_string(),
            pool_size: 10,
            default_ttl: 3600,
        }
    }
}

/// Unified data layer client
pub struct DataLayer {
    pub arango: ArangoClient,
    pub dragonfly: DragonflyClient,
}

impl DataLayer {
    /// Create new data layer with configuration
    pub async fn new(config: DataConfig) -> Result<Self> {
        let arango = ArangoClient::new(config.arangodb).await?;
        let dragonfly = DragonflyClient::new(config.dragonfly).await?;

        Ok(Self { arango, dragonfly })
    }

    /// Health check for all data services
    pub async fn health_check(&self) -> Result<HealthStatus> {
        let arango_ok = self.arango.ping().await.is_ok();
        let dragonfly_ok = self.dragonfly.ping().await.is_ok();

        Ok(HealthStatus {
            arangodb: if arango_ok { "pass" } else { "fail" }.to_string(),
            dragonfly: if dragonfly_ok { "pass" } else { "fail" }.to_string(),
            overall: if arango_ok && dragonfly_ok {
                "healthy"
            } else {
                "degraded"
            }
            .to_string(),
        })
    }
}

/// Health status for data layer
#[derive(Debug, Clone)]
pub struct HealthStatus {
    pub arangodb: String,
    pub dragonfly: String,
    pub overall: String,
}
