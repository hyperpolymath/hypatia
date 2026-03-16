// SPDX-License-Identifier: PMPL-1.0-or-later
//! Data layer clients for Hypatia
//!
//! Provides unified access to:
//! - Dragonfly: Redis-compatible cache for fast rule lookups
//! - Cache: High-level caching abstraction with connection pooling
//! - VeriSimDB: Versioned 8-modality database
//!
//! ArangoDB removed — uclient 0.2 pulls hyper 0.10 (CVE-vulnerable, unmaintained).
//! ArangoDB was 0% deployed and transitional. If needed in future, use a direct
//! HTTP client (reqwest) against ArangoDB's REST API instead of the arangors crate.

pub mod cache;
pub mod dragonfly;
pub mod error;
pub mod models;
pub mod verisimdb;

pub use cache::{CacheHandle, CachePrefix, CacheStats, DragonflyCache, InvalidationEvent};
pub use dragonfly::DragonflyClient;
pub use error::{DataError, Result};
pub use models::*;
pub use verisimdb::VerisimClient;

/// Configuration for data layer connections
#[derive(Debug, Clone)]
pub struct DataConfig {
    pub dragonfly: DragonflyConfig,
    pub verisimdb: VerisimConfig,
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

/// VeriSimDB configuration
#[derive(Debug, Clone)]
pub struct VerisimConfig {
    pub url: String,
    pub api_key: Option<String>,
}

impl Default for VerisimConfig {
    fn default() -> Self {
        Self {
            url: "http://localhost:8080".to_string(),
            api_key: None,
        }
    }
}

/// Unified data layer client
pub struct DataLayer {
    pub dragonfly: DragonflyClient,
    pub verisim: VerisimClient,
}

impl DataLayer {
    /// Create new data layer with configuration
    pub async fn new(config: DataConfig) -> Result<Self> {
        let dragonfly = DragonflyClient::new(config.dragonfly).await?;
        let verisim = VerisimClient::new(config.verisimdb).await?;

        Ok(Self {
            dragonfly,
            verisim,
        })
    }

    /// Health check for all data services
    pub async fn health_check(&self) -> Result<HealthStatus> {
        let dragonfly_ok = self.dragonfly.ping().await.is_ok();
        let verisim_ok = self.verisim.ping().await.is_ok();

        Ok(HealthStatus {
            dragonfly: if dragonfly_ok { "pass" } else { "fail" }.to_string(),
            verisimdb: if verisim_ok { "pass" } else { "fail" }.to_string(),
            overall: if dragonfly_ok && verisim_ok {
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
    pub dragonfly: String,
    pub verisimdb: String,
    pub overall: String,
}
