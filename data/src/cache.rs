// SPDX-License-Identifier: PMPL-1.0-or-later
//! High-level cache abstraction for cicd-hyper-a
//!
//! Provides a unified caching interface wrapping DragonflyClient with:
//! - Connection pooling via deadpool-redis
//! - Batch operations for efficiency
//! - Type-safe cache keys
//! - Pub/sub for cache invalidation
//! - Metrics and observability

use crate::error::{DataError, Result};
use crate::models::*;
use crate::DragonflyConfig;
use deadpool_redis::{Config, Pool, Runtime};
use redis::AsyncCommands;
use serde::{de::DeserializeOwned, Serialize};
use std::sync::Arc;
use tokio::sync::broadcast;
use tracing::{debug, info, instrument, warn};

/// Cache key prefixes for namespace isolation
#[derive(Debug, Clone, Copy)]
pub enum CachePrefix {
    /// Compiled rules: rule:{ruleset}:{rule_id}
    Rule,
    /// Ruleset metadata: ruleset:{name}
    Ruleset,
    /// Scan results: scan:{repo_key}:{scan_type}
    Scan,
    /// Fleet status: fleet:{bot_id}
    Fleet,
    /// Repository state: repo:{repo_key}
    Repo,
    /// Rate limiting: rl:{resource}:{identifier}
    RateLimit,
    /// Job queue: job:{job_id}
    Job,
}

impl CachePrefix {
    fn as_str(&self) -> &'static str {
        match self {
            Self::Rule => "rule",
            Self::Ruleset => "ruleset",
            Self::Scan => "scan",
            Self::Fleet => "fleet",
            Self::Repo => "repo",
            Self::RateLimit => "rl",
            Self::Job => "job",
        }
    }

    fn default_ttl(&self) -> u64 {
        match self {
            Self::Rule => 3600,       // 1 hour
            Self::Ruleset => 7200,    // 2 hours
            Self::Scan => 300,        // 5 minutes
            Self::Fleet => 60,        // 1 minute
            Self::Repo => 600,        // 10 minutes
            Self::RateLimit => 60,    // 1 minute
            Self::Job => 86400,       // 24 hours
        }
    }
}

/// Invalidation event for pub/sub
#[derive(Debug, Clone)]
pub enum InvalidationEvent {
    /// Single key invalidated
    Key(String),
    /// Pattern invalidated (e.g., "rule:*")
    Pattern(String),
    /// Entire ruleset invalidated
    Ruleset(String),
    /// Full cache flush
    FlushAll,
}

/// Cache statistics
#[derive(Debug, Clone, Default)]
pub struct CacheStats {
    pub total_keys: i64,
    pub rule_count: i64,
    pub ruleset_count: i64,
    pub scan_count: i64,
    pub fleet_count: i64,
    pub memory_used_bytes: i64,
    pub hit_rate: f64,
    pub miss_rate: f64,
}

/// High-level cache client with connection pooling
pub struct DragonflyCache {
    pool: Pool,
    config: DragonflyConfig,
    invalidation_tx: broadcast::Sender<InvalidationEvent>,
    _invalidation_rx: broadcast::Receiver<InvalidationEvent>,
}

impl DragonflyCache {
    /// Create new cache with connection pool
    #[instrument(skip(config))]
    pub async fn new(config: DragonflyConfig) -> Result<Self> {
        info!("Creating Dragonfly cache pool at {}", config.url);

        let pool_config = Config::from_url(&config.url);
        let pool = pool_config
            .create_pool(Some(Runtime::Tokio1))
            .map_err(|e| DataError::ConnectionError(e.to_string()))?;

        // Test connection
        let mut conn = pool
            .get()
            .await
            .map_err(|e| DataError::ConnectionError(e.to_string()))?;
        redis::cmd("PING")
            .query_async::<_, String>(&mut *conn)
            .await
            .map_err(|e| DataError::ConnectionError(e.to_string()))?;

        info!("Dragonfly cache pool created successfully");

        let (tx, rx) = broadcast::channel(1000);

        Ok(Self {
            pool,
            config,
            invalidation_tx: tx,
            _invalidation_rx: rx,
        })
    }

    /// Get a connection from the pool
    async fn get_conn(&self) -> Result<deadpool_redis::Connection> {
        self.pool
            .get()
            .await
            .map_err(|e| DataError::ConnectionError(e.to_string()))
    }

    /// Subscribe to invalidation events
    pub fn subscribe_invalidations(&self) -> broadcast::Receiver<InvalidationEvent> {
        self.invalidation_tx.subscribe()
    }

    // ============================================================
    // BASIC OPERATIONS
    // ============================================================

    /// Get a value by key
    #[instrument(skip(self))]
    pub async fn get<T: DeserializeOwned>(&self, key: &str) -> Result<T> {
        let mut conn = self.get_conn().await?;
        let json: String = conn
            .get(key)
            .await
            .map_err(|_| DataError::CacheMiss(key.to_string()))?;
        let value = serde_json::from_str(&json)?;
        debug!("Cache hit for key: {}", key);
        Ok(value)
    }

    /// Get a value, returning None if not found
    #[instrument(skip(self))]
    pub async fn get_opt<T: DeserializeOwned>(&self, key: &str) -> Result<Option<T>> {
        let mut conn = self.get_conn().await?;
        let result: Option<String> = conn.get(key).await?;
        match result {
            Some(json) => {
                let value = serde_json::from_str(&json)?;
                debug!("Cache hit for key: {}", key);
                Ok(Some(value))
            }
            None => {
                debug!("Cache miss for key: {}", key);
                Ok(None)
            }
        }
    }

    /// Set a value with TTL
    #[instrument(skip(self, value))]
    pub async fn set<T: Serialize>(&self, key: &str, value: &T, ttl: Option<u64>) -> Result<()> {
        let mut conn = self.get_conn().await?;
        let json = serde_json::to_string(value)?;
        let ttl_secs = ttl.unwrap_or(self.config.default_ttl);
        conn.set_ex::<_, _, ()>(key, json, ttl_secs).await?;
        debug!("Set key {} with TTL {}s", key, ttl_secs);
        Ok(())
    }

    /// Delete a key
    #[instrument(skip(self))]
    pub async fn delete(&self, key: &str) -> Result<bool> {
        let mut conn = self.get_conn().await?;
        let deleted: i64 = conn.del(key).await?;
        if deleted > 0 {
            let _ = self.invalidation_tx.send(InvalidationEvent::Key(key.to_string()));
        }
        Ok(deleted > 0)
    }

    /// Check if key exists
    pub async fn exists(&self, key: &str) -> Result<bool> {
        let mut conn = self.get_conn().await?;
        let exists: bool = conn.exists(key).await?;
        Ok(exists)
    }

    /// Get TTL for a key (-1 if no expiry, -2 if not found)
    pub async fn ttl(&self, key: &str) -> Result<i64> {
        let mut conn = self.get_conn().await?;
        let ttl: i64 = conn.ttl(key).await?;
        Ok(ttl)
    }

    // ============================================================
    // BATCH OPERATIONS
    // ============================================================

    /// Get multiple values by keys
    #[instrument(skip(self))]
    pub async fn mget<T: DeserializeOwned>(&self, keys: &[String]) -> Result<Vec<Option<T>>> {
        if keys.is_empty() {
            return Ok(Vec::new());
        }

        let mut conn = self.get_conn().await?;
        let results: Vec<Option<String>> = conn.mget(keys).await?;

        results
            .into_iter()
            .map(|opt| match opt {
                Some(json) => serde_json::from_str(&json).map(Some).map_err(Into::into),
                None => Ok(None),
            })
            .collect()
    }

    /// Set multiple values with the same TTL
    #[instrument(skip(self, entries))]
    pub async fn mset<T: Serialize>(
        &self,
        entries: &[(String, T)],
        ttl: Option<u64>,
    ) -> Result<()> {
        if entries.is_empty() {
            return Ok(());
        }

        let mut conn = self.get_conn().await?;
        let ttl_secs = ttl.unwrap_or(self.config.default_ttl);

        // Use pipeline for efficiency
        let mut pipe = redis::pipe();
        for (key, value) in entries {
            let json = serde_json::to_string(value)?;
            pipe.set_ex::<_, _>(key, json, ttl_secs);
        }
        pipe.query_async::<_, ()>(&mut *conn).await?;

        debug!("Set {} keys with TTL {}s", entries.len(), ttl_secs);
        Ok(())
    }

    /// Delete multiple keys
    #[instrument(skip(self))]
    pub async fn mdel(&self, keys: &[String]) -> Result<i64> {
        if keys.is_empty() {
            return Ok(0);
        }

        let mut conn = self.get_conn().await?;
        let deleted: i64 = conn.del(keys).await?;

        if deleted > 0 {
            for key in keys {
                let _ = self
                    .invalidation_tx
                    .send(InvalidationEvent::Key(key.clone()));
            }
        }

        Ok(deleted)
    }

    // ============================================================
    // RULE CACHING
    // ============================================================

    /// Build rule cache key
    fn rule_key(ruleset: &str, rule_id: &str) -> String {
        format!("{}:{}:{}", CachePrefix::Rule.as_str(), ruleset, rule_id)
    }

    /// Cache a compiled rule
    #[instrument(skip(self, rule))]
    pub async fn cache_rule(&self, rule: &CachedRule) -> Result<()> {
        let key = Self::rule_key(&rule.ruleset, &rule.rule_id);
        let ttl = if rule.ttl > 0 {
            Some(rule.ttl as u64)
        } else {
            Some(CachePrefix::Rule.default_ttl())
        };
        self.set(&key, rule, ttl).await?;

        // Add to ruleset's rule index
        let index_key = format!("{}:{}:rules", CachePrefix::Ruleset.as_str(), rule.ruleset);
        let mut conn = self.get_conn().await?;
        conn.sadd::<_, _, ()>(&index_key, &rule.rule_id).await?;

        Ok(())
    }

    /// Get a cached rule
    #[instrument(skip(self))]
    pub async fn get_cached_rule(&self, ruleset: &str, rule_id: &str) -> Result<CachedRule> {
        let key = Self::rule_key(ruleset, rule_id);
        self.get(&key).await
    }

    /// Get a cached rule (optional version)
    pub async fn get_cached_rule_opt(
        &self,
        ruleset: &str,
        rule_id: &str,
    ) -> Result<Option<CachedRule>> {
        let key = Self::rule_key(ruleset, rule_id);
        self.get_opt(&key).await
    }

    /// Cache multiple rules in batch
    #[instrument(skip(self, rules))]
    pub async fn cache_rules(&self, rules: &[CachedRule]) -> Result<()> {
        if rules.is_empty() {
            return Ok(());
        }

        let entries: Vec<(String, CachedRule)> = rules
            .iter()
            .map(|r| (Self::rule_key(&r.ruleset, &r.rule_id), r.clone()))
            .collect();

        let ttl = Some(CachePrefix::Rule.default_ttl());
        self.mset(&entries.iter().map(|(k, v)| (k.clone(), v)).collect::<Vec<_>>(), ttl)
            .await?;

        // Update indexes
        let mut conn = self.get_conn().await?;
        for rule in rules {
            let index_key = format!("{}:{}:rules", CachePrefix::Ruleset.as_str(), rule.ruleset);
            conn.sadd::<_, _, ()>(&index_key, &rule.rule_id).await?;
        }

        Ok(())
    }

    /// Invalidate a specific rule
    #[instrument(skip(self))]
    pub async fn invalidate_rule(&self, ruleset: &str, rule_id: &str) -> Result<bool> {
        let key = Self::rule_key(ruleset, rule_id);
        let deleted = self.delete(&key).await?;

        // Remove from index
        if deleted {
            let index_key = format!("{}:{}:rules", CachePrefix::Ruleset.as_str(), ruleset);
            let mut conn = self.get_conn().await?;
            conn.srem::<_, _, ()>(&index_key, rule_id).await?;
        }

        Ok(deleted)
    }

    /// Invalidate all rules in a ruleset
    #[instrument(skip(self))]
    pub async fn invalidate_ruleset(&self, ruleset: &str) -> Result<i64> {
        let mut conn = self.get_conn().await?;
        let pattern = format!("{}:{}:*", CachePrefix::Rule.as_str(), ruleset);
        let keys: Vec<String> = conn.keys(&pattern).await?;

        if keys.is_empty() {
            return Ok(0);
        }

        let count: i64 = conn.del(keys).await?;

        // Clean up index
        let index_key = format!("{}:{}:rules", CachePrefix::Ruleset.as_str(), ruleset);
        conn.del::<_, ()>(&index_key).await?;

        let _ = self
            .invalidation_tx
            .send(InvalidationEvent::Ruleset(ruleset.to_string()));

        info!("Invalidated {} rules for ruleset {}", count, ruleset);
        Ok(count)
    }

    // ============================================================
    // RULESET CACHING
    // ============================================================

    /// Build ruleset cache key
    fn ruleset_key(name: &str) -> String {
        format!("{}:{}", CachePrefix::Ruleset.as_str(), name)
    }

    /// Cache a ruleset
    #[instrument(skip(self, ruleset))]
    pub async fn cache_ruleset(&self, ruleset: &RulesetDoc) -> Result<()> {
        let key = Self::ruleset_key(&ruleset.name);
        self.set(&key, ruleset, Some(CachePrefix::Ruleset.default_ttl()))
            .await?;

        // Add to global index
        let mut conn = self.get_conn().await?;
        conn.sadd::<_, _, ()>("rulesets:index", &ruleset.name)
            .await?;

        Ok(())
    }

    /// Get a cached ruleset
    #[instrument(skip(self))]
    pub async fn get_cached_ruleset(&self, name: &str) -> Result<RulesetDoc> {
        let key = Self::ruleset_key(name);
        self.get(&key).await
    }

    /// List all cached ruleset names
    pub async fn list_cached_rulesets(&self) -> Result<Vec<String>> {
        let mut conn = self.get_conn().await?;
        let names: Vec<String> = conn.smembers("rulesets:index").await?;
        Ok(names)
    }

    // ============================================================
    // SCAN RESULT CACHING
    // ============================================================

    /// Build scan cache key
    fn scan_key(repo_key: &str, scan_type: &str) -> String {
        format!("{}:{}:{}", CachePrefix::Scan.as_str(), repo_key, scan_type)
    }

    /// Cache scan results
    #[instrument(skip(self, result))]
    pub async fn cache_scan_result<T: Serialize>(
        &self,
        repo_key: &str,
        scan_type: &str,
        result: &T,
        ttl: Option<u64>,
    ) -> Result<()> {
        let key = Self::scan_key(repo_key, scan_type);
        let ttl_secs = ttl.unwrap_or(CachePrefix::Scan.default_ttl());
        self.set(&key, result, Some(ttl_secs)).await?;

        // Update scan queue for scheduling
        let now = chrono::Utc::now().timestamp() as f64;
        let mut conn = self.get_conn().await?;
        conn.zadd::<_, _, _, ()>("queue:scans", repo_key, now)
            .await?;

        Ok(())
    }

    /// Get cached scan result
    #[instrument(skip(self))]
    pub async fn get_scan_result<T: DeserializeOwned>(
        &self,
        repo_key: &str,
        scan_type: &str,
    ) -> Result<Option<T>> {
        let key = Self::scan_key(repo_key, scan_type);
        self.get_opt(&key).await
    }

    /// Invalidate scan results for a repo
    #[instrument(skip(self))]
    pub async fn invalidate_scan_results(&self, repo_key: &str) -> Result<i64> {
        let mut conn = self.get_conn().await?;
        let pattern = format!("{}:{}:*", CachePrefix::Scan.as_str(), repo_key);
        let keys: Vec<String> = conn.keys(&pattern).await?;

        if keys.is_empty() {
            return Ok(0);
        }

        let count: i64 = conn.del(keys).await?;
        Ok(count)
    }

    // ============================================================
    // FLEET STATUS CACHING
    // ============================================================

    /// Build fleet cache key
    fn fleet_key(bot_id: &str) -> String {
        format!("{}:{}", CachePrefix::Fleet.as_str(), bot_id)
    }

    /// Update fleet/bot status
    #[instrument(skip(self, status))]
    pub async fn update_fleet_status<T: Serialize>(&self, bot_id: &str, status: &T) -> Result<()> {
        let key = Self::fleet_key(bot_id);
        self.set(&key, status, Some(CachePrefix::Fleet.default_ttl()))
            .await?;

        // Update active bots set
        let now = chrono::Utc::now().timestamp() as f64;
        let mut conn = self.get_conn().await?;
        conn.zadd::<_, _, _, ()>("fleet:active", bot_id, now)
            .await?;

        Ok(())
    }

    /// Get fleet status for a bot
    #[instrument(skip(self))]
    pub async fn get_fleet_status<T: DeserializeOwned>(&self, bot_id: &str) -> Result<Option<T>> {
        let key = Self::fleet_key(bot_id);
        self.get_opt(&key).await
    }

    /// Get all active bots
    pub async fn get_active_bots(&self) -> Result<Vec<String>> {
        let mut conn = self.get_conn().await?;
        let bots: Vec<String> = conn.zrange("fleet:active", 0, -1).await?;
        Ok(bots)
    }

    /// Get bots that haven't reported recently
    pub async fn get_stale_bots(&self, threshold_seconds: i64) -> Result<Vec<String>> {
        let mut conn = self.get_conn().await?;
        let cutoff = chrono::Utc::now().timestamp() - threshold_seconds;
        let bots: Vec<String> = conn
            .zrangebyscore("fleet:active", "-inf", cutoff)
            .await?;
        Ok(bots)
    }

    // ============================================================
    // PUB/SUB FOR INVALIDATION
    // ============================================================

    /// Publish invalidation event to other cache clients
    #[instrument(skip(self))]
    pub async fn publish_invalidation(&self, event: &InvalidationEvent) -> Result<()> {
        let mut conn = self.get_conn().await?;
        let channel = "cache:invalidation";
        let message = match event {
            InvalidationEvent::Key(key) => format!("key:{}", key),
            InvalidationEvent::Pattern(pattern) => format!("pattern:{}", pattern),
            InvalidationEvent::Ruleset(name) => format!("ruleset:{}", name),
            InvalidationEvent::FlushAll => "flush:all".to_string(),
        };
        conn.publish::<_, _, ()>(channel, message).await?;
        Ok(())
    }

    // ============================================================
    // CACHE STATISTICS
    // ============================================================

    /// Get cache statistics
    #[instrument(skip(self))]
    pub async fn get_stats(&self) -> Result<CacheStats> {
        let mut conn = self.get_conn().await?;

        let total_keys: i64 = redis::cmd("DBSIZE")
            .query_async(&mut *conn)
            .await
            .unwrap_or(0);

        let rule_keys: Vec<String> = conn
            .keys(format!("{}:*", CachePrefix::Rule.as_str()))
            .await
            .unwrap_or_default();
        let ruleset_keys: Vec<String> = conn
            .keys(format!("{}:*", CachePrefix::Ruleset.as_str()))
            .await
            .unwrap_or_default();
        let scan_keys: Vec<String> = conn
            .keys(format!("{}:*", CachePrefix::Scan.as_str()))
            .await
            .unwrap_or_default();
        let fleet_keys: Vec<String> = conn
            .keys(format!("{}:*", CachePrefix::Fleet.as_str()))
            .await
            .unwrap_or_default();

        // Get memory info
        let info: String = redis::cmd("INFO")
            .arg("memory")
            .query_async(&mut *conn)
            .await
            .unwrap_or_default();
        let memory_used = info
            .lines()
            .find(|l| l.starts_with("used_memory:"))
            .and_then(|l| l.split(':').nth(1))
            .and_then(|s| s.parse().ok())
            .unwrap_or(0);

        Ok(CacheStats {
            total_keys,
            rule_count: rule_keys.len() as i64,
            ruleset_count: ruleset_keys.len() as i64,
            scan_count: scan_keys.len() as i64,
            fleet_count: fleet_keys.len() as i64,
            memory_used_bytes: memory_used,
            hit_rate: 0.0, // Would require tracking
            miss_rate: 0.0,
        })
    }

    /// Ping the cache server
    pub async fn ping(&self) -> Result<()> {
        let mut conn = self.get_conn().await?;
        redis::cmd("PING")
            .query_async::<_, String>(&mut *conn)
            .await?;
        Ok(())
    }

    /// Flush entire cache (use with caution)
    #[instrument(skip(self))]
    pub async fn flush_all(&self) -> Result<()> {
        warn!("Flushing entire cache!");
        let mut conn = self.get_conn().await?;
        redis::cmd("FLUSHDB").query_async::<_, ()>(&mut *conn).await?;
        let _ = self.invalidation_tx.send(InvalidationEvent::FlushAll);
        Ok(())
    }
}

/// Thread-safe cache handle for sharing across async tasks
pub type CacheHandle = Arc<DragonflyCache>;

/// Create a shareable cache handle
pub async fn create_cache_handle(config: DragonflyConfig) -> Result<CacheHandle> {
    let cache = DragonflyCache::new(config).await?;
    Ok(Arc::new(cache))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cache_prefix() {
        assert_eq!(CachePrefix::Rule.as_str(), "rule");
        assert_eq!(CachePrefix::Rule.default_ttl(), 3600);
    }

    #[test]
    fn test_rule_key_generation() {
        let key = DragonflyCache::rule_key("rsr", "RSR-001");
        assert_eq!(key, "rule:rsr:RSR-001");
    }

    #[test]
    fn test_ruleset_key_generation() {
        let key = DragonflyCache::ruleset_key("openssf");
        assert_eq!(key, "ruleset:openssf");
    }

    #[test]
    fn test_scan_key_generation() {
        let key = DragonflyCache::scan_key("github:hyperpolymath:bunsenite", "security");
        assert_eq!(key, "scan:github:hyperpolymath:bunsenite:security");
    }

    #[tokio::test]
    #[ignore]
    async fn test_cache_integration() {
        let config = DragonflyConfig::default();
        let cache = DragonflyCache::new(config).await.unwrap();
        assert!(cache.ping().await.is_ok());
    }
}
