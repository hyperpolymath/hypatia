// SPDX-License-Identifier: PMPL-1.0-or-later
//! Dragonfly (Redis-compatible) client for cicd-hyper-a caching

use crate::error::{DataError, Result};
use crate::models::*;
use crate::DragonflyConfig;
use redis::aio::ConnectionManager;
use redis::{AsyncCommands, Client};
use serde::{de::DeserializeOwned, Serialize};
use std::sync::Arc;
use tracing::{debug, info, warn};

/// Dragonfly client with connection management
pub struct DragonflyClient {
    config: DragonflyConfig,
    conn: Arc<tokio::sync::RwLock<Option<ConnectionManager>>>,
}

impl DragonflyClient {
    /// Create new Dragonfly client
    pub async fn new(config: DragonflyConfig) -> Result<Self> {
        info!("Connecting to Dragonfly at {}", config.url);

        let client_builder = Client::open(config.url.clone())
            .map_err(|e| DataError::ConnectionError(e.to_string()))?;

        let conn_manager = ConnectionManager::new(client_builder)
            .await
            .map_err(|e| DataError::ConnectionError(e.to_string()))?;

        Ok(Self {
            config,
            conn: Arc::new(tokio::sync::RwLock::new(Some(conn_manager))),
        })
    }

    /// Get connection manager
    async fn get_conn(&self) -> Result<ConnectionManager> {
        let guard = self.conn.read().await;
        guard
            .clone()
            .ok_or_else(|| DataError::ConnectionError("No connection available".into()))
    }

    /// Ping the server
    pub async fn ping(&self) -> Result<()> {
        let mut conn = self.get_conn().await?;
        redis::cmd("PING")
            .query_async::<_, String>(&mut conn)
            .await?;
        Ok(())
    }

    // ============================================================
    // BASIC KEY-VALUE OPERATIONS
    // ============================================================

    /// Set a value with TTL
    pub async fn set<T: Serialize>(&self, key: &str, value: &T, ttl: Option<u64>) -> Result<()> {
        let mut conn = self.get_conn().await?;
        let json = serde_json::to_string(value)?;
        let ttl_secs = ttl.unwrap_or(self.config.default_ttl);

        conn.set_ex::<_, _, ()>(key, json, ttl_secs).await?;
        debug!("Set key {} with TTL {}s", key, ttl_secs);
        Ok(())
    }

    /// Get a value
    pub async fn get<T: DeserializeOwned>(&self, key: &str) -> Result<T> {
        let mut conn = self.get_conn().await?;
        let json: String = conn
            .get(key)
            .await
            .map_err(|_| DataError::CacheMiss(key.to_string()))?;
        let value = serde_json::from_str(&json)?;
        Ok(value)
    }

    /// Delete a key
    pub async fn delete(&self, key: &str) -> Result<()> {
        let mut conn = self.get_conn().await?;
        conn.del::<_, ()>(key).await?;
        Ok(())
    }

    /// Check if key exists
    pub async fn exists(&self, key: &str) -> Result<bool> {
        let mut conn = self.get_conn().await?;
        let exists: bool = conn.exists(key).await?;
        Ok(exists)
    }

    /// Get TTL for a key
    pub async fn ttl(&self, key: &str) -> Result<i64> {
        let mut conn = self.get_conn().await?;
        let ttl: i64 = conn.ttl(key).await?;
        Ok(ttl)
    }

    // ============================================================
    // RULE CACHING
    // ============================================================

    /// Cache a compiled rule
    pub async fn cache_rule(&self, rule: &CachedRule) -> Result<()> {
        let key = format!("rule:{}:{}", rule.ruleset, rule.rule_id);
        self.set(&key, rule, Some(rule.ttl as u64)).await
    }

    /// Get a cached rule
    pub async fn get_cached_rule(&self, ruleset: &str, rule_id: &str) -> Result<CachedRule> {
        let key = format!("rule:{}:{}", ruleset, rule_id);
        self.get(&key).await
    }

    /// Invalidate all rules in a ruleset
    pub async fn invalidate_ruleset(&self, ruleset: &str) -> Result<i64> {
        let mut conn = self.get_conn().await?;
        let pattern = format!("rule:{}:*", ruleset);
        let keys: Vec<String> = conn.keys(&pattern).await?;

        if keys.is_empty() {
            return Ok(0);
        }

        let count: i64 = conn.del(keys.clone()).await?;
        info!("Invalidated {} rules for ruleset {}", count, ruleset);
        Ok(count)
    }

    // ============================================================
    // REPO STATE CACHING
    // ============================================================

    /// Cache repo state
    pub async fn cache_repo_state(&self, state: &CachedRepoState) -> Result<()> {
        let key = format!("repo:{}", state.key);
        // Repo state has shorter TTL (5 minutes)
        self.set(&key, state, Some(300)).await
    }

    /// Get cached repo state
    pub async fn get_repo_state(&self, repo_key: &str) -> Result<CachedRepoState> {
        let key = format!("repo:{}", repo_key);
        self.get(&key).await
    }

    // ============================================================
    // SORTED SETS (Priority Queues)
    // ============================================================

    /// Add to alert priority queue
    pub async fn queue_alert(&self, alert_key: &str, severity_score: f64) -> Result<()> {
        let mut conn = self.get_conn().await?;
        conn.zadd::<_, _, _, ()>("queue:alerts", alert_key, severity_score)
            .await?;
        Ok(())
    }

    /// Get top N alerts by priority
    pub async fn get_priority_alerts(&self, count: isize) -> Result<Vec<String>> {
        let mut conn = self.get_conn().await?;
        let alerts: Vec<String> = conn.zrevrange("queue:alerts", 0, count - 1).await?;
        Ok(alerts)
    }

    /// Remove from alert queue
    pub async fn dequeue_alert(&self, alert_key: &str) -> Result<()> {
        let mut conn = self.get_conn().await?;
        conn.zrem::<_, _, ()>("queue:alerts", alert_key).await?;
        Ok(())
    }

    /// Add to fix queue (by timestamp)
    pub async fn queue_fix(&self, job_id: &str, timestamp: f64) -> Result<()> {
        let mut conn = self.get_conn().await?;
        conn.zadd::<_, _, _, ()>("queue:fixes", job_id, timestamp)
            .await?;
        Ok(())
    }

    /// Get pending fixes
    pub async fn get_pending_fixes(&self, count: isize) -> Result<Vec<String>> {
        let mut conn = self.get_conn().await?;
        let fixes: Vec<String> = conn.zrange("queue:fixes", 0, count - 1).await?;
        Ok(fixes)
    }

    /// Add to scan queue
    pub async fn queue_scan(&self, repo_key: &str, last_scanned: f64) -> Result<()> {
        let mut conn = self.get_conn().await?;
        conn.zadd::<_, _, _, ()>("queue:scans", repo_key, last_scanned)
            .await?;
        Ok(())
    }

    /// Get repos needing scan (oldest first)
    pub async fn get_scan_queue(&self, count: isize) -> Result<Vec<String>> {
        let mut conn = self.get_conn().await?;
        let repos: Vec<String> = conn.zrange("queue:scans", 0, count - 1).await?;
        Ok(repos)
    }

    // ============================================================
    // PUB/SUB
    // ============================================================

    /// Publish alert event
    pub async fn publish_alert(&self, alert: &AlertDoc) -> Result<()> {
        let mut conn = self.get_conn().await?;
        let json = serde_json::to_string(alert)?;
        conn.publish::<_, _, ()>("alerts:new", json).await?;
        debug!("Published alert to alerts:new channel");
        Ok(())
    }

    /// Publish fix event
    pub async fn publish_fix(&self, alert_key: &str, rule_key: &str) -> Result<()> {
        let mut conn = self.get_conn().await?;
        let json = serde_json::json!({
            "alert_key": alert_key,
            "rule_key": rule_key,
            "timestamp": chrono::Utc::now().timestamp()
        })
        .to_string();
        conn.publish::<_, _, ()>("alerts:fixed", json).await?;
        Ok(())
    }

    /// Publish rule triggered event
    pub async fn publish_rule_triggered(&self, rule_key: &str, repo_key: &str) -> Result<()> {
        let mut conn = self.get_conn().await?;
        let json = serde_json::json!({
            "rule_key": rule_key,
            "repo_key": repo_key,
            "timestamp": chrono::Utc::now().timestamp()
        })
        .to_string();
        conn.publish::<_, _, ()>("rules:triggered", json).await?;
        Ok(())
    }

    /// Publish scan complete event
    pub async fn publish_scan_complete(&self, repo_key: &str, alert_count: i32) -> Result<()> {
        let mut conn = self.get_conn().await?;
        let json = serde_json::json!({
            "repo_key": repo_key,
            "alert_count": alert_count,
            "timestamp": chrono::Utc::now().timestamp()
        })
        .to_string();
        conn.publish::<_, _, ()>("scans:complete", json).await?;
        Ok(())
    }

    // ============================================================
    // RATE LIMITING
    // ============================================================

    /// Check and increment rate limit
    pub async fn check_rate_limit(&self, key: &str, limit: i64, window_secs: u64) -> Result<bool> {
        let mut conn = self.get_conn().await?;
        let rate_key = format!("rl:{}", key);

        let current: Option<i64> = conn.get(&rate_key).await.ok();
        let count = current.unwrap_or(0);

        if count >= limit {
            warn!("Rate limit exceeded for {}: {}/{}", key, count, limit);
            return Ok(false);
        }

        // Increment and set expiry
        let new_count: i64 = conn.incr(&rate_key, 1).await?;
        if new_count == 1 {
            conn.expire::<_, ()>(&rate_key, window_secs as i64).await?;
        }

        Ok(true)
    }

    // ============================================================
    // JOB QUEUE
    // ============================================================

    /// Push job to queue
    pub async fn push_job(&self, job: &QueuedJob) -> Result<()> {
        let mut conn = self.get_conn().await?;
        let json = serde_json::to_string(job)?;
        let key = format!("job:{}", job.id);

        // Store job details
        conn.set::<_, _, ()>(&key, json).await?;

        // Add to processing queue
        conn.lpush::<_, _, ()>("jobs:pending", &job.id).await?;

        Ok(())
    }

    /// Pop job from queue
    pub async fn pop_job(&self) -> Result<Option<QueuedJob>> {
        let mut conn = self.get_conn().await?;

        // Blocking pop with timeout
        let job_id: Option<String> = conn.rpop("jobs:pending", None).await?;

        match job_id {
            Some(id) => {
                let key = format!("job:{}", id);
                let json: String = conn.get(&key).await?;
                let job: QueuedJob = serde_json::from_str(&json)?;
                Ok(Some(job))
            }
            None => Ok(None),
        }
    }

    /// Update job status
    pub async fn update_job_status(&self, job_id: &str, status: &str) -> Result<()> {
        let mut conn = self.get_conn().await?;
        let key = format!("job:{}", job_id);

        let json: String = conn.get(&key).await?;
        let mut job: QueuedJob = serde_json::from_str(&json)?;
        job.status = status.to_string();

        if status == "running" {
            job.started_at = Some(chrono::Utc::now().timestamp());
        } else if status == "completed" || status == "failed" {
            job.completed_at = Some(chrono::Utc::now().timestamp());
        }

        let updated = serde_json::to_string(&job)?;
        conn.set::<_, _, ()>(&key, updated).await?;

        Ok(())
    }

    /// Publish generic event to a channel
    pub async fn publish_event(&self, channel: &str, message: &str) -> Result<()> {
        let mut conn = self.get_conn().await?;
        conn.publish::<_, _, ()>(channel, message).await?;
        Ok(())
    }

    /// Queue a job (alias for push_job compatibility with service layer)
    pub async fn queue_job(&self, job: &QueuedJob) -> Result<()> {
        self.push_job(job).await
    }

    /// Dequeue a job (alias for pop_job compatibility with service layer)
    pub async fn dequeue_job(&self, _queue: &str) -> Result<Option<QueuedJob>> {
        self.pop_job().await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Integration tests require running Dragonfly
    #[tokio::test]
    #[ignore]
    async fn test_client_creation() {
        let config = DragonflyConfig::default();
        let client = DragonflyClient::new(config).await.unwrap();
        assert!(client.ping().await.is_ok());
    }
}
