// SPDX-License-Identifier: PLMP-1.0-or-later
//! Service layer integrating forge adapters with data layer
//!
//! This module provides the seam between forge operations and persistence,
//! ensuring all forge data flows seamlessly to ArangoDB and Dragonfly.

use crate::error::{AdapterError, Result};
use crate::forge::{Alert, ForgeAdapter, Repository, Severity};
use data::{
    AlertDoc, CachedRepoState, CachedRule, DataConfig, DataLayer, QueuedJob, RepoDoc, RuleDoc,
};
use std::sync::Arc;
use tokio::sync::RwLock;

/// Unified service combining forge adapters with data persistence
pub struct ForgeService {
    adapters: Vec<Arc<dyn ForgeAdapter + Send + Sync>>,
    data: DataLayer,
    rule_cache: Arc<RwLock<Vec<CachedRule>>>,
}

impl ForgeService {
    /// Create new forge service with data layer
    pub async fn new(
        adapters: Vec<Arc<dyn ForgeAdapter + Send + Sync>>,
        data_config: DataConfig,
    ) -> Result<Self> {
        let data = DataLayer::new(data_config)
            .await
            .map_err(|e| AdapterError::ConfigError(format!("Data layer init failed: {}", e)))?;

        Ok(Self {
            adapters,
            data,
            rule_cache: Arc::new(RwLock::new(Vec::new())),
        })
    }

    /// Create with existing data layer (for testing)
    pub fn with_data_layer(
        adapters: Vec<Arc<dyn ForgeAdapter + Send + Sync>>,
        data: DataLayer,
    ) -> Self {
        Self {
            adapters,
            data,
            rule_cache: Arc::new(RwLock::new(Vec::new())),
        }
    }

    /// Sync repository from forge to data layer
    pub async fn sync_repo(&self, owner: &str, repo_name: &str) -> Result<SyncResult> {
        let mut result = SyncResult::default();

        for adapter in &self.adapters {
            // Find repo in forge
            let repos = adapter.list_repos(owner).await?;
            if let Some(repo) = repos.into_iter().find(|r| r.name == repo_name) {
                // Store repo in ArangoDB
                let repo_doc = self.repo_to_doc(&repo);
                self.data
                    .arango
                    .upsert_document("repos", &repo_doc)
                    .await
                    .map_err(|e| AdapterError::ApiError(format!("ArangoDB error: {}", e)))?;
                result.repos_synced += 1;

                // Cache repo state in Dragonfly
                let state = CachedRepoState {
                    key: format!("repos/{}", repo.id),
                    repo_key: format!("repos/{}", repo.id),
                    health_score: 50, // Default score, will be calculated
                    alert_count: 0,
                    rulesets_applied: vec![],
                    last_scan: chrono::Utc::now().timestamp(),
                    pending_fixes: 0,
                    cached_at: chrono::Utc::now().timestamp(),
                };
                self.data
                    .dragonfly
                    .cache_repo_state(&state)
                    .await
                    .map_err(|e| AdapterError::ApiError(format!("Dragonfly error: {}", e)))?;

                // Fetch and sync alerts
                let alerts = adapter.get_alerts(owner, repo_name).await?;
                for alert in &alerts {
                    let alert_doc = self.alert_to_doc(alert, &repo.id);
                    self.data
                        .arango
                        .upsert_document("alerts", &alert_doc)
                        .await
                        .map_err(|e| AdapterError::ApiError(format!("ArangoDB error: {}", e)))?;

                    // Queue high-severity alerts for processing
                    if matches!(alert.severity, Severity::Critical | Severity::High) {
                        let score = self.severity_to_score(&alert.severity);
                        self.data
                            .dragonfly
                            .queue_alert(&alert_doc.key, score)
                            .await
                            .map_err(|e| {
                                AdapterError::ApiError(format!("Dragonfly error: {}", e))
                            })?;
                    }
                    result.alerts_synced += 1;
                }

                // Fetch and sync workflows
                let workflows = adapter.list_workflows(owner, repo_name).await?;
                for _workflow in &workflows {
                    result.workflows_synced += 1;
                }

                // Found and synced, break
                break;
            }
        }

        Ok(result)
    }

    /// Apply rule to alert and persist the fix
    pub async fn apply_fix(&self, alert_key: &str, rule_key: &str, confidence: f64) -> Result<()> {
        // Record fix application in ArangoDB
        self.data
            .arango
            .apply_fix(alert_key, rule_key, confidence)
            .await
            .map_err(|e| AdapterError::ApiError(format!("ArangoDB error: {}", e)))?;

        // Publish fix event via Dragonfly pub/sub
        self.data
            .dragonfly
            .publish_event(
                "cicd:events",
                &format!(
                    r#"{{"type":"fix_applied","alert":"{}","rule":"{}","confidence":{}}}"#,
                    alert_key, rule_key, confidence
                ),
            )
            .await
            .map_err(|e| AdapterError::ApiError(format!("Dragonfly error: {}", e)))?;

        Ok(())
    }

    /// Get matching rules for an alert using cached rules
    pub async fn get_matching_rules(&self, _alert: &Alert) -> Result<Vec<CachedRule>> {
        let cache = self.rule_cache.read().await;

        // Filter rules that match alert category
        let matching: Vec<CachedRule> = cache
            .iter()
            .filter(|r| {
                // Simple category matching - in production would use proper pattern matching
                r.effect == "curative" || r.effect == "diagnostic"
            })
            .cloned()
            .collect();

        Ok(matching)
    }

    /// Refresh rule cache from data layer
    pub async fn refresh_rule_cache(&self) -> Result<usize> {
        // Fetch all enabled rules from ArangoDB
        let rules: Vec<RuleDoc> = self
            .data
            .arango
            .query_documents("FOR r IN rules FILTER r.enabled == true RETURN r")
            .await
            .map_err(|e| AdapterError::ApiError(format!("ArangoDB error: {}", e)))?;

        // Convert to cached format and store in Dragonfly
        let mut cache = self.rule_cache.write().await;
        cache.clear();

        for rule in rules {
            let cached = CachedRule {
                key: rule.key.clone(),
                name: rule.name.clone(),
                ruleset: String::new(),
                rule_id: rule.key.clone(),
                effect: rule.effect.clone(),
                trigger_pattern: rule.trigger_pattern.clone().unwrap_or_default(),
                confidence: rule.confidence,
                condition_bytecode: vec![],
                action_bytecode: vec![],
                cached_at: chrono::Utc::now().timestamp(),
                ttl: 3600,
            };

            // Cache in Dragonfly
            self.data
                .dragonfly
                .cache_rule(&cached)
                .await
                .map_err(|e| AdapterError::ApiError(format!("Dragonfly error: {}", e)))?;

            cache.push(cached);
        }

        Ok(cache.len())
    }

    /// Queue a job for async processing
    pub async fn queue_job(&self, job: QueuedJob) -> Result<()> {
        self.data
            .dragonfly
            .queue_job(&job)
            .await
            .map_err(|e| AdapterError::ApiError(format!("Dragonfly error: {}", e)))?;
        Ok(())
    }

    /// Get next job from queue
    pub async fn dequeue_job(&self, queue: &str) -> Result<Option<QueuedJob>> {
        let job = self
            .data
            .dragonfly
            .dequeue_job(queue)
            .await
            .map_err(|e| AdapterError::ApiError(format!("Dragonfly error: {}", e)))?;
        Ok(job)
    }

    /// Health check for all services
    pub async fn health_check(&self) -> Result<ServiceHealth> {
        let data_health = self
            .data
            .health_check()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Health check failed: {}", e)))?;

        let adapters_ok = !self.adapters.is_empty();

        Ok(ServiceHealth {
            arangodb: data_health.arangodb,
            dragonfly: data_health.dragonfly,
            adapters: if adapters_ok { "pass" } else { "fail" }.to_string(),
            overall: if data_health.overall == "healthy" && adapters_ok {
                "healthy"
            } else {
                "degraded"
            }
            .to_string(),
        })
    }

    // Internal conversion helpers

    fn repo_to_doc(&self, repo: &Repository) -> RepoDoc {
        RepoDoc {
            key: repo.id.clone(),
            forge: format!("{:?}", repo.forge).to_lowercase(),
            owner: repo.owner.clone(),
            name: repo.name.clone(),
            url: repo.url.clone(),
            default_branch: repo.default_branch.clone(),
            visibility: String::new(),
            health_score: 50, // Default, will be calculated
            languages: repo.languages.clone(),
            last_scan: chrono::Utc::now().to_rfc3339(),
            scorecard_score: None,
            alert_count: 0,
        }
    }

    fn alert_to_doc(&self, alert: &Alert, repo_id: &str) -> AlertDoc {
        AlertDoc {
            key: format!("{}-{}", repo_id, alert.id),
            alert_id: alert.id.clone(),
            repo_key: format!("repos/{}", repo_id),
            rule_id: alert.rule_id.clone(),
            severity: format!("{:?}", alert.severity).to_lowercase(),
            category: format!("{:?}", alert.category),
            description: alert.description.clone(),
            file: alert.file.clone(),
            line: alert.line,
            fix_applied: false,
            auto_fixable: alert.auto_fixable,
            created_at: chrono::Utc::now().to_rfc3339(),
            dismissed_at: None,
        }
    }

    fn severity_to_score(&self, severity: &Severity) -> f64 {
        match severity {
            Severity::Critical => 100.0,
            Severity::High => 75.0,
            Severity::Medium => 50.0,
            Severity::Low => 25.0,
            Severity::Info => 10.0,
        }
    }
}

/// Result of a sync operation
#[derive(Debug, Default)]
pub struct SyncResult {
    pub repos_synced: usize,
    pub alerts_synced: usize,
    pub workflows_synced: usize,
}

/// Health status for entire service
#[derive(Debug)]
pub struct ServiceHealth {
    pub arangodb: String,
    pub dragonfly: String,
    pub adapters: String,
    pub overall: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    // Integration tests would go here with mock data layer
}
