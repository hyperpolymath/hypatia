// SPDX-License-Identifier: PLMP-1.0-or-later
//! Data models shared between ArangoDB and Dragonfly

use serde::{Deserialize, Serialize};

/// Repository document
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RepoDoc {
    #[serde(rename = "_key")]
    pub key: String,
    pub name: String,
    pub forge: String,
    pub owner: String,
    pub url: String,
    #[serde(default)]
    pub default_branch: String,
    #[serde(default)]
    pub visibility: String,
    #[serde(default)]
    pub languages: Vec<String>,
    #[serde(default)]
    pub last_scan: String,
    pub scorecard_score: Option<f64>,
    #[serde(default)]
    pub health_score: i32,
    #[serde(default)]
    pub alert_count: i32,
}

/// Alert document
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlertDoc {
    #[serde(rename = "_key")]
    pub key: String,
    pub alert_id: String,
    pub repo_key: String,
    pub category: String,
    pub severity: String,
    pub rule_id: String,
    pub description: String,
    pub file: Option<String>,
    pub line: Option<u32>,
    pub auto_fixable: bool,
    pub fix_applied: bool,
    pub created_at: String,
    pub dismissed_at: Option<String>,
}

/// Rule document
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RuleDoc {
    #[serde(rename = "_key")]
    pub key: String,
    pub name: String,
    pub effect: String,
    #[serde(default)]
    pub source: String,
    #[serde(default)]
    pub version: String,
    #[serde(default)]
    pub enabled: bool,
    #[serde(default)]
    pub trigger_count: i64,
    #[serde(default)]
    pub success_rate: f64,
    #[serde(default)]
    pub confidence: f64,
    #[serde(default)]
    pub trigger_pattern: Option<String>,
    pub last_triggered: Option<String>,
    #[serde(default)]
    pub condition_json: String,
    #[serde(default)]
    pub action_json: String,
}

/// Ruleset document
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RulesetDoc {
    #[serde(rename = "_key")]
    pub key: String,
    pub name: String,
    pub description: String,
    pub version: String,
    pub author: String,
    pub license: String,
    pub effect: String,
    pub verified: bool,
    pub signature: Option<String>,
    pub created_at: String,
    pub updated_at: String,
    pub downloads: i64,
}

/// Fix document
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FixDoc {
    #[serde(rename = "_key")]
    pub key: String,
    pub fix_type: String,
    pub pattern: Option<String>,
    pub replacement: Option<String>,
    pub applied_count: i64,
    pub reverted_count: i64,
}

/// Edge: repo_has_alert
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RepoHasAlert {
    #[serde(rename = "_from")]
    pub from: String,
    #[serde(rename = "_to")]
    pub to: String,
    pub discovered_at: String,
}

/// Edge: alert_fixed_by
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlertFixedBy {
    #[serde(rename = "_from")]
    pub from: String,
    #[serde(rename = "_to")]
    pub to: String,
    pub fixed_at: String,
    pub confidence: f64,
}

/// Edge: ruleset_contains
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RulesetContains {
    #[serde(rename = "_from")]
    pub from: String,
    #[serde(rename = "_to")]
    pub to: String,
    pub order: i32,
}

/// Cache entry for compiled rules
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CachedRule {
    pub key: String,
    pub name: String,
    #[serde(default)]
    pub ruleset: String,
    #[serde(default)]
    pub rule_id: String,
    pub effect: String,
    #[serde(default)]
    pub trigger_pattern: String,
    #[serde(default)]
    pub confidence: f64,
    #[serde(default)]
    pub condition_bytecode: Vec<u8>,
    #[serde(default)]
    pub action_bytecode: Vec<u8>,
    #[serde(default)]
    pub cached_at: i64,
    #[serde(default)]
    pub ttl: i64,
}

/// Cache entry for repo state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CachedRepoState {
    #[serde(default)]
    pub key: String,
    pub repo_key: String,
    pub health_score: i32,
    #[serde(default)]
    pub alert_count: i32,
    #[serde(default)]
    pub rulesets_applied: Vec<String>,
    pub last_scan: i64,
    #[serde(default)]
    pub pending_fixes: i32,
    #[serde(default)]
    pub cached_at: i64,
}

/// Job in the processing queue
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QueuedJob {
    pub id: String,
    pub job_type: String, // "scan", "fix", "deploy"
    pub repo_key: String,
    pub rule_id: Option<String>,
    pub priority: i32,
    pub created_at: i64,
    pub started_at: Option<i64>,
    pub completed_at: Option<i64>,
    pub status: String, // "pending", "running", "completed", "failed"
    pub result: Option<String>,
}

/// Graph traversal result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraversalResult<T> {
    pub vertices: Vec<T>,
    pub edges: Vec<serde_json::Value>,
    pub paths: Vec<Vec<String>>,
}

/// Aggregation result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AggregationResult {
    pub group: String,
    pub count: i64,
    pub sum: Option<f64>,
    pub avg: Option<f64>,
}
