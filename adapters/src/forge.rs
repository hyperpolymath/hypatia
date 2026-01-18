// SPDX-License-Identifier: AGPL-3.0-or-later
//! Common forge traits and types
//!
//! This module defines the unified interface for all forge adapters,
//! ensuring consistent behavior across GitHub, GitLab, Bitbucket, Codeberg,
//! Sourcehut, Gitea, and Radicle.

use crate::error::Result;
use async_trait::async_trait;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// Supported forge types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Forge {
    GitHub,
    GitLab,
    Bitbucket,
    Codeberg,
    Sourcehut,
    Gitea,
    Radicle,
}

impl std::fmt::Display for Forge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Forge::GitHub => write!(f, "github"),
            Forge::GitLab => write!(f, "gitlab"),
            Forge::Bitbucket => write!(f, "bitbucket"),
            Forge::Codeberg => write!(f, "codeberg"),
            Forge::Sourcehut => write!(f, "sourcehut"),
            Forge::Gitea => write!(f, "gitea"),
            Forge::Radicle => write!(f, "radicle"),
        }
    }
}

/// Repository information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Repository {
    pub id: String,
    pub name: String,
    pub owner: String,
    pub forge: Forge,
    pub url: String,
    pub visibility: Visibility,
    pub default_branch: String,
    pub languages: Vec<String>,
}

/// Repository visibility
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Visibility {
    Public,
    Private,
    Internal,
}

/// Security alert
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Alert {
    pub id: String,
    pub rule_id: String,
    pub severity: Severity,
    pub category: AlertCategory,
    pub description: String,
    pub file: Option<String>,
    pub line: Option<u32>,
    pub auto_fixable: bool,
}

/// Alert severity
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Severity {
    Info,
    Low,
    Medium,
    High,
    Critical,
}

/// Alert category
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum AlertCategory {
    WorkflowSecurity,
    CodeSecurity,
    CodeQuality,
    DependencyVuln,
    ProcessHygiene,
    MissingTests,
}

/// Workflow information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Workflow {
    pub id: String,
    pub name: String,
    pub file: String,
    pub state: WorkflowState,
}

/// Workflow state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum WorkflowState {
    Active,
    Disabled,
    Unknown,
}

/// Workflow run information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowRun {
    pub id: String,
    pub workflow_id: String,
    pub name: String,
    pub status: RunStatus,
    pub conclusion: Option<RunConclusion>,
    pub head_branch: String,
    pub head_sha: String,
    pub url: String,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

/// Workflow run status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RunStatus {
    Queued,
    InProgress,
    Completed,
    Waiting,
    Requested,
    Pending,
}

/// Workflow run conclusion
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RunConclusion {
    Success,
    Failure,
    Neutral,
    Cancelled,
    Skipped,
    TimedOut,
    ActionRequired,
}

/// Issue information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Issue {
    pub id: String,
    pub number: u64,
    pub title: String,
    pub body: Option<String>,
    pub state: IssueState,
    pub author: String,
    pub labels: Vec<String>,
    pub url: String,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

/// Issue state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum IssueState {
    Open,
    Closed,
}

/// Comment on an issue or PR
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Comment {
    pub id: String,
    pub body: String,
    pub author: String,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

/// Pull request / Merge request information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PullRequest {
    pub id: String,
    pub number: u64,
    pub title: String,
    pub body: Option<String>,
    pub state: PullRequestState,
    pub author: String,
    pub head_branch: String,
    pub base_branch: String,
    pub url: String,
    pub mergeable: Option<bool>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

/// Pull request state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum PullRequestState {
    Open,
    Closed,
    Merged,
}

/// Check run information (GitHub check runs, GitLab pipelines, etc.)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CheckRun {
    pub id: String,
    pub name: String,
    pub status: CheckStatus,
    pub conclusion: Option<CheckConclusion>,
    pub head_sha: String,
    pub url: Option<String>,
    pub started_at: Option<DateTime<Utc>>,
    pub completed_at: Option<DateTime<Utc>>,
}

/// Check run status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CheckStatus {
    Queued,
    InProgress,
    Completed,
}

/// Check run conclusion
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CheckConclusion {
    Success,
    Failure,
    Neutral,
    Cancelled,
    Skipped,
    TimedOut,
    ActionRequired,
}

/// Webhook event types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum WebhookEvent {
    Push,
    PullRequest,
    PullRequestReview,
    Issues,
    IssueComment,
    Create,
    Delete,
    WorkflowRun,
    CheckRun,
    CheckSuite,
    Release,
    Custom,
}

/// Webhook configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WebhookConfig {
    pub id: Option<String>,
    pub url: String,
    pub events: Vec<WebhookEvent>,
    pub active: bool,
    pub secret: Option<String>,
    pub content_type: String,
}

/// Webhook payload wrapper
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WebhookPayload {
    pub event: WebhookEvent,
    pub delivery_id: String,
    pub signature: Option<String>,
    pub payload: serde_json::Value,
}

/// Common forge adapter trait
///
/// All forge adapters must implement this trait to provide unified access
/// to forge operations. Methods are designed to be async and handle
/// forge-specific API differences internally.
#[async_trait]
pub trait ForgeAdapter: Send + Sync {
    // ============== Core Identity ==============

    /// Get forge type
    fn forge(&self) -> Forge;

    /// Get the base URL for this adapter instance
    fn base_url(&self) -> &str;

    // ============== Repository Operations ==============

    /// List repositories for an organization/user
    async fn list_repos(&self, owner: &str) -> Result<Vec<Repository>>;

    /// Get a single repository by owner and name
    async fn get_repo(&self, owner: &str, repo: &str) -> Result<Repository>;

    // ============== Security & Alerts ==============

    /// Get security alerts for a repository
    async fn get_alerts(&self, owner: &str, repo: &str) -> Result<Vec<Alert>>;

    // ============== Workflow/CI Operations ==============

    /// List workflows for a repository
    async fn list_workflows(&self, owner: &str, repo: &str) -> Result<Vec<Workflow>>;

    /// Deploy a workflow file
    async fn deploy_workflow(
        &self,
        owner: &str,
        repo: &str,
        path: &str,
        content: &str,
        message: &str,
    ) -> Result<()>;

    /// Trigger a workflow run
    async fn trigger_workflow(
        &self,
        owner: &str,
        repo: &str,
        workflow: &str,
        ref_name: &str,
    ) -> Result<()>;

    /// List workflow runs for a repository
    async fn list_workflow_runs(
        &self,
        owner: &str,
        repo: &str,
        workflow_id: Option<&str>,
    ) -> Result<Vec<WorkflowRun>>;

    /// Get a specific workflow run
    async fn get_workflow_run(
        &self,
        owner: &str,
        repo: &str,
        run_id: &str,
    ) -> Result<WorkflowRun>;

    // ============== Branch Protection ==============

    /// Enable branch protection
    async fn enable_branch_protection(&self, owner: &str, repo: &str, branch: &str) -> Result<()>;

    // ============== Pull Requests / Merge Requests ==============

    /// Create a pull request
    async fn create_pr(
        &self,
        owner: &str,
        repo: &str,
        title: &str,
        body: &str,
        head: &str,
        base: &str,
    ) -> Result<String>;

    /// List pull requests for a repository
    async fn list_prs(
        &self,
        owner: &str,
        repo: &str,
        state: Option<PullRequestState>,
    ) -> Result<Vec<PullRequest>>;

    /// Get a specific pull request
    async fn get_pr(&self, owner: &str, repo: &str, number: u64) -> Result<PullRequest>;

    /// Merge a pull request
    async fn merge_pr(
        &self,
        owner: &str,
        repo: &str,
        number: u64,
        commit_message: Option<&str>,
    ) -> Result<()>;

    /// Close a pull request without merging
    async fn close_pr(&self, owner: &str, repo: &str, number: u64) -> Result<()>;

    // ============== Issues ==============

    /// Create an issue
    async fn create_issue(
        &self,
        owner: &str,
        repo: &str,
        title: &str,
        body: &str,
        labels: Vec<String>,
    ) -> Result<Issue>;

    /// List issues for a repository
    async fn list_issues(
        &self,
        owner: &str,
        repo: &str,
        state: Option<IssueState>,
    ) -> Result<Vec<Issue>>;

    /// Get a specific issue
    async fn get_issue(&self, owner: &str, repo: &str, number: u64) -> Result<Issue>;

    /// Update an issue
    async fn update_issue(
        &self,
        owner: &str,
        repo: &str,
        number: u64,
        title: Option<&str>,
        body: Option<&str>,
        state: Option<IssueState>,
        labels: Option<Vec<String>>,
    ) -> Result<Issue>;

    /// Close an issue
    async fn close_issue(&self, owner: &str, repo: &str, number: u64) -> Result<()>;

    // ============== Comments ==============

    /// Add a comment to an issue
    async fn add_issue_comment(
        &self,
        owner: &str,
        repo: &str,
        issue_number: u64,
        body: &str,
    ) -> Result<Comment>;

    /// Add a comment to a pull request
    async fn add_pr_comment(
        &self,
        owner: &str,
        repo: &str,
        pr_number: u64,
        body: &str,
    ) -> Result<Comment>;

    /// List comments on an issue
    async fn list_issue_comments(
        &self,
        owner: &str,
        repo: &str,
        issue_number: u64,
    ) -> Result<Vec<Comment>>;

    /// List comments on a pull request
    async fn list_pr_comments(
        &self,
        owner: &str,
        repo: &str,
        pr_number: u64,
    ) -> Result<Vec<Comment>>;

    // ============== Check Runs ==============

    /// Create a check run
    async fn create_check_run(
        &self,
        owner: &str,
        repo: &str,
        name: &str,
        head_sha: &str,
        status: CheckStatus,
        conclusion: Option<CheckConclusion>,
    ) -> Result<CheckRun>;

    /// Update a check run
    async fn update_check_run(
        &self,
        owner: &str,
        repo: &str,
        check_run_id: &str,
        status: Option<CheckStatus>,
        conclusion: Option<CheckConclusion>,
    ) -> Result<CheckRun>;

    /// List check runs for a commit
    async fn list_check_runs(&self, owner: &str, repo: &str, ref_name: &str)
        -> Result<Vec<CheckRun>>;

    // ============== Webhooks ==============

    /// Create a webhook
    async fn create_webhook(
        &self,
        owner: &str,
        repo: &str,
        config: WebhookConfig,
    ) -> Result<WebhookConfig>;

    /// List webhooks for a repository
    async fn list_webhooks(&self, owner: &str, repo: &str) -> Result<Vec<WebhookConfig>>;

    /// Delete a webhook
    async fn delete_webhook(&self, owner: &str, repo: &str, webhook_id: &str) -> Result<()>;

    /// Parse and validate a webhook payload
    fn parse_webhook(
        &self,
        event_type: &str,
        signature: Option<&str>,
        payload: &[u8],
        secret: Option<&str>,
    ) -> Result<WebhookPayload>;
}

/// Default implementations for forges that don't support certain features
/// These can be used as fallbacks in adapter implementations
pub mod defaults {
    use super::*;
    use crate::error::AdapterError;

    /// Default implementation that returns "not supported" error
    pub fn not_supported<T>(feature: &str, forge: Forge) -> Result<T> {
        Err(AdapterError::ApiError(format!(
            "{} is not supported by {}",
            feature, forge
        )))
    }

    /// Default check run for forges without native check run support
    pub fn unsupported_check_runs(forge: Forge) -> Result<Vec<CheckRun>> {
        tracing::warn!("{} does not have native check run support", forge);
        Ok(vec![])
    }

    /// Default webhook parsing for forges without complex signature validation
    pub fn simple_webhook_parse(
        event_type: &str,
        payload: &[u8],
    ) -> Result<WebhookPayload> {
        let event = match event_type {
            "push" => WebhookEvent::Push,
            "pull_request" | "merge_request" => WebhookEvent::PullRequest,
            "issues" | "issue" => WebhookEvent::Issues,
            "issue_comment" | "note" => WebhookEvent::IssueComment,
            "create" => WebhookEvent::Create,
            "delete" => WebhookEvent::Delete,
            "workflow_run" | "pipeline" => WebhookEvent::WorkflowRun,
            "check_run" => WebhookEvent::CheckRun,
            "check_suite" => WebhookEvent::CheckSuite,
            "release" => WebhookEvent::Release,
            _ => WebhookEvent::Custom,
        };

        let payload_json: serde_json::Value = serde_json::from_slice(payload)?;

        Ok(WebhookPayload {
            event,
            delivery_id: uuid::Uuid::new_v4().to_string(),
            signature: None,
            payload: payload_json,
        })
    }
}
