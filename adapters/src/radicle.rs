// SPDX-License-Identifier: AGPL-3.0-or-later
//! Radicle adapter for cicd-hyper-a
//!
//! Provides integration with Radicle, a sovereign code forge built on Git.
//! Radicle is a peer-to-peer network for code collaboration that uses:
//! - Git for version control
//! - Radicle IDs (DIDs) for identity
//! - COBs (Collaborative Objects) for issues and patches
//!
//! This adapter connects to a local Radicle node's HTTP API.
//!
//! Key Radicle concepts:
//! - Project: A repository with a unique Radicle ID (rad:...)
//! - Patch: Similar to a PR, but stored as a COB
//! - Issue: Stored as a COB
//! - Seed: A node that hosts and replicates projects
//!
//! Note: Radicle is fundamentally different from centralized forges.
//! Many operations require a local node and may be eventually consistent.

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use reqwest::{header::HeaderMap, Client};
use serde::{Deserialize, Serialize};

use crate::error::AdapterError;
use crate::forge::{
    Alert, CheckConclusion, CheckRun, CheckStatus, Comment, Forge, ForgeAdapter, Issue,
    IssueState, PullRequest, PullRequestState, Repository, Visibility, WebhookConfig, WebhookEvent,
    WebhookPayload, Workflow, WorkflowRun,
};

type Result<T> = std::result::Result<T, AdapterError>;

/// Radicle adapter implementing the ForgeAdapter trait
///
/// Connects to a local Radicle node's HTTP API (default: http://127.0.0.1:8080).
/// The Radicle node must be running with the HTTP API enabled.
pub struct RadicleAdapter {
    client: Client,
    node_url: String,
}

impl RadicleAdapter {
    /// Creates a new RadicleAdapter connecting to the local node
    ///
    /// Defaults to http://127.0.0.1:8080
    pub fn new() -> Result<Self> {
        Self::with_node_url("http://127.0.0.1:8080")
    }

    /// Creates a new RadicleAdapter connecting to a specific node
    ///
    /// # Arguments
    /// * `node_url` - URL of the Radicle node HTTP API
    pub fn with_node_url(node_url: impl Into<String>) -> Result<Self> {
        let node_url = node_url.into().trim_end_matches('/').to_string();

        let mut headers = HeaderMap::new();
        headers.insert(
            "Accept",
            "application/json"
                .parse()
                .map_err(|e| AdapterError::ConfigError(format!("Invalid header: {}", e)))?,
        );
        headers.insert(
            "Content-Type",
            "application/json"
                .parse()
                .map_err(|e| AdapterError::ConfigError(format!("Invalid header: {}", e)))?,
        );

        let client = Client::builder()
            .default_headers(headers)
            .user_agent("cicd-hyper-a/1.0")
            .build()
            .map_err(|e| AdapterError::ConfigError(format!("Failed to build HTTP client: {}", e)))?;

        Ok(Self { client, node_url })
    }

    /// Helper to build API URLs
    fn api_url(&self, path: &str) -> String {
        format!("{}/api/v1{}", self.node_url, path)
    }

    /// Convert Radicle ID to URL-safe format
    fn encode_rid(rid: &str) -> String {
        // Radicle IDs look like rad:z3gqcJUoA1n9HaHKufZs5FCSGazv5
        // They can be used directly in URLs
        rid.to_string()
    }

    /// Parse a Radicle ID from owner/repo format
    /// Owner in Radicle context is the DID, repo is the project RID
    fn parse_project_id(owner: &str, repo: &str) -> String {
        // If repo starts with "rad:", use it directly
        if repo.starts_with("rad:") {
            repo.to_string()
        } else {
            // Otherwise, assume it's the project name and construct RID
            // In practice, callers should pass the actual RID
            repo.to_string()
        }
    }
}

impl Default for RadicleAdapter {
    fn default() -> Self {
        Self::new().expect("Failed to create default RadicleAdapter")
    }
}

// Radicle API response types

#[derive(Debug, Deserialize)]
struct RadProject {
    id: String, // rad:z3gqcJUoA1n9...
    name: String,
    description: Option<String>,
    #[serde(rename = "defaultBranch")]
    default_branch: String,
    head: String,
    delegates: Vec<RadDelegate>,
    threshold: u32,
    visibility: RadVisibility,
    #[serde(rename = "seeding")]
    seeding_count: u32,
}

#[derive(Debug, Deserialize)]
struct RadDelegate {
    id: String, // DID
    alias: Option<String>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct RadVisibility {
    #[serde(rename = "type")]
    visibility_type: String, // "public" or "private"
}

#[derive(Debug, Deserialize)]
struct RadPatch {
    id: String,
    title: String,
    #[serde(rename = "state")]
    state: RadPatchState,
    author: RadAuthor,
    target: String,
    labels: Vec<String>,
    revisions: Vec<RadRevision>,
}

#[derive(Debug, Deserialize)]
struct RadPatchState {
    status: String, // "open", "draft", "archived", "merged"
}

#[derive(Debug, Deserialize)]
struct RadRevision {
    id: String,
    description: Option<String>,
    base: String,
    oid: String,
    #[serde(rename = "timestamp")]
    timestamp: i64,
    reviews: Vec<RadReview>,
}

#[derive(Debug, Deserialize)]
struct RadReview {
    author: RadAuthor,
    verdict: Option<String>, // "accept", "reject"
    #[serde(rename = "timestamp")]
    timestamp: i64,
}

#[derive(Debug, Deserialize)]
struct RadIssue {
    id: String,
    title: String,
    state: RadIssueState,
    author: RadAuthor,
    labels: Vec<String>,
    assignees: Vec<RadAuthor>,
    discussion: Vec<RadComment>,
}

#[derive(Debug, Deserialize)]
struct RadIssueState {
    status: String, // "open", "closed"
    reason: Option<String>,
}

#[derive(Debug, Deserialize)]
struct RadAuthor {
    id: String, // DID
    alias: Option<String>,
}

#[derive(Debug, Deserialize)]
struct RadComment {
    id: String,
    body: String,
    author: RadAuthor,
    #[serde(rename = "timestamp")]
    timestamp: i64,
    #[serde(rename = "replyTo")]
    reply_to: Option<String>,
    reactions: Vec<RadReaction>,
    embeds: Vec<RadEmbed>,
}

#[derive(Debug, Deserialize)]
struct RadReaction {
    emoji: String,
    authors: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct RadEmbed {
    name: String,
    content: String,
}

#[derive(Debug, Deserialize)]
struct RadNode {
    id: String,
    agent: String,
    state: String,
}

// Request payloads

#[derive(Debug, Serialize)]
struct CreateIssuePayload {
    title: String,
    description: String,
    labels: Vec<String>,
    assignees: Vec<String>,
    embeds: Vec<EmbedPayload>,
}

#[derive(Debug, Serialize)]
struct EmbedPayload {
    name: String,
    content: String,
}

#[derive(Debug, Serialize)]
struct UpdateIssuePayload {
    #[serde(skip_serializing_if = "Option::is_none")]
    title: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    state: Option<IssueStatePayload>,
}

#[derive(Debug, Serialize)]
struct IssueStatePayload {
    status: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    reason: Option<String>,
}

#[derive(Debug, Serialize)]
struct CreateCommentPayload {
    body: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(rename = "replyTo")]
    reply_to: Option<String>,
    embeds: Vec<EmbedPayload>,
}

#[derive(Debug, Serialize)]
struct CreatePatchPayload {
    title: String,
    description: String,
    target: String,
    oid: String,
    labels: Vec<String>,
}

impl From<RadProject> for Repository {
    fn from(project: RadProject) -> Self {
        let visibility = if project.visibility.visibility_type == "private" {
            Visibility::Private
        } else {
            Visibility::Public
        };
        Repository {
            id: project.id.clone(),
            name: project.name,
            owner: String::new(), // Radicle projects don't have traditional owners
            forge: Forge::Radicle,
            url: format!("rad://{}", project.id),
            visibility,
            default_branch: project.default_branch,
            languages: vec![],
        }
    }
}

impl From<RadPatch> for PullRequest {
    fn from(patch: RadPatch) -> Self {
        let state = match patch.state.status.as_str() {
            "open" | "draft" => PullRequestState::Open,
            "merged" => PullRequestState::Merged,
            _ => PullRequestState::Closed,
        };

        let created_at = patch
            .revisions
            .first()
            .map(|r| DateTime::from_timestamp(r.timestamp, 0).unwrap_or_default())
            .unwrap_or_else(Utc::now);

        let updated_at = patch
            .revisions
            .last()
            .map(|r| DateTime::from_timestamp(r.timestamp, 0).unwrap_or_default())
            .unwrap_or(created_at);

        PullRequest {
            id: patch.id.clone(),
            number: 0, // Radicle uses string IDs, not numbers
            title: patch.title,
            body: patch.revisions.first().and_then(|r| r.description.clone()),
            state,
            author: patch.author.alias.unwrap_or(patch.author.id),
            head_branch: patch
                .revisions
                .last()
                .map(|r| r.id.clone())
                .unwrap_or_default(),
            base_branch: patch.target,
            url: String::new(), // Will be set by caller
            mergeable: None, // Radicle doesn't provide this directly
            created_at,
            updated_at,
        }
    }
}

impl From<RadIssue> for Issue {
    fn from(issue: RadIssue) -> Self {
        let state = match issue.state.status.as_str() {
            "open" => IssueState::Open,
            _ => IssueState::Closed,
        };

        let (created_at, updated_at) = issue
            .discussion
            .first()
            .map(|c| {
                let created = DateTime::from_timestamp(c.timestamp, 0).unwrap_or_default();
                let updated = issue
                    .discussion
                    .last()
                    .map(|l| DateTime::from_timestamp(l.timestamp, 0).unwrap_or_default())
                    .unwrap_or(created);
                (created, updated)
            })
            .unwrap_or_else(|| (Utc::now(), Utc::now()));

        Issue {
            id: issue.id,
            number: 0, // Radicle uses string IDs
            title: issue.title,
            body: issue.discussion.first().map(|c| c.body.clone()),
            state,
            url: String::new(), // Will be set by caller
            author: issue.author.alias.unwrap_or(issue.author.id),
            labels: issue.labels,
            created_at,
            updated_at,
        }
    }
}

impl From<RadComment> for Comment {
    fn from(comment: RadComment) -> Self {
        let created_at = DateTime::from_timestamp(comment.timestamp, 0).unwrap_or_default();
        Comment {
            id: comment.id,
            body: comment.body,
            author: comment.author.alias.unwrap_or(comment.author.id),
            created_at,
            updated_at: created_at, // Radicle comments don't have separate updated_at
        }
    }
}

#[async_trait]
impl ForgeAdapter for RadicleAdapter {
    fn forge(&self) -> Forge {
        Forge::Radicle
    }

    fn base_url(&self) -> &str {
        &self.node_url
    }

    async fn list_repos(&self, owner: &str) -> Result<Vec<Repository>> {
        // In Radicle, owner is a DID. List projects where they're a delegate.
        let url = self.api_url("/projects");

        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| AdapterError::ApiError(e.to_string()))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(AdapterError::ApiError(format!(
                "Failed to list projects: {} - {}",
                status, body
            )));
        }

        let projects: Vec<RadProject> = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse projects: {}", e)))?;

        // Filter by owner DID
        let repos: Vec<Repository> = projects
            .into_iter()
            .filter(|p| p.delegates.iter().any(|d| d.id == owner || d.alias.as_deref() == Some(owner)))
            .map(Repository::from)
            .collect();

        Ok(repos)
    }

    async fn get_repo(&self, owner: &str, repo: &str) -> Result<Repository> {
        let project_id = Self::parse_project_id(owner, repo);
        let url = self.api_url(&format!("/projects/{}", Self::encode_rid(&project_id)));

        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| AdapterError::ApiError(e.to_string()))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(AdapterError::ApiError(format!(
                "Failed to get project: {} - {}",
                status, body
            )));
        }

        let project: RadProject = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse project: {}", e)))?;

        Ok(Repository::from(project))
    }

    async fn get_alerts(&self, _owner: &str, _repo: &str) -> Result<Vec<Alert>> {
        // Radicle doesn't have built-in security alerts
        Ok(Vec::new())
    }

    async fn list_workflows(&self, _owner: &str, _repo: &str) -> Result<Vec<Workflow>> {
        // Radicle doesn't have built-in CI/CD
        // Users typically use external CI triggered by Radicle events
        Ok(Vec::new())
    }

    async fn deploy_workflow(
        &self,
        _owner: &str,
        _repo: &str,
        _path: &str,
        _content: &str,
        _message: &str,
    ) -> Result<()> {
        Err(AdapterError::ApiError(
            "Radicle doesn't have built-in CI. Use external CI systems with Radicle webhooks.".to_string(),
        ))
    }

    async fn trigger_workflow(
        &self,
        _owner: &str,
        _repo: &str,
        _workflow: &str,
        _ref_name: &str,
    ) -> Result<()> {
        Err(AdapterError::ApiError(
            "Radicle doesn't have built-in CI. Use external CI systems with Radicle webhooks.".to_string(),
        ))
    }

    async fn list_workflow_runs(
        &self,
        _owner: &str,
        _repo: &str,
        _workflow_id: Option<&str>,
    ) -> Result<Vec<WorkflowRun>> {
        Ok(Vec::new())
    }

    async fn get_workflow_run(
        &self,
        _owner: &str,
        _repo: &str,
        _run_id: &str,
    ) -> Result<WorkflowRun> {
        Err(AdapterError::ApiError(
            "Radicle doesn't have built-in CI.".to_string(),
        ))
    }

    async fn enable_branch_protection(
        &self,
        _owner: &str,
        _repo: &str,
        _branch: &str,
    ) -> Result<()> {
        // Radicle has its own security model based on delegates and thresholds
        Err(AdapterError::ApiError(
            "Radicle uses delegate-based access control. Configure delegates and threshold in project settings.".to_string(),
        ))
    }

    async fn create_pr(
        &self,
        owner: &str,
        repo: &str,
        title: &str,
        body: &str,
        head: &str, // This should be a commit OID
        base: &str,
    ) -> Result<String> {
        let project_id = Self::parse_project_id(owner, repo);
        let url = self.api_url(&format!(
            "/projects/{}/patches",
            Self::encode_rid(&project_id)
        ));

        let payload = CreatePatchPayload {
            title: title.to_string(),
            description: body.to_string(),
            target: base.to_string(),
            oid: head.to_string(),
            labels: Vec::new(),
        };

        let response = self
            .client
            .post(&url)
            .json(&payload)
            .send()
            .await
            .map_err(|e| AdapterError::ApiError(e.to_string()))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(AdapterError::ApiError(format!(
                "Failed to create patch: {} - {}",
                status, body
            )));
        }

        #[derive(Deserialize)]
        struct PatchResponse {
            id: String,
        }

        let patch: PatchResponse = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse patch: {}", e)))?;

        Ok(patch.id)
    }

    async fn list_prs(
        &self,
        owner: &str,
        repo: &str,
        state: Option<PullRequestState>,
    ) -> Result<Vec<PullRequest>> {
        let project_id = Self::parse_project_id(owner, repo);
        let url = self.api_url(&format!(
            "/projects/{}/patches",
            Self::encode_rid(&project_id)
        ));

        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| AdapterError::ApiError(e.to_string()))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(AdapterError::ApiError(format!(
                "Failed to list patches: {} - {}",
                status, body
            )));
        }

        let patches: Vec<RadPatch> = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse patches: {}", e)))?;

        let prs: Vec<PullRequest> = patches
            .into_iter()
            .filter(|p| {
                match state {
                    Some(PullRequestState::Open) => p.state.status == "open" || p.state.status == "draft",
                    Some(PullRequestState::Closed) => p.state.status == "archived",
                    Some(PullRequestState::Merged) => p.state.status == "merged",
                    None => true,
                }
            })
            .map(|p| {
                let mut pr = PullRequest::from(p);
                pr.url = format!("{}#{}", project_id, pr.id);
                pr
            })
            .collect();

        Ok(prs)
    }

    async fn get_pr(&self, owner: &str, repo: &str, number: u64) -> Result<PullRequest> {
        // In Radicle, patches are identified by string IDs, not numbers
        // This is a limitation of the ForgeAdapter interface
        Err(AdapterError::ApiError(
            "Radicle patches use string IDs. Use list_prs to find patches.".to_string(),
        ))
    }

    async fn merge_pr(
        &self,
        owner: &str,
        repo: &str,
        number: u64,
        commit_message: Option<&str>,
    ) -> Result<()> {
        // Radicle merging is done via git, not the API
        Err(AdapterError::ApiError(
            "Radicle patches are merged using git. Apply the patch locally and push.".to_string(),
        ))
    }

    async fn close_pr(&self, owner: &str, repo: &str, number: u64) -> Result<()> {
        Err(AdapterError::ApiError(
            "Radicle patches use string IDs. Archive patches via the API with the patch ID.".to_string(),
        ))
    }

    async fn create_issue(
        &self,
        owner: &str,
        repo: &str,
        title: &str,
        body: &str,
        labels: Vec<String>,
    ) -> Result<Issue> {
        let project_id = Self::parse_project_id(owner, repo);
        let url = self.api_url(&format!(
            "/projects/{}/issues",
            Self::encode_rid(&project_id)
        ));

        let payload = CreateIssuePayload {
            title: title.to_string(),
            description: body.to_string(),
            labels,
            assignees: Vec::new(),
            embeds: Vec::new(),
        };

        let response = self
            .client
            .post(&url)
            .json(&payload)
            .send()
            .await
            .map_err(|e| AdapterError::ApiError(e.to_string()))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(AdapterError::ApiError(format!(
                "Failed to create issue: {} - {}",
                status, body
            )));
        }

        let issue: RadIssue = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse issue: {}", e)))?;

        let mut result = Issue::from(issue);
        result.url = format!("{}#issues/{}", project_id, result.id);

        Ok(result)
    }

    async fn list_issues(
        &self,
        owner: &str,
        repo: &str,
        state: Option<IssueState>,
    ) -> Result<Vec<Issue>> {
        let project_id = Self::parse_project_id(owner, repo);
        let url = self.api_url(&format!(
            "/projects/{}/issues",
            Self::encode_rid(&project_id)
        ));

        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| AdapterError::ApiError(e.to_string()))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(AdapterError::ApiError(format!(
                "Failed to list issues: {} - {}",
                status, body
            )));
        }

        let issues: Vec<RadIssue> = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse issues: {}", e)))?;

        let result: Vec<Issue> = issues
            .into_iter()
            .filter(|i| {
                match state {
                    Some(IssueState::Open) => i.state.status == "open",
                    Some(IssueState::Closed) => i.state.status != "open",
                    None => true,
                }
            })
            .map(|i| {
                let mut issue = Issue::from(i);
                issue.url = format!("{}#issues/{}", project_id, issue.id);
                issue
            })
            .collect();

        Ok(result)
    }

    async fn get_issue(&self, owner: &str, repo: &str, number: u64) -> Result<Issue> {
        // Radicle uses string IDs for issues
        Err(AdapterError::ApiError(
            "Radicle issues use string IDs. Use list_issues to find issues.".to_string(),
        ))
    }

    async fn update_issue(
        &self,
        owner: &str,
        repo: &str,
        number: u64,
        title: Option<&str>,
        body: Option<&str>,
        state: Option<IssueState>,
        _labels: Option<Vec<String>>,
    ) -> Result<Issue> {
        Err(AdapterError::ApiError(
            "Radicle issues use string IDs. Update issues via the API with the issue ID.".to_string(),
        ))
    }

    async fn close_issue(&self, owner: &str, repo: &str, number: u64) -> Result<()> {
        Err(AdapterError::ApiError(
            "Radicle issues use string IDs. Close issues via the API with the issue ID.".to_string(),
        ))
    }

    async fn add_issue_comment(
        &self,
        owner: &str,
        repo: &str,
        issue_number: u64,
        body: &str,
    ) -> Result<Comment> {
        Err(AdapterError::ApiError(
            "Radicle issues use string IDs. Add comments via the API with the issue ID.".to_string(),
        ))
    }

    async fn add_pr_comment(
        &self,
        owner: &str,
        repo: &str,
        pr_number: u64,
        body: &str,
    ) -> Result<Comment> {
        Err(AdapterError::ApiError(
            "Radicle patches use string IDs. Add comments via the API with the patch ID.".to_string(),
        ))
    }

    async fn list_issue_comments(
        &self,
        owner: &str,
        repo: &str,
        issue_number: u64,
    ) -> Result<Vec<Comment>> {
        Err(AdapterError::ApiError(
            "Radicle issues use string IDs. List comments via the API with the issue ID.".to_string(),
        ))
    }

    async fn list_pr_comments(
        &self,
        owner: &str,
        repo: &str,
        pr_number: u64,
    ) -> Result<Vec<Comment>> {
        Err(AdapterError::ApiError(
            "Radicle patches use string IDs. List comments via the API with the patch ID.".to_string(),
        ))
    }

    async fn create_check_run(
        &self,
        _owner: &str,
        _repo: &str,
        _name: &str,
        _head_sha: &str,
        _status: CheckStatus,
        _conclusion: Option<CheckConclusion>,
    ) -> Result<CheckRun> {
        Err(AdapterError::ApiError(
            "Radicle doesn't have built-in CI. Use external CI systems.".to_string(),
        ))
    }

    async fn update_check_run(
        &self,
        _owner: &str,
        _repo: &str,
        _check_run_id: &str,
        _status: Option<CheckStatus>,
        _conclusion: Option<CheckConclusion>,
    ) -> Result<CheckRun> {
        Err(AdapterError::ApiError(
            "Radicle doesn't have built-in CI.".to_string(),
        ))
    }

    async fn list_check_runs(
        &self,
        _owner: &str,
        _repo: &str,
        _ref_name: &str,
    ) -> Result<Vec<CheckRun>> {
        Ok(Vec::new())
    }

    async fn create_webhook(
        &self,
        _owner: &str,
        _repo: &str,
        _config: WebhookConfig,
    ) -> Result<WebhookConfig> {
        // Radicle has a different event model - uses gossip protocol
        Err(AdapterError::ApiError(
            "Radicle uses a gossip-based event system. Configure event handlers in your Radicle node.".to_string(),
        ))
    }

    async fn list_webhooks(&self, _owner: &str, _repo: &str) -> Result<Vec<WebhookConfig>> {
        Ok(Vec::new())
    }

    async fn delete_webhook(&self, _owner: &str, _repo: &str, _webhook_id: &str) -> Result<()> {
        Err(AdapterError::ApiError(
            "Radicle uses a gossip-based event system.".to_string(),
        ))
    }

    fn parse_webhook(
        &self,
        event_type: &str,
        signature: Option<&str>,
        payload: &[u8],
        _secret: Option<&str>,
    ) -> Result<WebhookPayload> {
        // Parse Radicle node events
        let event = match event_type {
            "refs/updated" | "push" => WebhookEvent::Push,
            "patch/created" | "patch/updated" => WebhookEvent::PullRequest,
            "issue/created" | "issue/updated" => WebhookEvent::Issues,
            "comment/created" => WebhookEvent::IssueComment,
            _ => {
                return Err(AdapterError::ApiError(format!(
                    "Unknown event type: {}",
                    event_type
                )))
            }
        };

        let body: serde_json::Value = serde_json::from_slice(payload)
            .map_err(|e| AdapterError::ApiError(format!("Invalid JSON payload: {}", e)))?;

        Ok(WebhookPayload {
            event,
            delivery_id: uuid::Uuid::new_v4().to_string(),
            signature: signature.map(String::from),
            payload: body,
        })
    }
}

// Extended API for Radicle-specific operations
impl RadicleAdapter {
    /// Get node status
    pub async fn get_node_status(&self) -> Result<RadNode> {
        let url = self.api_url("/node");

        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| AdapterError::ApiError(e.to_string()))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(AdapterError::ApiError(format!(
                "Failed to get node status: {} - {}",
                status, body
            )));
        }

        response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse node: {}", e)))
    }

    /// Get a patch by its string ID
    pub async fn get_patch(&self, project_id: &str, patch_id: &str) -> Result<PullRequest> {
        let url = self.api_url(&format!(
            "/projects/{}/patches/{}",
            Self::encode_rid(project_id),
            patch_id
        ));

        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| AdapterError::ApiError(e.to_string()))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(AdapterError::ApiError(format!(
                "Failed to get patch: {} - {}",
                status, body
            )));
        }

        let patch: RadPatch = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse patch: {}", e)))?;

        let mut pr = PullRequest::from(patch);
        pr.url = format!("{}#{}", project_id, pr.id);

        Ok(pr)
    }

    /// Get an issue by its string ID
    pub async fn get_issue_by_id(&self, project_id: &str, issue_id: &str) -> Result<Issue> {
        let url = self.api_url(&format!(
            "/projects/{}/issues/{}",
            Self::encode_rid(project_id),
            issue_id
        ));

        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| AdapterError::ApiError(e.to_string()))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(AdapterError::ApiError(format!(
                "Failed to get issue: {} - {}",
                status, body
            )));
        }

        let issue: RadIssue = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse issue: {}", e)))?;

        let mut result = Issue::from(issue);
        result.url = format!("{}#issues/{}", project_id, result.id);

        Ok(result)
    }

    /// Add a comment to an issue by string ID
    pub async fn add_comment_to_issue(
        &self,
        project_id: &str,
        issue_id: &str,
        body: &str,
    ) -> Result<Comment> {
        let url = self.api_url(&format!(
            "/projects/{}/issues/{}/comments",
            Self::encode_rid(project_id),
            issue_id
        ));

        let payload = CreateCommentPayload {
            body: body.to_string(),
            reply_to: None,
            embeds: Vec::new(),
        };

        let response = self
            .client
            .post(&url)
            .json(&payload)
            .send()
            .await
            .map_err(|e| AdapterError::ApiError(e.to_string()))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(AdapterError::ApiError(format!(
                "Failed to add comment: {} - {}",
                status, body
            )));
        }

        let comment: RadComment = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse comment: {}", e)))?;

        Ok(Comment::from(comment))
    }

    /// Close an issue by string ID
    pub async fn close_issue_by_id(&self, project_id: &str, issue_id: &str) -> Result<()> {
        let url = self.api_url(&format!(
            "/projects/{}/issues/{}",
            Self::encode_rid(project_id),
            issue_id
        ));

        let payload = UpdateIssuePayload {
            title: None,
            state: Some(IssueStatePayload {
                status: "closed".to_string(),
                reason: Some("completed".to_string()),
            }),
        };

        let response = self
            .client
            .patch(&url)
            .json(&payload)
            .send()
            .await
            .map_err(|e| AdapterError::ApiError(e.to_string()))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(AdapterError::ApiError(format!(
                "Failed to close issue: {} - {}",
                status, body
            )));
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_adapter_creation() {
        let adapter = RadicleAdapter::new().unwrap();
        assert_eq!(adapter.base_url(), "http://127.0.0.1:8080");
        assert!(matches!(adapter.forge(), Forge::Radicle));
    }

    #[test]
    fn test_custom_node_url() {
        let adapter = RadicleAdapter::with_node_url("http://localhost:9000").unwrap();
        assert_eq!(adapter.base_url(), "http://localhost:9000");
    }

    #[test]
    fn test_default_implementation() {
        let adapter = RadicleAdapter::default();
        assert!(matches!(adapter.forge(), Forge::Radicle));
    }

    #[test]
    fn test_project_id_parsing() {
        // Direct RID
        assert_eq!(
            RadicleAdapter::parse_project_id("~did", "rad:z3gqcJUoA1n9HaHKufZs5FCSGazv5"),
            "rad:z3gqcJUoA1n9HaHKufZs5FCSGazv5"
        );

        // Name (passed through)
        assert_eq!(
            RadicleAdapter::parse_project_id("~did", "my-project"),
            "my-project"
        );
    }

    #[test]
    fn test_webhook_parsing() {
        let adapter = RadicleAdapter::new().unwrap();

        let payload = serde_json::json!({
            "project": {
                "id": "rad:z3gqcJUoA1n9HaHKufZs5FCSGazv5"
            },
            "author": {
                "id": "did:key:z6MksFqXN3Yhqk8pTJdUGLwATkRfQvwZXPqR2qMEhbS9wzpT",
                "alias": "alice"
            }
        });

        let result = adapter
            .parse_webhook("push", None, payload.to_string().as_bytes(), None)
            .unwrap();

        assert!(matches!(result.event, WebhookEvent::Push));
        assert_eq!(
            result.repository,
            Some("rad:z3gqcJUoA1n9HaHKufZs5FCSGazv5".to_string())
        );
        assert_eq!(result.sender, Some("alice".to_string()));
    }

    #[test]
    fn test_project_conversion() {
        let rad_project = RadProject {
            id: "rad:z3gqcJUoA1n9HaHKufZs5FCSGazv5".to_string(),
            name: "my-project".to_string(),
            description: Some("A test project".to_string()),
            default_branch: "main".to_string(),
            head: "abc123".to_string(),
            delegates: vec![RadDelegate {
                id: "did:key:z6Mk...".to_string(),
                alias: Some("alice".to_string()),
            }],
            threshold: 1,
            visibility: RadVisibility {
                visibility_type: "public".to_string(),
            },
            seeding_count: 5,
        };

        let repo = Repository::from(rad_project);
        assert_eq!(repo.id, "rad:z3gqcJUoA1n9HaHKufZs5FCSGazv5");
        assert_eq!(repo.name, "my-project");
        assert!(!repo.private);
    }
}
