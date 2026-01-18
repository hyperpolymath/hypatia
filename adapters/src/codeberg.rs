// SPDX-License-Identifier: PLMP-1.0-or-later
//! Codeberg/Gitea adapter for cicd-hyper-a
//!
//! Provides integration with Codeberg (and self-hosted Gitea instances) for:
//! - Repository management
//! - Pull requests
//! - Issues and comments
//! - Actions/CI workflows
//! - Webhooks
//!
//! The Gitea API is similar to GitHub's but with some differences in endpoints
//! and response structures.

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use hmac::{Hmac, Mac};
use reqwest::{header::HeaderMap, Client};
use serde::{Deserialize, Serialize};
use sha2::Sha256;

use crate::error::AdapterError;
use crate::forge::{
    defaults, Alert, AlertCategory, CheckConclusion, CheckRun, CheckStatus, Comment, Forge,
    ForgeAdapter, Issue, IssueState, PullRequest, PullRequestState, Repository, RunConclusion,
    RunStatus, Severity, Visibility, WebhookConfig, WebhookEvent, WebhookPayload, Workflow,
    WorkflowRun, WorkflowState,
};

type Result<T> = std::result::Result<T, AdapterError>;

/// Codeberg/Gitea adapter implementing the ForgeAdapter trait
///
/// Supports both Codeberg.org and self-hosted Gitea instances.
/// Authentication uses personal access tokens passed via Authorization header.
pub struct CodebergAdapter {
    client: Client,
    token: String,
    base_url: String,
}

impl CodebergAdapter {
    /// Creates a new CodebergAdapter for Codeberg.org
    ///
    /// # Arguments
    /// * `token` - Personal access token for Codeberg
    pub fn new(token: impl Into<String>) -> Result<Self> {
        Self::with_base_url(token, "https://codeberg.org")
    }

    /// Creates a new CodebergAdapter for a custom Gitea instance
    ///
    /// # Arguments
    /// * `token` - Personal access token for the Gitea instance
    /// * `base_url` - Base URL of the Gitea instance (e.g., "https://gitea.example.com")
    pub fn with_base_url(token: impl Into<String>, base_url: impl Into<String>) -> Result<Self> {
        let token = token.into();
        let base_url = base_url.into().trim_end_matches('/').to_string();

        let mut headers = HeaderMap::new();
        headers.insert(
            "Authorization",
            format!("token {}", token)
                .parse()
                .map_err(|e| AdapterError::ConfigError(format!("Invalid token format: {}", e)))?,
        );
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

        Ok(Self {
            client,
            token,
            base_url,
        })
    }

    /// Helper to build API URLs
    fn api_url(&self, path: &str) -> String {
        format!("{}/api/v1{}", self.base_url, path)
    }

    /// Helper to get paginated results
    async fn get_paginated<T: for<'de> Deserialize<'de>>(
        &self,
        url: &str,
        max_pages: usize,
    ) -> Result<Vec<T>> {
        let mut results = Vec::new();
        let mut page = 1;

        loop {
            let separator = if url.contains('?') { '&' } else { '?' };
            let page_url = format!("{}{}page={}&limit=50", url, separator, page);

            let response = self
                .client
                .get(&page_url)
                .send()
                .await
                .map_err(|e| AdapterError::ApiError(e.to_string()))?;

            if !response.status().is_success() {
                let status = response.status();
                let body = response.text().await.unwrap_or_default();
                return Err(AdapterError::ApiError(format!(
                    "API request failed: {} - {}",
                    status, body
                )));
            }

            let page_results: Vec<T> = response
                .json()
                .await
                .map_err(|e| AdapterError::ApiError(format!("Failed to parse response: {}", e)))?;

            let is_empty = page_results.is_empty();
            results.extend(page_results);

            if is_empty || page >= max_pages {
                break;
            }
            page += 1;
        }

        Ok(results)
    }

    /// Verify webhook signature using HMAC-SHA256
    fn verify_signature(&self, payload: &[u8], signature: &str, secret: &str) -> bool {
        // Gitea uses sha256=<hex> format (same as GitHub)
        let signature = signature.strip_prefix("sha256=").unwrap_or(signature);

        let Ok(expected_bytes) = hex::decode(signature) else {
            return false;
        };

        let mut mac = Hmac::<Sha256>::new_from_slice(secret.as_bytes())
            .expect("HMAC can take key of any size");
        mac.update(payload);

        mac.verify_slice(&expected_bytes).is_ok()
    }
}

// Gitea API response types

#[derive(Debug, Deserialize)]
struct GiteaRepo {
    id: u64,
    name: String,
    full_name: String,
    description: Option<String>,
    html_url: String,
    ssh_url: String,
    clone_url: String,
    default_branch: String,
    private: bool,
    archived: bool,
}

#[derive(Debug, Deserialize)]
struct GiteaUser {
    id: u64,
    login: String,
    full_name: Option<String>,
    email: Option<String>,
    avatar_url: Option<String>,
}

#[derive(Debug, Deserialize)]
struct GiteaPullRequest {
    id: u64,
    number: u64,
    title: String,
    body: Option<String>,
    state: String,
    html_url: String,
    user: GiteaUser,
    head: GiteaBranch,
    base: GiteaBranch,
    merged: bool,
    mergeable: Option<bool>,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
}

#[derive(Debug, Deserialize)]
struct GiteaBranch {
    label: String,
    #[serde(rename = "ref")]
    ref_name: String,
    sha: String,
}

#[derive(Debug, Deserialize)]
struct GiteaIssue {
    id: u64,
    number: u64,
    title: String,
    body: Option<String>,
    state: String,
    html_url: String,
    user: GiteaUser,
    labels: Vec<GiteaLabel>,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
}

#[derive(Debug, Deserialize)]
struct GiteaLabel {
    id: u64,
    name: String,
    color: String,
}

#[derive(Debug, Deserialize)]
struct GiteaComment {
    id: u64,
    body: String,
    user: GiteaUser,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
}

#[derive(Debug, Deserialize)]
struct GiteaCommitStatus {
    id: u64,
    status: String,
    context: String,
    description: Option<String>,
    target_url: Option<String>,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
}

#[derive(Debug, Deserialize)]
struct GiteaHook {
    id: u64,
    #[serde(rename = "type")]
    hook_type: String,
    active: bool,
    events: Vec<String>,
    config: GiteaHookConfig,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
}

#[derive(Debug, Deserialize)]
struct GiteaHookConfig {
    url: String,
    content_type: String,
    secret: Option<String>,
}

#[derive(Debug, Deserialize)]
struct GiteaAction {
    id: u64,
    name: String,
    path: String,
}

#[derive(Debug, Deserialize)]
struct GiteaActionRun {
    id: u64,
    title: String,
    status: String,
    conclusion: Option<String>,
    workflow_id: String,
    started_at: Option<DateTime<Utc>>,
    completed_at: Option<DateTime<Utc>>,
    html_url: String,
    head_sha: String,
    head_branch: String,
}

// Request payloads

#[derive(Debug, Serialize)]
struct CreatePullRequestPayload {
    title: String,
    body: String,
    head: String,
    base: String,
}

#[derive(Debug, Serialize)]
struct CreateIssuePayload {
    title: String,
    body: String,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    labels: Vec<u64>,
}

#[derive(Debug, Serialize)]
struct UpdateIssuePayload {
    #[serde(skip_serializing_if = "Option::is_none")]
    title: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    body: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    state: Option<String>,
}

#[derive(Debug, Serialize)]
struct CreateCommentPayload {
    body: String,
}

#[derive(Debug, Serialize)]
struct CreateStatusPayload {
    state: String,
    context: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    target_url: Option<String>,
}

#[derive(Debug, Serialize)]
struct CreateHookPayload {
    #[serde(rename = "type")]
    hook_type: String,
    active: bool,
    events: Vec<String>,
    config: CreateHookConfig,
}

#[derive(Debug, Serialize)]
struct CreateHookConfig {
    url: String,
    content_type: String,
    secret: String,
}

#[derive(Debug, Serialize)]
struct CreateFilePayload {
    content: String,
    message: String,
    branch: Option<String>,
}

#[derive(Debug, Serialize)]
struct UpdateFilePayload {
    content: String,
    message: String,
    sha: String,
    branch: Option<String>,
}

#[derive(Debug, Deserialize)]
struct GiteaFileContent {
    sha: String,
    content: Option<String>,
}

impl From<GiteaRepo> for Repository {
    fn from(repo: GiteaRepo) -> Self {
        let owner = repo
            .full_name
            .split('/')
            .next()
            .unwrap_or("")
            .to_string();
        let visibility = if repo.private {
            Visibility::Private
        } else {
            Visibility::Public
        };
        Repository {
            id: repo.id.to_string(),
            name: repo.name,
            owner,
            forge: Forge::Codeberg,
            url: repo.html_url,
            visibility,
            default_branch: repo.default_branch,
            languages: vec![],
        }
    }
}

impl From<GiteaPullRequest> for PullRequest {
    fn from(pr: GiteaPullRequest) -> Self {
        let state = match pr.state.as_str() {
            "open" => PullRequestState::Open,
            "closed" if pr.merged => PullRequestState::Merged,
            _ => PullRequestState::Closed,
        };

        PullRequest {
            id: pr.id.to_string(),
            number: pr.number,
            title: pr.title,
            body: pr.body,
            state,
            author: pr.user.login,
            head_branch: pr.head.ref_name,
            base_branch: pr.base.ref_name,
            url: pr.html_url,
            mergeable: pr.mergeable,
            created_at: pr.created_at,
            updated_at: pr.updated_at,
        }
    }
}

impl From<GiteaIssue> for Issue {
    fn from(issue: GiteaIssue) -> Self {
        let state = match issue.state.as_str() {
            "open" => IssueState::Open,
            _ => IssueState::Closed,
        };

        Issue {
            id: issue.id.to_string(),
            number: issue.number,
            title: issue.title,
            body: issue.body,
            state,
            url: issue.html_url,
            author: issue.user.login,
            labels: issue.labels.into_iter().map(|l| l.name).collect(),
            created_at: issue.created_at,
            updated_at: issue.updated_at,
        }
    }
}

impl From<GiteaComment> for Comment {
    fn from(comment: GiteaComment) -> Self {
        Comment {
            id: comment.id.to_string(),
            body: comment.body,
            author: comment.user.login,
            created_at: comment.created_at,
            updated_at: comment.updated_at,
        }
    }
}

/// Helper struct to convert GiteaCommitStatus with SHA context
struct GiteaCommitStatusWithSha {
    status: GiteaCommitStatus,
    head_sha: String,
}

impl From<GiteaCommitStatusWithSha> for CheckRun {
    fn from(wrapper: GiteaCommitStatusWithSha) -> Self {
        let status = wrapper.status;
        let (check_status, conclusion) = match status.status.as_str() {
            "pending" => (CheckStatus::InProgress, None),
            "success" => (CheckStatus::Completed, Some(CheckConclusion::Success)),
            "error" => (CheckStatus::Completed, Some(CheckConclusion::Failure)),
            "failure" => (CheckStatus::Completed, Some(CheckConclusion::Failure)),
            "warning" => (CheckStatus::Completed, Some(CheckConclusion::Neutral)),
            _ => (CheckStatus::Queued, None),
        };

        CheckRun {
            id: status.id.to_string(),
            name: status.context,
            status: check_status,
            conclusion,
            head_sha: wrapper.head_sha,
            url: status.target_url,
            started_at: Some(status.created_at),
            completed_at: if check_status == CheckStatus::Completed {
                Some(status.updated_at)
            } else {
                None
            },
        }
    }
}

impl From<GiteaHook> for WebhookConfig {
    fn from(hook: GiteaHook) -> Self {
        let events = hook
            .events
            .into_iter()
            .filter_map(|e| match e.as_str() {
                "push" => Some(WebhookEvent::Push),
                "pull_request" => Some(WebhookEvent::PullRequest),
                "issues" => Some(WebhookEvent::Issues),
                "issue_comment" => Some(WebhookEvent::IssueComment),
                "create" => Some(WebhookEvent::Create),
                "delete" => Some(WebhookEvent::Delete),
                "release" => Some(WebhookEvent::Release),
                _ => None,
            })
            .collect();

        WebhookConfig {
            id: Some(hook.id.to_string()),
            url: hook.config.url,
            secret: hook.config.secret,
            events,
            active: hook.active,
            content_type: hook.config.content_type,
        }
    }
}

impl From<GiteaActionRun> for WorkflowRun {
    fn from(run: GiteaActionRun) -> Self {
        let status = match run.status.as_str() {
            "queued" | "waiting" => RunStatus::Queued,
            "running" | "in_progress" => RunStatus::InProgress,
            "completed" | "success" | "failure" | "cancelled" => RunStatus::Completed,
            _ => RunStatus::Queued,
        };

        let conclusion = run.conclusion.as_deref().and_then(|c| match c {
            "success" => Some(RunConclusion::Success),
            "failure" => Some(RunConclusion::Failure),
            "cancelled" => Some(RunConclusion::Cancelled),
            "skipped" => Some(RunConclusion::Skipped),
            "timed_out" => Some(RunConclusion::TimedOut),
            _ => None,
        });

        let now = chrono::Utc::now();
        WorkflowRun {
            id: run.id.to_string(),
            workflow_id: run.workflow_id,
            name: run.title,
            status,
            conclusion,
            head_branch: run.head_branch,
            head_sha: run.head_sha,
            url: run.html_url,
            created_at: run.started_at.unwrap_or(now),
            updated_at: run.completed_at.unwrap_or(now),
        }
    }
}

#[async_trait]
impl ForgeAdapter for CodebergAdapter {
    fn forge(&self) -> Forge {
        if self.base_url.contains("codeberg.org") {
            Forge::Codeberg
        } else {
            Forge::Gitea
        }
    }

    fn base_url(&self) -> &str {
        &self.base_url
    }

    async fn list_repos(&self, owner: &str) -> Result<Vec<Repository>> {
        let url = self.api_url(&format!("/users/{}/repos", owner));
        let repos: Vec<GiteaRepo> = self.get_paginated(&url, 10).await?;
        Ok(repos.into_iter().map(Repository::from).collect())
    }

    async fn get_repo(&self, owner: &str, repo: &str) -> Result<Repository> {
        let url = self.api_url(&format!("/repos/{}/{}", owner, repo));
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
                "Failed to get repo: {} - {}",
                status, body
            )));
        }

        let repo: GiteaRepo = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse repo: {}", e)))?;

        Ok(Repository::from(repo))
    }

    async fn get_alerts(&self, _owner: &str, _repo: &str) -> Result<Vec<Alert>> {
        // Gitea/Codeberg doesn't have built-in security alerts like GitHub
        // Return empty list
        Ok(Vec::new())
    }

    async fn list_workflows(&self, owner: &str, repo: &str) -> Result<Vec<Workflow>> {
        // Gitea Actions - list workflow files from .gitea/workflows or .github/workflows
        let url = self.api_url(&format!(
            "/repos/{}/{}/contents/.gitea/workflows",
            owner, repo
        ));

        let response = self.client.get(&url).send().await;

        let workflows = match response {
            Ok(resp) if resp.status().is_success() => {
                let files: Vec<GiteaFileContent> = resp.json().await.unwrap_or_default();
                files
                    .into_iter()
                    .enumerate()
                    .map(|(idx, f)| Workflow {
                        id: idx.to_string(),
                        name: f.sha.clone(),
                        file: format!(".gitea/workflows/{}", f.sha),
                        state: WorkflowState::Active,
                    })
                    .collect()
            }
            _ => Vec::new(),
        };

        Ok(workflows)
    }

    async fn deploy_workflow(
        &self,
        owner: &str,
        repo: &str,
        path: &str,
        content: &str,
        message: &str,
    ) -> Result<()> {
        let workflow_path = if path.starts_with(".gitea/workflows/") || path.starts_with(".github/workflows/") {
            path.to_string()
        } else {
            format!(".gitea/workflows/{}", path)
        };

        let url = self.api_url(&format!(
            "/repos/{}/{}/contents/{}",
            owner, repo, workflow_path
        ));

        // Check if file exists to determine if we need to update
        let existing = self.client.get(&url).send().await;

        let encoded_content = base64::Engine::encode(
            &base64::engine::general_purpose::STANDARD,
            content.as_bytes(),
        );

        let response = if let Ok(resp) = existing {
            if resp.status().is_success() {
                let file: GiteaFileContent = resp.json().await.map_err(|e| {
                    AdapterError::ApiError(format!("Failed to parse existing file: {}", e))
                })?;

                let payload = UpdateFilePayload {
                    content: encoded_content,
                    message: message.to_string(),
                    sha: file.sha,
                    branch: None,
                };

                self.client.put(&url).json(&payload).send().await
            } else {
                let payload = CreateFilePayload {
                    content: encoded_content,
                    message: message.to_string(),
                    branch: None,
                };

                self.client.post(&url).json(&payload).send().await
            }
        } else {
            let payload = CreateFilePayload {
                content: encoded_content,
                message: message.to_string(),
                branch: None,
            };

            self.client.post(&url).json(&payload).send().await
        };

        let response = response.map_err(|e| AdapterError::ApiError(e.to_string()))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(AdapterError::ApiError(format!(
                "Failed to deploy workflow: {} - {}",
                status, body
            )));
        }

        Ok(())
    }

    async fn trigger_workflow(
        &self,
        owner: &str,
        repo: &str,
        workflow: &str,
        ref_name: &str,
    ) -> Result<()> {
        // Gitea Actions - trigger via workflow_dispatch
        let url = self.api_url(&format!(
            "/repos/{}/{}/actions/workflows/{}/dispatches",
            owner, repo, workflow
        ));

        let payload = serde_json::json!({
            "ref": ref_name
        });

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
                "Failed to trigger workflow: {} - {}",
                status, body
            )));
        }

        Ok(())
    }

    async fn list_workflow_runs(
        &self,
        owner: &str,
        repo: &str,
        workflow_id: Option<&str>,
    ) -> Result<Vec<WorkflowRun>> {
        let url = if let Some(wf_id) = workflow_id {
            self.api_url(&format!(
                "/repos/{}/{}/actions/workflows/{}/runs",
                owner, repo, wf_id
            ))
        } else {
            self.api_url(&format!("/repos/{}/{}/actions/runs", owner, repo))
        };

        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| AdapterError::ApiError(e.to_string()))?;

        if !response.status().is_success() {
            // Actions might not be enabled
            return Ok(Vec::new());
        }

        #[derive(Deserialize)]
        struct RunsResponse {
            workflow_runs: Vec<GiteaActionRun>,
        }

        let runs: RunsResponse = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse runs: {}", e)))?;

        Ok(runs.workflow_runs.into_iter().map(WorkflowRun::from).collect())
    }

    async fn get_workflow_run(
        &self,
        owner: &str,
        repo: &str,
        run_id: &str,
    ) -> Result<WorkflowRun> {
        let url = self.api_url(&format!(
            "/repos/{}/{}/actions/runs/{}",
            owner, repo, run_id
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
                "Failed to get workflow run: {} - {}",
                status, body
            )));
        }

        let run: GiteaActionRun = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse run: {}", e)))?;

        Ok(WorkflowRun::from(run))
    }

    async fn enable_branch_protection(
        &self,
        owner: &str,
        repo: &str,
        branch: &str,
    ) -> Result<()> {
        let url = self.api_url(&format!(
            "/repos/{}/{}/branch_protections",
            owner, repo
        ));

        let payload = serde_json::json!({
            "branch_name": branch,
            "enable_push": true,
            "enable_push_whitelist": false,
            "enable_merge_whitelist": false,
            "enable_status_check": true,
            "status_check_contexts": [],
            "required_approvals": 1,
            "enable_approvals_whitelist": false,
            "block_on_rejected_reviews": true,
            "block_on_outdated_branch": true,
            "dismiss_stale_approvals": true,
            "require_signed_commits": false,
            "protected_file_patterns": "",
            "unprotected_file_patterns": ""
        });

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
                "Failed to enable branch protection: {} - {}",
                status, body
            )));
        }

        Ok(())
    }

    async fn create_pr(
        &self,
        owner: &str,
        repo: &str,
        title: &str,
        body: &str,
        head: &str,
        base: &str,
    ) -> Result<String> {
        let url = self.api_url(&format!("/repos/{}/{}/pulls", owner, repo));

        let payload = CreatePullRequestPayload {
            title: title.to_string(),
            body: body.to_string(),
            head: head.to_string(),
            base: base.to_string(),
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
                "Failed to create PR: {} - {}",
                status, body
            )));
        }

        let pr: GiteaPullRequest = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse PR: {}", e)))?;

        Ok(pr.html_url)
    }

    async fn list_prs(
        &self,
        owner: &str,
        repo: &str,
        state: Option<PullRequestState>,
    ) -> Result<Vec<PullRequest>> {
        let state_param = match state {
            Some(PullRequestState::Open) => "open",
            Some(PullRequestState::Closed) => "closed",
            Some(PullRequestState::Merged) => "closed", // Gitea filters merged in closed
            None => "all",
        };

        let url = self.api_url(&format!(
            "/repos/{}/{}/pulls?state={}",
            owner, repo, state_param
        ));

        let prs: Vec<GiteaPullRequest> = self.get_paginated(&url, 5).await?;

        let mut result: Vec<PullRequest> = prs.into_iter().map(PullRequest::from).collect();

        // Filter merged PRs if specifically requested
        if let Some(PullRequestState::Merged) = state {
            result.retain(|pr| pr.state == PullRequestState::Merged);
        }

        Ok(result)
    }

    async fn get_pr(&self, owner: &str, repo: &str, number: u64) -> Result<PullRequest> {
        let url = self.api_url(&format!("/repos/{}/{}/pulls/{}", owner, repo, number));

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
                "Failed to get PR: {} - {}",
                status, body
            )));
        }

        let pr: GiteaPullRequest = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse PR: {}", e)))?;

        Ok(PullRequest::from(pr))
    }

    async fn merge_pr(
        &self,
        owner: &str,
        repo: &str,
        number: u64,
        commit_message: Option<&str>,
    ) -> Result<()> {
        let url = self.api_url(&format!("/repos/{}/{}/pulls/{}/merge", owner, repo, number));

        let mut payload = serde_json::json!({
            "do": "merge"
        });

        if let Some(msg) = commit_message {
            payload["merge_message_field"] = serde_json::Value::String(msg.to_string());
        }

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
                "Failed to merge PR: {} - {}",
                status, body
            )));
        }

        Ok(())
    }

    async fn close_pr(&self, owner: &str, repo: &str, number: u64) -> Result<()> {
        let url = self.api_url(&format!("/repos/{}/{}/pulls/{}", owner, repo, number));

        let payload = serde_json::json!({
            "state": "closed"
        });

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
                "Failed to close PR: {} - {}",
                status, body
            )));
        }

        Ok(())
    }

    async fn create_issue(
        &self,
        owner: &str,
        repo: &str,
        title: &str,
        body: &str,
        labels: Vec<String>,
    ) -> Result<Issue> {
        let url = self.api_url(&format!("/repos/{}/{}/issues", owner, repo));

        // First, get label IDs if labels are provided
        let label_ids = if !labels.is_empty() {
            let labels_url = self.api_url(&format!("/repos/{}/{}/labels", owner, repo));
            let existing_labels: Vec<GiteaLabel> =
                self.get_paginated(&labels_url, 3).await.unwrap_or_default();

            labels
                .iter()
                .filter_map(|name| {
                    existing_labels
                        .iter()
                        .find(|l| l.name.eq_ignore_ascii_case(name))
                        .map(|l| l.id)
                })
                .collect()
        } else {
            Vec::new()
        };

        let payload = CreateIssuePayload {
            title: title.to_string(),
            body: body.to_string(),
            labels: label_ids,
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

        let issue: GiteaIssue = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse issue: {}", e)))?;

        Ok(Issue::from(issue))
    }

    async fn list_issues(
        &self,
        owner: &str,
        repo: &str,
        state: Option<IssueState>,
    ) -> Result<Vec<Issue>> {
        let state_param = match state {
            Some(IssueState::Open) => "open",
            Some(IssueState::Closed) => "closed",
            None => "all",
        };

        let url = self.api_url(&format!(
            "/repos/{}/{}/issues?state={}&type=issues",
            owner, repo, state_param
        ));

        let issues: Vec<GiteaIssue> = self.get_paginated(&url, 5).await?;
        Ok(issues.into_iter().map(Issue::from).collect())
    }

    async fn get_issue(&self, owner: &str, repo: &str, number: u64) -> Result<Issue> {
        let url = self.api_url(&format!("/repos/{}/{}/issues/{}", owner, repo, number));

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

        let issue: GiteaIssue = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse issue: {}", e)))?;

        Ok(Issue::from(issue))
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
        let url = self.api_url(&format!("/repos/{}/{}/issues/{}", owner, repo, number));

        let payload = UpdateIssuePayload {
            title: title.map(String::from),
            body: body.map(String::from),
            state: state.map(|s| match s {
                IssueState::Open => "open".to_string(),
                IssueState::Closed => "closed".to_string(),
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
                "Failed to update issue: {} - {}",
                status, body
            )));
        }

        let issue: GiteaIssue = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse issue: {}", e)))?;

        Ok(Issue::from(issue))
    }

    async fn close_issue(&self, owner: &str, repo: &str, number: u64) -> Result<()> {
        self.update_issue(owner, repo, number, None, None, Some(IssueState::Closed), None)
            .await?;
        Ok(())
    }

    async fn add_issue_comment(
        &self,
        owner: &str,
        repo: &str,
        issue_number: u64,
        body: &str,
    ) -> Result<Comment> {
        let url = self.api_url(&format!(
            "/repos/{}/{}/issues/{}/comments",
            owner, repo, issue_number
        ));

        let payload = CreateCommentPayload {
            body: body.to_string(),
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

        let comment: GiteaComment = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse comment: {}", e)))?;

        Ok(Comment::from(comment))
    }

    async fn add_pr_comment(
        &self,
        owner: &str,
        repo: &str,
        pr_number: u64,
        body: &str,
    ) -> Result<Comment> {
        // In Gitea, PRs are issues so we use the same endpoint
        self.add_issue_comment(owner, repo, pr_number, body).await
    }

    async fn list_issue_comments(
        &self,
        owner: &str,
        repo: &str,
        issue_number: u64,
    ) -> Result<Vec<Comment>> {
        let url = self.api_url(&format!(
            "/repos/{}/{}/issues/{}/comments",
            owner, repo, issue_number
        ));

        let comments: Vec<GiteaComment> = self.get_paginated(&url, 5).await?;
        Ok(comments.into_iter().map(Comment::from).collect())
    }

    async fn list_pr_comments(
        &self,
        owner: &str,
        repo: &str,
        pr_number: u64,
    ) -> Result<Vec<Comment>> {
        // In Gitea, PRs are issues so we use the same endpoint
        self.list_issue_comments(owner, repo, pr_number).await
    }

    async fn create_check_run(
        &self,
        owner: &str,
        repo: &str,
        name: &str,
        head_sha: &str,
        status: CheckStatus,
        conclusion: Option<CheckConclusion>,
    ) -> Result<CheckRun> {
        // Gitea uses commit statuses instead of check runs
        let url = self.api_url(&format!(
            "/repos/{}/{}/statuses/{}",
            owner, repo, head_sha
        ));

        let state = match (&status, &conclusion) {
            (CheckStatus::Completed, Some(CheckConclusion::Success)) => "success",
            (CheckStatus::Completed, Some(CheckConclusion::Failure)) => "failure",
            (CheckStatus::Completed, Some(CheckConclusion::Cancelled)) => "error",
            (CheckStatus::Completed, _) => "error",
            (CheckStatus::InProgress, _) => "pending",
            (CheckStatus::Queued, _) => "pending",
        };

        let payload = CreateStatusPayload {
            state: state.to_string(),
            context: name.to_string(),
            description: None,
            target_url: None,
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
                "Failed to create status: {} - {}",
                status, body
            )));
        }

        let git_status: GiteaCommitStatus = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse status: {}", e)))?;

        Ok(CheckRun::from(GiteaCommitStatusWithSha {
            status: git_status,
            head_sha: head_sha.to_string(),
        }))
    }

    async fn update_check_run(
        &self,
        owner: &str,
        repo: &str,
        _check_run_id: &str,
        status: Option<CheckStatus>,
        conclusion: Option<CheckConclusion>,
    ) -> Result<CheckRun> {
        // Gitea doesn't have updatable check runs; create a new status
        // We need the SHA from context - use defaults
        defaults::update_check_run(self.forge())
    }

    async fn list_check_runs(
        &self,
        owner: &str,
        repo: &str,
        ref_name: &str,
    ) -> Result<Vec<CheckRun>> {
        let url = self.api_url(&format!(
            "/repos/{}/{}/commits/{}/statuses",
            owner, repo, ref_name
        ));

        let statuses: Vec<GiteaCommitStatus> = self.get_paginated(&url, 3).await?;
        Ok(statuses
            .into_iter()
            .map(|s| {
                CheckRun::from(GiteaCommitStatusWithSha {
                    status: s,
                    head_sha: ref_name.to_string(),
                })
            })
            .collect())
    }

    async fn create_webhook(
        &self,
        owner: &str,
        repo: &str,
        config: WebhookConfig,
    ) -> Result<WebhookConfig> {
        let url = self.api_url(&format!("/repos/{}/{}/hooks", owner, repo));

        let events: Vec<String> = config
            .events
            .iter()
            .map(|e| match e {
                WebhookEvent::Push => "push",
                WebhookEvent::PullRequest => "pull_request",
                WebhookEvent::PullRequestReview => "pull_request_review",
                WebhookEvent::Issues => "issues",
                WebhookEvent::IssueComment => "issue_comment",
                WebhookEvent::Create => "create",
                WebhookEvent::Delete => "delete",
                WebhookEvent::Release => "release",
                WebhookEvent::WorkflowRun => "workflow_run",
                WebhookEvent::CheckRun => "status",
                WebhookEvent::CheckSuite => "status",
                WebhookEvent::Custom => "custom",
            })
            .map(String::from)
            .collect();

        let payload = CreateHookPayload {
            hook_type: "gitea".to_string(),
            active: config.active,
            events,
            config: CreateHookConfig {
                url: config.url.clone(),
                content_type: "json".to_string(),
                secret: config.secret.clone().unwrap_or_default(),
            },
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
                "Failed to create webhook: {} - {}",
                status, body
            )));
        }

        let hook: GiteaHook = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse webhook: {}", e)))?;

        Ok(WebhookConfig::from(hook))
    }

    async fn list_webhooks(&self, owner: &str, repo: &str) -> Result<Vec<WebhookConfig>> {
        let url = self.api_url(&format!("/repos/{}/{}/hooks", owner, repo));
        let hooks: Vec<GiteaHook> = self.get_paginated(&url, 3).await?;
        Ok(hooks.into_iter().map(WebhookConfig::from).collect())
    }

    async fn delete_webhook(&self, owner: &str, repo: &str, webhook_id: &str) -> Result<()> {
        let url = self.api_url(&format!("/repos/{}/{}/hooks/{}", owner, repo, webhook_id));

        let response = self
            .client
            .delete(&url)
            .send()
            .await
            .map_err(|e| AdapterError::ApiError(e.to_string()))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(AdapterError::ApiError(format!(
                "Failed to delete webhook: {} - {}",
                status, body
            )));
        }

        Ok(())
    }

    fn parse_webhook(
        &self,
        event_type: &str,
        signature: Option<&str>,
        payload: &[u8],
        secret: Option<&str>,
    ) -> Result<WebhookPayload> {
        // Verify signature if secret is provided
        if let (Some(sig), Some(sec)) = (signature, secret) {
            if !self.verify_signature(payload, sig, sec) {
                return Err(AdapterError::AuthError(
                    "Invalid webhook signature".to_string(),
                ));
            }
        }

        let event = match event_type {
            "push" => WebhookEvent::Push,
            "pull_request" => WebhookEvent::PullRequest,
            "issues" => WebhookEvent::Issues,
            "issue_comment" => WebhookEvent::IssueComment,
            "create" => WebhookEvent::Create,
            "delete" => WebhookEvent::Delete,
            "release" => WebhookEvent::Release,
            "workflow_run" => WebhookEvent::WorkflowRun,
            "status" => WebhookEvent::CheckRun,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_adapter_creation() {
        let adapter = CodebergAdapter::new("test-token").unwrap();
        assert_eq!(adapter.base_url(), "https://codeberg.org");
        assert!(matches!(adapter.forge(), Forge::Codeberg));
    }

    #[test]
    fn test_custom_base_url() {
        let adapter = CodebergAdapter::with_base_url("test-token", "https://gitea.example.com")
            .unwrap();
        assert_eq!(adapter.base_url(), "https://gitea.example.com");
        assert!(matches!(adapter.forge(), Forge::Gitea));
    }

    #[test]
    fn test_webhook_signature_verification() {
        let adapter = CodebergAdapter::new("test-token").unwrap();
        let payload = b"test payload";
        let secret = "test-secret";

        // Calculate expected signature
        let mut mac = Hmac::<Sha256>::new_from_slice(secret.as_bytes()).unwrap();
        mac.update(payload);
        let signature = format!("sha256={}", hex::encode(mac.finalize().into_bytes()));

        assert!(adapter.verify_signature(payload, &signature, secret));
        assert!(!adapter.verify_signature(payload, "sha256=invalid", secret));
    }

    #[test]
    fn test_webhook_parsing() {
        let adapter = CodebergAdapter::new("test-token").unwrap();

        let payload = serde_json::json!({
            "repository": {
                "full_name": "owner/repo"
            },
            "sender": {
                "login": "user"
            },
            "action": "opened"
        });

        let result = adapter
            .parse_webhook("push", None, payload.to_string().as_bytes(), None)
            .unwrap();

        assert!(matches!(result.event, WebhookEvent::Push));
        assert_eq!(result.repository, Some("owner/repo".to_string()));
        assert_eq!(result.sender, Some("user".to_string()));
    }
}
