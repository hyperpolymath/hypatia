// SPDX-License-Identifier: PLMP-1.0-or-later
//! SourceHut (sr.ht) adapter for cicd-hyper-a
//!
//! Provides integration with SourceHut's suite of services:
//! - git.sr.ht - Git repository hosting
//! - builds.sr.ht - CI/CD builds
//! - todo.sr.ht - Issue tracking (tickets)
//! - lists.sr.ht - Mailing lists (for patch review)
//!
//! SourceHut uses a GraphQL API (api.sr.ht) alongside REST APIs for each service.
//! This adapter primarily uses the REST APIs for simplicity.
//!
//! Note: SourceHut has a unique workflow model:
//! - Uses email-based patch review instead of pull requests
//! - Builds are triggered via build manifests or webhooks
//! - Tickets (issues) are in todo.sr.ht

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use reqwest::{header::HeaderMap, Client};
use serde::{Deserialize, Serialize};

use crate::error::AdapterError;
use crate::forge::{
    defaults, Alert, AlertCategory, CheckConclusion, CheckRun, CheckStatus, Comment, Forge,
    ForgeAdapter, Issue, IssueState, PullRequest, PullRequestState, Repository, RunConclusion,
    RunStatus, Severity, Visibility, WebhookConfig, WebhookEvent, WebhookPayload, Workflow,
    WorkflowRun, WorkflowState,
};

type Result<T> = std::result::Result<T, AdapterError>;

/// SourceHut adapter implementing the ForgeAdapter trait
///
/// Connects to sr.ht services (git.sr.ht, builds.sr.ht, todo.sr.ht).
/// Authentication uses OAuth2 personal access tokens.
pub struct SourcehutAdapter {
    client: Client,
    token: String,
    base_url: String,
}

impl SourcehutAdapter {
    /// Creates a new SourcehutAdapter for sr.ht
    ///
    /// # Arguments
    /// * `token` - Personal access token from meta.sr.ht
    pub fn new(token: impl Into<String>) -> Result<Self> {
        Self::with_base_url(token, "https://sr.ht")
    }

    /// Creates a new SourcehutAdapter for a self-hosted instance
    ///
    /// # Arguments
    /// * `token` - Personal access token
    /// * `base_url` - Base URL of the sr.ht instance (e.g., "https://sr.example.com")
    pub fn with_base_url(token: impl Into<String>, base_url: impl Into<String>) -> Result<Self> {
        let token = token.into();
        let base_url = base_url.into().trim_end_matches('/').to_string();

        let mut headers = HeaderMap::new();
        headers.insert(
            "Authorization",
            format!("Bearer {}", token)
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

    /// Helper to build git.sr.ht API URLs
    fn git_api_url(&self, path: &str) -> String {
        format!("{}/api{}", self.base_url.replace("sr.ht", "git.sr.ht"), path)
    }

    /// Helper to build builds.sr.ht API URLs
    fn builds_api_url(&self, path: &str) -> String {
        format!(
            "{}/api{}",
            self.base_url.replace("sr.ht", "builds.sr.ht"),
            path
        )
    }

    /// Helper to build todo.sr.ht API URLs
    fn todo_api_url(&self, path: &str) -> String {
        format!(
            "{}/api{}",
            self.base_url.replace("sr.ht", "todo.sr.ht"),
            path
        )
    }

    /// Helper to get paginated results using cursor-based pagination
    async fn get_paginated<T: for<'de> Deserialize<'de>>(
        &self,
        url: &str,
        results_key: &str,
        max_pages: usize,
    ) -> Result<Vec<T>> {
        let mut results = Vec::new();
        let mut cursor: Option<String> = None;
        let mut page = 0;

        loop {
            let page_url = match &cursor {
                Some(c) => {
                    let separator = if url.contains('?') { '&' } else { '?' };
                    format!("{}{}start={}", url, separator, c)
                }
                None => url.to_string(),
            };

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

            let body: serde_json::Value = response
                .json()
                .await
                .map_err(|e| AdapterError::ApiError(format!("Failed to parse response: {}", e)))?;

            // Extract results from the specified key
            let page_results: Vec<T> = if let Some(arr) = body.get(results_key) {
                serde_json::from_value(arr.clone())
                    .map_err(|e| AdapterError::ApiError(format!("Failed to parse results: {}", e)))?
            } else {
                Vec::new()
            };

            let is_empty = page_results.is_empty();
            results.extend(page_results);

            // Check for next cursor
            cursor = body
                .get("next")
                .and_then(|n| n.as_str())
                .map(String::from);

            page += 1;
            if is_empty || cursor.is_none() || page >= max_pages {
                break;
            }
        }

        Ok(results)
    }
}

// SourceHut API response types

#[derive(Debug, Deserialize)]
struct SrhtRepo {
    id: u64,
    name: String,
    description: Option<String>,
    visibility: String,
    #[serde(rename = "HEAD")]
    head: Option<SrhtRef>,
}

#[derive(Debug, Deserialize)]
struct SrhtRef {
    name: String,
}

#[derive(Debug, Deserialize)]
struct SrhtUser {
    #[serde(rename = "canonical_name")]
    name: String,
}

#[derive(Debug, Deserialize)]
struct SrhtTicket {
    id: u64,
    #[serde(rename = "ref")]
    ref_id: String,
    subject: String,
    body: Option<String>,
    status: String,
    submitter: SrhtUser,
    labels: Vec<String>,
    created: DateTime<Utc>,
    updated: DateTime<Utc>,
}

#[derive(Debug, Deserialize)]
struct SrhtTicketComment {
    id: u64,
    text: String,
    submitter: SrhtUser,
    created: DateTime<Utc>,
}

#[derive(Debug, Deserialize)]
struct SrhtJob {
    id: u64,
    status: String,
    note: Option<String>,
    tags: Vec<String>,
    created: DateTime<Utc>,
    updated: DateTime<Utc>,
}

#[derive(Debug, Deserialize)]
struct SrhtManifest {
    id: u64,
    path: String,
}

// Request payloads

#[derive(Debug, Serialize)]
struct CreateTicketPayload {
    subject: String,
    body: String,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    labels: Vec<String>,
}

#[derive(Debug, Serialize)]
struct UpdateTicketPayload {
    #[serde(skip_serializing_if = "Option::is_none")]
    subject: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    body: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    status: Option<String>,
}

#[derive(Debug, Serialize)]
struct CreateCommentPayload {
    text: String,
}

#[derive(Debug, Serialize)]
struct SubmitBuildPayload {
    manifest: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    note: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    tags: Vec<String>,
}

impl From<SrhtRepo> for Repository {
    fn from(repo: SrhtRepo) -> Self {
        let default_branch = repo
            .head
            .map(|h| h.name)
            .unwrap_or_else(|| "main".to_string());
        let visibility = if repo.visibility == "public" {
            Visibility::Public
        } else {
            Visibility::Private
        };

        Repository {
            id: repo.id.to_string(),
            name: repo.name,
            owner: String::new(), // Will be set by caller
            forge: Forge::Sourcehut,
            url: String::new(), // Will be set by caller
            visibility,
            default_branch,
            languages: vec![],
        }
    }
}

impl From<SrhtTicket> for Issue {
    fn from(ticket: SrhtTicket) -> Self {
        let state = match ticket.status.as_str() {
            "reported" | "confirmed" | "in_progress" => IssueState::Open,
            _ => IssueState::Closed,
        };

        Issue {
            id: ticket.id.to_string(),
            number: ticket.ref_id.parse().unwrap_or(0),
            title: ticket.subject,
            body: ticket.body,
            state,
            url: String::new(), // Will be set by caller
            author: ticket.submitter.name,
            labels: ticket.labels,
            created_at: ticket.created,
            updated_at: ticket.updated,
        }
    }
}

impl From<SrhtTicketComment> for Comment {
    fn from(comment: SrhtTicketComment) -> Self {
        Comment {
            id: comment.id.to_string(),
            body: comment.text,
            author: comment.submitter.name,
            created_at: comment.created,
            updated_at: comment.created, // SourceHut comments don't have separate updated_at
        }
    }
}

impl From<SrhtJob> for WorkflowRun {
    fn from(job: SrhtJob) -> Self {
        let (status, conclusion) = match job.status.as_str() {
            "pending" | "queued" => (RunStatus::Queued, None),
            "running" => (RunStatus::InProgress, None),
            "success" => (RunStatus::Completed, Some(RunConclusion::Success)),
            "failed" => (RunStatus::Completed, Some(RunConclusion::Failure)),
            "cancelled" => (RunStatus::Completed, Some(RunConclusion::Cancelled)),
            "timeout" => (RunStatus::Completed, Some(RunConclusion::TimedOut)),
            _ => (RunStatus::Queued, None),
        };

        WorkflowRun {
            id: job.id.to_string(),
            workflow_id: String::new(), // SourceHut doesn't have workflow IDs
            name: job.note.unwrap_or_else(|| format!("Build #{}", job.id)),
            status,
            conclusion,
            head_branch: String::new(),
            head_sha: String::new(),
            url: String::new(), // Will be set by caller
            created_at: job.created,
            updated_at: job.updated,
        }
    }
}

impl From<SrhtJob> for CheckRun {
    fn from(job: SrhtJob) -> Self {
        let (check_status, conclusion) = match job.status.as_str() {
            "pending" | "queued" => (CheckStatus::Queued, None),
            "running" => (CheckStatus::InProgress, None),
            "success" => (CheckStatus::Completed, Some(CheckConclusion::Success)),
            "failed" => (CheckStatus::Completed, Some(CheckConclusion::Failure)),
            "cancelled" => (CheckStatus::Completed, Some(CheckConclusion::Cancelled)),
            "timeout" => (CheckStatus::Completed, Some(CheckConclusion::TimedOut)),
            _ => (CheckStatus::Queued, None),
        };

        CheckRun {
            id: job.id.to_string(),
            name: job.note.unwrap_or_else(|| format!("Build #{}", job.id)),
            status: check_status,
            conclusion,
            head_sha: String::new(), // SourceHut jobs don't have commit SHAs directly
            url: None,
            started_at: Some(job.created),
            completed_at: if check_status == CheckStatus::Completed {
                Some(job.updated)
            } else {
                None
            },
        }
    }
}

#[async_trait]
impl ForgeAdapter for SourcehutAdapter {
    fn forge(&self) -> Forge {
        Forge::Sourcehut
    }

    fn base_url(&self) -> &str {
        &self.base_url
    }

    async fn list_repos(&self, owner: &str) -> Result<Vec<Repository>> {
        let url = self.git_api_url(&format!("/~{}/repos", owner));
        let repos: Vec<SrhtRepo> = self.get_paginated(&url, "results", 10).await?;

        Ok(repos
            .into_iter()
            .map(|r| {
                let mut repo = Repository::from(r);
                repo.url = format!("{}/~{}/{}", self.base_url.replace("sr.ht", "git.sr.ht"), owner, &repo.name);
                repo.owner = owner.to_string();
                repo
            })
            .collect())
    }

    async fn get_repo(&self, owner: &str, repo: &str) -> Result<Repository> {
        let url = self.git_api_url(&format!("/~{}/repos/{}", owner, repo));

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

        let srht_repo: SrhtRepo = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse repo: {}", e)))?;

        let mut repository = Repository::from(srht_repo);
        repository.url = format!(
            "{}/~{}/{}",
            self.base_url.replace("sr.ht", "git.sr.ht"),
            owner,
            &repository.name
        );
        repository.owner = owner.to_string();

        Ok(repository)
    }

    async fn get_alerts(&self, _owner: &str, _repo: &str) -> Result<Vec<Alert>> {
        // SourceHut doesn't have built-in security alerts
        Ok(Vec::new())
    }

    async fn list_workflows(&self, owner: &str, repo: &str) -> Result<Vec<Workflow>> {
        // SourceHut uses .build.yml manifests in the repo
        // We can list them via git tree API or check known paths
        let common_paths = vec![".build.yml", ".builds/"];

        let mut workflows = Vec::new();
        for (idx, workflow_path) in common_paths.iter().enumerate() {
            workflows.push(Workflow {
                id: idx.to_string(),
                name: workflow_path.to_string(),
                file: workflow_path.to_string(),
                state: WorkflowState::Active,
            });
        }

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
        // SourceHut doesn't have a file creation API like GitHub
        // Users need to push .build.yml to their repo
        Err(AdapterError::ApiError(
            "SourceHut requires pushing build manifests via git. Use git push to deploy .build.yml".to_string(),
        ))
    }

    async fn trigger_workflow(
        &self,
        owner: &str,
        repo: &str,
        workflow: &str,
        ref_name: &str,
    ) -> Result<()> {
        // Submit a build job to builds.sr.ht
        let url = self.builds_api_url("/jobs");

        // Read manifest from repo (simplified - in practice would fetch from git)
        let manifest = format!(
            r#"image: alpine/latest
sources:
  - {}/~{}/{}
tasks:
  - build: |
      cd {}
      # Build commands here
"#,
            self.base_url.replace("sr.ht", "git.sr.ht"),
            owner,
            repo,
            repo
        );

        let payload = SubmitBuildPayload {
            manifest,
            note: Some(format!("Triggered from {}", ref_name)),
            tags: vec![owner.to_string(), repo.to_string()],
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
                "Failed to trigger build: {} - {}",
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
        let url = self.builds_api_url("/jobs");
        let jobs: Vec<SrhtJob> = self.get_paginated(&url, "results", 5).await?;

        // Filter by tags if we have repo context
        let runs: Vec<WorkflowRun> = jobs
            .into_iter()
            .filter(|j| j.tags.contains(&repo.to_string()))
            .map(|mut j| {
                let mut run = WorkflowRun::from(j);
                run.url = format!("{}/~{}/job/{}", self.base_url.replace("sr.ht", "builds.sr.ht"), owner, &run.id);
                run
            })
            .collect();

        Ok(runs)
    }

    async fn get_workflow_run(
        &self,
        owner: &str,
        repo: &str,
        run_id: &str,
    ) -> Result<WorkflowRun> {
        let url = self.builds_api_url(&format!("/jobs/{}", run_id));

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
                "Failed to get job: {} - {}",
                status, body
            )));
        }

        let job: SrhtJob = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse job: {}", e)))?;

        let mut run = WorkflowRun::from(job);
        run.url = format!(
            "{}/~{}/job/{}",
            self.base_url.replace("sr.ht", "builds.sr.ht"),
            owner,
            &run.id
        );

        Ok(run)
    }

    async fn enable_branch_protection(
        &self,
        owner: &str,
        repo: &str,
        branch: &str,
    ) -> Result<()> {
        // SourceHut doesn't have branch protection in the traditional sense
        // Protection is managed via ACLs
        Err(AdapterError::ApiError(
            "SourceHut uses ACL-based access control. Configure via web UI or API ACLs.".to_string(),
        ))
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
        // SourceHut uses email-based patch review via lists.sr.ht
        Err(AdapterError::ApiError(
            "SourceHut uses email-based patch review. Send patches to the project mailing list.".to_string(),
        ))
    }

    async fn list_prs(
        &self,
        owner: &str,
        repo: &str,
        state: Option<PullRequestState>,
    ) -> Result<Vec<PullRequest>> {
        // SourceHut doesn't have PRs - uses mailing list patches
        Ok(Vec::new())
    }

    async fn get_pr(&self, owner: &str, repo: &str, number: u64) -> Result<PullRequest> {
        Err(AdapterError::ApiError(
            "SourceHut uses email-based patch review instead of pull requests.".to_string(),
        ))
    }

    async fn merge_pr(
        &self,
        owner: &str,
        repo: &str,
        number: u64,
        commit_message: Option<&str>,
    ) -> Result<()> {
        Err(AdapterError::ApiError(
            "SourceHut uses email-based patch review. Apply patches with git am.".to_string(),
        ))
    }

    async fn close_pr(&self, owner: &str, repo: &str, number: u64) -> Result<()> {
        Err(AdapterError::ApiError(
            "SourceHut uses email-based patch review instead of pull requests.".to_string(),
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
        // Use todo.sr.ht tracker
        // First, we need to find or create a tracker for this repo
        let tracker_name = repo;
        let url = self.todo_api_url(&format!("/~{}/{}/tickets", owner, tracker_name));

        let payload = CreateTicketPayload {
            subject: title.to_string(),
            body: body.to_string(),
            labels,
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
                "Failed to create ticket: {} - {}",
                status, body
            )));
        }

        let ticket: SrhtTicket = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse ticket: {}", e)))?;

        let mut issue = Issue::from(ticket);
        issue.url = format!(
            "{}/~{}/{}/{}",
            self.base_url.replace("sr.ht", "todo.sr.ht"),
            owner,
            tracker_name,
            issue.number
        );

        Ok(issue)
    }

    async fn list_issues(
        &self,
        owner: &str,
        repo: &str,
        state: Option<IssueState>,
    ) -> Result<Vec<Issue>> {
        let tracker_name = repo;
        let url = self.todo_api_url(&format!("/~{}/{}/tickets", owner, tracker_name));

        let tickets: Vec<SrhtTicket> = self.get_paginated(&url, "results", 5).await?;

        let issues: Vec<Issue> = tickets
            .into_iter()
            .filter(|t| {
                match state {
                    Some(IssueState::Open) => {
                        matches!(t.status.as_str(), "reported" | "confirmed" | "in_progress")
                    }
                    Some(IssueState::Closed) => {
                        matches!(t.status.as_str(), "resolved" | "closed" | "wontfix")
                    }
                    None => true,
                }
            })
            .map(|t| {
                let mut issue = Issue::from(t);
                issue.url = format!(
                    "{}/~{}/{}/{}",
                    self.base_url.replace("sr.ht", "todo.sr.ht"),
                    owner,
                    tracker_name,
                    issue.number
                );
                issue
            })
            .collect();

        Ok(issues)
    }

    async fn get_issue(&self, owner: &str, repo: &str, number: u64) -> Result<Issue> {
        let tracker_name = repo;
        let url = self.todo_api_url(&format!("/~{}/{}/tickets/{}", owner, tracker_name, number));

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
                "Failed to get ticket: {} - {}",
                status, body
            )));
        }

        let ticket: SrhtTicket = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse ticket: {}", e)))?;

        let mut issue = Issue::from(ticket);
        issue.url = format!(
            "{}/~{}/{}/{}",
            self.base_url.replace("sr.ht", "todo.sr.ht"),
            owner,
            tracker_name,
            number
        );

        Ok(issue)
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
        let tracker_name = repo;
        let url = self.todo_api_url(&format!("/~{}/{}/tickets/{}", owner, tracker_name, number));

        let status = state.map(|s| match s {
            IssueState::Open => "confirmed".to_string(),
            IssueState::Closed => "resolved".to_string(),
        });

        let payload = UpdateTicketPayload {
            subject: title.map(String::from),
            body: body.map(String::from),
            status,
        };

        let response = self
            .client
            .put(&url)
            .json(&payload)
            .send()
            .await
            .map_err(|e| AdapterError::ApiError(e.to_string()))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(AdapterError::ApiError(format!(
                "Failed to update ticket: {} - {}",
                status, body
            )));
        }

        let ticket: SrhtTicket = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse ticket: {}", e)))?;

        let mut issue = Issue::from(ticket);
        issue.url = format!(
            "{}/~{}/{}/{}",
            self.base_url.replace("sr.ht", "todo.sr.ht"),
            owner,
            tracker_name,
            number
        );

        Ok(issue)
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
        let tracker_name = repo;
        let url = self.todo_api_url(&format!(
            "/~{}/{}/tickets/{}/comments",
            owner, tracker_name, issue_number
        ));

        let payload = CreateCommentPayload {
            text: body.to_string(),
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

        let comment: SrhtTicketComment = response
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
        // SourceHut doesn't have PRs - use mailing lists
        Err(AdapterError::ApiError(
            "SourceHut uses mailing list discussions. Reply to the patch email instead.".to_string(),
        ))
    }

    async fn list_issue_comments(
        &self,
        owner: &str,
        repo: &str,
        issue_number: u64,
    ) -> Result<Vec<Comment>> {
        let tracker_name = repo;
        let url = self.todo_api_url(&format!(
            "/~{}/{}/tickets/{}/comments",
            owner, tracker_name, issue_number
        ));

        let comments: Vec<SrhtTicketComment> = self.get_paginated(&url, "results", 5).await?;
        Ok(comments.into_iter().map(Comment::from).collect())
    }

    async fn list_pr_comments(
        &self,
        owner: &str,
        repo: &str,
        pr_number: u64,
    ) -> Result<Vec<Comment>> {
        // SourceHut doesn't have PRs
        Ok(Vec::new())
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
        // SourceHut builds can be submitted as check-like entities
        let url = self.builds_api_url("/jobs");

        let manifest = format!(
            r#"image: alpine/latest
sources:
  - {}#{}
tasks:
  - {}: |
      echo "Check run: {}"
"#,
            format!(
                "{}/~{}/{}",
                self.base_url.replace("sr.ht", "git.sr.ht"),
                owner,
                repo
            ),
            head_sha,
            name,
            name
        );

        let payload = SubmitBuildPayload {
            manifest,
            note: Some(name.to_string()),
            tags: vec![owner.to_string(), repo.to_string(), head_sha.to_string()],
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
                "Failed to create build: {} - {}",
                status, body
            )));
        }

        let job: SrhtJob = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse job: {}", e)))?;

        Ok(CheckRun::from(job))
    }

    async fn update_check_run(
        &self,
        owner: &str,
        repo: &str,
        check_run_id: &str,
        status: Option<CheckStatus>,
        conclusion: Option<CheckConclusion>,
    ) -> Result<CheckRun> {
        // SourceHut builds cannot be updated after submission
        defaults::update_check_run(self.forge())
    }

    async fn list_check_runs(
        &self,
        owner: &str,
        repo: &str,
        ref_name: &str,
    ) -> Result<Vec<CheckRun>> {
        // List builds tagged with the ref
        let url = self.builds_api_url("/jobs");
        let jobs: Vec<SrhtJob> = self.get_paginated(&url, "results", 5).await?;

        let checks: Vec<CheckRun> = jobs
            .into_iter()
            .filter(|j| j.tags.contains(&ref_name.to_string()))
            .map(CheckRun::from)
            .collect();

        Ok(checks)
    }

    async fn create_webhook(
        &self,
        owner: &str,
        repo: &str,
        config: WebhookConfig,
    ) -> Result<WebhookConfig> {
        // SourceHut uses webhooks per-service
        let url = self.git_api_url(&format!("/~{}/repos/{}/webhooks", owner, repo));

        let events: Vec<String> = config
            .events
            .iter()
            .map(|e| match e {
                WebhookEvent::Push => "repo:post-update",
                _ => "repo:post-update",
            })
            .map(String::from)
            .collect();

        let payload = serde_json::json!({
            "url": config.url,
            "events": events
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
                "Failed to create webhook: {} - {}",
                status, body
            )));
        }

        #[derive(Deserialize)]
        struct WebhookResponse {
            id: u64,
            url: String,
            events: Vec<String>,
        }

        let wh: WebhookResponse = response
            .json()
            .await
            .map_err(|e| AdapterError::ApiError(format!("Failed to parse webhook: {}", e)))?;

        Ok(WebhookConfig {
            id: Some(wh.id.to_string()),
            url: wh.url,
            secret: None,
            events: config.events,
            active: true,
            content_type: "json".to_string(),
        })
    }

    async fn list_webhooks(&self, owner: &str, repo: &str) -> Result<Vec<WebhookConfig>> {
        let url = self.git_api_url(&format!("/~{}/repos/{}/webhooks", owner, repo));

        #[derive(Deserialize)]
        struct WebhookResponse {
            id: u64,
            url: String,
            events: Vec<String>,
        }

        let webhooks: Vec<WebhookResponse> = self.get_paginated(&url, "results", 3).await?;

        Ok(webhooks
            .into_iter()
            .map(|wh| WebhookConfig {
                id: Some(wh.id.to_string()),
                url: wh.url,
                secret: None,
                events: wh
                    .events
                    .into_iter()
                    .filter_map(|e| {
                        if e.contains("post-update") {
                            Some(WebhookEvent::Push)
                        } else {
                            None
                        }
                    })
                    .collect(),
                active: true,
                content_type: "json".to_string(),
            })
            .collect())
    }

    async fn delete_webhook(&self, owner: &str, repo: &str, webhook_id: &str) -> Result<()> {
        let url = self.git_api_url(&format!(
            "/~{}/repos/{}/webhooks/{}",
            owner, repo, webhook_id
        ));

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
        // SourceHut webhooks don't have signature verification by default
        let event = match event_type {
            "repo:post-update" | "push" => WebhookEvent::Push,
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
        let adapter = SourcehutAdapter::new("test-token").unwrap();
        assert_eq!(adapter.base_url(), "https://sr.ht");
        assert!(matches!(adapter.forge(), Forge::Sourcehut));
    }

    #[test]
    fn test_custom_base_url() {
        let adapter =
            SourcehutAdapter::with_base_url("test-token", "https://sr.example.com").unwrap();
        assert_eq!(adapter.base_url(), "https://sr.example.com");
    }

    #[test]
    fn test_api_url_construction() {
        let adapter = SourcehutAdapter::new("test-token").unwrap();

        // Git API URL
        let git_url = adapter.git_api_url("/~user/repos");
        assert!(git_url.contains("git.sr.ht"));

        // Builds API URL
        let builds_url = adapter.builds_api_url("/jobs");
        assert!(builds_url.contains("builds.sr.ht"));

        // Todo API URL
        let todo_url = adapter.todo_api_url("/~user/tracker/tickets");
        assert!(todo_url.contains("todo.sr.ht"));
    }

    #[test]
    fn test_webhook_parsing() {
        let adapter = SourcehutAdapter::new("test-token").unwrap();

        let payload = serde_json::json!({
            "repository": {
                "name": "test-repo"
            },
            "pusher": {
                "canonical_name": "~testuser"
            }
        });

        let result = adapter
            .parse_webhook("push", None, payload.to_string().as_bytes(), None)
            .unwrap();

        assert!(matches!(result.event, WebhookEvent::Push));
        assert_eq!(result.repository, Some("test-repo".to_string()));
        assert_eq!(result.sender, Some("~testuser".to_string()));
    }
}
