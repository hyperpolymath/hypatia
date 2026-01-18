// SPDX-License-Identifier: PLMP-1.0-or-later
//! GitLab forge adapter
//!
//! Full implementation of the ForgeAdapter trait for GitLab.
//! Supports both GitLab.com and self-hosted GitLab instances via configurable base URL.

use crate::error::{AdapterError, Result};
use crate::forge::{
    Alert, AlertCategory, CheckConclusion, CheckRun, CheckStatus, Comment, Forge, ForgeAdapter,
    Issue, IssueState, PullRequest, PullRequestState, Repository, RunConclusion, RunStatus,
    Severity, Visibility, WebhookConfig, WebhookEvent, WebhookPayload, Workflow, WorkflowRun,
    WorkflowState,
};
use async_trait::async_trait;
use chrono::{DateTime, Utc};
use reqwest::header::{HeaderMap, HeaderValue, CONTENT_TYPE};
use serde::Deserialize;

/// GitLab adapter configuration
pub struct GitLabAdapter {
    client: reqwest::Client,
    base_url: String,
}

impl GitLabAdapter {
    /// Create new GitLab adapter
    pub fn new(token: &str) -> Result<Self> {
        Self::with_base_url(token, "https://gitlab.com/api/v4")
    }

    /// Create with custom base URL (for self-hosted GitLab)
    pub fn with_base_url(token: &str, base_url: &str) -> Result<Self> {
        let mut headers = HeaderMap::new();
        headers.insert(
            "PRIVATE-TOKEN",
            HeaderValue::from_str(token)
                .map_err(|_| AdapterError::ConfigError("Invalid token".into()))?,
        );
        headers.insert(CONTENT_TYPE, HeaderValue::from_static("application/json"));

        let client = reqwest::Client::builder()
            .default_headers(headers)
            .build()?;

        Ok(Self {
            client,
            base_url: base_url.trim_end_matches('/').to_string(),
        })
    }

    fn encode_path(path: &str) -> String {
        urlencoding::encode(path).to_string()
    }

    /// Get project ID from owner/repo path
    async fn get_project_id(&self, owner: &str, repo: &str) -> Result<u64> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}",
            self.base_url,
            Self::encode_path(&project_path)
        );

        let response: GitLabProject = self.client.get(&url).send().await?.json().await?;
        Ok(response.id)
    }
}

#[async_trait]
impl ForgeAdapter for GitLabAdapter {
    fn forge(&self) -> Forge {
        Forge::GitLab
    }

    fn base_url(&self) -> &str {
        &self.base_url
    }

    async fn list_repos(&self, owner: &str) -> Result<Vec<Repository>> {
        let url = format!(
            "{}/groups/{}/projects?per_page=100",
            self.base_url,
            Self::encode_path(owner)
        );
        let response: Vec<GitLabProject> = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .into_iter()
            .map(|p| Repository {
                id: p.id.to_string(),
                name: p.path,
                owner: owner.to_string(),
                forge: Forge::GitLab,
                url: p.web_url,
                visibility: match p.visibility.as_str() {
                    "public" => Visibility::Public,
                    "internal" => Visibility::Internal,
                    _ => Visibility::Private,
                },
                default_branch: p.default_branch.unwrap_or_else(|| "main".to_string()),
                languages: vec![],
            })
            .collect())
    }

    async fn get_repo(&self, owner: &str, repo: &str) -> Result<Repository> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}",
            self.base_url,
            Self::encode_path(&project_path)
        );

        let p: GitLabProject = self.client.get(&url).send().await?.json().await?;

        Ok(Repository {
            id: p.id.to_string(),
            name: p.path,
            owner: owner.to_string(),
            forge: Forge::GitLab,
            url: p.web_url,
            visibility: match p.visibility.as_str() {
                "public" => Visibility::Public,
                "internal" => Visibility::Internal,
                _ => Visibility::Private,
            },
            default_branch: p.default_branch.unwrap_or_else(|| "main".to_string()),
            languages: vec![],
        })
    }

    async fn get_alerts(&self, owner: &str, repo: &str) -> Result<Vec<Alert>> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/vulnerability_findings",
            self.base_url,
            Self::encode_path(&project_path)
        );

        let response: Vec<GitLabVulnerability> = match self.client.get(&url).send().await {
            Ok(resp) if resp.status().is_success() => resp.json().await?,
            Ok(resp) if resp.status().as_u16() == 404 => return Ok(vec![]),
            Ok(resp) => return Err(AdapterError::ApiError(format!("HTTP {}", resp.status()))),
            Err(e) => return Err(e.into()),
        };

        Ok(response
            .into_iter()
            .map(|v| Alert {
                id: v.id.to_string(),
                rule_id: v.scanner.id,
                severity: match v.severity.as_str() {
                    "critical" => Severity::Critical,
                    "high" => Severity::High,
                    "medium" => Severity::Medium,
                    "low" => Severity::Low,
                    _ => Severity::Info,
                },
                category: AlertCategory::CodeSecurity,
                description: v.name,
                file: v.location.file,
                line: v.location.start_line,
                auto_fixable: false,
            })
            .collect())
    }

    async fn list_workflows(&self, owner: &str, repo: &str) -> Result<Vec<Workflow>> {
        // GitLab uses .gitlab-ci.yml, not GitHub Actions
        // Return a single "workflow" representing the CI configuration
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/repository/files/.gitlab-ci.yml?ref=main",
            self.base_url,
            Self::encode_path(&project_path)
        );

        match self.client.get(&url).send().await {
            Ok(resp) if resp.status().is_success() => Ok(vec![Workflow {
                id: "gitlab-ci".to_string(),
                name: "GitLab CI".to_string(),
                file: ".gitlab-ci.yml".to_string(),
                state: WorkflowState::Active,
            }]),
            _ => Ok(vec![]),
        }
    }

    async fn deploy_workflow(
        &self,
        owner: &str,
        repo: &str,
        path: &str,
        content: &str,
        message: &str,
    ) -> Result<()> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/repository/files/{}",
            self.base_url,
            Self::encode_path(&project_path),
            Self::encode_path(path)
        );

        let body = serde_json::json!({
            "branch": "main",
            "content": content,
            "commit_message": message
        });

        // Try PUT first (update), then POST (create)
        let response = self.client.put(&url).json(&body).send().await?;

        if response.status().as_u16() == 404 {
            let response = self.client.post(&url).json(&body).send().await?;
            if !response.status().is_success() {
                return Err(AdapterError::ApiError(format!(
                    "Failed to create file: HTTP {}",
                    response.status()
                )));
            }
        } else if !response.status().is_success() {
            return Err(AdapterError::ApiError(format!(
                "Failed to update file: HTTP {}",
                response.status()
            )));
        }

        Ok(())
    }

    async fn trigger_workflow(
        &self,
        owner: &str,
        repo: &str,
        _workflow: &str,
        ref_name: &str,
    ) -> Result<()> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/pipeline",
            self.base_url,
            Self::encode_path(&project_path)
        );

        let body = serde_json::json!({
            "ref": ref_name
        });

        let response = self.client.post(&url).json(&body).send().await?;

        if !response.status().is_success() {
            return Err(AdapterError::ApiError(format!(
                "Failed to trigger pipeline: HTTP {}",
                response.status()
            )));
        }

        Ok(())
    }

    async fn list_workflow_runs(
        &self,
        owner: &str,
        repo: &str,
        _workflow_id: Option<&str>,
    ) -> Result<Vec<WorkflowRun>> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/pipelines?per_page=50",
            self.base_url,
            Self::encode_path(&project_path)
        );

        let response: Vec<GitLabPipeline> = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .into_iter()
            .map(|p| WorkflowRun {
                id: p.id.to_string(),
                workflow_id: "gitlab-ci".to_string(),
                name: format!("Pipeline #{}", p.id),
                status: match p.status.as_str() {
                    "pending" => RunStatus::Pending,
                    "running" => RunStatus::InProgress,
                    "success" | "failed" | "canceled" | "skipped" => RunStatus::Completed,
                    "waiting_for_resource" => RunStatus::Waiting,
                    "created" => RunStatus::Queued,
                    _ => RunStatus::Pending,
                },
                conclusion: match p.status.as_str() {
                    "success" => Some(RunConclusion::Success),
                    "failed" => Some(RunConclusion::Failure),
                    "canceled" => Some(RunConclusion::Cancelled),
                    "skipped" => Some(RunConclusion::Skipped),
                    _ => None,
                },
                head_branch: p.ref_name,
                head_sha: p.sha,
                url: p.web_url,
                created_at: p.created_at,
                updated_at: p.updated_at.unwrap_or(p.created_at),
            })
            .collect())
    }

    async fn get_workflow_run(
        &self,
        owner: &str,
        repo: &str,
        run_id: &str,
    ) -> Result<WorkflowRun> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/pipelines/{}",
            self.base_url,
            Self::encode_path(&project_path),
            run_id
        );

        let p: GitLabPipeline = self.client.get(&url).send().await?.json().await?;

        Ok(WorkflowRun {
            id: p.id.to_string(),
            workflow_id: "gitlab-ci".to_string(),
            name: format!("Pipeline #{}", p.id),
            status: match p.status.as_str() {
                "pending" => RunStatus::Pending,
                "running" => RunStatus::InProgress,
                "success" | "failed" | "canceled" | "skipped" => RunStatus::Completed,
                "waiting_for_resource" => RunStatus::Waiting,
                "created" => RunStatus::Queued,
                _ => RunStatus::Pending,
            },
            conclusion: match p.status.as_str() {
                "success" => Some(RunConclusion::Success),
                "failed" => Some(RunConclusion::Failure),
                "canceled" => Some(RunConclusion::Cancelled),
                "skipped" => Some(RunConclusion::Skipped),
                _ => None,
            },
            head_branch: p.ref_name,
            head_sha: p.sha,
            url: p.web_url,
            created_at: p.created_at,
            updated_at: p.updated_at.unwrap_or(p.created_at),
        })
    }

    async fn enable_branch_protection(&self, owner: &str, repo: &str, branch: &str) -> Result<()> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/protected_branches",
            self.base_url,
            Self::encode_path(&project_path)
        );

        let body = serde_json::json!({
            "name": branch,
            "push_access_level": 40,  // Maintainers
            "merge_access_level": 40,
            "allow_force_push": false
        });

        let response = self.client.post(&url).json(&body).send().await?;

        if !response.status().is_success() && response.status().as_u16() != 409 {
            return Err(AdapterError::ApiError(format!(
                "Failed to protect branch: HTTP {}",
                response.status()
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
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/merge_requests",
            self.base_url,
            Self::encode_path(&project_path)
        );

        let request_body = serde_json::json!({
            "source_branch": head,
            "target_branch": base,
            "title": title,
            "description": body
        });

        let response: GitLabMergeRequest = self
            .client
            .post(&url)
            .json(&request_body)
            .send()
            .await?
            .json()
            .await?;

        Ok(response.web_url)
    }

    async fn list_prs(
        &self,
        owner: &str,
        repo: &str,
        state: Option<PullRequestState>,
    ) -> Result<Vec<PullRequest>> {
        let project_path = format!("{}/{}", owner, repo);
        let state_param = match state {
            Some(PullRequestState::Open) => "opened",
            Some(PullRequestState::Closed) => "closed",
            Some(PullRequestState::Merged) => "merged",
            None => "all",
        };

        let url = format!(
            "{}/projects/{}/merge_requests?state={}",
            self.base_url,
            Self::encode_path(&project_path),
            state_param
        );

        let response: Vec<GitLabMergeRequestFull> =
            self.client.get(&url).send().await?.json().await?;

        Ok(response
            .into_iter()
            .map(|mr| PullRequest {
                id: mr.id.to_string(),
                number: mr.iid,
                title: mr.title,
                body: mr.description,
                state: match mr.state.as_str() {
                    "opened" => PullRequestState::Open,
                    "merged" => PullRequestState::Merged,
                    _ => PullRequestState::Closed,
                },
                author: mr.author.username,
                head_branch: mr.source_branch,
                base_branch: mr.target_branch,
                url: mr.web_url,
                mergeable: mr.merge_status.map(|s| s == "can_be_merged"),
                created_at: mr.created_at,
                updated_at: mr.updated_at,
            })
            .collect())
    }

    async fn get_pr(&self, owner: &str, repo: &str, number: u64) -> Result<PullRequest> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/merge_requests/{}",
            self.base_url,
            Self::encode_path(&project_path),
            number
        );

        let mr: GitLabMergeRequestFull = self.client.get(&url).send().await?.json().await?;

        Ok(PullRequest {
            id: mr.id.to_string(),
            number: mr.iid,
            title: mr.title,
            body: mr.description,
            state: match mr.state.as_str() {
                "opened" => PullRequestState::Open,
                "merged" => PullRequestState::Merged,
                _ => PullRequestState::Closed,
            },
            author: mr.author.username,
            head_branch: mr.source_branch,
            base_branch: mr.target_branch,
            url: mr.web_url,
            mergeable: mr.merge_status.map(|s| s == "can_be_merged"),
            created_at: mr.created_at,
            updated_at: mr.updated_at,
        })
    }

    async fn merge_pr(
        &self,
        owner: &str,
        repo: &str,
        number: u64,
        commit_message: Option<&str>,
    ) -> Result<()> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/merge_requests/{}/merge",
            self.base_url,
            Self::encode_path(&project_path),
            number
        );

        let mut body = serde_json::Map::new();
        if let Some(msg) = commit_message {
            body.insert(
                "merge_commit_message".to_string(),
                serde_json::json!(msg),
            );
        }
        body.insert("squash".to_string(), serde_json::json!(true));

        let response = self
            .client
            .put(&url)
            .json(&serde_json::Value::Object(body))
            .send()
            .await?;

        if !response.status().is_success() {
            return Err(AdapterError::ApiError(format!(
                "Failed to merge MR: HTTP {}",
                response.status()
            )));
        }

        Ok(())
    }

    async fn close_pr(&self, owner: &str, repo: &str, number: u64) -> Result<()> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/merge_requests/{}",
            self.base_url,
            Self::encode_path(&project_path),
            number
        );

        let body = serde_json::json!({
            "state_event": "close"
        });

        let response = self.client.put(&url).json(&body).send().await?;

        if !response.status().is_success() {
            return Err(AdapterError::ApiError(format!(
                "Failed to close MR: HTTP {}",
                response.status()
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
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/issues",
            self.base_url,
            Self::encode_path(&project_path)
        );

        let request_body = serde_json::json!({
            "title": title,
            "description": body,
            "labels": labels.join(",")
        });

        let response: GitLabIssue = self
            .client
            .post(&url)
            .json(&request_body)
            .send()
            .await?
            .json()
            .await?;

        Ok(Issue {
            id: response.id.to_string(),
            number: response.iid,
            title: response.title,
            body: response.description,
            state: if response.state == "opened" {
                IssueState::Open
            } else {
                IssueState::Closed
            },
            author: response.author.username,
            labels: response.labels,
            url: response.web_url,
            created_at: response.created_at,
            updated_at: response.updated_at,
        })
    }

    async fn list_issues(
        &self,
        owner: &str,
        repo: &str,
        state: Option<IssueState>,
    ) -> Result<Vec<Issue>> {
        let project_path = format!("{}/{}", owner, repo);
        let state_param = match state {
            Some(IssueState::Open) => "opened",
            Some(IssueState::Closed) => "closed",
            None => "all",
        };

        let url = format!(
            "{}/projects/{}/issues?state={}",
            self.base_url,
            Self::encode_path(&project_path),
            state_param
        );

        let response: Vec<GitLabIssue> = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .into_iter()
            .map(|i| Issue {
                id: i.id.to_string(),
                number: i.iid,
                title: i.title,
                body: i.description,
                state: if i.state == "opened" {
                    IssueState::Open
                } else {
                    IssueState::Closed
                },
                author: i.author.username,
                labels: i.labels,
                url: i.web_url,
                created_at: i.created_at,
                updated_at: i.updated_at,
            })
            .collect())
    }

    async fn get_issue(&self, owner: &str, repo: &str, number: u64) -> Result<Issue> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/issues/{}",
            self.base_url,
            Self::encode_path(&project_path),
            number
        );

        let i: GitLabIssue = self.client.get(&url).send().await?.json().await?;

        Ok(Issue {
            id: i.id.to_string(),
            number: i.iid,
            title: i.title,
            body: i.description,
            state: if i.state == "opened" {
                IssueState::Open
            } else {
                IssueState::Closed
            },
            author: i.author.username,
            labels: i.labels,
            url: i.web_url,
            created_at: i.created_at,
            updated_at: i.updated_at,
        })
    }

    async fn update_issue(
        &self,
        owner: &str,
        repo: &str,
        number: u64,
        title: Option<&str>,
        body: Option<&str>,
        state: Option<IssueState>,
        labels: Option<Vec<String>>,
    ) -> Result<Issue> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/issues/{}",
            self.base_url,
            Self::encode_path(&project_path),
            number
        );

        let mut request_body = serde_json::Map::new();
        if let Some(t) = title {
            request_body.insert("title".to_string(), serde_json::json!(t));
        }
        if let Some(b) = body {
            request_body.insert("description".to_string(), serde_json::json!(b));
        }
        if let Some(s) = state {
            request_body.insert(
                "state_event".to_string(),
                serde_json::json!(match s {
                    IssueState::Open => "reopen",
                    IssueState::Closed => "close",
                }),
            );
        }
        if let Some(l) = labels {
            request_body.insert("labels".to_string(), serde_json::json!(l.join(",")));
        }

        let i: GitLabIssue = self
            .client
            .put(&url)
            .json(&serde_json::Value::Object(request_body))
            .send()
            .await?
            .json()
            .await?;

        Ok(Issue {
            id: i.id.to_string(),
            number: i.iid,
            title: i.title,
            body: i.description,
            state: if i.state == "opened" {
                IssueState::Open
            } else {
                IssueState::Closed
            },
            author: i.author.username,
            labels: i.labels,
            url: i.web_url,
            created_at: i.created_at,
            updated_at: i.updated_at,
        })
    }

    async fn close_issue(&self, owner: &str, repo: &str, number: u64) -> Result<()> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/issues/{}",
            self.base_url,
            Self::encode_path(&project_path),
            number
        );

        let body = serde_json::json!({
            "state_event": "close"
        });

        let response = self.client.put(&url).json(&body).send().await?;

        if !response.status().is_success() {
            return Err(AdapterError::ApiError(format!(
                "Failed to close issue: HTTP {}",
                response.status()
            )));
        }

        Ok(())
    }

    async fn add_issue_comment(
        &self,
        owner: &str,
        repo: &str,
        issue_number: u64,
        body: &str,
    ) -> Result<Comment> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/issues/{}/notes",
            self.base_url,
            Self::encode_path(&project_path),
            issue_number
        );

        let request_body = serde_json::json!({
            "body": body
        });

        let n: GitLabNote = self
            .client
            .post(&url)
            .json(&request_body)
            .send()
            .await?
            .json()
            .await?;

        Ok(Comment {
            id: n.id.to_string(),
            body: n.body,
            author: n.author.username,
            created_at: n.created_at,
            updated_at: n.updated_at,
        })
    }

    async fn add_pr_comment(
        &self,
        owner: &str,
        repo: &str,
        pr_number: u64,
        body: &str,
    ) -> Result<Comment> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/merge_requests/{}/notes",
            self.base_url,
            Self::encode_path(&project_path),
            pr_number
        );

        let request_body = serde_json::json!({
            "body": body
        });

        let n: GitLabNote = self
            .client
            .post(&url)
            .json(&request_body)
            .send()
            .await?
            .json()
            .await?;

        Ok(Comment {
            id: n.id.to_string(),
            body: n.body,
            author: n.author.username,
            created_at: n.created_at,
            updated_at: n.updated_at,
        })
    }

    async fn list_issue_comments(
        &self,
        owner: &str,
        repo: &str,
        issue_number: u64,
    ) -> Result<Vec<Comment>> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/issues/{}/notes",
            self.base_url,
            Self::encode_path(&project_path),
            issue_number
        );

        let response: Vec<GitLabNote> = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .into_iter()
            .filter(|n| !n.system) // Filter out system notes
            .map(|n| Comment {
                id: n.id.to_string(),
                body: n.body,
                author: n.author.username,
                created_at: n.created_at,
                updated_at: n.updated_at,
            })
            .collect())
    }

    async fn list_pr_comments(
        &self,
        owner: &str,
        repo: &str,
        pr_number: u64,
    ) -> Result<Vec<Comment>> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/merge_requests/{}/notes",
            self.base_url,
            Self::encode_path(&project_path),
            pr_number
        );

        let response: Vec<GitLabNote> = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .into_iter()
            .filter(|n| !n.system) // Filter out system notes
            .map(|n| Comment {
                id: n.id.to_string(),
                body: n.body,
                author: n.author.username,
                created_at: n.created_at,
                updated_at: n.updated_at,
            })
            .collect())
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
        // GitLab uses commit statuses instead of check runs
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/statuses/{}",
            self.base_url,
            Self::encode_path(&project_path),
            head_sha
        );

        let gitlab_state = match (status, conclusion) {
            (CheckStatus::Completed, Some(CheckConclusion::Success)) => "success",
            (CheckStatus::Completed, Some(CheckConclusion::Failure)) => "failed",
            (CheckStatus::Completed, Some(CheckConclusion::Cancelled)) => "canceled",
            (CheckStatus::InProgress, _) => "running",
            (CheckStatus::Queued, _) | (CheckStatus::Completed, _) => "pending",
        };

        let body = serde_json::json!({
            "state": gitlab_state,
            "name": name,
            "context": name
        });

        let cs: GitLabCommitStatus = self
            .client
            .post(&url)
            .json(&body)
            .send()
            .await?
            .json()
            .await?;

        Ok(CheckRun {
            id: cs.id.to_string(),
            name: cs.name,
            status: match cs.status.as_str() {
                "pending" | "created" => CheckStatus::Queued,
                "running" => CheckStatus::InProgress,
                _ => CheckStatus::Completed,
            },
            conclusion: match cs.status.as_str() {
                "success" => Some(CheckConclusion::Success),
                "failed" => Some(CheckConclusion::Failure),
                "canceled" => Some(CheckConclusion::Cancelled),
                _ => None,
            },
            head_sha: head_sha.to_string(),
            url: cs.target_url,
            started_at: cs.created_at,
            completed_at: cs.finished_at,
        })
    }

    async fn update_check_run(
        &self,
        owner: &str,
        repo: &str,
        _check_run_id: &str,
        status: Option<CheckStatus>,
        conclusion: Option<CheckConclusion>,
    ) -> Result<CheckRun> {
        // GitLab doesn't support updating commit statuses, need to create a new one
        // This requires knowing the sha, which we don't have from just the check_run_id
        // Return an error for unsupported operation
        crate::forge::defaults::not_supported("update_check_run", Forge::GitLab)?;

        // Unreachable but needed for type inference
        Ok(CheckRun {
            id: String::new(),
            name: String::new(),
            status: status.unwrap_or(CheckStatus::Queued),
            conclusion,
            head_sha: String::new(),
            url: None,
            started_at: None,
            completed_at: None,
        })
    }

    async fn list_check_runs(
        &self,
        owner: &str,
        repo: &str,
        ref_name: &str,
    ) -> Result<Vec<CheckRun>> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/repository/commits/{}/statuses",
            self.base_url,
            Self::encode_path(&project_path),
            ref_name
        );

        let response: Vec<GitLabCommitStatus> =
            self.client.get(&url).send().await?.json().await?;

        Ok(response
            .into_iter()
            .map(|cs| CheckRun {
                id: cs.id.to_string(),
                name: cs.name,
                status: match cs.status.as_str() {
                    "pending" | "created" => CheckStatus::Queued,
                    "running" => CheckStatus::InProgress,
                    _ => CheckStatus::Completed,
                },
                conclusion: match cs.status.as_str() {
                    "success" => Some(CheckConclusion::Success),
                    "failed" => Some(CheckConclusion::Failure),
                    "canceled" => Some(CheckConclusion::Cancelled),
                    _ => None,
                },
                head_sha: cs.sha.unwrap_or_default(),
                url: cs.target_url,
                started_at: cs.created_at,
                completed_at: cs.finished_at,
            })
            .collect())
    }

    async fn create_webhook(
        &self,
        owner: &str,
        repo: &str,
        config: WebhookConfig,
    ) -> Result<WebhookConfig> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/hooks",
            self.base_url,
            Self::encode_path(&project_path)
        );

        // Map webhook events to GitLab hook triggers
        let mut body = serde_json::json!({
            "url": config.url,
            "enable_ssl_verification": true
        });

        if let Some(secret) = &config.secret {
            body["token"] = serde_json::json!(secret);
        }

        for event in &config.events {
            match event {
                WebhookEvent::Push => body["push_events"] = serde_json::json!(true),
                WebhookEvent::PullRequest => {
                    body["merge_requests_events"] = serde_json::json!(true)
                }
                WebhookEvent::Issues => body["issues_events"] = serde_json::json!(true),
                WebhookEvent::IssueComment => body["note_events"] = serde_json::json!(true),
                WebhookEvent::Create | WebhookEvent::Delete => {
                    body["tag_push_events"] = serde_json::json!(true)
                }
                WebhookEvent::WorkflowRun => body["pipeline_events"] = serde_json::json!(true),
                WebhookEvent::Release => body["releases_events"] = serde_json::json!(true),
                _ => {}
            }
        }

        let hook: GitLabWebhook = self
            .client
            .post(&url)
            .json(&body)
            .send()
            .await?
            .json()
            .await?;

        Ok(WebhookConfig {
            id: Some(hook.id.to_string()),
            url: hook.url,
            events: config.events,
            active: true,
            secret: None, // GitLab doesn't return the secret
            content_type: "application/json".to_string(),
        })
    }

    async fn list_webhooks(&self, owner: &str, repo: &str) -> Result<Vec<WebhookConfig>> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/hooks",
            self.base_url,
            Self::encode_path(&project_path)
        );

        let response: Vec<GitLabWebhook> = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .into_iter()
            .map(|h| {
                let mut events = Vec::new();
                if h.push_events.unwrap_or(false) {
                    events.push(WebhookEvent::Push);
                }
                if h.merge_requests_events.unwrap_or(false) {
                    events.push(WebhookEvent::PullRequest);
                }
                if h.issues_events.unwrap_or(false) {
                    events.push(WebhookEvent::Issues);
                }
                if h.note_events.unwrap_or(false) {
                    events.push(WebhookEvent::IssueComment);
                }
                if h.tag_push_events.unwrap_or(false) {
                    events.push(WebhookEvent::Create);
                }
                if h.pipeline_events.unwrap_or(false) {
                    events.push(WebhookEvent::WorkflowRun);
                }
                if h.releases_events.unwrap_or(false) {
                    events.push(WebhookEvent::Release);
                }

                WebhookConfig {
                    id: Some(h.id.to_string()),
                    url: h.url,
                    events,
                    active: true,
                    secret: None,
                    content_type: "application/json".to_string(),
                }
            })
            .collect())
    }

    async fn delete_webhook(&self, owner: &str, repo: &str, webhook_id: &str) -> Result<()> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/hooks/{}",
            self.base_url,
            Self::encode_path(&project_path),
            webhook_id
        );

        let response = self.client.delete(&url).send().await?;

        if !response.status().is_success() && response.status().as_u16() != 404 {
            return Err(AdapterError::ApiError(format!(
                "Failed to delete webhook: HTTP {}",
                response.status()
            )));
        }

        Ok(())
    }

    fn parse_webhook(
        &self,
        event_type: &str,
        _signature: Option<&str>,
        payload: &[u8],
        _secret: Option<&str>,
    ) -> Result<WebhookPayload> {
        // GitLab uses X-Gitlab-Token header for validation, which should be done at HTTP layer
        // Here we just parse the event type
        let event = match event_type {
            "Push Hook" => WebhookEvent::Push,
            "Merge Request Hook" => WebhookEvent::PullRequest,
            "Issue Hook" => WebhookEvent::Issues,
            "Note Hook" => WebhookEvent::IssueComment,
            "Tag Push Hook" => WebhookEvent::Create,
            "Pipeline Hook" => WebhookEvent::WorkflowRun,
            "Release Hook" => WebhookEvent::Release,
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

// ============== GitLab API Response Types ==============

#[derive(Debug, Deserialize)]
struct GitLabProject {
    id: u64,
    path: String,
    web_url: String,
    visibility: String,
    default_branch: Option<String>,
}

#[derive(Debug, Deserialize)]
struct GitLabVulnerability {
    id: u64,
    name: String,
    severity: String,
    scanner: GitLabScanner,
    location: GitLabLocation,
}

#[derive(Debug, Deserialize)]
struct GitLabScanner {
    id: String,
}

#[derive(Debug, Deserialize)]
struct GitLabLocation {
    file: Option<String>,
    start_line: Option<u32>,
}

#[derive(Debug, Deserialize)]
struct GitLabMergeRequest {
    web_url: String,
}

#[derive(Debug, Deserialize)]
struct GitLabMergeRequestFull {
    id: u64,
    iid: u64,
    title: String,
    description: Option<String>,
    state: String,
    author: GitLabUser,
    source_branch: String,
    target_branch: String,
    web_url: String,
    merge_status: Option<String>,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
}

#[derive(Debug, Deserialize)]
struct GitLabUser {
    username: String,
}

#[derive(Debug, Deserialize)]
struct GitLabIssue {
    id: u64,
    iid: u64,
    title: String,
    description: Option<String>,
    state: String,
    author: GitLabUser,
    labels: Vec<String>,
    web_url: String,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
}

#[derive(Debug, Deserialize)]
struct GitLabNote {
    id: u64,
    body: String,
    author: GitLabUser,
    system: bool,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
}

#[derive(Debug, Deserialize)]
struct GitLabPipeline {
    id: u64,
    status: String,
    #[serde(rename = "ref")]
    ref_name: String,
    sha: String,
    web_url: String,
    created_at: DateTime<Utc>,
    updated_at: Option<DateTime<Utc>>,
}

#[derive(Debug, Deserialize)]
struct GitLabCommitStatus {
    id: u64,
    name: String,
    status: String,
    sha: Option<String>,
    target_url: Option<String>,
    created_at: Option<DateTime<Utc>>,
    finished_at: Option<DateTime<Utc>>,
}

#[derive(Debug, Deserialize)]
struct GitLabWebhook {
    id: u64,
    url: String,
    push_events: Option<bool>,
    merge_requests_events: Option<bool>,
    issues_events: Option<bool>,
    note_events: Option<bool>,
    tag_push_events: Option<bool>,
    pipeline_events: Option<bool>,
    releases_events: Option<bool>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gitlab_adapter_creation() {
        let adapter = GitLabAdapter::new("test-token");
        assert!(adapter.is_ok());
        let adapter = adapter.unwrap();
        assert_eq!(adapter.forge(), Forge::GitLab);
        assert_eq!(adapter.base_url(), "https://gitlab.com/api/v4");
    }

    #[test]
    fn test_gitlab_adapter_self_hosted() {
        let adapter =
            GitLabAdapter::with_base_url("test-token", "https://gitlab.company.com/api/v4");
        assert!(adapter.is_ok());
        let adapter = adapter.unwrap();
        assert_eq!(adapter.base_url(), "https://gitlab.company.com/api/v4");
    }

    #[test]
    fn test_path_encoding() {
        let encoded = GitLabAdapter::encode_path("group/subgroup/project");
        assert_eq!(encoded, "group%2Fsubgroup%2Fproject");
    }
}
