// SPDX-License-Identifier: PLMP-1.0-or-later
//! GitHub forge adapter
//!
//! Full implementation of the ForgeAdapter trait for GitHub.
//! Supports both GitHub.com and GitHub Enterprise via configurable base URL.

use crate::error::{AdapterError, Result};
use crate::forge::{
    Alert, AlertCategory, CheckConclusion, CheckRun, CheckStatus, Comment, Forge, ForgeAdapter,
    Issue, IssueState, PullRequest, PullRequestState, Repository, RunConclusion, RunStatus,
    Severity, Visibility, WebhookConfig, WebhookEvent, WebhookPayload, Workflow, WorkflowRun,
    WorkflowState,
};
use async_trait::async_trait;
use chrono::{DateTime, Utc};
use hmac::{Hmac, Mac};
use reqwest::header::{HeaderMap, HeaderValue, ACCEPT, AUTHORIZATION, USER_AGENT};
use serde::{Deserialize, Serialize};
use sha2::Sha256;

type HmacSha256 = Hmac<Sha256>;

/// GitHub adapter configuration
pub struct GitHubAdapter {
    client: reqwest::Client,
    base_url: String,
    #[allow(dead_code)]
    token: String,
}

impl GitHubAdapter {
    /// Create new GitHub adapter
    pub fn new(token: &str) -> Result<Self> {
        Self::with_base_url(token, "https://api.github.com")
    }

    /// Create with custom base URL (for GitHub Enterprise)
    pub fn with_base_url(token: &str, base_url: &str) -> Result<Self> {
        let mut headers = HeaderMap::new();
        headers.insert(
            ACCEPT,
            HeaderValue::from_static("application/vnd.github+json"),
        );
        headers.insert(
            AUTHORIZATION,
            HeaderValue::from_str(&format!("Bearer {}", token))
                .map_err(|_| AdapterError::ConfigError("Invalid token".into()))?,
        );
        headers.insert(USER_AGENT, HeaderValue::from_static("cicd-hyper-a/0.1.0"));
        headers.insert(
            "X-GitHub-Api-Version",
            HeaderValue::from_static("2022-11-28"),
        );

        let client = reqwest::Client::builder()
            .default_headers(headers)
            .build()?;

        Ok(Self {
            client,
            base_url: base_url.trim_end_matches('/').to_string(),
            token: token.to_string(),
        })
    }

    /// Helper to make GET requests with pagination support
    async fn get_paginated<T: for<'de> Deserialize<'de>>(
        &self,
        url: &str,
        per_page: u32,
    ) -> Result<Vec<T>> {
        let mut all_results = Vec::new();
        let mut page = 1;

        loop {
            let paginated_url = format!("{}?per_page={}&page={}", url, per_page, page);
            let response = self.client.get(&paginated_url).send().await?;

            if !response.status().is_success() {
                if response.status().as_u16() == 404 {
                    return Ok(vec![]);
                }
                return Err(AdapterError::ApiError(format!(
                    "HTTP {}",
                    response.status()
                )));
            }

            let results: Vec<T> = response.json().await?;
            if results.is_empty() {
                break;
            }

            all_results.extend(results);
            page += 1;

            // Safety limit to prevent infinite loops
            if page > 100 {
                break;
            }
        }

        Ok(all_results)
    }
}

#[async_trait]
impl ForgeAdapter for GitHubAdapter {
    fn forge(&self) -> Forge {
        Forge::GitHub
    }

    fn base_url(&self) -> &str {
        &self.base_url
    }

    async fn list_repos(&self, owner: &str) -> Result<Vec<Repository>> {
        let url = format!("{}/orgs/{}/repos", self.base_url, owner);
        let response: Vec<GitHubRepo> = self.get_paginated(&url, 100).await?;

        Ok(response
            .into_iter()
            .map(|r| Repository {
                id: r.id.to_string(),
                name: r.name,
                owner: owner.to_string(),
                forge: Forge::GitHub,
                url: r.html_url,
                visibility: if r.private {
                    Visibility::Private
                } else {
                    Visibility::Public
                },
                default_branch: r.default_branch,
                languages: vec![],
            })
            .collect())
    }

    async fn get_repo(&self, owner: &str, repo: &str) -> Result<Repository> {
        let url = format!("{}/repos/{}/{}", self.base_url, owner, repo);
        let response: GitHubRepo = self.client.get(&url).send().await?.json().await?;

        Ok(Repository {
            id: response.id.to_string(),
            name: response.name,
            owner: owner.to_string(),
            forge: Forge::GitHub,
            url: response.html_url,
            visibility: if response.private {
                Visibility::Private
            } else {
                Visibility::Public
            },
            default_branch: response.default_branch,
            languages: vec![],
        })
    }

    async fn get_alerts(&self, owner: &str, repo: &str) -> Result<Vec<Alert>> {
        let url = format!(
            "{}/repos/{}/{}/code-scanning/alerts",
            self.base_url, owner, repo
        );
        let response: Vec<GitHubAlert> = match self.client.get(&url).send().await {
            Ok(resp) if resp.status().is_success() => resp.json().await?,
            Ok(resp) if resp.status().as_u16() == 404 => return Ok(vec![]),
            Ok(resp) => return Err(AdapterError::ApiError(format!("HTTP {}", resp.status()))),
            Err(e) => return Err(e.into()),
        };

        Ok(response
            .into_iter()
            .map(|a| Alert {
                id: a.number.to_string(),
                rule_id: a.rule.id,
                severity: match a.rule.severity.as_str() {
                    "critical" | "error" => Severity::Critical,
                    "high" | "warning" => Severity::High,
                    "medium" => Severity::Medium,
                    "low" | "note" => Severity::Low,
                    _ => Severity::Info,
                },
                category: AlertCategory::CodeSecurity,
                description: a.rule.description,
                file: a.most_recent_instance.location.path,
                line: a.most_recent_instance.location.start_line,
                auto_fixable: false,
            })
            .collect())
    }

    async fn list_workflows(&self, owner: &str, repo: &str) -> Result<Vec<Workflow>> {
        let url = format!(
            "{}/repos/{}/{}/actions/workflows",
            self.base_url, owner, repo
        );
        let response: GitHubWorkflows = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .workflows
            .into_iter()
            .map(|w| Workflow {
                id: w.id.to_string(),
                name: w.name,
                file: w.path,
                state: match w.state.as_str() {
                    "active" => WorkflowState::Active,
                    "disabled" | "disabled_manually" => WorkflowState::Disabled,
                    _ => WorkflowState::Unknown,
                },
            })
            .collect())
    }

    async fn deploy_workflow(
        &self,
        owner: &str,
        repo: &str,
        path: &str,
        content: &str,
        message: &str,
    ) -> Result<()> {
        let url = format!(
            "{}/repos/{}/{}/contents/{}",
            self.base_url, owner, repo, path
        );

        let encoded = base64::Engine::encode(
            &base64::engine::general_purpose::STANDARD,
            content.as_bytes(),
        );

        let body = serde_json::json!({
            "message": message,
            "content": encoded,
        });

        let response = self.client.put(&url).json(&body).send().await?;

        if !response.status().is_success() {
            return Err(AdapterError::ApiError(format!(
                "Failed to deploy workflow: HTTP {}",
                response.status()
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
        let url = format!(
            "{}/repos/{}/{}/actions/workflows/{}/dispatches",
            self.base_url, owner, repo, workflow
        );

        let body = serde_json::json!({
            "ref": ref_name
        });

        let response = self.client.post(&url).json(&body).send().await?;

        if !response.status().is_success() {
            return Err(AdapterError::ApiError(format!(
                "Failed to trigger workflow: HTTP {}",
                response.status()
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
        let url = match workflow_id {
            Some(wf_id) => format!(
                "{}/repos/{}/{}/actions/workflows/{}/runs",
                self.base_url, owner, repo, wf_id
            ),
            None => format!(
                "{}/repos/{}/{}/actions/runs",
                self.base_url, owner, repo
            ),
        };

        let response: GitHubWorkflowRuns = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .workflow_runs
            .into_iter()
            .map(|r| WorkflowRun {
                id: r.id.to_string(),
                workflow_id: r.workflow_id.to_string(),
                name: r.name,
                status: match r.status.as_str() {
                    "queued" => RunStatus::Queued,
                    "in_progress" => RunStatus::InProgress,
                    "completed" => RunStatus::Completed,
                    "waiting" => RunStatus::Waiting,
                    "requested" => RunStatus::Requested,
                    "pending" => RunStatus::Pending,
                    _ => RunStatus::Pending,
                },
                conclusion: r.conclusion.map(|c| match c.as_str() {
                    "success" => RunConclusion::Success,
                    "failure" => RunConclusion::Failure,
                    "neutral" => RunConclusion::Neutral,
                    "cancelled" => RunConclusion::Cancelled,
                    "skipped" => RunConclusion::Skipped,
                    "timed_out" => RunConclusion::TimedOut,
                    "action_required" => RunConclusion::ActionRequired,
                    _ => RunConclusion::Neutral,
                }),
                head_branch: r.head_branch,
                head_sha: r.head_sha,
                url: r.html_url,
                created_at: r.created_at,
                updated_at: r.updated_at,
            })
            .collect())
    }

    async fn get_workflow_run(
        &self,
        owner: &str,
        repo: &str,
        run_id: &str,
    ) -> Result<WorkflowRun> {
        let url = format!(
            "{}/repos/{}/{}/actions/runs/{}",
            self.base_url, owner, repo, run_id
        );

        let r: GitHubWorkflowRunDetail = self.client.get(&url).send().await?.json().await?;

        Ok(WorkflowRun {
            id: r.id.to_string(),
            workflow_id: r.workflow_id.to_string(),
            name: r.name,
            status: match r.status.as_str() {
                "queued" => RunStatus::Queued,
                "in_progress" => RunStatus::InProgress,
                "completed" => RunStatus::Completed,
                "waiting" => RunStatus::Waiting,
                "requested" => RunStatus::Requested,
                "pending" => RunStatus::Pending,
                _ => RunStatus::Pending,
            },
            conclusion: r.conclusion.map(|c| match c.as_str() {
                "success" => RunConclusion::Success,
                "failure" => RunConclusion::Failure,
                "neutral" => RunConclusion::Neutral,
                "cancelled" => RunConclusion::Cancelled,
                "skipped" => RunConclusion::Skipped,
                "timed_out" => RunConclusion::TimedOut,
                "action_required" => RunConclusion::ActionRequired,
                _ => RunConclusion::Neutral,
            }),
            head_branch: r.head_branch,
            head_sha: r.head_sha,
            url: r.html_url,
            created_at: r.created_at,
            updated_at: r.updated_at,
        })
    }

    async fn enable_branch_protection(&self, owner: &str, repo: &str, branch: &str) -> Result<()> {
        let url = format!(
            "{}/repos/{}/{}/branches/{}/protection",
            self.base_url, owner, repo, branch
        );

        let body = serde_json::json!({
            "required_status_checks": null,
            "enforce_admins": false,
            "required_pull_request_reviews": {
                "required_approving_review_count": 1
            },
            "restrictions": null,
            "allow_force_pushes": false,
            "allow_deletions": false
        });

        let response = self.client.put(&url).json(&body).send().await?;

        if !response.status().is_success() {
            return Err(AdapterError::ApiError(format!(
                "Failed to enable branch protection: HTTP {}",
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
        let url = format!("{}/repos/{}/{}/pulls", self.base_url, owner, repo);

        let request_body = serde_json::json!({
            "title": title,
            "body": body,
            "head": head,
            "base": base
        });

        let response: GitHubPullRequestResponse = self
            .client
            .post(&url)
            .json(&request_body)
            .send()
            .await?
            .json()
            .await?;

        Ok(response.html_url)
    }

    async fn list_prs(
        &self,
        owner: &str,
        repo: &str,
        state: Option<PullRequestState>,
    ) -> Result<Vec<PullRequest>> {
        let state_str = match state {
            Some(PullRequestState::Open) => "open",
            Some(PullRequestState::Closed) => "closed",
            Some(PullRequestState::Merged) => "closed", // GitHub doesn't have merged state filter
            None => "all",
        };

        let url = format!(
            "{}/repos/{}/{}/pulls?state={}",
            self.base_url, owner, repo, state_str
        );

        let response: Vec<GitHubPullRequestFull> = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .into_iter()
            .filter(|pr| {
                // Filter for merged if specifically requested
                if matches!(state, Some(PullRequestState::Merged)) {
                    pr.merged_at.is_some()
                } else {
                    true
                }
            })
            .map(|pr| PullRequest {
                id: pr.id.to_string(),
                number: pr.number,
                title: pr.title,
                body: pr.body,
                state: if pr.merged_at.is_some() {
                    PullRequestState::Merged
                } else if pr.state == "closed" {
                    PullRequestState::Closed
                } else {
                    PullRequestState::Open
                },
                author: pr.user.login,
                head_branch: pr.head.ref_name,
                base_branch: pr.base.ref_name,
                url: pr.html_url,
                mergeable: pr.mergeable,
                created_at: pr.created_at,
                updated_at: pr.updated_at,
            })
            .collect())
    }

    async fn get_pr(&self, owner: &str, repo: &str, number: u64) -> Result<PullRequest> {
        let url = format!(
            "{}/repos/{}/{}/pulls/{}",
            self.base_url, owner, repo, number
        );

        let pr: GitHubPullRequestFull = self.client.get(&url).send().await?.json().await?;

        Ok(PullRequest {
            id: pr.id.to_string(),
            number: pr.number,
            title: pr.title,
            body: pr.body,
            state: if pr.merged_at.is_some() {
                PullRequestState::Merged
            } else if pr.state == "closed" {
                PullRequestState::Closed
            } else {
                PullRequestState::Open
            },
            author: pr.user.login,
            head_branch: pr.head.ref_name,
            base_branch: pr.base.ref_name,
            url: pr.html_url,
            mergeable: pr.mergeable,
            created_at: pr.created_at,
            updated_at: pr.updated_at,
        })
    }

    async fn merge_pr(
        &self,
        owner: &str,
        repo: &str,
        number: u64,
        commit_message: Option<&str>,
    ) -> Result<()> {
        let url = format!(
            "{}/repos/{}/{}/pulls/{}/merge",
            self.base_url, owner, repo, number
        );

        let body = serde_json::json!({
            "commit_message": commit_message,
            "merge_method": "squash"
        });

        let response = self.client.put(&url).json(&body).send().await?;

        if !response.status().is_success() {
            return Err(AdapterError::ApiError(format!(
                "Failed to merge PR: HTTP {}",
                response.status()
            )));
        }

        Ok(())
    }

    async fn close_pr(&self, owner: &str, repo: &str, number: u64) -> Result<()> {
        let url = format!(
            "{}/repos/{}/{}/pulls/{}",
            self.base_url, owner, repo, number
        );

        let body = serde_json::json!({
            "state": "closed"
        });

        let response = self.client.patch(&url).json(&body).send().await?;

        if !response.status().is_success() {
            return Err(AdapterError::ApiError(format!(
                "Failed to close PR: HTTP {}",
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
        let url = format!("{}/repos/{}/{}/issues", self.base_url, owner, repo);

        let request_body = serde_json::json!({
            "title": title,
            "body": body,
            "labels": labels
        });

        let response: GitHubIssue = self
            .client
            .post(&url)
            .json(&request_body)
            .send()
            .await?
            .json()
            .await?;

        Ok(Issue {
            id: response.id.to_string(),
            number: response.number,
            title: response.title,
            body: response.body,
            state: if response.state == "open" {
                IssueState::Open
            } else {
                IssueState::Closed
            },
            author: response.user.login,
            labels: response.labels.into_iter().map(|l| l.name).collect(),
            url: response.html_url,
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
        let state_str = match state {
            Some(IssueState::Open) => "open",
            Some(IssueState::Closed) => "closed",
            None => "all",
        };

        let url = format!(
            "{}/repos/{}/{}/issues?state={}",
            self.base_url, owner, repo, state_str
        );

        let response: Vec<GitHubIssue> = self.client.get(&url).send().await?.json().await?;

        // Filter out pull requests (GitHub API returns PRs in issues endpoint)
        Ok(response
            .into_iter()
            .filter(|i| i.pull_request.is_none())
            .map(|i| Issue {
                id: i.id.to_string(),
                number: i.number,
                title: i.title,
                body: i.body,
                state: if i.state == "open" {
                    IssueState::Open
                } else {
                    IssueState::Closed
                },
                author: i.user.login,
                labels: i.labels.into_iter().map(|l| l.name).collect(),
                url: i.html_url,
                created_at: i.created_at,
                updated_at: i.updated_at,
            })
            .collect())
    }

    async fn get_issue(&self, owner: &str, repo: &str, number: u64) -> Result<Issue> {
        let url = format!(
            "{}/repos/{}/{}/issues/{}",
            self.base_url, owner, repo, number
        );

        let i: GitHubIssue = self.client.get(&url).send().await?.json().await?;

        Ok(Issue {
            id: i.id.to_string(),
            number: i.number,
            title: i.title,
            body: i.body,
            state: if i.state == "open" {
                IssueState::Open
            } else {
                IssueState::Closed
            },
            author: i.user.login,
            labels: i.labels.into_iter().map(|l| l.name).collect(),
            url: i.html_url,
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
        let url = format!(
            "{}/repos/{}/{}/issues/{}",
            self.base_url, owner, repo, number
        );

        let mut request_body = serde_json::Map::new();
        if let Some(t) = title {
            request_body.insert("title".to_string(), serde_json::json!(t));
        }
        if let Some(b) = body {
            request_body.insert("body".to_string(), serde_json::json!(b));
        }
        if let Some(s) = state {
            request_body.insert(
                "state".to_string(),
                serde_json::json!(match s {
                    IssueState::Open => "open",
                    IssueState::Closed => "closed",
                }),
            );
        }
        if let Some(l) = labels {
            request_body.insert("labels".to_string(), serde_json::json!(l));
        }

        let i: GitHubIssue = self
            .client
            .patch(&url)
            .json(&serde_json::Value::Object(request_body))
            .send()
            .await?
            .json()
            .await?;

        Ok(Issue {
            id: i.id.to_string(),
            number: i.number,
            title: i.title,
            body: i.body,
            state: if i.state == "open" {
                IssueState::Open
            } else {
                IssueState::Closed
            },
            author: i.user.login,
            labels: i.labels.into_iter().map(|l| l.name).collect(),
            url: i.html_url,
            created_at: i.created_at,
            updated_at: i.updated_at,
        })
    }

    async fn close_issue(&self, owner: &str, repo: &str, number: u64) -> Result<()> {
        let url = format!(
            "{}/repos/{}/{}/issues/{}",
            self.base_url, owner, repo, number
        );

        let body = serde_json::json!({
            "state": "closed"
        });

        let response = self.client.patch(&url).json(&body).send().await?;

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
        let url = format!(
            "{}/repos/{}/{}/issues/{}/comments",
            self.base_url, owner, repo, issue_number
        );

        let request_body = serde_json::json!({
            "body": body
        });

        let c: GitHubComment = self
            .client
            .post(&url)
            .json(&request_body)
            .send()
            .await?
            .json()
            .await?;

        Ok(Comment {
            id: c.id.to_string(),
            body: c.body,
            author: c.user.login,
            created_at: c.created_at,
            updated_at: c.updated_at,
        })
    }

    async fn add_pr_comment(
        &self,
        owner: &str,
        repo: &str,
        pr_number: u64,
        body: &str,
    ) -> Result<Comment> {
        // GitHub uses the issues API for PR comments
        self.add_issue_comment(owner, repo, pr_number, body).await
    }

    async fn list_issue_comments(
        &self,
        owner: &str,
        repo: &str,
        issue_number: u64,
    ) -> Result<Vec<Comment>> {
        let url = format!(
            "{}/repos/{}/{}/issues/{}/comments",
            self.base_url, owner, repo, issue_number
        );

        let response: Vec<GitHubComment> = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .into_iter()
            .map(|c| Comment {
                id: c.id.to_string(),
                body: c.body,
                author: c.user.login,
                created_at: c.created_at,
                updated_at: c.updated_at,
            })
            .collect())
    }

    async fn list_pr_comments(
        &self,
        owner: &str,
        repo: &str,
        pr_number: u64,
    ) -> Result<Vec<Comment>> {
        // GitHub uses the issues API for PR comments
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
        let url = format!("{}/repos/{}/{}/check-runs", self.base_url, owner, repo);

        let mut body = serde_json::json!({
            "name": name,
            "head_sha": head_sha,
            "status": match status {
                CheckStatus::Queued => "queued",
                CheckStatus::InProgress => "in_progress",
                CheckStatus::Completed => "completed",
            }
        });

        if let Some(c) = conclusion {
            body["conclusion"] = serde_json::json!(match c {
                CheckConclusion::Success => "success",
                CheckConclusion::Failure => "failure",
                CheckConclusion::Neutral => "neutral",
                CheckConclusion::Cancelled => "cancelled",
                CheckConclusion::Skipped => "skipped",
                CheckConclusion::TimedOut => "timed_out",
                CheckConclusion::ActionRequired => "action_required",
            });
        }

        let cr: GitHubCheckRun = self
            .client
            .post(&url)
            .json(&body)
            .send()
            .await?
            .json()
            .await?;

        Ok(CheckRun {
            id: cr.id.to_string(),
            name: cr.name,
            status: match cr.status.as_str() {
                "queued" => CheckStatus::Queued,
                "in_progress" => CheckStatus::InProgress,
                "completed" => CheckStatus::Completed,
                _ => CheckStatus::Queued,
            },
            conclusion: cr.conclusion.map(|c| match c.as_str() {
                "success" => CheckConclusion::Success,
                "failure" => CheckConclusion::Failure,
                "neutral" => CheckConclusion::Neutral,
                "cancelled" => CheckConclusion::Cancelled,
                "skipped" => CheckConclusion::Skipped,
                "timed_out" => CheckConclusion::TimedOut,
                "action_required" => CheckConclusion::ActionRequired,
                _ => CheckConclusion::Neutral,
            }),
            head_sha: cr.head_sha,
            url: cr.html_url,
            started_at: cr.started_at,
            completed_at: cr.completed_at,
        })
    }

    async fn update_check_run(
        &self,
        owner: &str,
        repo: &str,
        check_run_id: &str,
        status: Option<CheckStatus>,
        conclusion: Option<CheckConclusion>,
    ) -> Result<CheckRun> {
        let url = format!(
            "{}/repos/{}/{}/check-runs/{}",
            self.base_url, owner, repo, check_run_id
        );

        let mut body = serde_json::Map::new();

        if let Some(s) = status {
            body.insert(
                "status".to_string(),
                serde_json::json!(match s {
                    CheckStatus::Queued => "queued",
                    CheckStatus::InProgress => "in_progress",
                    CheckStatus::Completed => "completed",
                }),
            );
        }

        if let Some(c) = conclusion {
            body.insert(
                "conclusion".to_string(),
                serde_json::json!(match c {
                    CheckConclusion::Success => "success",
                    CheckConclusion::Failure => "failure",
                    CheckConclusion::Neutral => "neutral",
                    CheckConclusion::Cancelled => "cancelled",
                    CheckConclusion::Skipped => "skipped",
                    CheckConclusion::TimedOut => "timed_out",
                    CheckConclusion::ActionRequired => "action_required",
                }),
            );
        }

        let cr: GitHubCheckRun = self
            .client
            .patch(&url)
            .json(&serde_json::Value::Object(body))
            .send()
            .await?
            .json()
            .await?;

        Ok(CheckRun {
            id: cr.id.to_string(),
            name: cr.name,
            status: match cr.status.as_str() {
                "queued" => CheckStatus::Queued,
                "in_progress" => CheckStatus::InProgress,
                "completed" => CheckStatus::Completed,
                _ => CheckStatus::Queued,
            },
            conclusion: cr.conclusion.map(|c| match c.as_str() {
                "success" => CheckConclusion::Success,
                "failure" => CheckConclusion::Failure,
                "neutral" => CheckConclusion::Neutral,
                "cancelled" => CheckConclusion::Cancelled,
                "skipped" => CheckConclusion::Skipped,
                "timed_out" => CheckConclusion::TimedOut,
                "action_required" => CheckConclusion::ActionRequired,
                _ => CheckConclusion::Neutral,
            }),
            head_sha: cr.head_sha,
            url: cr.html_url,
            started_at: cr.started_at,
            completed_at: cr.completed_at,
        })
    }

    async fn list_check_runs(
        &self,
        owner: &str,
        repo: &str,
        ref_name: &str,
    ) -> Result<Vec<CheckRun>> {
        let url = format!(
            "{}/repos/{}/{}/commits/{}/check-runs",
            self.base_url, owner, repo, ref_name
        );

        let response: GitHubCheckRuns = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .check_runs
            .into_iter()
            .map(|cr| CheckRun {
                id: cr.id.to_string(),
                name: cr.name,
                status: match cr.status.as_str() {
                    "queued" => CheckStatus::Queued,
                    "in_progress" => CheckStatus::InProgress,
                    "completed" => CheckStatus::Completed,
                    _ => CheckStatus::Queued,
                },
                conclusion: cr.conclusion.map(|c| match c.as_str() {
                    "success" => CheckConclusion::Success,
                    "failure" => CheckConclusion::Failure,
                    "neutral" => CheckConclusion::Neutral,
                    "cancelled" => CheckConclusion::Cancelled,
                    "skipped" => CheckConclusion::Skipped,
                    "timed_out" => CheckConclusion::TimedOut,
                    "action_required" => CheckConclusion::ActionRequired,
                    _ => CheckConclusion::Neutral,
                }),
                head_sha: cr.head_sha,
                url: cr.html_url,
                started_at: cr.started_at,
                completed_at: cr.completed_at,
            })
            .collect())
    }

    async fn create_webhook(
        &self,
        owner: &str,
        repo: &str,
        config: WebhookConfig,
    ) -> Result<WebhookConfig> {
        let url = format!("{}/repos/{}/{}/hooks", self.base_url, owner, repo);

        let events: Vec<&str> = config
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
                WebhookEvent::WorkflowRun => "workflow_run",
                WebhookEvent::CheckRun => "check_run",
                WebhookEvent::CheckSuite => "check_suite",
                WebhookEvent::Release => "release",
                WebhookEvent::Custom => "*",
            })
            .collect();

        let body = serde_json::json!({
            "config": {
                "url": config.url,
                "content_type": config.content_type,
                "secret": config.secret,
                "insecure_ssl": "0"
            },
            "events": events,
            "active": config.active
        });

        let hook: GitHubWebhook = self
            .client
            .post(&url)
            .json(&body)
            .send()
            .await?
            .json()
            .await?;

        Ok(WebhookConfig {
            id: Some(hook.id.to_string()),
            url: hook.config.url,
            events: config.events,
            active: hook.active,
            secret: None, // Secret is not returned by API
            content_type: hook.config.content_type.unwrap_or_default(),
        })
    }

    async fn list_webhooks(&self, owner: &str, repo: &str) -> Result<Vec<WebhookConfig>> {
        let url = format!("{}/repos/{}/{}/hooks", self.base_url, owner, repo);

        let response: Vec<GitHubWebhook> = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .into_iter()
            .map(|h| WebhookConfig {
                id: Some(h.id.to_string()),
                url: h.config.url,
                events: h
                    .events
                    .into_iter()
                    .map(|e| match e.as_str() {
                        "push" => WebhookEvent::Push,
                        "pull_request" => WebhookEvent::PullRequest,
                        "pull_request_review" => WebhookEvent::PullRequestReview,
                        "issues" => WebhookEvent::Issues,
                        "issue_comment" => WebhookEvent::IssueComment,
                        "create" => WebhookEvent::Create,
                        "delete" => WebhookEvent::Delete,
                        "workflow_run" => WebhookEvent::WorkflowRun,
                        "check_run" => WebhookEvent::CheckRun,
                        "check_suite" => WebhookEvent::CheckSuite,
                        "release" => WebhookEvent::Release,
                        _ => WebhookEvent::Custom,
                    })
                    .collect(),
                active: h.active,
                secret: None,
                content_type: h.config.content_type.unwrap_or_default(),
            })
            .collect())
    }

    async fn delete_webhook(&self, owner: &str, repo: &str, webhook_id: &str) -> Result<()> {
        let url = format!(
            "{}/repos/{}/{}/hooks/{}",
            self.base_url, owner, repo, webhook_id
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
        signature: Option<&str>,
        payload: &[u8],
        secret: Option<&str>,
    ) -> Result<WebhookPayload> {
        // Validate signature if secret is provided
        if let (Some(sig), Some(sec)) = (signature, secret) {
            let expected = sig.strip_prefix("sha256=").unwrap_or(sig);

            let mut mac =
                HmacSha256::new_from_slice(sec.as_bytes()).map_err(|_| {
                    AdapterError::AuthError("Invalid webhook secret".to_string())
                })?;
            mac.update(payload);
            let computed = hex::encode(mac.finalize().into_bytes());

            if computed != expected {
                return Err(AdapterError::AuthError(
                    "Webhook signature verification failed".to_string(),
                ));
            }
        }

        let event = match event_type {
            "push" => WebhookEvent::Push,
            "pull_request" => WebhookEvent::PullRequest,
            "pull_request_review" => WebhookEvent::PullRequestReview,
            "issues" => WebhookEvent::Issues,
            "issue_comment" => WebhookEvent::IssueComment,
            "create" => WebhookEvent::Create,
            "delete" => WebhookEvent::Delete,
            "workflow_run" => WebhookEvent::WorkflowRun,
            "check_run" => WebhookEvent::CheckRun,
            "check_suite" => WebhookEvent::CheckSuite,
            "release" => WebhookEvent::Release,
            _ => WebhookEvent::Custom,
        };

        let payload_json: serde_json::Value = serde_json::from_slice(payload)?;

        Ok(WebhookPayload {
            event,
            delivery_id: uuid::Uuid::new_v4().to_string(),
            signature: signature.map(|s| s.to_string()),
            payload: payload_json,
        })
    }
}

// ============== GitHub API Response Types ==============

#[derive(Debug, Deserialize)]
struct GitHubRepo {
    id: u64,
    name: String,
    html_url: String,
    private: bool,
    default_branch: String,
}

#[derive(Debug, Deserialize)]
struct GitHubAlert {
    number: u64,
    rule: GitHubAlertRule,
    most_recent_instance: GitHubAlertInstance,
}

#[derive(Debug, Deserialize)]
struct GitHubAlertRule {
    id: String,
    severity: String,
    description: String,
}

#[derive(Debug, Deserialize)]
struct GitHubAlertInstance {
    location: GitHubAlertLocation,
}

#[derive(Debug, Deserialize)]
struct GitHubAlertLocation {
    path: Option<String>,
    start_line: Option<u32>,
}

#[derive(Debug, Deserialize)]
struct GitHubWorkflows {
    workflows: Vec<GitHubWorkflow>,
}

#[derive(Debug, Deserialize)]
struct GitHubWorkflow {
    id: u64,
    name: String,
    path: String,
    state: String,
}

#[derive(Debug, Deserialize)]
struct GitHubWorkflowRuns {
    workflow_runs: Vec<GitHubWorkflowRunDetail>,
}

#[derive(Debug, Deserialize)]
struct GitHubWorkflowRunDetail {
    id: u64,
    workflow_id: u64,
    name: String,
    status: String,
    conclusion: Option<String>,
    head_branch: String,
    head_sha: String,
    html_url: String,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
}

#[derive(Debug, Deserialize)]
struct GitHubPullRequestResponse {
    html_url: String,
}

#[derive(Debug, Deserialize)]
struct GitHubPullRequestFull {
    id: u64,
    number: u64,
    title: String,
    body: Option<String>,
    state: String,
    user: GitHubUser,
    head: GitHubRef,
    base: GitHubRef,
    html_url: String,
    mergeable: Option<bool>,
    merged_at: Option<DateTime<Utc>>,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
}

#[derive(Debug, Deserialize)]
struct GitHubRef {
    #[serde(rename = "ref")]
    ref_name: String,
}

#[derive(Debug, Deserialize)]
struct GitHubUser {
    login: String,
}

#[derive(Debug, Deserialize)]
struct GitHubIssue {
    id: u64,
    number: u64,
    title: String,
    body: Option<String>,
    state: String,
    user: GitHubUser,
    labels: Vec<GitHubLabel>,
    html_url: String,
    pull_request: Option<serde_json::Value>,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
}

#[derive(Debug, Deserialize)]
struct GitHubLabel {
    name: String,
}

#[derive(Debug, Deserialize)]
struct GitHubComment {
    id: u64,
    body: String,
    user: GitHubUser,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
}

#[derive(Debug, Deserialize)]
struct GitHubCheckRuns {
    check_runs: Vec<GitHubCheckRun>,
}

#[derive(Debug, Deserialize)]
struct GitHubCheckRun {
    id: u64,
    name: String,
    status: String,
    conclusion: Option<String>,
    head_sha: String,
    html_url: Option<String>,
    started_at: Option<DateTime<Utc>>,
    completed_at: Option<DateTime<Utc>>,
}

#[derive(Debug, Deserialize)]
struct GitHubWebhook {
    id: u64,
    config: GitHubWebhookConfig,
    events: Vec<String>,
    active: bool,
}

#[derive(Debug, Deserialize, Serialize)]
struct GitHubWebhookConfig {
    url: String,
    content_type: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_github_adapter_creation() {
        let adapter = GitHubAdapter::new("test-token");
        assert!(adapter.is_ok());
        let adapter = adapter.unwrap();
        assert_eq!(adapter.forge(), Forge::GitHub);
        assert_eq!(adapter.base_url(), "https://api.github.com");
    }

    #[test]
    fn test_github_adapter_enterprise() {
        let adapter =
            GitHubAdapter::with_base_url("test-token", "https://github.enterprise.com/api/v3");
        assert!(adapter.is_ok());
        let adapter = adapter.unwrap();
        assert_eq!(
            adapter.base_url(),
            "https://github.enterprise.com/api/v3"
        );
    }

    #[test]
    fn test_webhook_signature_validation() {
        let adapter = GitHubAdapter::new("test-token").unwrap();

        // Test with no signature and no secret (should pass)
        let payload = br#"{"action": "opened"}"#;
        let result = adapter.parse_webhook("push", None, payload, None);
        assert!(result.is_ok());
        assert!(matches!(result.unwrap().event, WebhookEvent::Push));
    }
}
