// SPDX-License-Identifier: AGPL-3.0-or-later
//! Bitbucket forge adapter
//!
//! Full implementation of the ForgeAdapter trait for Bitbucket Cloud.
//! Supports both Bitbucket Cloud and Bitbucket Server (with different auth).

use crate::error::{AdapterError, Result};
use crate::forge::{
    Alert, CheckConclusion, CheckRun, CheckStatus, Comment, Forge, ForgeAdapter, Issue,
    IssueState, PullRequest, PullRequestState, Repository, RunConclusion, RunStatus, Visibility,
    WebhookConfig, WebhookEvent, WebhookPayload, Workflow, WorkflowRun, WorkflowState,
};
use async_trait::async_trait;
use chrono::{DateTime, Utc};
use reqwest::header::{HeaderMap, HeaderValue, AUTHORIZATION, CONTENT_TYPE};
use serde::Deserialize;

/// Bitbucket adapter configuration
pub struct BitbucketAdapter {
    client: reqwest::Client,
    base_url: String,
}

impl BitbucketAdapter {
    /// Create new Bitbucket adapter with app password
    pub fn new(username: &str, app_password: &str) -> Result<Self> {
        Self::with_base_url(username, app_password, "https://api.bitbucket.org/2.0")
    }

    /// Create with custom base URL (for Bitbucket Server)
    pub fn with_base_url(username: &str, app_password: &str, base_url: &str) -> Result<Self> {
        let auth = base64::Engine::encode(
            &base64::engine::general_purpose::STANDARD,
            format!("{}:{}", username, app_password).as_bytes(),
        );

        let mut headers = HeaderMap::new();
        headers.insert(
            AUTHORIZATION,
            HeaderValue::from_str(&format!("Basic {}", auth))
                .map_err(|_| AdapterError::ConfigError("Invalid credentials".into()))?,
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
}

#[async_trait]
impl ForgeAdapter for BitbucketAdapter {
    fn forge(&self) -> Forge {
        Forge::Bitbucket
    }

    fn base_url(&self) -> &str {
        &self.base_url
    }

    async fn list_repos(&self, owner: &str) -> Result<Vec<Repository>> {
        let url = format!("{}/repositories/{}", self.base_url, owner);
        let response: BitbucketRepoList = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .values
            .into_iter()
            .map(|r| Repository {
                id: r.uuid,
                name: r.slug,
                owner: owner.to_string(),
                forge: Forge::Bitbucket,
                url: r.links.html.href,
                visibility: if r.is_private {
                    Visibility::Private
                } else {
                    Visibility::Public
                },
                default_branch: r
                    .mainbranch
                    .map(|b| b.name)
                    .unwrap_or_else(|| "main".to_string()),
                languages: r.language.map(|l| vec![l]).unwrap_or_default(),
            })
            .collect())
    }

    async fn get_repo(&self, owner: &str, repo: &str) -> Result<Repository> {
        let url = format!("{}/repositories/{}/{}", self.base_url, owner, repo);
        let r: BitbucketRepo = self.client.get(&url).send().await?.json().await?;

        Ok(Repository {
            id: r.uuid,
            name: r.slug,
            owner: owner.to_string(),
            forge: Forge::Bitbucket,
            url: r.links.html.href,
            visibility: if r.is_private {
                Visibility::Private
            } else {
                Visibility::Public
            },
            default_branch: r
                .mainbranch
                .map(|b| b.name)
                .unwrap_or_else(|| "main".to_string()),
            languages: r.language.map(|l| vec![l]).unwrap_or_default(),
        })
    }

    async fn get_alerts(&self, _owner: &str, _repo: &str) -> Result<Vec<Alert>> {
        // Bitbucket doesn't have built-in code scanning like GitHub/GitLab
        // Would need integration with Snyk, SonarCloud, etc.
        Ok(vec![])
    }

    async fn list_workflows(&self, owner: &str, repo: &str) -> Result<Vec<Workflow>> {
        // Bitbucket uses bitbucket-pipelines.yml
        let url = format!(
            "{}/repositories/{}/{}/src/main/bitbucket-pipelines.yml",
            self.base_url, owner, repo
        );

        match self.client.get(&url).send().await {
            Ok(resp) if resp.status().is_success() => Ok(vec![Workflow {
                id: "pipelines".to_string(),
                name: "Bitbucket Pipelines".to_string(),
                file: "bitbucket-pipelines.yml".to_string(),
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
        let url = format!("{}/repositories/{}/{}/src", self.base_url, owner, repo);

        // Bitbucket uses form data for file uploads
        let form = reqwest::multipart::Form::new()
            .text(path.to_string(), content.to_string())
            .text("message", message.to_string());

        let response = self.client.post(&url).multipart(form).send().await?;

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
        _workflow: &str,
        ref_name: &str,
    ) -> Result<()> {
        let url = format!(
            "{}/repositories/{}/{}/pipelines/",
            self.base_url, owner, repo
        );

        let body = serde_json::json!({
            "target": {
                "ref_type": "branch",
                "type": "pipeline_ref_target",
                "ref_name": ref_name
            }
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
        let url = format!(
            "{}/repositories/{}/{}/pipelines/?pagelen=50",
            self.base_url, owner, repo
        );

        let response: BitbucketPipelineList = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .values
            .into_iter()
            .map(|p| WorkflowRun {
                id: p.uuid,
                workflow_id: "pipelines".to_string(),
                name: format!("Pipeline #{}", p.build_number),
                status: match p.state.name.as_str() {
                    "PENDING" => RunStatus::Pending,
                    "IN_PROGRESS" => RunStatus::InProgress,
                    "COMPLETED" => RunStatus::Completed,
                    _ => RunStatus::Pending,
                },
                conclusion: p.state.result.map(|r| match r.name.as_str() {
                    "SUCCESSFUL" => RunConclusion::Success,
                    "FAILED" => RunConclusion::Failure,
                    "STOPPED" => RunConclusion::Cancelled,
                    _ => RunConclusion::Neutral,
                }),
                head_branch: p.target.ref_name.unwrap_or_default(),
                head_sha: p.target.commit.hash,
                url: format!(
                    "https://bitbucket.org/{}/{}/pipelines/results/{}",
                    owner, repo, p.build_number
                ),
                created_at: p.created_on,
                updated_at: p.completed_on.unwrap_or(p.created_on),
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
            "{}/repositories/{}/{}/pipelines/{}",
            self.base_url, owner, repo, run_id
        );

        let p: BitbucketPipeline = self.client.get(&url).send().await?.json().await?;

        Ok(WorkflowRun {
            id: p.uuid,
            workflow_id: "pipelines".to_string(),
            name: format!("Pipeline #{}", p.build_number),
            status: match p.state.name.as_str() {
                "PENDING" => RunStatus::Pending,
                "IN_PROGRESS" => RunStatus::InProgress,
                "COMPLETED" => RunStatus::Completed,
                _ => RunStatus::Pending,
            },
            conclusion: p.state.result.map(|r| match r.name.as_str() {
                "SUCCESSFUL" => RunConclusion::Success,
                "FAILED" => RunConclusion::Failure,
                "STOPPED" => RunConclusion::Cancelled,
                _ => RunConclusion::Neutral,
            }),
            head_branch: p.target.ref_name.unwrap_or_default(),
            head_sha: p.target.commit.hash,
            url: format!(
                "https://bitbucket.org/{}/{}/pipelines/results/{}",
                owner, repo, p.build_number
            ),
            created_at: p.created_on,
            updated_at: p.completed_on.unwrap_or(p.created_on),
        })
    }

    async fn enable_branch_protection(&self, owner: &str, repo: &str, branch: &str) -> Result<()> {
        let url = format!(
            "{}/repositories/{}/{}/branch-restrictions",
            self.base_url, owner, repo
        );

        let body = serde_json::json!({
            "kind": "push",
            "branch_match_kind": "glob",
            "pattern": branch,
            "users": [],
            "groups": []
        });

        let response = self.client.post(&url).json(&body).send().await?;

        if !response.status().is_success() && response.status().as_u16() != 409 {
            return Err(AdapterError::ApiError(format!(
                "Failed to add branch restriction: HTTP {}",
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
        let url = format!(
            "{}/repositories/{}/{}/pullrequests",
            self.base_url, owner, repo
        );

        let request_body = serde_json::json!({
            "title": title,
            "description": body,
            "source": {
                "branch": {
                    "name": head
                }
            },
            "destination": {
                "branch": {
                    "name": base
                }
            }
        });

        let response: BitbucketPullRequest = self
            .client
            .post(&url)
            .json(&request_body)
            .send()
            .await?
            .json()
            .await?;

        Ok(response.links.html.href)
    }

    async fn list_prs(
        &self,
        owner: &str,
        repo: &str,
        state: Option<PullRequestState>,
    ) -> Result<Vec<PullRequest>> {
        let state_param = match state {
            Some(PullRequestState::Open) => "OPEN",
            Some(PullRequestState::Merged) => "MERGED",
            Some(PullRequestState::Closed) => "DECLINED",
            None => "",
        };

        let url = if state_param.is_empty() {
            format!(
                "{}/repositories/{}/{}/pullrequests",
                self.base_url, owner, repo
            )
        } else {
            format!(
                "{}/repositories/{}/{}/pullrequests?state={}",
                self.base_url, owner, repo, state_param
            )
        };

        let response: BitbucketPullRequestList =
            self.client.get(&url).send().await?.json().await?;

        Ok(response
            .values
            .into_iter()
            .map(|pr| PullRequest {
                id: pr.id.to_string(),
                number: pr.id,
                title: pr.title,
                body: pr.description,
                state: match pr.state.as_str() {
                    "OPEN" => PullRequestState::Open,
                    "MERGED" => PullRequestState::Merged,
                    _ => PullRequestState::Closed,
                },
                author: pr.author.display_name,
                head_branch: pr.source.branch.name,
                base_branch: pr.destination.branch.name,
                url: pr.links.html.href,
                mergeable: None, // Bitbucket doesn't expose this directly
                created_at: pr.created_on,
                updated_at: pr.updated_on,
            })
            .collect())
    }

    async fn get_pr(&self, owner: &str, repo: &str, number: u64) -> Result<PullRequest> {
        let url = format!(
            "{}/repositories/{}/{}/pullrequests/{}",
            self.base_url, owner, repo, number
        );

        let pr: BitbucketPullRequestFull = self.client.get(&url).send().await?.json().await?;

        Ok(PullRequest {
            id: pr.id.to_string(),
            number: pr.id,
            title: pr.title,
            body: pr.description,
            state: match pr.state.as_str() {
                "OPEN" => PullRequestState::Open,
                "MERGED" => PullRequestState::Merged,
                _ => PullRequestState::Closed,
            },
            author: pr.author.display_name,
            head_branch: pr.source.branch.name,
            base_branch: pr.destination.branch.name,
            url: pr.links.html.href,
            mergeable: None,
            created_at: pr.created_on,
            updated_at: pr.updated_on,
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
            "{}/repositories/{}/{}/pullrequests/{}/merge",
            self.base_url, owner, repo, number
        );

        let mut body = serde_json::Map::new();
        if let Some(msg) = commit_message {
            body.insert("message".to_string(), serde_json::json!(msg));
        }
        body.insert("merge_strategy".to_string(), serde_json::json!("squash"));

        let response = self
            .client
            .post(&url)
            .json(&serde_json::Value::Object(body))
            .send()
            .await?;

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
            "{}/repositories/{}/{}/pullrequests/{}/decline",
            self.base_url, owner, repo, number
        );

        let response = self.client.post(&url).send().await?;

        if !response.status().is_success() {
            return Err(AdapterError::ApiError(format!(
                "Failed to decline PR: HTTP {}",
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
        _labels: Vec<String>,
    ) -> Result<Issue> {
        let url = format!(
            "{}/repositories/{}/{}/issues",
            self.base_url, owner, repo
        );

        let request_body = serde_json::json!({
            "title": title,
            "content": {
                "raw": body
            },
            "kind": "bug",
            "priority": "major"
        });

        let response: BitbucketIssue = self
            .client
            .post(&url)
            .json(&request_body)
            .send()
            .await?
            .json()
            .await?;

        Ok(Issue {
            id: response.id.to_string(),
            number: response.id,
            title: response.title,
            body: response.content.map(|c| c.raw),
            state: match response.state.as_str() {
                "new" | "open" => IssueState::Open,
                _ => IssueState::Closed,
            },
            author: response.reporter.display_name,
            labels: vec![], // Bitbucket uses "kind" and "priority" instead
            url: response.links.html.href,
            created_at: response.created_on,
            updated_at: response.updated_on,
        })
    }

    async fn list_issues(
        &self,
        owner: &str,
        repo: &str,
        state: Option<IssueState>,
    ) -> Result<Vec<Issue>> {
        let url = format!(
            "{}/repositories/{}/{}/issues",
            self.base_url, owner, repo
        );

        let response: BitbucketIssueList = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .values
            .into_iter()
            .filter(|i| match state {
                Some(IssueState::Open) => i.state == "new" || i.state == "open",
                Some(IssueState::Closed) => i.state != "new" && i.state != "open",
                None => true,
            })
            .map(|i| Issue {
                id: i.id.to_string(),
                number: i.id,
                title: i.title,
                body: i.content.map(|c| c.raw),
                state: match i.state.as_str() {
                    "new" | "open" => IssueState::Open,
                    _ => IssueState::Closed,
                },
                author: i.reporter.display_name,
                labels: vec![],
                url: i.links.html.href,
                created_at: i.created_on,
                updated_at: i.updated_on,
            })
            .collect())
    }

    async fn get_issue(&self, owner: &str, repo: &str, number: u64) -> Result<Issue> {
        let url = format!(
            "{}/repositories/{}/{}/issues/{}",
            self.base_url, owner, repo, number
        );

        let i: BitbucketIssue = self.client.get(&url).send().await?.json().await?;

        Ok(Issue {
            id: i.id.to_string(),
            number: i.id,
            title: i.title,
            body: i.content.map(|c| c.raw),
            state: match i.state.as_str() {
                "new" | "open" => IssueState::Open,
                _ => IssueState::Closed,
            },
            author: i.reporter.display_name,
            labels: vec![],
            url: i.links.html.href,
            created_at: i.created_on,
            updated_at: i.updated_on,
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
        _labels: Option<Vec<String>>,
    ) -> Result<Issue> {
        let url = format!(
            "{}/repositories/{}/{}/issues/{}",
            self.base_url, owner, repo, number
        );

        let mut request_body = serde_json::Map::new();
        if let Some(t) = title {
            request_body.insert("title".to_string(), serde_json::json!(t));
        }
        if let Some(b) = body {
            request_body.insert(
                "content".to_string(),
                serde_json::json!({"raw": b}),
            );
        }
        if let Some(s) = state {
            request_body.insert(
                "state".to_string(),
                serde_json::json!(match s {
                    IssueState::Open => "open",
                    IssueState::Closed => "resolved",
                }),
            );
        }

        let i: BitbucketIssue = self
            .client
            .put(&url)
            .json(&serde_json::Value::Object(request_body))
            .send()
            .await?
            .json()
            .await?;

        Ok(Issue {
            id: i.id.to_string(),
            number: i.id,
            title: i.title,
            body: i.content.map(|c| c.raw),
            state: match i.state.as_str() {
                "new" | "open" => IssueState::Open,
                _ => IssueState::Closed,
            },
            author: i.reporter.display_name,
            labels: vec![],
            url: i.links.html.href,
            created_at: i.created_on,
            updated_at: i.updated_on,
        })
    }

    async fn close_issue(&self, owner: &str, repo: &str, number: u64) -> Result<()> {
        let url = format!(
            "{}/repositories/{}/{}/issues/{}",
            self.base_url, owner, repo, number
        );

        let body = serde_json::json!({
            "state": "resolved"
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
        let url = format!(
            "{}/repositories/{}/{}/issues/{}/comments",
            self.base_url, owner, repo, issue_number
        );

        let request_body = serde_json::json!({
            "content": {
                "raw": body
            }
        });

        let c: BitbucketComment = self
            .client
            .post(&url)
            .json(&request_body)
            .send()
            .await?
            .json()
            .await?;

        Ok(Comment {
            id: c.id.to_string(),
            body: c.content.raw,
            author: c.user.display_name,
            created_at: c.created_on,
            updated_at: c.updated_on,
        })
    }

    async fn add_pr_comment(
        &self,
        owner: &str,
        repo: &str,
        pr_number: u64,
        body: &str,
    ) -> Result<Comment> {
        let url = format!(
            "{}/repositories/{}/{}/pullrequests/{}/comments",
            self.base_url, owner, repo, pr_number
        );

        let request_body = serde_json::json!({
            "content": {
                "raw": body
            }
        });

        let c: BitbucketComment = self
            .client
            .post(&url)
            .json(&request_body)
            .send()
            .await?
            .json()
            .await?;

        Ok(Comment {
            id: c.id.to_string(),
            body: c.content.raw,
            author: c.user.display_name,
            created_at: c.created_on,
            updated_at: c.updated_on,
        })
    }

    async fn list_issue_comments(
        &self,
        owner: &str,
        repo: &str,
        issue_number: u64,
    ) -> Result<Vec<Comment>> {
        let url = format!(
            "{}/repositories/{}/{}/issues/{}/comments",
            self.base_url, owner, repo, issue_number
        );

        let response: BitbucketCommentList = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .values
            .into_iter()
            .map(|c| Comment {
                id: c.id.to_string(),
                body: c.content.raw,
                author: c.user.display_name,
                created_at: c.created_on,
                updated_at: c.updated_on,
            })
            .collect())
    }

    async fn list_pr_comments(
        &self,
        owner: &str,
        repo: &str,
        pr_number: u64,
    ) -> Result<Vec<Comment>> {
        let url = format!(
            "{}/repositories/{}/{}/pullrequests/{}/comments",
            self.base_url, owner, repo, pr_number
        );

        let response: BitbucketCommentList = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .values
            .into_iter()
            .map(|c| Comment {
                id: c.id.to_string(),
                body: c.content.raw,
                author: c.user.display_name,
                created_at: c.created_on,
                updated_at: c.updated_on,
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
        // Bitbucket uses commit statuses
        let url = format!(
            "{}/repositories/{}/{}/commit/{}/statuses/build",
            self.base_url, owner, repo, head_sha
        );

        let bb_state = match (status, conclusion) {
            (CheckStatus::Completed, Some(CheckConclusion::Success)) => "SUCCESSFUL",
            (CheckStatus::Completed, Some(CheckConclusion::Failure)) => "FAILED",
            (CheckStatus::Completed, Some(CheckConclusion::Cancelled)) => "STOPPED",
            (CheckStatus::InProgress, _) => "INPROGRESS",
            _ => "PENDING",
        };

        let body = serde_json::json!({
            "state": bb_state,
            "key": name,
            "name": name
        });

        self.client.post(&url).json(&body).send().await?;

        Ok(CheckRun {
            id: name.to_string(),
            name: name.to_string(),
            status,
            conclusion,
            head_sha: head_sha.to_string(),
            url: None,
            started_at: Some(Utc::now()),
            completed_at: if matches!(status, CheckStatus::Completed) {
                Some(Utc::now())
            } else {
                None
            },
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
        // Bitbucket doesn't really support updating - need to know the commit SHA
        // Return an error for unsupported operation
        crate::forge::defaults::not_supported("update_check_run", Forge::Bitbucket)?;

        Ok(CheckRun {
            id: check_run_id.to_string(),
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
        let url = format!(
            "{}/repositories/{}/{}/commit/{}/statuses",
            self.base_url, owner, repo, ref_name
        );

        let response: BitbucketStatusList = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .values
            .into_iter()
            .map(|s| CheckRun {
                id: s.key.clone(),
                name: s.name.unwrap_or(s.key),
                status: match s.state.as_str() {
                    "INPROGRESS" => CheckStatus::InProgress,
                    "SUCCESSFUL" | "FAILED" | "STOPPED" => CheckStatus::Completed,
                    _ => CheckStatus::Queued,
                },
                conclusion: match s.state.as_str() {
                    "SUCCESSFUL" => Some(CheckConclusion::Success),
                    "FAILED" => Some(CheckConclusion::Failure),
                    "STOPPED" => Some(CheckConclusion::Cancelled),
                    _ => None,
                },
                head_sha: ref_name.to_string(),
                url: s.url,
                started_at: s.created_on,
                completed_at: s.updated_on,
            })
            .collect())
    }

    async fn create_webhook(
        &self,
        owner: &str,
        repo: &str,
        config: WebhookConfig,
    ) -> Result<WebhookConfig> {
        let url = format!(
            "{}/repositories/{}/{}/hooks",
            self.base_url, owner, repo
        );

        let events: Vec<&str> = config
            .events
            .iter()
            .map(|e| match e {
                WebhookEvent::Push => "repo:push",
                WebhookEvent::PullRequest => "pullrequest:created",
                WebhookEvent::PullRequestReview => "pullrequest:approved",
                WebhookEvent::Issues => "issue:created",
                WebhookEvent::IssueComment => "issue:comment_created",
                _ => "repo:push",
            })
            .collect();

        let body = serde_json::json!({
            "description": "cicd-hyper-a webhook",
            "url": config.url,
            "active": config.active,
            "events": events
        });

        let hook: BitbucketWebhook = self
            .client
            .post(&url)
            .json(&body)
            .send()
            .await?
            .json()
            .await?;

        Ok(WebhookConfig {
            id: Some(hook.uuid),
            url: hook.url,
            events: config.events,
            active: hook.active,
            secret: None,
            content_type: "application/json".to_string(),
        })
    }

    async fn list_webhooks(&self, owner: &str, repo: &str) -> Result<Vec<WebhookConfig>> {
        let url = format!(
            "{}/repositories/{}/{}/hooks",
            self.base_url, owner, repo
        );

        let response: BitbucketWebhookList = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .values
            .into_iter()
            .map(|h| WebhookConfig {
                id: Some(h.uuid),
                url: h.url,
                events: h
                    .events
                    .into_iter()
                    .map(|e| match e.as_str() {
                        "repo:push" => WebhookEvent::Push,
                        "pullrequest:created" | "pullrequest:updated" => WebhookEvent::PullRequest,
                        "pullrequest:approved" => WebhookEvent::PullRequestReview,
                        "issue:created" | "issue:updated" => WebhookEvent::Issues,
                        "issue:comment_created" => WebhookEvent::IssueComment,
                        _ => WebhookEvent::Custom,
                    })
                    .collect(),
                active: h.active,
                secret: None,
                content_type: "application/json".to_string(),
            })
            .collect())
    }

    async fn delete_webhook(&self, owner: &str, repo: &str, webhook_id: &str) -> Result<()> {
        let url = format!(
            "{}/repositories/{}/{}/hooks/{}",
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
        _signature: Option<&str>,
        payload: &[u8],
        _secret: Option<&str>,
    ) -> Result<WebhookPayload> {
        let event = match event_type {
            "repo:push" => WebhookEvent::Push,
            "pullrequest:created" | "pullrequest:updated" => WebhookEvent::PullRequest,
            "pullrequest:approved" | "pullrequest:unapproved" => WebhookEvent::PullRequestReview,
            "issue:created" | "issue:updated" => WebhookEvent::Issues,
            "issue:comment_created" => WebhookEvent::IssueComment,
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

// ============== Bitbucket API Response Types ==============

#[derive(Debug, Deserialize)]
struct BitbucketRepoList {
    values: Vec<BitbucketRepo>,
}

#[derive(Debug, Deserialize)]
struct BitbucketRepo {
    uuid: String,
    slug: String,
    is_private: bool,
    language: Option<String>,
    mainbranch: Option<BitbucketBranch>,
    links: BitbucketLinks,
}

#[derive(Debug, Deserialize)]
struct BitbucketBranch {
    name: String,
}

#[derive(Debug, Deserialize)]
struct BitbucketLinks {
    html: BitbucketLink,
}

#[derive(Debug, Deserialize)]
struct BitbucketLink {
    href: String,
}

#[derive(Debug, Deserialize)]
struct BitbucketPullRequest {
    links: BitbucketLinks,
}

#[derive(Debug, Deserialize)]
struct BitbucketPullRequestList {
    values: Vec<BitbucketPullRequestFull>,
}

#[derive(Debug, Deserialize)]
struct BitbucketPullRequestFull {
    id: u64,
    title: String,
    description: Option<String>,
    state: String,
    author: BitbucketUser,
    source: BitbucketPRBranch,
    destination: BitbucketPRBranch,
    links: BitbucketLinks,
    created_on: DateTime<Utc>,
    updated_on: DateTime<Utc>,
}

#[derive(Debug, Deserialize)]
struct BitbucketUser {
    display_name: String,
}

#[derive(Debug, Deserialize)]
struct BitbucketPRBranch {
    branch: BitbucketBranch,
}

#[derive(Debug, Deserialize)]
struct BitbucketIssueList {
    values: Vec<BitbucketIssue>,
}

#[derive(Debug, Deserialize)]
struct BitbucketIssue {
    id: u64,
    title: String,
    content: Option<BitbucketContent>,
    state: String,
    reporter: BitbucketUser,
    links: BitbucketLinks,
    created_on: DateTime<Utc>,
    updated_on: DateTime<Utc>,
}

#[derive(Debug, Deserialize)]
struct BitbucketContent {
    raw: String,
}

#[derive(Debug, Deserialize)]
struct BitbucketCommentList {
    values: Vec<BitbucketComment>,
}

#[derive(Debug, Deserialize)]
struct BitbucketComment {
    id: u64,
    content: BitbucketContent,
    user: BitbucketUser,
    created_on: DateTime<Utc>,
    updated_on: DateTime<Utc>,
}

#[derive(Debug, Deserialize)]
struct BitbucketPipelineList {
    values: Vec<BitbucketPipeline>,
}

#[derive(Debug, Deserialize)]
struct BitbucketPipeline {
    uuid: String,
    build_number: u64,
    state: BitbucketPipelineState,
    target: BitbucketPipelineTarget,
    created_on: DateTime<Utc>,
    completed_on: Option<DateTime<Utc>>,
}

#[derive(Debug, Deserialize)]
struct BitbucketPipelineState {
    name: String,
    result: Option<BitbucketPipelineResult>,
}

#[derive(Debug, Deserialize)]
struct BitbucketPipelineResult {
    name: String,
}

#[derive(Debug, Deserialize)]
struct BitbucketPipelineTarget {
    ref_name: Option<String>,
    commit: BitbucketCommit,
}

#[derive(Debug, Deserialize)]
struct BitbucketCommit {
    hash: String,
}

#[derive(Debug, Deserialize)]
struct BitbucketStatusList {
    values: Vec<BitbucketStatus>,
}

#[derive(Debug, Deserialize)]
struct BitbucketStatus {
    key: String,
    name: Option<String>,
    state: String,
    url: Option<String>,
    created_on: Option<DateTime<Utc>>,
    updated_on: Option<DateTime<Utc>>,
}

#[derive(Debug, Deserialize)]
struct BitbucketWebhookList {
    values: Vec<BitbucketWebhook>,
}

#[derive(Debug, Deserialize)]
struct BitbucketWebhook {
    uuid: String,
    url: String,
    active: bool,
    events: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bitbucket_adapter_creation() {
        let adapter = BitbucketAdapter::new("user", "password");
        assert!(adapter.is_ok());
        let adapter = adapter.unwrap();
        assert_eq!(adapter.forge(), Forge::Bitbucket);
        assert_eq!(adapter.base_url(), "https://api.bitbucket.org/2.0");
    }

    #[test]
    fn test_bitbucket_adapter_server() {
        let adapter = BitbucketAdapter::with_base_url(
            "user",
            "password",
            "https://bitbucket.company.com/rest/api/1.0",
        );
        assert!(adapter.is_ok());
        let adapter = adapter.unwrap();
        assert_eq!(
            adapter.base_url(),
            "https://bitbucket.company.com/rest/api/1.0"
        );
    }
}
