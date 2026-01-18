// SPDX-License-Identifier: AGPL-3.0-or-later
//! Forge Adapters Integration Tests
//!
//! Tests forge adapters with mock servers:
//! - GitHub API mock server
//! - GitLab API mock server
//! - Bitbucket API mock server
//! - Webhook handling
//! - Rate limiting behavior
//! - Error handling

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, info, warn};
use wiremock::matchers::{header, method, path, path_regex};
use wiremock::{Mock, MockServer, ResponseTemplate};

mod common;
use common::setup_test_logging;

// ============================================================================
// Forge API Models
// ============================================================================

/// Repository response matching forge APIs
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RepositoryResponse {
    pub id: u64,
    pub name: String,
    pub full_name: String,
    pub owner: OwnerResponse,
    pub html_url: String,
    pub description: Option<String>,
    pub default_branch: String,
    pub visibility: String,
    pub language: Option<String>,
    pub languages_url: Option<String>,
}

/// Owner response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OwnerResponse {
    pub login: String,
    pub id: u64,
    #[serde(rename = "type")]
    pub owner_type: String,
}

/// Workflow response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowResponse {
    pub id: u64,
    pub name: String,
    pub path: String,
    pub state: String,
}

/// Workflow run response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowRunResponse {
    pub id: u64,
    pub name: String,
    pub head_branch: String,
    pub head_sha: String,
    pub status: String,
    pub conclusion: Option<String>,
    pub html_url: String,
    pub created_at: String,
    pub updated_at: String,
}

/// Alert response (Code scanning / Dependabot)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlertResponse {
    pub number: u64,
    pub state: String,
    pub rule: AlertRuleResponse,
    pub most_recent_instance: AlertInstanceResponse,
}

/// Alert rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlertRuleResponse {
    pub id: String,
    pub severity: String,
    pub description: String,
}

/// Alert instance
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlertInstanceResponse {
    pub location: AlertLocationResponse,
}

/// Alert location
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlertLocationResponse {
    pub path: String,
    pub start_line: u32,
}

// ============================================================================
// Mock Forge Server
// ============================================================================

/// Mock forge server configuration
struct MockForgeServer {
    server: MockServer,
    repositories: Arc<RwLock<HashMap<String, RepositoryResponse>>>,
    workflows: Arc<RwLock<HashMap<String, Vec<WorkflowResponse>>>>,
    alerts: Arc<RwLock<HashMap<String, Vec<AlertResponse>>>>,
}

impl MockForgeServer {
    async fn new() -> Result<Self> {
        let server = MockServer::start().await;
        Ok(Self {
            server,
            repositories: Arc::new(RwLock::new(HashMap::new())),
            workflows: Arc::new(RwLock::new(HashMap::new())),
            alerts: Arc::new(RwLock::new(HashMap::new())),
        })
    }

    fn url(&self) -> String {
        self.server.uri()
    }

    /// Add a mock repository
    async fn add_repository(&self, owner: &str, name: &str) {
        let full_name = format!("{}/{}", owner, name);
        let repo = RepositoryResponse {
            id: rand::random(),
            name: name.to_string(),
            full_name: full_name.clone(),
            owner: OwnerResponse {
                login: owner.to_string(),
                id: rand::random(),
                owner_type: "User".to_string(),
            },
            html_url: format!("{}/repos/{}/{}", self.url(), owner, name),
            description: Some(format!("Test repository {}", name)),
            default_branch: "main".to_string(),
            visibility: "public".to_string(),
            language: Some("Rust".to_string()),
            languages_url: Some(format!("{}/repos/{}/{}/languages", self.url(), owner, name)),
        };

        self.repositories.write().await.insert(full_name, repo);
    }

    /// Add mock workflow
    async fn add_workflow(&self, owner: &str, repo: &str, workflow: WorkflowResponse) {
        let key = format!("{}/{}", owner, repo);
        let mut workflows = self.workflows.write().await;
        workflows.entry(key).or_insert_with(Vec::new).push(workflow);
    }

    /// Add mock alert
    async fn add_alert(&self, owner: &str, repo: &str, alert: AlertResponse) {
        let key = format!("{}/{}", owner, repo);
        let mut alerts = self.alerts.write().await;
        alerts.entry(key).or_insert_with(Vec::new).push(alert);
    }

    /// Setup mock endpoints for GitHub API
    async fn setup_github_mocks(&self) -> Result<()> {
        // List repositories endpoint
        Mock::given(method("GET"))
            .and(path_regex(r"^/users/[^/]+/repos$"))
            .respond_with(ResponseTemplate::new(200).set_body_json(
                self.repositories.read().await.values().collect::<Vec<_>>(),
            ))
            .mount(&self.server)
            .await;

        // Get repository endpoint
        Mock::given(method("GET"))
            .and(path_regex(r"^/repos/[^/]+/[^/]+$"))
            .respond_with(move |request: &wiremock::Request| {
                let path = request.url.path();
                let parts: Vec<&str> = path.split('/').collect();
                if parts.len() >= 4 {
                    let full_name = format!("{}/{}", parts[2], parts[3]);
                    // Return the repo or 404
                    ResponseTemplate::new(200).set_body_json(serde_json::json!({
                        "id": 123456,
                        "name": parts[3],
                        "full_name": full_name,
                        "owner": {
                            "login": parts[2],
                            "id": 1,
                            "type": "User"
                        },
                        "html_url": format!("https://github.com/{}", full_name),
                        "default_branch": "main",
                        "visibility": "public"
                    }))
                } else {
                    ResponseTemplate::new(404)
                }
            })
            .mount(&self.server)
            .await;

        // List workflows endpoint
        Mock::given(method("GET"))
            .and(path_regex(r"^/repos/[^/]+/[^/]+/actions/workflows$"))
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
                "total_count": 1,
                "workflows": [{
                    "id": 1,
                    "name": "CI",
                    "path": ".github/workflows/ci.yml",
                    "state": "active"
                }]
            })))
            .mount(&self.server)
            .await;

        // Code scanning alerts endpoint
        Mock::given(method("GET"))
            .and(path_regex(r"^/repos/[^/]+/[^/]+/code-scanning/alerts$"))
            .respond_with(ResponseTemplate::new(200).set_body_json(Vec::<AlertResponse>::new()))
            .mount(&self.server)
            .await;

        // Rate limit endpoint
        Mock::given(method("GET"))
            .and(path("/rate_limit"))
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
                "resources": {
                    "core": {
                        "limit": 5000,
                        "remaining": 4999,
                        "reset": 1234567890,
                        "used": 1
                    }
                }
            })))
            .mount(&self.server)
            .await;

        Ok(())
    }

    /// Setup mock endpoints for GitLab API
    async fn setup_gitlab_mocks(&self) -> Result<()> {
        // List projects endpoint
        Mock::given(method("GET"))
            .and(path("/api/v4/projects"))
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!([{
                "id": 1,
                "name": "test-project",
                "path_with_namespace": "user/test-project",
                "web_url": "https://gitlab.com/user/test-project",
                "default_branch": "main",
                "visibility": "public"
            }])))
            .mount(&self.server)
            .await;

        // Get project endpoint
        Mock::given(method("GET"))
            .and(path_regex(r"^/api/v4/projects/[^/]+$"))
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
                "id": 1,
                "name": "test-project",
                "path_with_namespace": "user/test-project",
                "web_url": "https://gitlab.com/user/test-project",
                "default_branch": "main",
                "visibility": "public"
            })))
            .mount(&self.server)
            .await;

        // Pipelines endpoint
        Mock::given(method("GET"))
            .and(path_regex(r"^/api/v4/projects/[^/]+/pipelines$"))
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!([{
                "id": 1,
                "status": "success",
                "ref": "main",
                "sha": "abc123",
                "web_url": "https://gitlab.com/user/test-project/-/pipelines/1"
            }])))
            .mount(&self.server)
            .await;

        Ok(())
    }

    /// Setup mock endpoints for Bitbucket API
    async fn setup_bitbucket_mocks(&self) -> Result<()> {
        // List repositories endpoint
        Mock::given(method("GET"))
            .and(path_regex(r"^/2.0/repositories/[^/]+$"))
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
                "values": [{
                    "uuid": "{12345}",
                    "slug": "test-repo",
                    "full_name": "user/test-repo",
                    "links": {
                        "html": {
                            "href": "https://bitbucket.org/user/test-repo"
                        }
                    },
                    "mainbranch": {
                        "name": "main"
                    },
                    "is_private": false
                }]
            })))
            .mount(&self.server)
            .await;

        // Get repository endpoint
        Mock::given(method("GET"))
            .and(path_regex(r"^/2.0/repositories/[^/]+/[^/]+$"))
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
                "uuid": "{12345}",
                "slug": "test-repo",
                "full_name": "user/test-repo",
                "links": {
                    "html": {
                        "href": "https://bitbucket.org/user/test-repo"
                    }
                },
                "mainbranch": {
                    "name": "main"
                },
                "is_private": false
            })))
            .mount(&self.server)
            .await;

        // Pipelines endpoint
        Mock::given(method("GET"))
            .and(path_regex(r"^/2.0/repositories/[^/]+/[^/]+/pipelines$"))
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
                "values": [{
                    "uuid": "{pipeline-1}",
                    "state": {
                        "name": "COMPLETED",
                        "result": {
                            "name": "SUCCESSFUL"
                        }
                    }
                }]
            })))
            .mount(&self.server)
            .await;

        Ok(())
    }
}

// ============================================================================
// Mock Forge Client
// ============================================================================

/// Mock forge client for testing API interactions
struct MockForgeClient {
    base_url: String,
    forge_type: ForgeType,
    client: reqwest::Client,
    token: Option<String>,
}

#[derive(Debug, Clone, Copy)]
enum ForgeType {
    GitHub,
    GitLab,
    Bitbucket,
}

impl MockForgeClient {
    fn new(base_url: &str, forge_type: ForgeType) -> Self {
        Self {
            base_url: base_url.to_string(),
            forge_type,
            client: reqwest::Client::new(),
            token: None,
        }
    }

    fn with_token(mut self, token: &str) -> Self {
        self.token = Some(token.to_string());
        self
    }

    async fn get_repository(&self, owner: &str, repo: &str) -> Result<RepositoryResponse> {
        let url = match self.forge_type {
            ForgeType::GitHub => format!("{}/repos/{}/{}", self.base_url, owner, repo),
            ForgeType::GitLab => format!(
                "{}/api/v4/projects/{}%2F{}",
                self.base_url, owner, repo
            ),
            ForgeType::Bitbucket => format!("{}/2.0/repositories/{}/{}", self.base_url, owner, repo),
        };

        let mut request = self.client.get(&url);
        if let Some(ref token) = self.token {
            request = request.header("Authorization", format!("Bearer {}", token));
        }

        let response = request.send().await?;
        let body = response.text().await?;
        debug!("Response: {}", body);

        // Parse based on forge type
        let repo: RepositoryResponse = serde_json::from_str(&body)?;
        Ok(repo)
    }

    async fn list_workflows(&self, owner: &str, repo: &str) -> Result<Vec<WorkflowResponse>> {
        let url = match self.forge_type {
            ForgeType::GitHub => format!(
                "{}/repos/{}/{}/actions/workflows",
                self.base_url, owner, repo
            ),
            ForgeType::GitLab => {
                // GitLab uses pipelines
                return Ok(vec![]);
            }
            ForgeType::Bitbucket => {
                // Bitbucket uses pipelines
                return Ok(vec![]);
            }
        };

        let response = self.client.get(&url).send().await?;
        let body: serde_json::Value = response.json().await?;

        let workflows: Vec<WorkflowResponse> = serde_json::from_value(
            body.get("workflows").cloned().unwrap_or(serde_json::json!([])),
        )?;

        Ok(workflows)
    }

    async fn get_rate_limit(&self) -> Result<u64> {
        if !matches!(self.forge_type, ForgeType::GitHub) {
            return Ok(u64::MAX);
        }

        let url = format!("{}/rate_limit", self.base_url);
        let response = self.client.get(&url).send().await?;
        let body: serde_json::Value = response.json().await?;

        let remaining = body["resources"]["core"]["remaining"]
            .as_u64()
            .unwrap_or(0);

        Ok(remaining)
    }
}

// ============================================================================
// Test Cases
// ============================================================================

#[tokio::test]
async fn test_github_mock_server() -> Result<()> {
    setup_test_logging();

    let mock_server = MockForgeServer::new().await?;
    mock_server.setup_github_mocks().await?;

    info!("GitHub mock server running at: {}", mock_server.url());

    let client = MockForgeClient::new(&mock_server.url(), ForgeType::GitHub);

    // Test get repository
    let repo = client.get_repository("user", "test-repo").await?;
    assert_eq!(repo.name, "test-repo");

    // Test list workflows
    let workflows = client.list_workflows("user", "test-repo").await?;
    assert_eq!(workflows.len(), 1);
    assert_eq!(workflows[0].name, "CI");

    // Test rate limit
    let remaining = client.get_rate_limit().await?;
    assert_eq!(remaining, 4999);

    info!("GitHub mock server test passed");
    Ok(())
}

#[tokio::test]
async fn test_gitlab_mock_server() -> Result<()> {
    setup_test_logging();

    let mock_server = MockForgeServer::new().await?;
    mock_server.setup_gitlab_mocks().await?;

    info!("GitLab mock server running at: {}", mock_server.url());

    // Make request to list projects
    let response = reqwest::get(format!("{}/api/v4/projects", mock_server.url())).await?;
    assert!(response.status().is_success());

    let projects: Vec<serde_json::Value> = response.json().await?;
    assert_eq!(projects.len(), 1);
    assert_eq!(projects[0]["name"], "test-project");

    info!("GitLab mock server test passed");
    Ok(())
}

#[tokio::test]
async fn test_bitbucket_mock_server() -> Result<()> {
    setup_test_logging();

    let mock_server = MockForgeServer::new().await?;
    mock_server.setup_bitbucket_mocks().await?;

    info!("Bitbucket mock server running at: {}", mock_server.url());

    // Make request to list repositories
    let response = reqwest::get(format!("{}/2.0/repositories/user", mock_server.url())).await?;
    assert!(response.status().is_success());

    let body: serde_json::Value = response.json().await?;
    let repos = body["values"].as_array().unwrap();
    assert_eq!(repos.len(), 1);
    assert_eq!(repos[0]["slug"], "test-repo");

    info!("Bitbucket mock server test passed");
    Ok(())
}

#[tokio::test]
async fn test_authentication_header() -> Result<()> {
    setup_test_logging();

    let mock_server = MockServer::start().await;

    // Mock that requires authentication
    Mock::given(method("GET"))
        .and(path("/repos/owner/repo"))
        .and(header("Authorization", "Bearer test-token"))
        .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
            "id": 1,
            "name": "repo",
            "full_name": "owner/repo",
            "owner": {"login": "owner", "id": 1, "type": "User"},
            "html_url": "https://github.com/owner/repo",
            "default_branch": "main",
            "visibility": "public"
        })))
        .mount(&mock_server)
        .await;

    // Without token - should fail or return different response
    Mock::given(method("GET"))
        .and(path("/repos/owner/repo"))
        .respond_with(ResponseTemplate::new(401).set_body_json(serde_json::json!({
            "message": "Bad credentials"
        })))
        .mount(&mock_server)
        .await;

    let client = MockForgeClient::new(&mock_server.uri(), ForgeType::GitHub)
        .with_token("test-token");

    let repo = client.get_repository("owner", "repo").await?;
    assert_eq!(repo.name, "repo");

    info!("Authentication header test passed");
    Ok(())
}

#[tokio::test]
async fn test_rate_limiting_response() -> Result<()> {
    setup_test_logging();

    let mock_server = MockServer::start().await;

    // Mock rate limit exceeded response
    Mock::given(method("GET"))
        .and(path("/repos/owner/repo"))
        .respond_with(
            ResponseTemplate::new(403)
                .set_body_json(serde_json::json!({
                    "message": "API rate limit exceeded"
                }))
                .append_header("X-RateLimit-Remaining", "0")
                .append_header("X-RateLimit-Reset", "1234567890"),
        )
        .mount(&mock_server)
        .await;

    let client = reqwest::Client::new();
    let response = client
        .get(format!("{}/repos/owner/repo", mock_server.uri()))
        .send()
        .await?;

    assert_eq!(response.status(), 403);
    assert_eq!(
        response.headers().get("X-RateLimit-Remaining").unwrap(),
        "0"
    );

    info!("Rate limiting response test passed");
    Ok(())
}

#[tokio::test]
async fn test_webhook_payload_handling() -> Result<()> {
    setup_test_logging();

    let mock_server = MockServer::start().await;

    // Mock webhook receiver
    Mock::given(method("POST"))
        .and(path("/webhook"))
        .and(header("X-GitHub-Event", "push"))
        .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
            "status": "received"
        })))
        .mount(&mock_server)
        .await;

    // Simulate webhook delivery
    let webhook_payload = serde_json::json!({
        "ref": "refs/heads/main",
        "repository": {
            "full_name": "owner/repo"
        },
        "pusher": {
            "name": "testuser"
        },
        "commits": []
    });

    let client = reqwest::Client::new();
    let response = client
        .post(format!("{}/webhook", mock_server.uri()))
        .header("X-GitHub-Event", "push")
        .header("X-GitHub-Delivery", uuid::Uuid::new_v4().to_string())
        .json(&webhook_payload)
        .send()
        .await?;

    assert!(response.status().is_success());

    info!("Webhook payload handling test passed");
    Ok(())
}

#[tokio::test]
async fn test_error_handling() -> Result<()> {
    setup_test_logging();

    let mock_server = MockServer::start().await;

    // Mock 404 response
    Mock::given(method("GET"))
        .and(path("/repos/nonexistent/repo"))
        .respond_with(ResponseTemplate::new(404).set_body_json(serde_json::json!({
            "message": "Not Found"
        })))
        .mount(&mock_server)
        .await;

    // Mock 500 response
    Mock::given(method("GET"))
        .and(path("/repos/error/repo"))
        .respond_with(ResponseTemplate::new(500).set_body_json(serde_json::json!({
            "message": "Internal Server Error"
        })))
        .mount(&mock_server)
        .await;

    let client = reqwest::Client::new();

    // Test 404
    let response = client
        .get(format!("{}/repos/nonexistent/repo", mock_server.uri()))
        .send()
        .await?;
    assert_eq!(response.status(), 404);

    // Test 500
    let response = client
        .get(format!("{}/repos/error/repo", mock_server.uri()))
        .send()
        .await?;
    assert_eq!(response.status(), 500);

    info!("Error handling test passed");
    Ok(())
}

#[tokio::test]
async fn test_pagination_handling() -> Result<()> {
    setup_test_logging();

    let mock_server = MockServer::start().await;

    // First page
    Mock::given(method("GET"))
        .and(path("/repos/owner/repo/issues"))
        .and(wiremock::matchers::query_param("page", "1"))
        .respond_with(
            ResponseTemplate::new(200)
                .set_body_json(serde_json::json!([
                    {"number": 1, "title": "Issue 1"},
                    {"number": 2, "title": "Issue 2"}
                ]))
                .append_header("Link", r#"<http://api.github.com/repos/owner/repo/issues?page=2>; rel="next""#),
        )
        .mount(&mock_server)
        .await;

    // Second page (last)
    Mock::given(method("GET"))
        .and(path("/repos/owner/repo/issues"))
        .and(wiremock::matchers::query_param("page", "2"))
        .respond_with(
            ResponseTemplate::new(200)
                .set_body_json(serde_json::json!([
                    {"number": 3, "title": "Issue 3"}
                ])),
        )
        .mount(&mock_server)
        .await;

    let client = reqwest::Client::new();

    // Fetch first page
    let response = client
        .get(format!("{}/repos/owner/repo/issues?page=1", mock_server.uri()))
        .send()
        .await?;

    assert!(response.status().is_success());
    let link_header = response.headers().get("Link");
    assert!(link_header.is_some());

    let issues: Vec<serde_json::Value> = response.json().await?;
    assert_eq!(issues.len(), 2);

    // Fetch second page
    let response = client
        .get(format!("{}/repos/owner/repo/issues?page=2", mock_server.uri()))
        .send()
        .await?;

    let issues: Vec<serde_json::Value> = response.json().await?;
    assert_eq!(issues.len(), 1);

    info!("Pagination handling test passed");
    Ok(())
}

#[tokio::test]
async fn test_concurrent_requests() -> Result<()> {
    setup_test_logging();

    let mock_server = MockServer::start().await;

    Mock::given(method("GET"))
        .and(path_regex(r"^/repos/[^/]+/repo-\d+$"))
        .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
            "id": 1,
            "name": "repo",
            "full_name": "owner/repo",
            "owner": {"login": "owner", "id": 1, "type": "User"},
            "html_url": "https://github.com/owner/repo",
            "default_branch": "main",
            "visibility": "public"
        })))
        .mount(&mock_server)
        .await;

    let client = reqwest::Client::new();
    let base_url = mock_server.uri();

    // Make 10 concurrent requests
    let handles: Vec<_> = (0..10)
        .map(|i| {
            let client = client.clone();
            let url = format!("{}/repos/owner/repo-{}", base_url, i);
            tokio::spawn(async move { client.get(&url).send().await })
        })
        .collect();

    let mut successes = 0;
    for handle in handles {
        let result = handle.await?;
        if let Ok(response) = result {
            if response.status().is_success() {
                successes += 1;
            }
        }
    }

    assert_eq!(successes, 10);

    info!("Concurrent requests test passed");
    Ok(())
}

// ============================================================================
// Main Test Runner
// ============================================================================

fn main() {
    let rt = tokio::runtime::Runtime::new().unwrap();

    println!("Running Forge Adapters Integration Tests\n");
    println!("=========================================\n");

    let tests: Vec<(&str, fn() -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<()>> + Send>>)> = vec![
        ("test_github_mock_server", || Box::pin(test_github_mock_server())),
        ("test_gitlab_mock_server", || Box::pin(test_gitlab_mock_server())),
        ("test_bitbucket_mock_server", || Box::pin(test_bitbucket_mock_server())),
        ("test_authentication_header", || Box::pin(test_authentication_header())),
        ("test_rate_limiting_response", || Box::pin(test_rate_limiting_response())),
        ("test_webhook_payload_handling", || Box::pin(test_webhook_payload_handling())),
        ("test_error_handling", || Box::pin(test_error_handling())),
        ("test_pagination_handling", || Box::pin(test_pagination_handling())),
        ("test_concurrent_requests", || Box::pin(test_concurrent_requests())),
    ];

    let mut passed = 0;
    let mut failed = 0;

    for (name, test_fn) in tests {
        print!("  {} ... ", name);
        match rt.block_on(test_fn()) {
            Ok(_) => {
                println!("ok");
                passed += 1;
            }
            Err(e) => {
                println!("FAILED");
                eprintln!("    Error: {}", e);
                failed += 1;
            }
        }
    }

    println!("\n=========================================");
    println!("Results: {} passed, {} failed", passed, failed);

    if failed > 0 {
        std::process::exit(1);
    }
}

// ============================================================================
// Common Test Utilities
// ============================================================================

mod common {
    use tracing_subscriber::{fmt, prelude::*, EnvFilter};

    pub fn setup_test_logging() {
        let _ = tracing_subscriber::registry()
            .with(fmt::layer().with_test_writer())
            .with(EnvFilter::from_default_env().add_directive("info".parse().unwrap()))
            .try_init();
    }
}
