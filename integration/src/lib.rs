// SPDX-License-Identifier: PLMP-1.0-or-later
//! Integration test utilities and shared code for cicd-hyper-a
//!
//! This module provides common utilities used across integration tests:
//! - Test logging setup
//! - Mock server helpers
//! - Test fixture loading
//! - Container management utilities

use std::path::PathBuf;
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

/// Initialize test logging with tracing
///
/// Call this at the beginning of each test to enable log output.
/// Uses the RUST_LOG environment variable for filtering.
pub fn setup_test_logging() {
    let _ = tracing_subscriber::registry()
        .with(fmt::layer().with_test_writer())
        .with(
            EnvFilter::from_default_env()
                .add_directive("info".parse().unwrap()),
        )
        .try_init();
}

/// Get the path to the fixtures directory
pub fn fixtures_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("fixtures")
}

/// Get the path to a specific fixture file
pub fn fixture_file(name: &str) -> PathBuf {
    fixtures_path().join(name)
}

/// Get the path to the test-repo fixture
pub fn test_repo_path() -> PathBuf {
    fixtures_path().join("test-repo")
}

/// Get the path to mock responses
pub fn mock_responses_path() -> PathBuf {
    fixtures_path().join("mock-responses")
}

/// Load a mock response JSON file
pub fn load_mock_response(name: &str) -> anyhow::Result<serde_json::Value> {
    let path = mock_responses_path().join(name);
    let content = std::fs::read_to_string(&path)?;
    Ok(serde_json::from_str(&content)?)
}

/// Environment configuration for tests
#[derive(Debug, Clone)]
pub struct TestConfig {
    /// ArangoDB connection URL
    pub arangodb_url: String,
    /// ArangoDB database name
    pub arangodb_database: String,
    /// ArangoDB username
    pub arangodb_username: String,
    /// ArangoDB password
    pub arangodb_password: String,
    /// Dragonfly/Redis connection URL
    pub dragonfly_url: String,
    /// Mock GitHub server URL
    pub mock_github_url: Option<String>,
    /// Mock GitLab server URL
    pub mock_gitlab_url: Option<String>,
    /// Mock Bitbucket server URL
    pub mock_bitbucket_url: Option<String>,
}

impl Default for TestConfig {
    fn default() -> Self {
        Self {
            arangodb_url: std::env::var("ARANGODB_URL")
                .unwrap_or_else(|_| "http://localhost:8529".to_string()),
            arangodb_database: std::env::var("ARANGODB_DATABASE")
                .unwrap_or_else(|_| "cicd_hyper_a_test".to_string()),
            arangodb_username: std::env::var("ARANGODB_USERNAME")
                .unwrap_or_else(|_| "root".to_string()),
            arangodb_password: std::env::var("ARANGODB_PASSWORD")
                .unwrap_or_else(|_| "testpassword".to_string()),
            dragonfly_url: std::env::var("DRAGONFLY_URL")
                .unwrap_or_else(|_| "redis://localhost:6379".to_string()),
            mock_github_url: std::env::var("MOCK_GITHUB_URL").ok(),
            mock_gitlab_url: std::env::var("MOCK_GITLAB_URL").ok(),
            mock_bitbucket_url: std::env::var("MOCK_BITBUCKET_URL").ok(),
        }
    }
}

impl TestConfig {
    /// Load configuration from environment
    pub fn from_env() -> Self {
        Self::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fixtures_path_exists() {
        let path = fixtures_path();
        // Path should be valid even if directory doesn't exist in test context
        assert!(path.ends_with("fixtures"));
    }

    #[test]
    fn test_config_defaults() {
        let config = TestConfig::default();
        assert!(config.arangodb_url.contains("8529"));
        assert!(config.dragonfly_url.contains("6379"));
    }
}
