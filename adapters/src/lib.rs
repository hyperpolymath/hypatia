// SPDX-License-Identifier: AGPL-3.0-or-later
//! Multi-forge adapters for cicd-hyper-a
//!
//! Provides forge adapters for various code hosting platforms:
//!
//! - **GitHub** - Full GitHub.com and GitHub Enterprise support
//! - **GitLab** - Full GitLab.com and self-hosted GitLab support
//! - **Bitbucket** - Bitbucket Cloud and Data Center support
//! - **Codeberg** - Codeberg.org and self-hosted Gitea instances
//! - **SourceHut** - sr.ht services (git, builds, todo)
//! - **Radicle** - Peer-to-peer code collaboration
//!
//! All adapters implement the `ForgeAdapter` trait for consistent usage.
//! Adapters are integrated with the data layer (ArangoDB, Dragonfly) via
//! the service module.
//!
//! # Example
//!
//! ```rust,no_run
//! use adapters::{github::GitHubAdapter, ForgeAdapter, Forge};
//!
//! #[tokio::main]
//! async fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     let adapter = GitHubAdapter::new("your-token")?;
//!
//!     // List repositories
//!     let repos = adapter.list_repos("owner").await?;
//!
//!     // Create an issue
//!     let issue = adapter.create_issue(
//!         "owner",
//!         "repo",
//!         "Bug report",
//!         "Description of the bug",
//!         vec!["bug".to_string()],
//!     ).await?;
//!
//!     Ok(())
//! }
//! ```

// Core adapters
pub mod bitbucket;
pub mod codeberg;
pub mod error;
pub mod forge;
pub mod github;
pub mod gitlab;
pub mod radicle;
pub mod service;
pub mod sourcehut;

// Re-export core types for convenience
pub use error::AdapterError;
pub use forge::{
    Alert, AlertCategory, CheckConclusion, CheckRun, CheckStatus, Comment, Forge, ForgeAdapter,
    Issue, IssueState, PullRequest, PullRequestState, Repository, RunConclusion, RunStatus,
    Severity, Visibility, WebhookConfig, WebhookEvent, WebhookPayload, Workflow, WorkflowRun,
    WorkflowState,
};
pub use service::{ForgeService, ServiceHealth, SyncResult};

// Re-export adapters for convenient access
pub use bitbucket::BitbucketAdapter;
pub use codeberg::CodebergAdapter;
pub use github::GitHubAdapter;
pub use gitlab::GitLabAdapter;
pub use radicle::RadicleAdapter;
pub use sourcehut::SourcehutAdapter;

/// Create an adapter for the specified forge type
///
/// This is a convenience function for creating adapters dynamically.
///
/// # Arguments
/// * `forge` - The type of forge to create an adapter for
/// * `token` - Authentication token for the forge
/// * `base_url` - Optional custom base URL (for self-hosted instances)
///
/// # Returns
/// A boxed ForgeAdapter implementation
pub fn create_adapter(
    forge: Forge,
    token: &str,
    base_url: Option<&str>,
) -> Result<Box<dyn ForgeAdapter>, AdapterError> {
    match forge {
        Forge::GitHub => {
            let adapter = if let Some(url) = base_url {
                GitHubAdapter::with_base_url(token, url)?
            } else {
                GitHubAdapter::new(token)?
            };
            Ok(Box::new(adapter))
        }
        Forge::GitLab => {
            let adapter = if let Some(url) = base_url {
                GitLabAdapter::with_base_url(token, url)?
            } else {
                GitLabAdapter::new(token)?
            };
            Ok(Box::new(adapter))
        }
        Forge::Bitbucket => {
            // Bitbucket requires username:app_password format
            let parts: Vec<&str> = token.splitn(2, ':').collect();
            if parts.len() != 2 {
                return Err(AdapterError::ConfigError(
                    "Bitbucket token must be in 'username:app_password' format".to_string(),
                ));
            }
            let adapter = if let Some(url) = base_url {
                BitbucketAdapter::with_base_url(parts[0], parts[1], url)?
            } else {
                BitbucketAdapter::new(parts[0], parts[1])?
            };
            Ok(Box::new(adapter))
        }
        Forge::Codeberg | Forge::Gitea => {
            let adapter = if let Some(url) = base_url {
                CodebergAdapter::with_base_url(token, url)?
            } else {
                CodebergAdapter::new(token)?
            };
            Ok(Box::new(adapter))
        }
        Forge::Sourcehut => {
            let adapter = if let Some(url) = base_url {
                SourcehutAdapter::with_base_url(token, url)?
            } else {
                SourcehutAdapter::new(token)?
            };
            Ok(Box::new(adapter))
        }
        Forge::Radicle => {
            // Radicle doesn't use tokens, connects to local node
            let adapter = if let Some(url) = base_url {
                RadicleAdapter::with_node_url(url)?
            } else {
                RadicleAdapter::new()?
            };
            Ok(Box::new(adapter))
        }
    }
}

/// Detect the forge type from a repository URL
///
/// # Arguments
/// * `url` - Repository URL to analyze
///
/// # Returns
/// The detected forge type, or None if unrecognized
pub fn detect_forge(url: &str) -> Option<Forge> {
    let url_lower = url.to_lowercase();

    if url_lower.contains("github.com") || url_lower.contains("github.") {
        Some(Forge::GitHub)
    } else if url_lower.contains("gitlab.com") || url_lower.contains("gitlab.") {
        Some(Forge::GitLab)
    } else if url_lower.contains("bitbucket.org") || url_lower.contains("bitbucket.") {
        Some(Forge::Bitbucket)
    } else if url_lower.contains("codeberg.org") {
        Some(Forge::Codeberg)
    } else if url_lower.contains("gitea.") || url_lower.contains("/gitea/") {
        Some(Forge::Gitea)
    } else if url_lower.contains("sr.ht") || url_lower.contains("sourcehut.") {
        Some(Forge::Sourcehut)
    } else if url_lower.starts_with("rad:") || url_lower.contains("radicle.") {
        Some(Forge::Radicle)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_forge_github() {
        assert_eq!(
            detect_forge("https://github.com/owner/repo"),
            Some(Forge::GitHub)
        );
        assert_eq!(
            detect_forge("git@github.com:owner/repo.git"),
            Some(Forge::GitHub)
        );
        assert_eq!(
            detect_forge("https://github.enterprise.com/owner/repo"),
            Some(Forge::GitHub)
        );
    }

    #[test]
    fn test_detect_forge_gitlab() {
        assert_eq!(
            detect_forge("https://gitlab.com/owner/repo"),
            Some(Forge::GitLab)
        );
        assert_eq!(
            detect_forge("https://gitlab.example.com/owner/repo"),
            Some(Forge::GitLab)
        );
    }

    #[test]
    fn test_detect_forge_bitbucket() {
        assert_eq!(
            detect_forge("https://bitbucket.org/owner/repo"),
            Some(Forge::Bitbucket)
        );
    }

    #[test]
    fn test_detect_forge_codeberg() {
        assert_eq!(
            detect_forge("https://codeberg.org/owner/repo"),
            Some(Forge::Codeberg)
        );
    }

    #[test]
    fn test_detect_forge_gitea() {
        assert_eq!(
            detect_forge("https://gitea.example.com/owner/repo"),
            Some(Forge::Gitea)
        );
    }

    #[test]
    fn test_detect_forge_sourcehut() {
        assert_eq!(
            detect_forge("https://git.sr.ht/~owner/repo"),
            Some(Forge::Sourcehut)
        );
    }

    #[test]
    fn test_detect_forge_radicle() {
        assert_eq!(
            detect_forge("rad:z3gqcJUoA1n9HaHKufZs5FCSGazv5"),
            Some(Forge::Radicle)
        );
    }

    #[test]
    fn test_detect_forge_unknown() {
        assert_eq!(detect_forge("https://unknown.example.com/repo"), None);
    }
}
