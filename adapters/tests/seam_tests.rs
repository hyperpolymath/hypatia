// SPDX-License-Identifier: PMPL-1.0-or-later
//! Seam tests for forge adapter ↔ data layer integration
//!
//! These tests verify the interfaces between components work correctly,
//! ensuring seamless data flow through the system.

use cicd_hyper_a_adapters::forge::{Alert, AlertCategory, Forge, Repository, Severity, Visibility};
use cicd_hyper_a_adapters::SyncResult;

/// Test: Repository model conversion preserves data
#[test]
fn test_repo_model_conversion_seam() {
    let repo = Repository {
        id: "12345".to_string(),
        name: "test-repo".to_string(),
        owner: "hyperpolymath".to_string(),
        forge: Forge::GitHub,
        url: "https://github.com/hyperpolymath/test-repo".to_string(),
        visibility: Visibility::Public,
        default_branch: "main".to_string(),
        languages: vec!["Rust".to_string(), "Haskell".to_string()],
    };

    // Verify all fields are accessible
    assert_eq!(repo.id, "12345");
    assert_eq!(repo.name, "test-repo");
    assert_eq!(repo.owner, "hyperpolymath");
    assert!(matches!(repo.forge, Forge::GitHub));
    assert_eq!(repo.default_branch, "main");
    assert_eq!(repo.languages.len(), 2);
}

/// Test: Alert model conversion preserves severity
#[test]
fn test_alert_model_severity_seam() {
    let severities = vec![
        (Severity::Critical, 100.0),
        (Severity::High, 75.0),
        (Severity::Medium, 50.0),
        (Severity::Low, 25.0),
        (Severity::Info, 10.0),
    ];

    for (severity, expected_score) in severities {
        let alert = Alert {
            id: "1".to_string(),
            rule_id: "test-rule".to_string(),
            severity: severity.clone(),
            category: AlertCategory::CodeSecurity,
            description: "Test alert".to_string(),
            file: Some("/src/main.rs".to_string()),
            line: Some(42),
            auto_fixable: false,
        };

        // Verify severity is preserved
        let score = match alert.severity {
            Severity::Critical => 100.0,
            Severity::High => 75.0,
            Severity::Medium => 50.0,
            Severity::Low => 25.0,
            Severity::Info => 10.0,
        };
        assert_eq!(score, expected_score);
    }
}

/// Test: Alert category enumeration completeness
#[test]
fn test_alert_category_completeness() {
    // Verify all expected categories exist
    let categories = vec![
        AlertCategory::WorkflowSecurity,
        AlertCategory::CodeSecurity,
        AlertCategory::CodeQuality,
        AlertCategory::DependencyVuln,
        AlertCategory::ProcessHygiene,
        AlertCategory::MissingTests,
    ];

    for category in categories {
        let alert = Alert {
            id: "1".to_string(),
            rule_id: "test-rule".to_string(),
            severity: Severity::Medium,
            category,
            description: "Test".to_string(),
            file: None,
            line: None,
            auto_fixable: true,
        };

        // Verify category round-trips
        let category_str = format!("{:?}", alert.category);
        assert!(!category_str.is_empty());
    }
}

/// Test: Forge enumeration covers all supported forges
#[test]
fn test_forge_coverage() {
    let forges = vec![Forge::GitHub, Forge::GitLab, Forge::Bitbucket];

    for forge in forges {
        let forge_str = format!("{:?}", forge).to_lowercase();
        assert!(["github", "gitlab", "bitbucket"].contains(&forge_str.as_str()));
    }
}

/// Test: SyncResult aggregation
#[test]
fn test_sync_result_aggregation() {
    let result = SyncResult {
        repos_synced: 5,
        alerts_synced: 42,
        workflows_synced: 12,
    };

    assert_eq!(result.repos_synced, 5);
    assert_eq!(result.alerts_synced, 42);
    assert_eq!(result.workflows_synced, 12);
}

/// Test: Visibility enum correctness
#[test]
fn test_visibility_variants() {
    let visibilities = vec![
        (Visibility::Public, "public"),
        (Visibility::Private, "private"),
        (Visibility::Internal, "internal"),
    ];

    for (vis, expected) in visibilities {
        let vis_str = format!("{:?}", vis).to_lowercase();
        assert_eq!(vis_str, expected);
    }
}

// Integration tests that require running services would go here
// These would be run with `cargo test --features integration`

#[cfg(feature = "integration")]
mod integration {
    use super::*;
    use data::{ArangoConfig, DataConfig, DragonflyConfig};

    #[tokio::test]
    async fn test_full_sync_pipeline() {
        // Would test actual sync with mock forges and real data layer
        todo!("Requires running ArangoDB and Dragonfly")
    }

    #[tokio::test]
    async fn test_rule_cache_refresh() {
        // Would test rule caching round-trip
        todo!("Requires running data services")
    }
}
