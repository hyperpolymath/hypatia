// SPDX-License-Identifier: PLMP-1.0-or-later
//! CI Simulation Integration Tests
//!
//! This test suite validates the CI simulation framework functionality
//! and demonstrates how to use it for testing the CI/CD intelligence platform.

use integration::ci_simulation::{
    assertions::{
        assert_all_jobs_success, assert_build_failure, assert_build_success,
        assert_category_present, assert_fix_suggested, assert_has_error_logs, assert_log_contains,
        assert_max_severity, assert_no_error_logs, assert_no_false_positives, assert_rule_triggered,
        assert_severity_correct, AssertionCollector,
    },
    scenarios::{
        cache_hit_miss, cache_hit_miss_with_config, deployment_rollback,
        deployment_rollback_with_reason, failing_test_scenario, failing_test_scenario_with_config,
        happy_path_build, happy_path_build_with_config, matrix_builds, matrix_builds_with_config,
        parallel_jobs, parallel_jobs_with_count, security_scan_findings,
        security_scan_findings_with_severity, CacheScenarioConfig, ScenarioBuilder, ScenarioConfig,
    },
    BuildConclusion, BuildConfig, BuildStatus, CIProvider, FindingCategory, FindingSeverity,
    LogEntry, MatrixConfig, MockCircleCI, MockGitHubActions, MockGitLabCI, SecurityFinding,
    SimulatedArtifact, SimulatedCI, SimulatedJob, SimulationStats,
};
use std::collections::HashMap;
use tracing_subscriber::{fmt, prelude::*, EnvFilter};
use uuid::Uuid;

// ============================================================================
// Test Setup
// ============================================================================

fn setup_test_logging() {
    let _ = tracing_subscriber::registry()
        .with(fmt::layer().with_test_writer())
        .with(EnvFilter::from_default_env().add_directive("info".parse().unwrap()))
        .try_init();
}

// ============================================================================
// Basic CI Simulation Tests
// ============================================================================

mod basic_simulation {
    use super::*;

    #[tokio::test]
    async fn test_github_actions_basic_flow() {
        setup_test_logging();

        let mut ci = MockGitHubActions::new().with_default_ci();

        // Trigger a build
        let build_id = ci
            .trigger_build(BuildConfig {
                workflow: "ci.yml".to_string(),
                branch: "main".to_string(),
                commit_sha: "abc123def456".to_string(),
                owner: "hyperpolymath".to_string(),
                repo: "test-repo".to_string(),
                ..Default::default()
            })
            .await
            .expect("Failed to trigger build");

        // Verify initial state
        let build = ci.get_build(&build_id).await.unwrap();
        assert_eq!(build.status, BuildStatus::Queued);
        assert!(build.conclusion.is_none());
        assert_eq!(build.branch, "main");
        assert_eq!(build.owner, "hyperpolymath");

        // Advance to in-progress
        let status = ci.advance_build(&build_id).await.unwrap();
        assert_eq!(status, BuildStatus::InProgress);

        // Complete successfully
        ci.complete_build(&build_id, BuildConclusion::Success)
            .await
            .unwrap();

        let build = ci.get_build(&build_id).await.unwrap();
        assert_eq!(build.status, BuildStatus::Completed);
        assert_eq!(build.conclusion, Some(BuildConclusion::Success));
        assert!(build.completed_at.is_some());
    }

    #[tokio::test]
    async fn test_gitlab_ci_basic_flow() {
        setup_test_logging();

        let mut ci = MockGitLabCI::new();

        let build_id = ci
            .trigger_build(BuildConfig::default())
            .await
            .expect("Failed to trigger build");

        assert_eq!(ci.provider(), CIProvider::GitLabCI);

        ci.complete_build(&build_id, BuildConclusion::Success)
            .await
            .unwrap();

        let build = ci.get_build(&build_id).await.unwrap();
        assert_build_success(&build);
    }

    #[tokio::test]
    async fn test_circleci_basic_flow() {
        setup_test_logging();

        let mut ci = MockCircleCI::new();

        let build_id = ci
            .trigger_build(BuildConfig::default())
            .await
            .expect("Failed to trigger build");

        assert_eq!(ci.provider(), CIProvider::CircleCI);

        ci.complete_build(&build_id, BuildConclusion::Failure)
            .await
            .unwrap();

        let build = ci.get_build(&build_id).await.unwrap();
        assert_build_failure(&build);
    }

    #[tokio::test]
    async fn test_build_cancellation() {
        setup_test_logging();

        let mut ci = MockGitHubActions::new();

        let build_id = ci
            .trigger_build(BuildConfig::default())
            .await
            .expect("Failed to trigger build");

        // Advance to in-progress
        ci.advance_build(&build_id).await.unwrap();

        // Cancel the build
        ci.cancel_build(&build_id).await.unwrap();

        let build = ci.get_build(&build_id).await.unwrap();
        assert_eq!(build.status, BuildStatus::Cancelled);
        assert_eq!(build.conclusion, Some(BuildConclusion::Cancelled));
    }

    #[tokio::test]
    async fn test_multiple_builds() {
        setup_test_logging();

        let mut ci = MockGitHubActions::new();

        // Trigger multiple builds
        let build1 = ci.trigger_build(BuildConfig::default()).await.unwrap();
        let build2 = ci.trigger_build(BuildConfig::default()).await.unwrap();
        let build3 = ci.trigger_build(BuildConfig::default()).await.unwrap();

        // List all builds
        let builds = ci.list_builds().await.unwrap();
        assert_eq!(builds.len(), 3);

        // Complete them with different outcomes
        ci.complete_build(&build1, BuildConclusion::Success)
            .await
            .unwrap();
        ci.complete_build(&build2, BuildConclusion::Failure)
            .await
            .unwrap();
        ci.cancel_build(&build3).await.unwrap();

        // Check stats
        let stats = ci.stats().await;
        assert_eq!(stats.total_builds, 3);
        assert_eq!(stats.successful_builds, 1);
        assert_eq!(stats.failed_builds, 1);
        assert_eq!(stats.cancelled_builds, 1);
    }
}

// ============================================================================
// Artifact Tests
// ============================================================================

mod artifact_tests {
    use super::*;

    #[tokio::test]
    async fn test_add_text_artifact() {
        let mut ci = MockGitHubActions::new();

        let build_id = ci.trigger_build(BuildConfig::default()).await.unwrap();

        let artifact = SimulatedArtifact::text("output.log", "Build log content here");
        ci.add_artifact(&build_id, artifact).await.unwrap();

        let artifacts = ci.get_artifacts(&build_id).await.unwrap();
        assert_eq!(artifacts.len(), 1);
        assert_eq!(artifacts[0].name, "output.log");
        assert_eq!(artifacts[0].content_type, "text/plain");
    }

    #[tokio::test]
    async fn test_add_json_artifact() {
        let mut ci = MockGitHubActions::new();

        let build_id = ci.trigger_build(BuildConfig::default()).await.unwrap();

        let data = serde_json::json!({
            "test_results": {
                "passed": 45,
                "failed": 2,
                "skipped": 3
            }
        });
        let artifact = SimulatedArtifact::json("test-results.json", &data);
        ci.add_artifact(&build_id, artifact).await.unwrap();

        let artifacts = ci.get_artifacts(&build_id).await.unwrap();
        assert_eq!(artifacts.len(), 1);
        assert_eq!(artifacts[0].name, "test-results.json");
        assert_eq!(artifacts[0].content_type, "application/json");
    }

    #[tokio::test]
    async fn test_multiple_artifacts() {
        let mut ci = MockGitHubActions::new();

        let build_id = ci.trigger_build(BuildConfig::default()).await.unwrap();

        ci.add_artifact(&build_id, SimulatedArtifact::text("log1.txt", "Log 1"))
            .await
            .unwrap();
        ci.add_artifact(&build_id, SimulatedArtifact::text("log2.txt", "Log 2"))
            .await
            .unwrap();
        ci.add_artifact(
            &build_id,
            SimulatedArtifact::binary("binary.bin", vec![0, 1, 2, 3]),
        )
        .await
        .unwrap();

        let artifacts = ci.get_artifacts(&build_id).await.unwrap();
        assert_eq!(artifacts.len(), 3);
    }
}

// ============================================================================
// Security Finding Tests
// ============================================================================

mod security_finding_tests {
    use super::*;

    #[tokio::test]
    async fn test_add_security_finding() {
        let mut ci = MockGitHubActions::new();

        let build_id = ci.trigger_build(BuildConfig::default()).await.unwrap();

        let finding = SecurityFinding {
            id: Uuid::new_v4().to_string(),
            rule_id: "SEC-001".to_string(),
            severity: FindingSeverity::High,
            category: FindingCategory::CodeSecurity,
            title: "SQL Injection vulnerability".to_string(),
            description: "User input not sanitized".to_string(),
            file: Some("src/db.rs".to_string()),
            line: Some(42),
            column: Some(15),
            suggested_fix: Some("Use parameterized queries".to_string()),
            auto_fixable: false,
            cwe_ids: vec!["CWE-89".to_string()],
            cve_ids: vec![],
        };

        ci.add_security_finding(&build_id, finding).await.unwrap();

        let findings = ci.get_security_findings(&build_id).await.unwrap();
        assert_eq!(findings.len(), 1);
        assert_eq!(findings[0].rule_id, "SEC-001");
        assert_eq!(findings[0].severity, FindingSeverity::High);
    }

    #[tokio::test]
    async fn test_multiple_security_findings() {
        let mut ci = MockGitHubActions::new();

        let build_id = ci.trigger_build(BuildConfig::default()).await.unwrap();

        // Add findings of different severities
        for (rule_id, severity) in [
            ("CRIT-001", FindingSeverity::Critical),
            ("HIGH-001", FindingSeverity::High),
            ("MED-001", FindingSeverity::Medium),
            ("LOW-001", FindingSeverity::Low),
            ("INFO-001", FindingSeverity::Info),
        ] {
            ci.add_security_finding(
                &build_id,
                SecurityFinding {
                    id: Uuid::new_v4().to_string(),
                    rule_id: rule_id.to_string(),
                    severity,
                    category: FindingCategory::CodeSecurity,
                    title: format!("{:?} finding", severity),
                    description: "Test finding".to_string(),
                    file: None,
                    line: None,
                    column: None,
                    suggested_fix: None,
                    auto_fixable: false,
                    cwe_ids: vec![],
                    cve_ids: vec![],
                },
            )
            .await
            .unwrap();
        }

        let findings = ci.get_security_findings(&build_id).await.unwrap();
        assert_eq!(findings.len(), 5);

        // Verify stats
        let stats = ci.stats().await;
        assert_eq!(stats.total_security_findings, 5);
        assert_eq!(stats.critical_findings, 2); // Critical + High
    }
}

// ============================================================================
// Logging Tests
// ============================================================================

mod logging_tests {
    use super::*;

    #[tokio::test]
    async fn test_add_logs() {
        let mut ci = MockGitHubActions::new();

        let build_id = ci.trigger_build(BuildConfig::default()).await.unwrap();

        ci.add_log(&build_id, LogEntry::info("Build started"))
            .await
            .unwrap();
        ci.add_log(&build_id, LogEntry::debug("Debug info"))
            .await
            .unwrap();
        ci.add_log(&build_id, LogEntry::warning("Warning message"))
            .await
            .unwrap();
        ci.add_log(&build_id, LogEntry::error("Error occurred"))
            .await
            .unwrap();

        let logs = ci.get_logs(&build_id).await.unwrap();
        assert_eq!(logs.len(), 4);
        assert_log_contains(&logs, "Build started");
        assert_has_error_logs(&logs);
    }
}

// ============================================================================
// Scenario Tests
// ============================================================================

mod scenario_tests {
    use super::*;

    #[tokio::test]
    async fn test_happy_path_build_scenario() {
        setup_test_logging();

        let result = happy_path_build().await;

        assert!(result.is_success());
        assert!(!result.has_failed_jobs());
        assert!(!result.artifacts.is_empty());
        assert_no_error_logs(&result.logs);
        assert_log_contains(&result.logs, "completed successfully");
    }

    #[tokio::test]
    async fn test_happy_path_with_custom_config() {
        let config = ScenarioConfig {
            owner: "my-org".to_string(),
            repo: "my-repo".to_string(),
            branch: "feature/test".to_string(),
            ..Default::default()
        };

        let result = happy_path_build_with_config(config).await;

        assert!(result.is_success());
        assert_eq!(result.build.owner, "my-org");
        assert_eq!(result.build.repo, "my-repo");
        assert_eq!(result.build.branch, "feature/test");
    }

    #[tokio::test]
    async fn test_failing_test_scenario() {
        setup_test_logging();

        let result = failing_test_scenario().await;

        assert!(result.is_failure());
        assert_has_error_logs(&result.logs);
        assert_log_contains(&result.logs, "FAILED");
    }

    #[tokio::test]
    async fn test_failing_test_with_specific_tests() {
        let failing_tests = vec![
            "test_user_login".to_string(),
            "test_session_timeout".to_string(),
        ];

        let result = failing_test_scenario_with_config(ScenarioConfig::default(), failing_tests).await;

        assert!(result.is_failure());
        assert_log_contains(&result.logs, "test_user_login");
        assert_log_contains(&result.logs, "test_session_timeout");
    }

    #[tokio::test]
    async fn test_security_scan_findings_scenario() {
        setup_test_logging();

        let result = security_scan_findings().await;

        assert!(!result.findings.is_empty());
        assert!(!result.critical_findings().is_empty());

        // Verify findings have correct properties
        for finding in &result.findings {
            assert!(!finding.rule_id.is_empty());
            assert!(!finding.title.is_empty());
        }
    }

    #[tokio::test]
    async fn test_security_scan_with_specific_severities() {
        let severities = vec![FindingSeverity::Medium, FindingSeverity::Low];

        let result = security_scan_findings_with_severity(severities).await;

        // Should pass (no blocking findings)
        assert!(result.is_success());

        // Should have only medium and low findings
        assert_max_severity(&result.findings, FindingSeverity::Medium);
    }

    #[tokio::test]
    async fn test_deployment_rollback_scenario() {
        setup_test_logging();

        let result = deployment_rollback().await;

        assert!(result.is_failure());
        assert_log_contains(&result.logs, "rollback");
        assert_log_contains(&result.logs, "Rollback completed");

        // Check metadata
        assert!(result
            .metadata
            .get("rollback_successful")
            .map(|v| v.as_bool().unwrap_or(false))
            .unwrap_or(false));
    }

    #[tokio::test]
    async fn test_deployment_rollback_custom_reason() {
        let result =
            deployment_rollback_with_reason("Database migration failed".to_string()).await;

        assert!(result.is_failure());
        assert_log_contains(&result.logs, "Database migration failed");
    }

    #[tokio::test]
    async fn test_parallel_jobs_scenario() {
        setup_test_logging();

        let result = parallel_jobs().await;

        assert!(result.is_success());
        assert_eq!(
            result.metadata.get("job_count").unwrap().as_u64().unwrap(),
            4
        );
    }

    #[tokio::test]
    async fn test_parallel_jobs_custom_count() {
        let result = parallel_jobs_with_count(8).await;

        assert!(result.is_success());
        assert_eq!(
            result.metadata.get("job_count").unwrap().as_u64().unwrap(),
            8
        );
    }

    #[tokio::test]
    async fn test_matrix_builds_scenario() {
        setup_test_logging();

        let result = matrix_builds().await;

        assert!(result.is_success());
        // Default matrix: 2 OS x 2 Rust versions = 4 combinations
        assert_eq!(
            result
                .metadata
                .get("matrix_combinations")
                .unwrap()
                .as_u64()
                .unwrap(),
            4
        );
    }

    #[tokio::test]
    async fn test_matrix_builds_custom_config() {
        let matrix = MatrixConfig {
            dimensions: {
                let mut map = HashMap::new();
                map.insert(
                    "os".to_string(),
                    vec!["ubuntu".to_string(), "macos".to_string(), "windows".to_string()],
                );
                map.insert(
                    "node".to_string(),
                    vec!["18".to_string(), "20".to_string()],
                );
                map
            },
            exclude: vec![],
            include: vec![],
            fail_fast: true,
        };

        let result = matrix_builds_with_config(matrix).await;

        assert!(result.is_success());
        // 3 OS x 2 Node versions = 6 combinations
        assert_eq!(
            result
                .metadata
                .get("matrix_combinations")
                .unwrap()
                .as_u64()
                .unwrap(),
            6
        );
    }

    #[tokio::test]
    async fn test_cache_hit_scenario() {
        let result = cache_hit_miss_with_config(CacheScenarioConfig {
            cache_hit: true,
            cache_key: "cargo-deps-main".to_string(),
            cache_size: 200 * 1024 * 1024,
            time_saved_seconds: 60,
        })
        .await;

        assert!(result.is_success());
        assert_log_contains(&result.logs, "Cache hit");
        assert!(result.metadata.get("cache_hit").unwrap().as_bool().unwrap());
    }

    #[tokio::test]
    async fn test_cache_miss_scenario() {
        let result = cache_hit_miss_with_config(CacheScenarioConfig {
            cache_hit: false,
            ..Default::default()
        })
        .await;

        assert!(result.is_success());
        assert_log_contains(&result.logs, "Cache miss");
        assert!(!result.metadata.get("cache_hit").unwrap().as_bool().unwrap());
    }
}

// ============================================================================
// Scenario Builder Tests
// ============================================================================

mod scenario_builder_tests {
    use super::*;

    #[tokio::test]
    async fn test_custom_scenario_with_builder() {
        let result = ScenarioBuilder::github_actions()
            .add_log(LogEntry::info("Custom build started"))
            .add_log(LogEntry::info("Running custom tests"))
            .add_finding(SecurityFinding {
                id: Uuid::new_v4().to_string(),
                rule_id: "CUSTOM-001".to_string(),
                severity: FindingSeverity::Medium,
                category: FindingCategory::CodeQuality,
                title: "Custom finding".to_string(),
                description: "A custom test finding".to_string(),
                file: Some("src/main.rs".to_string()),
                line: Some(10),
                column: None,
                suggested_fix: Some("Fix the issue".to_string()),
                auto_fixable: true,
                cwe_ids: vec![],
                cve_ids: vec![],
            })
            .add_artifact(SimulatedArtifact::text("custom.log", "Custom log content"))
            .run(BuildConclusion::Success)
            .await;

        assert!(result.is_success());
        assert_log_contains(&result.logs, "Custom build started");
        assert_rule_triggered(&result.findings, "CUSTOM-001");
        assert_fix_suggested(&result.findings, "CUSTOM-001");
        assert!(!result.artifacts.is_empty());
    }

    #[tokio::test]
    async fn test_gitlab_scenario_with_builder() {
        let result = ScenarioBuilder::gitlab_ci()
            .add_log(LogEntry::info("GitLab CI started"))
            .run(BuildConclusion::Success)
            .await;

        assert!(result.is_success());
        assert_log_contains(&result.logs, "GitLab CI");
    }

    #[tokio::test]
    async fn test_circleci_scenario_with_builder() {
        let result = ScenarioBuilder::circleci()
            .add_log(LogEntry::info("CircleCI started"))
            .run(BuildConclusion::Failure)
            .await;

        assert!(result.is_failure());
    }
}

// ============================================================================
// Assertion Tests
// ============================================================================

mod assertion_tests {
    use super::*;
    use integration::ci_simulation::assertions::{
        check_fix_suggested, check_rule_triggered, check_severity_correct,
    };

    fn create_sample_findings() -> Vec<SecurityFinding> {
        vec![
            SecurityFinding {
                id: Uuid::new_v4().to_string(),
                rule_id: "SEC-001".to_string(),
                severity: FindingSeverity::Critical,
                category: FindingCategory::SecretExposure,
                title: "Hardcoded API key".to_string(),
                description: "API key found in source".to_string(),
                file: Some("src/config.rs".to_string()),
                line: Some(25),
                column: Some(10),
                suggested_fix: Some("Use environment variables".to_string()),
                auto_fixable: false,
                cwe_ids: vec!["CWE-798".to_string()],
                cve_ids: vec![],
            },
            SecurityFinding {
                id: Uuid::new_v4().to_string(),
                rule_id: "SEC-002".to_string(),
                severity: FindingSeverity::High,
                category: FindingCategory::CodeSecurity,
                title: "SQL Injection".to_string(),
                description: "Unsanitized input".to_string(),
                file: Some("src/db.rs".to_string()),
                line: Some(50),
                column: Some(15),
                suggested_fix: Some("Use prepared statements".to_string()),
                auto_fixable: false,
                cwe_ids: vec!["CWE-89".to_string()],
                cve_ids: vec![],
            },
            SecurityFinding {
                id: Uuid::new_v4().to_string(),
                rule_id: "SEC-003".to_string(),
                severity: FindingSeverity::Low,
                category: FindingCategory::CodeQuality,
                title: "Unused variable".to_string(),
                description: "Variable declared but not used".to_string(),
                file: Some("src/utils.rs".to_string()),
                line: Some(10),
                column: Some(5),
                suggested_fix: Some("Remove unused variable".to_string()),
                auto_fixable: true,
                cwe_ids: vec![],
                cve_ids: vec![],
            },
        ]
    }

    #[test]
    fn test_assertion_rule_triggered() {
        let findings = create_sample_findings();

        assert_rule_triggered(&findings, "SEC-001");
        assert_rule_triggered(&findings, "SEC-002");
        assert_rule_triggered(&findings, "SEC-003");
    }

    #[test]
    fn test_check_rule_triggered_returns_result() {
        let findings = create_sample_findings();

        let result = check_rule_triggered(&findings, "SEC-001");
        assert!(result.passed);

        let result = check_rule_triggered(&findings, "NON-EXISTENT");
        assert!(!result.passed);
    }

    #[test]
    fn test_assertion_severity() {
        let findings = create_sample_findings();

        assert_severity_correct(&findings, "SEC-001", FindingSeverity::Critical);
        assert_severity_correct(&findings, "SEC-002", FindingSeverity::High);
        assert_severity_correct(&findings, "SEC-003", FindingSeverity::Low);
    }

    #[test]
    fn test_check_severity_returns_result() {
        let findings = create_sample_findings();

        let result = check_severity_correct(&findings, "SEC-001", FindingSeverity::Critical);
        assert!(result.passed);

        let result = check_severity_correct(&findings, "SEC-001", FindingSeverity::Low);
        assert!(!result.passed);
    }

    #[test]
    fn test_assertion_fix_suggested() {
        let findings = create_sample_findings();

        assert_fix_suggested(&findings, "SEC-001");
        assert_fix_suggested(&findings, "SEC-002");
        assert_fix_suggested(&findings, "SEC-003");
    }

    #[test]
    fn test_check_fix_returns_result() {
        let findings = create_sample_findings();

        let result = check_fix_suggested(&findings, "SEC-001");
        assert!(result.passed);
    }

    #[test]
    fn test_assertion_no_false_positives() {
        let findings = create_sample_findings();

        // These rule IDs are not in our findings
        let known_fps = vec!["FP-001", "FP-002"];
        assert_no_false_positives(&findings, &known_fps);
    }

    #[test]
    fn test_assertion_max_severity() {
        let findings = create_sample_findings();

        // Should fail - we have Critical findings
        let result = std::panic::catch_unwind(|| {
            assert_max_severity(&findings, FindingSeverity::High);
        });
        assert!(result.is_err());
    }

    #[test]
    fn test_assertion_category_present() {
        let findings = create_sample_findings();

        assert_category_present(&findings, FindingCategory::SecretExposure);
        assert_category_present(&findings, FindingCategory::CodeSecurity);
        assert_category_present(&findings, FindingCategory::CodeQuality);
    }

    #[test]
    fn test_assertion_collector() {
        let findings = create_sample_findings();

        let mut collector = AssertionCollector::new();

        collector.add(check_rule_triggered(&findings, "SEC-001"));
        collector.add(check_rule_triggered(&findings, "SEC-002"));
        collector.add(check_severity_correct(
            &findings,
            "SEC-001",
            FindingSeverity::Critical,
        ));

        assert!(collector.all_passed());
        assert!(collector.failures().is_empty());
    }

    #[test]
    fn test_assertion_collector_with_failures() {
        let findings = create_sample_findings();

        let mut collector = AssertionCollector::new();

        collector.add(check_rule_triggered(&findings, "SEC-001"));
        collector.add(check_rule_triggered(&findings, "MISSING-RULE"));
        collector.add(check_severity_correct(
            &findings,
            "SEC-001",
            FindingSeverity::Low, // Wrong severity
        ));

        assert!(!collector.all_passed());
        assert_eq!(collector.failures().len(), 2);
    }
}

// ============================================================================
// Job Tests
// ============================================================================

mod job_tests {
    use super::*;

    #[tokio::test]
    async fn test_get_jobs() {
        let mut ci = MockGitHubActions::new().with_default_ci();

        let build_id = ci.trigger_build(BuildConfig::default()).await.unwrap();

        let jobs = ci.get_jobs(&build_id).await.unwrap();
        assert!(!jobs.is_empty());

        for job in &jobs {
            assert!(!job.name.is_empty());
            assert_eq!(job.status, BuildStatus::Queued);
        }
    }

    #[tokio::test]
    async fn test_complete_individual_job() {
        let mut ci = MockGitHubActions::new().with_default_ci();

        let build_id = ci.trigger_build(BuildConfig::default()).await.unwrap();
        ci.advance_build(&build_id).await.unwrap();

        let jobs = ci.get_jobs(&build_id).await.unwrap();
        let first_job = &jobs[0];

        ci.complete_job(&build_id, &first_job.id, BuildConclusion::Success)
            .await
            .unwrap();

        let updated_jobs = ci.get_jobs(&build_id).await.unwrap();
        let completed_job = updated_jobs.iter().find(|j| j.id == first_job.id).unwrap();

        assert_eq!(completed_job.status, BuildStatus::Completed);
        assert_eq!(completed_job.conclusion, Some(BuildConclusion::Success));
    }

    #[tokio::test]
    async fn test_job_with_matrix() {
        let result = matrix_builds().await;

        // Verify jobs have matrix configurations
        let jobs_with_matrix: Vec<_> = result
            .build
            .jobs
            .iter()
            .filter(|j| j.matrix.is_some())
            .collect();

        assert!(!jobs_with_matrix.is_empty());
    }
}

// ============================================================================
// Stats Tests
// ============================================================================

mod stats_tests {
    use super::*;

    #[tokio::test]
    async fn test_simulation_stats() {
        let mut ci = MockGitHubActions::new();

        // Create builds with different outcomes
        for _ in 0..3 {
            let id = ci.trigger_build(BuildConfig::default()).await.unwrap();
            ci.complete_build(&id, BuildConclusion::Success)
                .await
                .unwrap();
        }

        for _ in 0..2 {
            let id = ci.trigger_build(BuildConfig::default()).await.unwrap();
            ci.complete_build(&id, BuildConclusion::Failure)
                .await
                .unwrap();
        }

        let id = ci.trigger_build(BuildConfig::default()).await.unwrap();
        ci.advance_build(&id).await.unwrap();
        ci.cancel_build(&id).await.unwrap();

        // Add a running build
        let id = ci.trigger_build(BuildConfig::default()).await.unwrap();
        ci.advance_build(&id).await.unwrap();

        let stats = ci.stats().await;

        assert_eq!(stats.total_builds, 7);
        assert_eq!(stats.successful_builds, 3);
        assert_eq!(stats.failed_builds, 2);
        assert_eq!(stats.cancelled_builds, 1);
        assert_eq!(stats.running_builds, 1);
    }

    #[tokio::test]
    async fn test_reset_clears_state() {
        let mut ci = MockGitHubActions::new();

        // Add some builds
        for _ in 0..5 {
            ci.trigger_build(BuildConfig::default()).await.unwrap();
        }

        let stats = ci.stats().await;
        assert_eq!(stats.total_builds, 5);

        // Reset
        ci.reset().await.unwrap();

        let stats = ci.stats().await;
        assert_eq!(stats.total_builds, 0);

        let builds = ci.list_builds().await.unwrap();
        assert!(builds.is_empty());
    }
}

// ============================================================================
// Error Handling Tests
// ============================================================================

mod error_handling_tests {
    use super::*;
    use integration::ci_simulation::SimulationError;

    #[tokio::test]
    async fn test_get_nonexistent_build() {
        let ci = MockGitHubActions::new();

        let result = ci.get_build("nonexistent-id").await;
        assert!(matches!(result, Err(SimulationError::BuildNotFound(_))));
    }

    #[tokio::test]
    async fn test_complete_already_completed_build() {
        let mut ci = MockGitHubActions::new();

        let build_id = ci.trigger_build(BuildConfig::default()).await.unwrap();

        // Complete it once
        ci.complete_build(&build_id, BuildConclusion::Success)
            .await
            .unwrap();

        // Try to complete again
        let result = ci
            .complete_build(&build_id, BuildConclusion::Failure)
            .await;
        assert!(matches!(
            result,
            Err(SimulationError::InvalidStateTransition { .. })
        ));
    }

    #[tokio::test]
    async fn test_cancel_completed_build() {
        let mut ci = MockGitHubActions::new();

        let build_id = ci.trigger_build(BuildConfig::default()).await.unwrap();

        // Complete it
        ci.complete_build(&build_id, BuildConclusion::Success)
            .await
            .unwrap();

        // Try to cancel
        let result = ci.cancel_build(&build_id).await;
        assert!(matches!(
            result,
            Err(SimulationError::InvalidStateTransition { .. })
        ));
    }

    #[tokio::test]
    async fn test_add_artifact_to_nonexistent_build() {
        let mut ci = MockGitHubActions::new();

        let result = ci
            .add_artifact("nonexistent", SimulatedArtifact::text("test.txt", "content"))
            .await;
        assert!(matches!(result, Err(SimulationError::BuildNotFound(_))));
    }
}

// ============================================================================
// Integration with Intelligence Platform Tests
// ============================================================================

mod intelligence_platform_tests {
    use super::*;

    /// Test that simulates how the intelligence platform would detect
    /// and respond to security findings from CI
    #[tokio::test]
    async fn test_security_detection_flow() {
        setup_test_logging();

        // Run security scan scenario
        let result = security_scan_findings_with_severity(vec![
            FindingSeverity::Critical,
            FindingSeverity::High,
        ])
        .await;

        // Verify the platform would detect critical issues
        assert!(!result.critical_findings().is_empty());

        // Verify rules are properly identified
        for finding in &result.findings {
            assert!(!finding.rule_id.is_empty());
            assert!(finding.severity.requires_attention());
        }

        // The build should fail due to critical findings
        assert!(result.is_failure());
    }

    /// Test that simulates detecting workflow issues
    #[tokio::test]
    async fn test_workflow_issue_detection() {
        let result = ScenarioBuilder::github_actions()
            .add_finding(SecurityFinding {
                id: Uuid::new_v4().to_string(),
                rule_id: "WF-SEC-001".to_string(),
                severity: FindingSeverity::High,
                category: FindingCategory::WorkflowSecurity,
                title: "Untrusted input in workflow".to_string(),
                description: "GitHub Actions workflow uses untrusted input".to_string(),
                file: Some(".github/workflows/ci.yml".to_string()),
                line: Some(15),
                column: None,
                suggested_fix: Some("Validate input before use".to_string()),
                auto_fixable: false,
                cwe_ids: vec!["CWE-94".to_string()],
                cve_ids: vec![],
            })
            .run(BuildConclusion::Failure)
            .await;

        assert_category_present(&result.findings, FindingCategory::WorkflowSecurity);
        assert_rule_triggered(&result.findings, "WF-SEC-001");
    }

    /// Test dependency vulnerability detection
    #[tokio::test]
    async fn test_dependency_vulnerability_detection() {
        let result = ScenarioBuilder::github_actions()
            .add_finding(SecurityFinding {
                id: Uuid::new_v4().to_string(),
                rule_id: "DEP-VULN-001".to_string(),
                severity: FindingSeverity::Critical,
                category: FindingCategory::DependencyVuln,
                title: "Critical vulnerability in serde".to_string(),
                description: "serde 1.0.100 has a known vulnerability".to_string(),
                file: Some("Cargo.lock".to_string()),
                line: None,
                column: None,
                suggested_fix: Some("Upgrade to serde 1.0.150 or later".to_string()),
                auto_fixable: true,
                cwe_ids: vec![],
                cve_ids: vec!["CVE-2023-12345".to_string()],
            })
            .run(BuildConclusion::Failure)
            .await;

        assert_category_present(&result.findings, FindingCategory::DependencyVuln);

        let vuln_finding = result
            .findings
            .iter()
            .find(|f| f.category == FindingCategory::DependencyVuln)
            .unwrap();

        assert!(!vuln_finding.cve_ids.is_empty());
    }
}

// ============================================================================
// Main Test Runner
// ============================================================================

fn main() {
    let rt = tokio::runtime::Runtime::new().unwrap();

    println!("Running CI Simulation Integration Tests\n");
    println!("=======================================\n");

    // Run a quick smoke test
    rt.block_on(async {
        println!("Running smoke test...");

        let result = happy_path_build().await;
        assert!(result.is_success());

        println!("Smoke test passed!\n");

        // Run stats summary
        let mut ci = MockGitHubActions::new();

        for _ in 0..10 {
            let id = ci.trigger_build(BuildConfig::default()).await.unwrap();
            ci.complete_build(&id, BuildConclusion::Success)
                .await
                .unwrap();
        }

        let stats = ci.stats().await;
        println!("Summary Statistics:");
        println!("  Total builds: {}", stats.total_builds);
        println!("  Successful: {}", stats.successful_builds);
        println!("  Failed: {}", stats.failed_builds);
        println!("\nAll tests completed successfully!");
    });
}
