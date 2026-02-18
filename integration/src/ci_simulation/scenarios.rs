// SPDX-License-Identifier: PMPL-1.0-or-later
//! Pre-built CI Simulation Scenarios
//!
//! This module provides ready-to-use test scenarios for common CI situations.
//! Each scenario sets up a simulated CI environment with specific conditions
//! to test the intelligence platform's detection and response capabilities.
//!
//! # Available Scenarios
//!
//! - `happy_path_build` - Successful build with all checks passing
//! - `failing_test_scenario` - Build fails due to test failures
//! - `security_scan_findings` - Build with security vulnerabilities detected
//! - `deployment_rollback` - Deployment failure requiring rollback
//! - `parallel_jobs` - Multiple jobs running in parallel
//! - `matrix_builds` - Matrix build with multiple configurations
//! - `cache_hit_miss` - Scenarios involving cache behavior
//!
//! # Example
//!
//! ```rust
//! use integration::ci_simulation::scenarios::{happy_path_build, ScenarioResult};
//!
//! #[tokio::test]
//! async fn test_happy_path() {
//!     let result = happy_path_build().await;
//!     assert!(result.build.conclusion.unwrap().is_success());
//! }
//! ```

use super::{
    BuildConclusion, BuildConfig, BuildStatus, FindingCategory, FindingSeverity, LogEntry,
    MatrixConfig, MockCircleCI, MockGitHubActions, MockGitLabCI, SecurityFinding,
    SimulatedArtifact, SimulatedBuild, SimulatedCI, SimulatedJob, SimulatedStep,
};
use chrono::Utc;
use std::collections::HashMap;
use uuid::Uuid;

// ============================================================================
// Scenario Result Types
// ============================================================================

/// Result of running a scenario
#[derive(Debug)]
pub struct ScenarioResult {
    /// The final build state
    pub build: SimulatedBuild,
    /// All logs generated during the scenario
    pub logs: Vec<LogEntry>,
    /// Security findings detected
    pub findings: Vec<SecurityFinding>,
    /// Artifacts produced
    pub artifacts: Vec<SimulatedArtifact>,
    /// Additional scenario-specific metadata
    pub metadata: HashMap<String, serde_json::Value>,
}

impl ScenarioResult {
    /// Check if the scenario completed successfully
    pub fn is_success(&self) -> bool {
        self.build
            .conclusion
            .map(|conclusion| conclusion.is_success())
            .unwrap_or(false)
    }

    /// Check if the scenario had failures
    pub fn is_failure(&self) -> bool {
        self.build
            .conclusion
            .map(|conclusion| conclusion.is_failure())
            .unwrap_or(false)
    }

    /// Get critical findings
    pub fn critical_findings(&self) -> Vec<&SecurityFinding> {
        self.findings
            .iter()
            .filter(|finding| finding.severity.requires_attention())
            .collect()
    }

    /// Check if any jobs failed
    pub fn has_failed_jobs(&self) -> bool {
        self.build.has_failed_jobs()
    }
}

/// Configuration for running scenarios
#[derive(Debug, Clone)]
pub struct ScenarioConfig {
    /// Repository owner
    pub owner: String,
    /// Repository name
    pub repo: String,
    /// Branch to build
    pub branch: String,
    /// Commit SHA
    pub commit_sha: String,
    /// Whether to run in verbose mode (more logs)
    pub verbose: bool,
    /// Simulated build duration in seconds
    pub duration_seconds: u64,
    /// Additional inputs
    pub inputs: HashMap<String, String>,
}

impl Default for ScenarioConfig {
    fn default() -> Self {
        Self {
            owner: "test-owner".to_string(),
            repo: "test-repo".to_string(),
            branch: "main".to_string(),
            commit_sha: format!("{:040x}", rand::random::<u128>()),
            verbose: false,
            duration_seconds: 30,
            inputs: HashMap::new(),
        }
    }
}

// ============================================================================
// Happy Path Scenarios
// ============================================================================

/// Run a successful build scenario with all checks passing
///
/// This scenario simulates:
/// - Code checkout
/// - Dependency installation
/// - Build process
/// - All tests passing
/// - Artifact generation
///
/// # Returns
/// A `ScenarioResult` with a successful build
pub async fn happy_path_build() -> ScenarioResult {
    happy_path_build_with_config(ScenarioConfig::default()).await
}

/// Run a successful build scenario with custom configuration
pub async fn happy_path_build_with_config(config: ScenarioConfig) -> ScenarioResult {
    let mut ci = MockGitHubActions::new().with_default_ci();

    let build_config = BuildConfig {
        workflow: "ci.yml".to_string(),
        branch: config.branch.clone(),
        commit_sha: config.commit_sha.clone(),
        owner: config.owner.clone(),
        repo: config.repo.clone(),
        inputs: config.inputs.clone(),
        simulated_duration: Some(config.duration_seconds),
        simulate_failure: false,
        failing_jobs: vec![],
        matrix: None,
    };

    let build_id = ci
        .trigger_build(build_config)
        .await
        .expect("Failed to trigger build");

    // Add initial logs
    ci.add_log(
        &build_id,
        LogEntry::info(format!("Starting build for {}/{}", config.owner, config.repo)),
    )
    .await
    .unwrap();

    // Advance build through stages
    ci.advance_build(&build_id).await.unwrap(); // Queued -> InProgress

    ci.add_log(&build_id, LogEntry::info("Checking out repository..."))
        .await
        .unwrap();
    ci.add_log(&build_id, LogEntry::info("Installing dependencies..."))
        .await
        .unwrap();
    ci.add_log(&build_id, LogEntry::info("Building project..."))
        .await
        .unwrap();

    // Complete the build successfully
    ci.complete_build(&build_id, BuildConclusion::Success)
        .await
        .unwrap();

    // Add success artifact
    let coverage_data = serde_json::json!({
        "coverage": 85.5,
        "lines_covered": 1250,
        "lines_total": 1462,
        "branches_covered": 89.2
    });
    let artifact = SimulatedArtifact::json("coverage-report.json", &coverage_data);
    ci.add_artifact(&build_id, artifact).await.unwrap();

    ci.add_log(&build_id, LogEntry::info("Build completed successfully!"))
        .await
        .unwrap();

    // Collect results
    let build = ci.get_build(&build_id).await.unwrap();
    let logs = ci.get_logs(&build_id).await.unwrap();
    let findings = ci.get_security_findings(&build_id).await.unwrap();
    let artifacts = ci.get_artifacts(&build_id).await.unwrap();

    ScenarioResult {
        build,
        logs,
        findings,
        artifacts,
        metadata: HashMap::new(),
    }
}

// ============================================================================
// Failing Test Scenarios
// ============================================================================

/// Run a scenario where tests fail
///
/// This scenario simulates:
/// - Successful code checkout
/// - Successful dependency installation
/// - Successful build
/// - Test failures with specific error messages
///
/// # Returns
/// A `ScenarioResult` with a failed build due to test failures
pub async fn failing_test_scenario() -> ScenarioResult {
    failing_test_scenario_with_config(ScenarioConfig::default(), vec!["test_user_auth".to_string()])
        .await
}

/// Run a failing test scenario with specific failing tests
pub async fn failing_test_scenario_with_config(
    config: ScenarioConfig,
    failing_tests: Vec<String>,
) -> ScenarioResult {
    let mut ci = MockGitHubActions::new().with_default_ci();

    let build_config = BuildConfig {
        workflow: "ci.yml".to_string(),
        branch: config.branch.clone(),
        commit_sha: config.commit_sha.clone(),
        owner: config.owner.clone(),
        repo: config.repo.clone(),
        inputs: config.inputs.clone(),
        simulated_duration: Some(config.duration_seconds),
        simulate_failure: true,
        failing_jobs: vec!["test".to_string()],
        matrix: None,
    };

    let build_id = ci
        .trigger_build(build_config)
        .await
        .expect("Failed to trigger build");

    // Add logs
    ci.add_log(&build_id, LogEntry::info("Starting CI pipeline..."))
        .await
        .unwrap();

    ci.advance_build(&build_id).await.unwrap(); // Queued -> InProgress

    ci.add_log(&build_id, LogEntry::info("Checkout completed"))
        .await
        .unwrap();
    ci.add_log(&build_id, LogEntry::info("Dependencies installed"))
        .await
        .unwrap();
    ci.add_log(&build_id, LogEntry::info("Build succeeded"))
        .await
        .unwrap();
    ci.add_log(&build_id, LogEntry::info("Running tests..."))
        .await
        .unwrap();

    // Log failing tests
    for test_name in &failing_tests {
        ci.add_log(
            &build_id,
            LogEntry::error(format!("FAILED: {} - assertion failed", test_name)),
        )
        .await
        .unwrap();
    }

    ci.add_log(
        &build_id,
        LogEntry::error(format!(
            "Test suite failed: {} test(s) failed",
            failing_tests.len()
        )),
    )
    .await
    .unwrap();

    // Complete with failure
    ci.complete_build(&build_id, BuildConclusion::Failure)
        .await
        .unwrap();

    // Add test results artifact
    let test_results = serde_json::json!({
        "total": 50,
        "passed": 50 - failing_tests.len(),
        "failed": failing_tests.len(),
        "skipped": 0,
        "failing_tests": failing_tests
    });
    let artifact = SimulatedArtifact::json("test-results.json", &test_results);
    ci.add_artifact(&build_id, artifact).await.unwrap();

    // Collect results
    let build = ci.get_build(&build_id).await.unwrap();
    let logs = ci.get_logs(&build_id).await.unwrap();
    let findings = ci.get_security_findings(&build_id).await.unwrap();
    let artifacts = ci.get_artifacts(&build_id).await.unwrap();

    let mut metadata = HashMap::new();
    metadata.insert(
        "failing_tests".to_string(),
        serde_json::json!(failing_tests),
    );

    ScenarioResult {
        build,
        logs,
        findings,
        artifacts,
        metadata,
    }
}

// ============================================================================
// Security Scan Scenarios
// ============================================================================

/// Run a scenario with security scan findings
///
/// This scenario simulates:
/// - Security scanning during build
/// - Detection of various security issues
/// - Proper severity classification
///
/// # Returns
/// A `ScenarioResult` with security findings
pub async fn security_scan_findings() -> ScenarioResult {
    security_scan_findings_with_severity(vec![
        FindingSeverity::Critical,
        FindingSeverity::High,
        FindingSeverity::Medium,
    ])
    .await
}

/// Run a security scan scenario with specific severity levels
pub async fn security_scan_findings_with_severity(
    severities: Vec<FindingSeverity>,
) -> ScenarioResult {
    let mut ci = MockGitHubActions::new();
    let config = ScenarioConfig::default();

    let build_config = BuildConfig {
        workflow: "security.yml".to_string(),
        branch: config.branch.clone(),
        commit_sha: config.commit_sha.clone(),
        owner: config.owner.clone(),
        repo: config.repo.clone(),
        ..Default::default()
    };

    let build_id = ci
        .trigger_build(build_config)
        .await
        .expect("Failed to trigger build");

    ci.advance_build(&build_id).await.unwrap();

    ci.add_log(&build_id, LogEntry::info("Running security scan..."))
        .await
        .unwrap();

    // Add security findings based on requested severities
    for (index, severity) in severities.iter().enumerate() {
        let (rule_id, title, category, description) = match severity {
            FindingSeverity::Critical => (
                "SEC-CRIT-001",
                "Hardcoded secrets detected",
                FindingCategory::SecretExposure,
                "API key found in source code",
            ),
            FindingSeverity::High => (
                "SEC-HIGH-001",
                "SQL injection vulnerability",
                FindingCategory::CodeSecurity,
                "User input not properly sanitized",
            ),
            FindingSeverity::Medium => (
                "SEC-MED-001",
                "Outdated dependency with known vulnerability",
                FindingCategory::DependencyVuln,
                "Package xyz has a known CVE",
            ),
            FindingSeverity::Low => (
                "SEC-LOW-001",
                "Missing Content-Security-Policy header",
                FindingCategory::Configuration,
                "CSP header not configured",
            ),
            FindingSeverity::Info => (
                "SEC-INFO-001",
                "Debug logging enabled",
                FindingCategory::CodeQuality,
                "Debug statements found in production code",
            ),
        };

        let finding = SecurityFinding {
            id: Uuid::new_v4().to_string(),
            rule_id: rule_id.to_string(),
            severity: *severity,
            category,
            title: title.to_string(),
            description: description.to_string(),
            file: Some(format!("src/module{}.rs", index)),
            line: Some((index as u32 + 1) * 10),
            column: Some(5),
            suggested_fix: Some("Review and fix the security issue".to_string()),
            auto_fixable: *severity == FindingSeverity::Low || *severity == FindingSeverity::Info,
            cwe_ids: vec!["CWE-798".to_string()],
            cve_ids: vec![],
        };

        ci.add_security_finding(&build_id, finding).await.unwrap();

        ci.add_log(
            &build_id,
            LogEntry::warning(format!(
                "Security finding: [{}] {} - {}",
                format!("{:?}", severity).to_uppercase(),
                title,
                description
            )),
        )
        .await
        .unwrap();
    }

    // Complete build - might still succeed depending on severity thresholds
    let has_blocking_findings = severities
        .iter()
        .any(|severity| severity.requires_attention());

    let conclusion = if has_blocking_findings {
        BuildConclusion::Failure
    } else {
        BuildConclusion::Success
    };

    ci.complete_build(&build_id, conclusion).await.unwrap();

    // Add security report artifact
    let report = serde_json::json!({
        "scan_completed": true,
        "findings_count": severities.len(),
        "critical": severities.iter().filter(|s| **s == FindingSeverity::Critical).count(),
        "high": severities.iter().filter(|s| **s == FindingSeverity::High).count(),
        "medium": severities.iter().filter(|s| **s == FindingSeverity::Medium).count(),
        "low": severities.iter().filter(|s| **s == FindingSeverity::Low).count(),
        "info": severities.iter().filter(|s| **s == FindingSeverity::Info).count(),
    });
    let artifact = SimulatedArtifact::json("security-report.json", &report);
    ci.add_artifact(&build_id, artifact).await.unwrap();

    // Collect results
    let build = ci.get_build(&build_id).await.unwrap();
    let logs = ci.get_logs(&build_id).await.unwrap();
    let findings = ci.get_security_findings(&build_id).await.unwrap();
    let artifacts = ci.get_artifacts(&build_id).await.unwrap();

    ScenarioResult {
        build,
        logs,
        findings,
        artifacts,
        metadata: HashMap::new(),
    }
}

// ============================================================================
// Deployment Scenarios
// ============================================================================

/// Run a deployment rollback scenario
///
/// This scenario simulates:
/// - Successful build
/// - Deployment attempt to staging
/// - Deployment failure
/// - Rollback procedure
///
/// # Returns
/// A `ScenarioResult` representing the rollback operation
pub async fn deployment_rollback() -> ScenarioResult {
    deployment_rollback_with_reason("Health check failed after deployment".to_string()).await
}

/// Run a deployment rollback scenario with a specific failure reason
pub async fn deployment_rollback_with_reason(failure_reason: String) -> ScenarioResult {
    let mut ci = MockGitHubActions::new();
    let config = ScenarioConfig::default();

    let build_config = BuildConfig {
        workflow: "deploy.yml".to_string(),
        branch: config.branch.clone(),
        commit_sha: config.commit_sha.clone(),
        owner: config.owner.clone(),
        repo: config.repo.clone(),
        ..Default::default()
    };

    let build_id = ci
        .trigger_build(build_config)
        .await
        .expect("Failed to trigger build");

    ci.advance_build(&build_id).await.unwrap();

    // Simulate deployment stages
    ci.add_log(&build_id, LogEntry::info("Build completed successfully"))
        .await
        .unwrap();
    ci.add_log(&build_id, LogEntry::info("Preparing deployment to staging..."))
        .await
        .unwrap();
    ci.add_log(&build_id, LogEntry::info("Uploading artifacts..."))
        .await
        .unwrap();
    ci.add_log(&build_id, LogEntry::info("Starting deployment..."))
        .await
        .unwrap();

    // Deployment failure
    ci.add_log(
        &build_id,
        LogEntry::error(format!("Deployment failed: {}", failure_reason)),
    )
    .await
    .unwrap();

    // Rollback
    ci.add_log(
        &build_id,
        LogEntry::warning("Initiating automatic rollback..."),
    )
    .await
    .unwrap();
    ci.add_log(&build_id, LogEntry::info("Rolling back to previous version..."))
        .await
        .unwrap();
    ci.add_log(&build_id, LogEntry::info("Rollback completed successfully"))
        .await
        .unwrap();

    ci.complete_build(&build_id, BuildConclusion::Failure)
        .await
        .unwrap();

    // Add deployment log artifact
    let deployment_log = serde_json::json!({
        "deployment_target": "staging",
        "status": "rolled_back",
        "failure_reason": failure_reason,
        "rollback_successful": true,
        "previous_version": "v1.2.3",
        "attempted_version": "v1.2.4"
    });
    let artifact = SimulatedArtifact::json("deployment-log.json", &deployment_log);
    ci.add_artifact(&build_id, artifact).await.unwrap();

    // Collect results
    let build = ci.get_build(&build_id).await.unwrap();
    let logs = ci.get_logs(&build_id).await.unwrap();
    let findings = ci.get_security_findings(&build_id).await.unwrap();
    let artifacts = ci.get_artifacts(&build_id).await.unwrap();

    let mut metadata = HashMap::new();
    metadata.insert(
        "failure_reason".to_string(),
        serde_json::json!(failure_reason),
    );
    metadata.insert("rollback_successful".to_string(), serde_json::json!(true));

    ScenarioResult {
        build,
        logs,
        findings,
        artifacts,
        metadata,
    }
}

// ============================================================================
// Parallel Jobs Scenarios
// ============================================================================

/// Run a scenario with parallel jobs
///
/// This scenario simulates:
/// - Multiple jobs running in parallel
/// - Independent job execution
/// - Different job completion times
///
/// # Returns
/// A `ScenarioResult` with multiple parallel jobs
pub async fn parallel_jobs() -> ScenarioResult {
    parallel_jobs_with_count(4).await
}

/// Run a parallel jobs scenario with a specific number of jobs
pub async fn parallel_jobs_with_count(job_count: usize) -> ScenarioResult {
    let mut ci = MockGitHubActions::new();
    let config = ScenarioConfig::default();

    let build_config = BuildConfig {
        workflow: "parallel-ci.yml".to_string(),
        branch: config.branch.clone(),
        commit_sha: config.commit_sha.clone(),
        owner: config.owner.clone(),
        repo: config.repo.clone(),
        ..Default::default()
    };

    let build_id = ci
        .trigger_build(build_config)
        .await
        .expect("Failed to trigger build");

    // Manually add parallel jobs
    {
        let mut builds = ci.builds.write().await;
        let build = builds.get_mut(&build_id).unwrap();

        build.jobs.clear();
        for i in 0..job_count {
            let mut job = SimulatedJob::new(&format!("parallel-job-{}", i + 1));
            job.steps = vec![
                SimulatedStep::new("Setup", 1),
                SimulatedStep::new("Run", 2),
                SimulatedStep::new("Cleanup", 3),
            ];
            build.jobs.push(job);
        }
    }

    ci.advance_build(&build_id).await.unwrap();

    ci.add_log(
        &build_id,
        LogEntry::info(format!("Starting {} parallel jobs...", job_count)),
    )
    .await
    .unwrap();

    // Simulate all jobs starting
    {
        let mut builds = ci.builds.write().await;
        let build = builds.get_mut(&build_id).unwrap();
        for job in &mut build.jobs {
            job.status = BuildStatus::InProgress;
            job.started_at = Some(Utc::now());
        }
    }

    for i in 0..job_count {
        ci.add_log(
            &build_id,
            LogEntry::info(format!("Job parallel-job-{} started", i + 1)),
        )
        .await
        .unwrap();
    }

    // Simulate jobs completing at different times
    for i in 0..job_count {
        ci.add_log(
            &build_id,
            LogEntry::info(format!("Job parallel-job-{} completed", i + 1)),
        )
        .await
        .unwrap();
    }

    ci.complete_build(&build_id, BuildConclusion::Success)
        .await
        .unwrap();

    // Collect results
    let build = ci.get_build(&build_id).await.unwrap();
    let logs = ci.get_logs(&build_id).await.unwrap();
    let findings = ci.get_security_findings(&build_id).await.unwrap();
    let artifacts = ci.get_artifacts(&build_id).await.unwrap();

    let mut metadata = HashMap::new();
    metadata.insert("job_count".to_string(), serde_json::json!(job_count));

    ScenarioResult {
        build,
        logs,
        findings,
        artifacts,
        metadata,
    }
}

// ============================================================================
// Matrix Build Scenarios
// ============================================================================

/// Run a matrix build scenario
///
/// This scenario simulates:
/// - Matrix expansion with multiple configurations
/// - Parallel execution of matrix jobs
/// - Individual job success/failure tracking
///
/// # Returns
/// A `ScenarioResult` with matrix build results
pub async fn matrix_builds() -> ScenarioResult {
    let matrix = MatrixConfig {
        dimensions: {
            let mut map = HashMap::new();
            map.insert(
                "os".to_string(),
                vec!["ubuntu-latest".to_string(), "macos-latest".to_string()],
            );
            map.insert(
                "rust".to_string(),
                vec!["stable".to_string(), "nightly".to_string()],
            );
            map
        },
        exclude: vec![],
        include: vec![],
        fail_fast: true,
    };

    matrix_builds_with_config(matrix).await
}

/// Run a matrix build scenario with custom configuration
pub async fn matrix_builds_with_config(matrix: MatrixConfig) -> ScenarioResult {
    let mut ci = MockGitHubActions::new();
    let config = ScenarioConfig::default();

    let build_config = BuildConfig {
        workflow: "matrix-ci.yml".to_string(),
        branch: config.branch.clone(),
        commit_sha: config.commit_sha.clone(),
        owner: config.owner.clone(),
        repo: config.repo.clone(),
        matrix: Some(matrix.clone()),
        ..Default::default()
    };

    let build_id = ci
        .trigger_build(build_config)
        .await
        .expect("Failed to trigger build");

    // Expand matrix into jobs
    let matrix_combinations = expand_matrix(&matrix);
    let combination_count = matrix_combinations.len();

    {
        let mut builds = ci.builds.write().await;
        let build = builds.get_mut(&build_id).unwrap();

        build.jobs.clear();
        for combo in matrix_combinations.iter() {
            let job_name = combo
                .iter()
                .map(|(key, value)| format!("{}={}", key, value))
                .collect::<Vec<_>>()
                .join(", ");

            let mut job = SimulatedJob::new(&format!("build ({})", job_name));
            job.matrix = Some(combo.clone());
            job.steps = vec![
                SimulatedStep::new("Setup", 1),
                SimulatedStep::new("Build", 2),
                SimulatedStep::new("Test", 3),
            ];
            build.jobs.push(job);
        }
    }

    ci.advance_build(&build_id).await.unwrap();

    ci.add_log(
        &build_id,
        LogEntry::info(format!(
            "Matrix expanded to {} combinations",
            combination_count
        )),
    )
    .await
    .unwrap();

    for combo in &matrix_combinations {
        let combo_str = combo
            .iter()
            .map(|(key, value)| format!("{}={}", key, value))
            .collect::<Vec<_>>()
            .join(", ");
        ci.add_log(
            &build_id,
            LogEntry::info(format!("Running: {}", combo_str)),
        )
        .await
        .unwrap();
    }

    ci.complete_build(&build_id, BuildConclusion::Success)
        .await
        .unwrap();

    // Add matrix results artifact
    let matrix_results = serde_json::json!({
        "total_combinations": combination_count,
        "successful": combination_count,
        "failed": 0,
        "matrix": matrix.dimensions
    });
    let artifact = SimulatedArtifact::json("matrix-results.json", &matrix_results);
    ci.add_artifact(&build_id, artifact).await.unwrap();

    // Collect results
    let build = ci.get_build(&build_id).await.unwrap();
    let logs = ci.get_logs(&build_id).await.unwrap();
    let findings = ci.get_security_findings(&build_id).await.unwrap();
    let artifacts = ci.get_artifacts(&build_id).await.unwrap();

    let mut metadata = HashMap::new();
    metadata.insert(
        "matrix_combinations".to_string(),
        serde_json::json!(combination_count),
    );

    ScenarioResult {
        build,
        logs,
        findings,
        artifacts,
        metadata,
    }
}

/// Expand a matrix configuration into all combinations
fn expand_matrix(matrix: &MatrixConfig) -> Vec<HashMap<String, String>> {
    let mut combinations = vec![HashMap::new()];

    for (key, values) in &matrix.dimensions {
        let mut new_combinations = Vec::new();
        for combo in &combinations {
            for value in values {
                let mut new_combo = combo.clone();
                new_combo.insert(key.clone(), value.clone());
                new_combinations.push(new_combo);
            }
        }
        combinations = new_combinations;
    }

    // Apply excludes
    combinations.retain(|combo| {
        !matrix.exclude.iter().any(|exclude| {
            exclude
                .iter()
                .all(|(key, value)| combo.get(key) == Some(value))
        })
    });

    // Add includes
    for include in &matrix.include {
        combinations.push(include.clone());
    }

    combinations
}

// ============================================================================
// Cache Scenarios
// ============================================================================

/// Cache behavior configuration
#[derive(Debug, Clone)]
pub struct CacheScenarioConfig {
    /// Cache key to use
    pub cache_key: String,
    /// Whether the cache should hit
    pub cache_hit: bool,
    /// Cache size in bytes
    pub cache_size: u64,
    /// Time saved by cache hit (simulated)
    pub time_saved_seconds: u64,
}

impl Default for CacheScenarioConfig {
    fn default() -> Self {
        Self {
            cache_key: "cargo-deps-v1".to_string(),
            cache_hit: true,
            cache_size: 150 * 1024 * 1024, // 150 MB
            time_saved_seconds: 45,
        }
    }
}

/// Run a cache hit scenario
///
/// This scenario simulates:
/// - Cache lookup at the start of build
/// - Successful cache restoration
/// - Faster build due to cached dependencies
///
/// # Returns
/// A `ScenarioResult` with cache hit behavior
pub async fn cache_hit_miss() -> ScenarioResult {
    cache_hit_miss_with_config(CacheScenarioConfig::default()).await
}

/// Run a cache scenario with specific configuration
pub async fn cache_hit_miss_with_config(cache_config: CacheScenarioConfig) -> ScenarioResult {
    let mut ci = MockGitHubActions::new();
    let config = ScenarioConfig::default();

    let build_config = BuildConfig {
        workflow: "ci.yml".to_string(),
        branch: config.branch.clone(),
        commit_sha: config.commit_sha.clone(),
        owner: config.owner.clone(),
        repo: config.repo.clone(),
        ..Default::default()
    };

    let build_id = ci
        .trigger_build(build_config)
        .await
        .expect("Failed to trigger build");

    ci.advance_build(&build_id).await.unwrap();

    // Cache lookup
    ci.add_log(
        &build_id,
        LogEntry::info(format!("Looking up cache key: {}", cache_config.cache_key)),
    )
    .await
    .unwrap();

    if cache_config.cache_hit {
        ci.add_log(
            &build_id,
            LogEntry::info(format!(
                "Cache hit! Restoring {} of cached data",
                format_bytes(cache_config.cache_size)
            )),
        )
        .await
        .unwrap();
        ci.add_log(
            &build_id,
            LogEntry::info(format!(
                "Cache restored, saved ~{}s",
                cache_config.time_saved_seconds
            )),
        )
        .await
        .unwrap();
    } else {
        ci.add_log(&build_id, LogEntry::info("Cache miss, rebuilding dependencies..."))
            .await
            .unwrap();
        ci.add_log(
            &build_id,
            LogEntry::info("Dependencies built successfully"),
        )
        .await
        .unwrap();
        ci.add_log(
            &build_id,
            LogEntry::info(format!(
                "Saving cache: {} ({} bytes)",
                cache_config.cache_key,
                format_bytes(cache_config.cache_size)
            )),
        )
        .await
        .unwrap();
    }

    ci.complete_build(&build_id, BuildConclusion::Success)
        .await
        .unwrap();

    // Add cache stats artifact
    let cache_stats = serde_json::json!({
        "cache_key": cache_config.cache_key,
        "cache_hit": cache_config.cache_hit,
        "cache_size_bytes": cache_config.cache_size,
        "time_saved_seconds": if cache_config.cache_hit { cache_config.time_saved_seconds } else { 0 }
    });
    let artifact = SimulatedArtifact::json("cache-stats.json", &cache_stats);
    ci.add_artifact(&build_id, artifact).await.unwrap();

    // Collect results
    let build = ci.get_build(&build_id).await.unwrap();
    let logs = ci.get_logs(&build_id).await.unwrap();
    let findings = ci.get_security_findings(&build_id).await.unwrap();
    let artifacts = ci.get_artifacts(&build_id).await.unwrap();

    let mut metadata = HashMap::new();
    metadata.insert(
        "cache_hit".to_string(),
        serde_json::json!(cache_config.cache_hit),
    );
    metadata.insert(
        "cache_size".to_string(),
        serde_json::json!(cache_config.cache_size),
    );

    ScenarioResult {
        build,
        logs,
        findings,
        artifacts,
        metadata,
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Format bytes into human-readable string
fn format_bytes(bytes: u64) -> String {
    const KB: u64 = 1024;
    const MB: u64 = KB * 1024;
    const GB: u64 = MB * 1024;

    if bytes >= GB {
        format!("{:.2} GB", bytes as f64 / GB as f64)
    } else if bytes >= MB {
        format!("{:.2} MB", bytes as f64 / MB as f64)
    } else if bytes >= KB {
        format!("{:.2} KB", bytes as f64 / KB as f64)
    } else {
        format!("{} bytes", bytes)
    }
}

// ============================================================================
// Additional Scenario Builders
// ============================================================================

/// Builder for creating custom scenarios
pub struct ScenarioBuilder {
    ci: Box<dyn SimulatedCI + Send>,
    config: ScenarioConfig,
    custom_jobs: Vec<SimulatedJob>,
    custom_findings: Vec<SecurityFinding>,
    custom_logs: Vec<LogEntry>,
    custom_artifacts: Vec<SimulatedArtifact>,
}

impl ScenarioBuilder {
    /// Create a new scenario builder for GitHub Actions
    pub fn github_actions() -> Self {
        Self {
            ci: Box::new(MockGitHubActions::new().with_default_ci()),
            config: ScenarioConfig::default(),
            custom_jobs: Vec::new(),
            custom_findings: Vec::new(),
            custom_logs: Vec::new(),
            custom_artifacts: Vec::new(),
        }
    }

    /// Create a new scenario builder for GitLab CI
    pub fn gitlab_ci() -> Self {
        Self {
            ci: Box::new(MockGitLabCI::new()),
            config: ScenarioConfig::default(),
            custom_jobs: Vec::new(),
            custom_findings: Vec::new(),
            custom_logs: Vec::new(),
            custom_artifacts: Vec::new(),
        }
    }

    /// Create a new scenario builder for CircleCI
    pub fn circleci() -> Self {
        Self {
            ci: Box::new(MockCircleCI::new()),
            config: ScenarioConfig::default(),
            custom_jobs: Vec::new(),
            custom_findings: Vec::new(),
            custom_logs: Vec::new(),
            custom_artifacts: Vec::new(),
        }
    }

    /// Set the scenario configuration
    pub fn with_config(mut self, config: ScenarioConfig) -> Self {
        self.config = config;
        self
    }

    /// Add a custom job
    pub fn add_job(mut self, job: SimulatedJob) -> Self {
        self.custom_jobs.push(job);
        self
    }

    /// Add a security finding
    pub fn add_finding(mut self, finding: SecurityFinding) -> Self {
        self.custom_findings.push(finding);
        self
    }

    /// Add a log entry
    pub fn add_log(mut self, entry: LogEntry) -> Self {
        self.custom_logs.push(entry);
        self
    }

    /// Add an artifact
    pub fn add_artifact(mut self, artifact: SimulatedArtifact) -> Self {
        self.custom_artifacts.push(artifact);
        self
    }

    /// Run the scenario and return results
    pub async fn run(mut self, conclusion: BuildConclusion) -> ScenarioResult {
        let build_config = BuildConfig {
            workflow: "custom.yml".to_string(),
            branch: self.config.branch.clone(),
            commit_sha: self.config.commit_sha.clone(),
            owner: self.config.owner.clone(),
            repo: self.config.repo.clone(),
            ..Default::default()
        };

        let build_id = self
            .ci
            .trigger_build(build_config)
            .await
            .expect("Failed to trigger build");

        // Add custom logs
        for entry in self.custom_logs {
            self.ci.add_log(&build_id, entry).await.unwrap();
        }

        // Advance and complete build
        self.ci.advance_build(&build_id).await.unwrap();

        // Add custom findings
        for finding in self.custom_findings.clone() {
            self.ci.add_security_finding(&build_id, finding).await.unwrap();
        }

        // Add custom artifacts
        for artifact in self.custom_artifacts.clone() {
            self.ci.add_artifact(&build_id, artifact).await.unwrap();
        }

        self.ci.complete_build(&build_id, conclusion).await.unwrap();

        // Collect results
        let build = self.ci.get_build(&build_id).await.unwrap();
        let logs = self.ci.get_logs(&build_id).await.unwrap();
        let findings = self.ci.get_security_findings(&build_id).await.unwrap();
        let artifacts = self.ci.get_artifacts(&build_id).await.unwrap();

        ScenarioResult {
            build,
            logs,
            findings,
            artifacts,
            metadata: HashMap::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_happy_path_build() {
        let result = happy_path_build().await;
        assert!(result.is_success());
        assert!(!result.has_failed_jobs());
        assert!(!result.artifacts.is_empty());
    }

    #[tokio::test]
    async fn test_failing_test_scenario() {
        let result = failing_test_scenario().await;
        assert!(result.is_failure());
        assert!(result.logs.iter().any(|log| log.level == LogLevel::Error));
    }

    #[tokio::test]
    async fn test_security_scan_findings() {
        let result = security_scan_findings().await;
        assert!(!result.findings.is_empty());
        assert!(!result.critical_findings().is_empty());
    }

    #[tokio::test]
    async fn test_deployment_rollback() {
        let result = deployment_rollback().await;
        assert!(result.is_failure());
        assert!(result
            .metadata
            .get("rollback_successful")
            .map(|v| v.as_bool().unwrap_or(false))
            .unwrap_or(false));
    }

    #[tokio::test]
    async fn test_parallel_jobs() {
        let result = parallel_jobs_with_count(4).await;
        assert!(result.is_success());
        assert_eq!(
            result
                .metadata
                .get("job_count")
                .map(|v| v.as_u64().unwrap_or(0))
                .unwrap_or(0),
            4
        );
    }

    #[tokio::test]
    async fn test_matrix_builds() {
        let result = matrix_builds().await;
        assert!(result.is_success());
        // 2 OS * 2 Rust versions = 4 combinations
        assert_eq!(
            result
                .metadata
                .get("matrix_combinations")
                .map(|v| v.as_u64().unwrap_or(0))
                .unwrap_or(0),
            4
        );
    }

    #[tokio::test]
    async fn test_cache_hit() {
        let result = cache_hit_miss_with_config(CacheScenarioConfig {
            cache_hit: true,
            ..Default::default()
        })
        .await;

        assert!(result.is_success());
        assert!(result
            .logs
            .iter()
            .any(|log| log.message.contains("Cache hit")));
    }

    #[tokio::test]
    async fn test_cache_miss() {
        let result = cache_hit_miss_with_config(CacheScenarioConfig {
            cache_hit: false,
            ..Default::default()
        })
        .await;

        assert!(result.is_success());
        assert!(result
            .logs
            .iter()
            .any(|log| log.message.contains("Cache miss")));
    }

    #[tokio::test]
    async fn test_scenario_builder() {
        let result = ScenarioBuilder::github_actions()
            .add_log(LogEntry::info("Custom log entry"))
            .add_finding(SecurityFinding {
                id: Uuid::new_v4().to_string(),
                rule_id: "TEST-001".to_string(),
                severity: FindingSeverity::Medium,
                category: FindingCategory::CodeQuality,
                title: "Test finding".to_string(),
                description: "A test finding".to_string(),
                file: None,
                line: None,
                column: None,
                suggested_fix: None,
                auto_fixable: false,
                cwe_ids: vec![],
                cve_ids: vec![],
            })
            .run(BuildConclusion::Success)
            .await;

        assert!(result.is_success());
        assert!(!result.findings.is_empty());
        assert!(result
            .logs
            .iter()
            .any(|log| log.message.contains("Custom log")));
    }

    #[test]
    fn test_expand_matrix() {
        let matrix = MatrixConfig {
            dimensions: {
                let mut map = HashMap::new();
                map.insert("os".to_string(), vec!["ubuntu".to_string(), "macos".to_string()]);
                map.insert("version".to_string(), vec!["1".to_string(), "2".to_string()]);
                map
            },
            exclude: vec![],
            include: vec![],
            fail_fast: true,
        };

        let combinations = expand_matrix(&matrix);
        assert_eq!(combinations.len(), 4);
    }

    #[test]
    fn test_expand_matrix_with_exclude() {
        let matrix = MatrixConfig {
            dimensions: {
                let mut map = HashMap::new();
                map.insert("os".to_string(), vec!["ubuntu".to_string(), "macos".to_string()]);
                map.insert("version".to_string(), vec!["1".to_string(), "2".to_string()]);
                map
            },
            exclude: vec![{
                let mut exclude = HashMap::new();
                exclude.insert("os".to_string(), "macos".to_string());
                exclude.insert("version".to_string(), "1".to_string());
                exclude
            }],
            include: vec![],
            fail_fast: true,
        };

        let combinations = expand_matrix(&matrix);
        assert_eq!(combinations.len(), 3);
    }

    #[test]
    fn test_format_bytes() {
        assert_eq!(format_bytes(500), "500 bytes");
        assert_eq!(format_bytes(1500), "1.46 KB");
        assert_eq!(format_bytes(1500000), "1.43 MB");
        assert_eq!(format_bytes(1500000000), "1.40 GB");
    }
}
