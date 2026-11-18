// SPDX-License-Identifier: MPL-2.0
//! CI Simulation Testing Framework
//!
//! This module provides a comprehensive framework for testing the CI/CD
//! intelligence platform against simulated CI environments without requiring
//! actual CI provider connections.
//!
//! # Architecture
//!
//! The simulation framework consists of:
//!
//! - `SimulatedCI` trait - Common interface for all simulated CI providers
//! - Provider implementations (GitHub Actions, GitLab CI, CircleCI)
//! - Workflow trigger simulation with realistic timing
//! - Build status lifecycle simulation
//! - Artifact generation and retrieval simulation
//!
//! # Example
//!
//! ```rust
//! use integration::ci_simulation::{MockGitHubActions, SimulatedCI, BuildConfig};
//!
//! #[tokio::test]
//! async fn test_build_simulation() {
//!     let mut ci = MockGitHubActions::new();
//!
//!     let build_id = ci.trigger_build(BuildConfig {
//!         workflow: "ci.yml".to_string(),
//!         branch: "main".to_string(),
//!         commit_sha: "abc123".to_string(),
//!         ..Default::default()
//!     }).await.unwrap();
//!
//!     // Simulate build completion
//!     ci.complete_build(&build_id, BuildResult::Success).await.unwrap();
//!
//!     let status = ci.get_build_status(&build_id).await.unwrap();
//!     assert_eq!(status.conclusion, Some(BuildConclusion::Success));
//! }
//! ```

pub mod assertions;
pub mod scenarios;

use async_trait::async_trait;
use jiff::{SignedDuration, Timestamp};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use thiserror::Error;
use tokio::sync::RwLock;
use uuid::Uuid;

// ============================================================================
// Error Types
// ============================================================================

/// Errors that can occur during CI simulation
#[derive(Debug, Error)]
pub enum SimulationError {
    /// Build not found in simulation state
    #[error("Build not found: {0}")]
    BuildNotFound(String),

    /// Workflow not found in simulation state
    #[error("Workflow not found: {0}")]
    WorkflowNotFound(String),

    /// Job not found in simulation state
    #[error("Job not found: {0}")]
    JobNotFound(String),

    /// Artifact not found in simulation state
    #[error("Artifact not found: {0}")]
    ArtifactNotFound(String),

    /// Invalid state transition
    #[error("Invalid state transition from {from:?} to {to:?}")]
    InvalidStateTransition { from: BuildStatus, to: BuildStatus },

    /// Configuration error
    #[error("Configuration error: {0}")]
    ConfigError(String),

    /// Simulation timeout
    #[error("Simulation timeout: {0}")]
    Timeout(String),

    /// General simulation error
    #[error("Simulation error: {0}")]
    General(String),
}

/// Result type for simulation operations
pub type SimulationResult<T> = Result<T, SimulationError>;

// ============================================================================
// Core Types
// ============================================================================

/// Build status in the CI lifecycle
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BuildStatus {
    /// Build is waiting in the queue
    Queued,
    /// Build is currently running
    InProgress,
    /// Build has completed (check conclusion for result)
    Completed,
    /// Build was cancelled
    Cancelled,
    /// Build is waiting for approval
    WaitingApproval,
    /// Build is pending external action
    Pending,
}

impl BuildStatus {
    /// Check if this status represents a terminal state
    pub fn is_terminal(&self) -> bool {
        matches!(self, BuildStatus::Completed | BuildStatus::Cancelled)
    }

    /// Check if this status can transition to another status
    pub fn can_transition_to(&self, target: BuildStatus) -> bool {
        match (self, target) {
            // Queued can go to InProgress, Cancelled, or Pending
            (BuildStatus::Queued, BuildStatus::InProgress) => true,
            (BuildStatus::Queued, BuildStatus::Cancelled) => true,
            (BuildStatus::Queued, BuildStatus::Pending) => true,

            // InProgress can go to Completed, Cancelled, or WaitingApproval
            (BuildStatus::InProgress, BuildStatus::Completed) => true,
            (BuildStatus::InProgress, BuildStatus::Cancelled) => true,
            (BuildStatus::InProgress, BuildStatus::WaitingApproval) => true,

            // WaitingApproval can resume to InProgress or be Cancelled
            (BuildStatus::WaitingApproval, BuildStatus::InProgress) => true,
            (BuildStatus::WaitingApproval, BuildStatus::Cancelled) => true,

            // Pending can go to Queued or Cancelled
            (BuildStatus::Pending, BuildStatus::Queued) => true,
            (BuildStatus::Pending, BuildStatus::Cancelled) => true,

            // Terminal states cannot transition
            (BuildStatus::Completed, _) => false,
            (BuildStatus::Cancelled, _) => false,

            // Any other transition is invalid
            _ => false,
        }
    }
}

/// Build conclusion (result when completed)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BuildConclusion {
    /// Build succeeded
    Success,
    /// Build failed
    Failure,
    /// Build completed with neutral result (e.g., skipped)
    Neutral,
    /// Build timed out
    TimedOut,
    /// Build was cancelled by user
    Cancelled,
    /// Build was skipped due to conditions
    Skipped,
    /// Build requires manual action
    ActionRequired,
    /// Build produced stale results
    Stale,
}

impl BuildConclusion {
    /// Check if this conclusion indicates success
    pub fn is_success(&self) -> bool {
        matches!(
            self,
            BuildConclusion::Success | BuildConclusion::Neutral | BuildConclusion::Skipped
        )
    }

    /// Check if this conclusion indicates failure
    pub fn is_failure(&self) -> bool {
        matches!(
            self,
            BuildConclusion::Failure | BuildConclusion::TimedOut | BuildConclusion::Stale
        )
    }
}

/// CI provider type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CIProvider {
    GitHubActions,
    GitLabCI,
    CircleCI,
    JenkinsCI,
    TravisCI,
    AzurePipelines,
}

impl std::fmt::Display for CIProvider {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CIProvider::GitHubActions => write!(f, "GitHub Actions"),
            CIProvider::GitLabCI => write!(f, "GitLab CI"),
            CIProvider::CircleCI => write!(f, "CircleCI"),
            CIProvider::JenkinsCI => write!(f, "Jenkins"),
            CIProvider::TravisCI => write!(f, "Travis CI"),
            CIProvider::AzurePipelines => write!(f, "Azure Pipelines"),
        }
    }
}

/// Configuration for triggering a build
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildConfig {
    /// Workflow or pipeline file path
    pub workflow: String,
    /// Branch name to build
    pub branch: String,
    /// Commit SHA to build
    pub commit_sha: String,
    /// Repository owner
    pub owner: String,
    /// Repository name
    pub repo: String,
    /// Optional inputs/environment variables
    pub inputs: HashMap<String, String>,
    /// Optional matrix configurations
    pub matrix: Option<MatrixConfig>,
    /// Simulated duration in seconds (if None, uses default)
    pub simulated_duration: Option<u64>,
    /// Whether to simulate failure
    pub simulate_failure: bool,
    /// Specific jobs to simulate failure for
    pub failing_jobs: Vec<String>,
}

impl Default for BuildConfig {
    fn default() -> Self {
        Self {
            workflow: "ci.yml".to_string(),
            branch: "main".to_string(),
            commit_sha: "0000000000000000000000000000000000000000".to_string(),
            owner: "test-owner".to_string(),
            repo: "test-repo".to_string(),
            inputs: HashMap::new(),
            matrix: None,
            simulated_duration: Some(30),
            simulate_failure: false,
            failing_jobs: vec![],
        }
    }
}

/// Matrix build configuration for parallel jobs
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MatrixConfig {
    /// Matrix dimensions (e.g., {"os": ["ubuntu", "macos"], "rust": ["stable", "nightly"]})
    pub dimensions: HashMap<String, Vec<String>>,
    /// Combinations to exclude
    pub exclude: Vec<HashMap<String, String>>,
    /// Additional combinations to include
    pub include: Vec<HashMap<String, String>>,
    /// Fail-fast behavior
    pub fail_fast: bool,
}

impl Default for MatrixConfig {
    fn default() -> Self {
        Self {
            dimensions: HashMap::new(),
            exclude: vec![],
            include: vec![],
            fail_fast: true,
        }
    }
}

/// Represents a simulated build/workflow run
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SimulatedBuild {
    /// Unique build identifier
    pub id: String,
    /// Build number (incremental)
    pub number: u64,
    /// Current status
    pub status: BuildStatus,
    /// Conclusion (when completed)
    pub conclusion: Option<BuildConclusion>,
    /// Workflow file that triggered this build
    pub workflow: String,
    /// Branch name
    pub branch: String,
    /// Commit SHA
    pub commit_sha: String,
    /// Repository owner
    pub owner: String,
    /// Repository name
    pub repo: String,
    /// Jobs in this build
    pub jobs: Vec<SimulatedJob>,
    /// Build artifacts
    pub artifacts: Vec<SimulatedArtifact>,
    /// Build logs (accumulated)
    pub logs: Vec<LogEntry>,
    /// Build start time
    pub started_at: Option<Timestamp>,
    /// Build completion time
    pub completed_at: Option<Timestamp>,
    /// Created timestamp
    pub created_at: Timestamp,
    /// Last updated timestamp
    pub updated_at: Timestamp,
    /// Security findings from the build
    pub security_findings: Vec<SecurityFinding>,
    /// Custom metadata
    pub metadata: HashMap<String, serde_json::Value>,
}

impl SimulatedBuild {
    /// Create a new simulated build from configuration
    pub fn from_config(config: &BuildConfig, build_number: u64) -> Self {
        let now = Timestamp::now();
        Self {
            id: Uuid::new_v4().to_string(),
            number: build_number,
            status: BuildStatus::Queued,
            conclusion: None,
            workflow: config.workflow.clone(),
            branch: config.branch.clone(),
            commit_sha: config.commit_sha.clone(),
            owner: config.owner.clone(),
            repo: config.repo.clone(),
            jobs: vec![],
            artifacts: vec![],
            logs: vec![],
            started_at: None,
            completed_at: None,
            created_at: now,
            updated_at: now,
            security_findings: vec![],
            metadata: HashMap::new(),
        }
    }

    /// Calculate build duration
    pub fn duration(&self) -> Option<SignedDuration> {
        match (self.started_at, self.completed_at) {
            (Some(start), Some(end)) => Some(end.duration_since(start)),
            (Some(start), None) => Some(Timestamp::now().duration_since(start)),
            _ => None,
        }
    }

    /// Check if build has any failed jobs
    pub fn has_failed_jobs(&self) -> bool {
        self.jobs.iter().any(|job| {
            job.conclusion
                .map(|conclusion| conclusion.is_failure())
                .unwrap_or(false)
        })
    }

    /// Get the overall job success rate
    pub fn job_success_rate(&self) -> f64 {
        if self.jobs.is_empty() {
            return 0.0;
        }

        let successful_job_count = self
            .jobs
            .iter()
            .filter(|job| {
                job.conclusion
                    .map(|conclusion| conclusion.is_success())
                    .unwrap_or(false)
            })
            .count();

        successful_job_count as f64 / self.jobs.len() as f64
    }
}

/// Represents a job within a build
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SimulatedJob {
    /// Unique job identifier
    pub id: String,
    /// Job name
    pub name: String,
    /// Current status
    pub status: BuildStatus,
    /// Conclusion (when completed)
    pub conclusion: Option<BuildConclusion>,
    /// Steps in this job
    pub steps: Vec<SimulatedStep>,
    /// Runner information
    pub runner: Option<RunnerInfo>,
    /// Job start time
    pub started_at: Option<Timestamp>,
    /// Job completion time
    pub completed_at: Option<Timestamp>,
    /// Job logs
    pub logs: Vec<LogEntry>,
    /// Matrix combination for this job (if part of matrix)
    pub matrix: Option<HashMap<String, String>>,
    /// Dependencies on other jobs
    pub needs: Vec<String>,
}

impl SimulatedJob {
    /// Create a new simulated job
    pub fn new(name: &str) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            name: name.to_string(),
            status: BuildStatus::Queued,
            conclusion: None,
            steps: vec![],
            runner: None,
            started_at: None,
            completed_at: None,
            logs: vec![],
            matrix: None,
            needs: vec![],
        }
    }

    /// Add a step to this job
    pub fn add_step(&mut self, step: SimulatedStep) {
        self.steps.push(step);
    }

    /// Calculate job duration
    pub fn duration(&self) -> Option<SignedDuration> {
        match (self.started_at, self.completed_at) {
            (Some(start), Some(end)) => Some(end.duration_since(start)),
            (Some(start), None) => Some(Timestamp::now().duration_since(start)),
            _ => None,
        }
    }
}

/// Represents a step within a job
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SimulatedStep {
    /// Unique step identifier
    pub id: String,
    /// Step name
    pub name: String,
    /// Step number (1-indexed)
    pub number: u32,
    /// Current status
    pub status: BuildStatus,
    /// Conclusion (when completed)
    pub conclusion: Option<BuildConclusion>,
    /// Step start time
    pub started_at: Option<Timestamp>,
    /// Step completion time
    pub completed_at: Option<Timestamp>,
    /// Step logs
    pub logs: Vec<LogEntry>,
}

impl SimulatedStep {
    /// Create a new simulated step
    pub fn new(name: &str, number: u32) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            name: name.to_string(),
            number,
            status: BuildStatus::Queued,
            conclusion: None,
            started_at: None,
            completed_at: None,
            logs: vec![],
        }
    }
}

/// Runner information for a job
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RunnerInfo {
    /// Runner name
    pub name: String,
    /// Runner type (e.g., "ubuntu-latest", "self-hosted")
    pub runner_type: String,
    /// Runner OS
    pub os: String,
    /// Runner architecture
    pub arch: String,
    /// Runner labels
    pub labels: Vec<String>,
}

impl Default for RunnerInfo {
    fn default() -> Self {
        Self {
            name: "runner-1".to_string(),
            runner_type: "hosted".to_string(),
            os: "Linux".to_string(),
            arch: "X64".to_string(),
            labels: vec!["ubuntu-latest".to_string()],
        }
    }
}

/// Log entry in a build/job/step
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogEntry {
    /// Log timestamp
    pub timestamp: Timestamp,
    /// Log level
    pub level: LogLevel,
    /// Log message
    pub message: String,
    /// Optional structured data
    pub data: Option<serde_json::Value>,
}

impl LogEntry {
    /// Create a new log entry
    pub fn new(level: LogLevel, message: impl Into<String>) -> Self {
        Self {
            timestamp: Timestamp::now(),
            level,
            message: message.into(),
            data: None,
        }
    }

    /// Create an info log entry
    pub fn info(message: impl Into<String>) -> Self {
        Self::new(LogLevel::Info, message)
    }

    /// Create a warning log entry
    pub fn warning(message: impl Into<String>) -> Self {
        Self::new(LogLevel::Warning, message)
    }

    /// Create an error log entry
    pub fn error(message: impl Into<String>) -> Self {
        Self::new(LogLevel::Error, message)
    }

    /// Create a debug log entry
    pub fn debug(message: impl Into<String>) -> Self {
        Self::new(LogLevel::Debug, message)
    }
}

/// Log level
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum LogLevel {
    Debug,
    Info,
    Warning,
    Error,
}

/// Simulated build artifact
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SimulatedArtifact {
    /// Unique artifact identifier
    pub id: String,
    /// Artifact name
    pub name: String,
    /// Size in bytes
    pub size_bytes: u64,
    /// MIME type
    pub content_type: String,
    /// Artifact content (simulated)
    pub content: Vec<u8>,
    /// Expiration time
    pub expires_at: Timestamp,
    /// Created timestamp
    pub created_at: Timestamp,
}

impl SimulatedArtifact {
    /// Create a new simulated artifact
    pub fn new(name: &str, content: Vec<u8>, content_type: &str) -> Self {
        let size_bytes = content.len() as u64;
        let now = Timestamp::now();
        Self {
            id: Uuid::new_v4().to_string(),
            name: name.to_string(),
            size_bytes,
            content_type: content_type.to_string(),
            content,
            expires_at: now + SignedDuration::from_hours(24 * 90),
            created_at: now,
        }
    }

    /// Create a text artifact
    pub fn text(name: &str, content: &str) -> Self {
        Self::new(name, content.as_bytes().to_vec(), "text/plain")
    }

    /// Create a JSON artifact
    pub fn json(name: &str, value: &serde_json::Value) -> Self {
        let content = serde_json::to_vec_pretty(value).unwrap_or_default();
        Self::new(name, content, "application/json")
    }

    /// Create a binary artifact
    pub fn binary(name: &str, content: Vec<u8>) -> Self {
        Self::new(name, content, "application/octet-stream")
    }
}

/// Security finding from a build
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityFinding {
    /// Unique finding identifier
    pub id: String,
    /// Rule ID that triggered this finding
    pub rule_id: String,
    /// Finding severity
    pub severity: FindingSeverity,
    /// Finding category
    pub category: FindingCategory,
    /// Finding title
    pub title: String,
    /// Detailed description
    pub description: String,
    /// File path (if applicable)
    pub file: Option<String>,
    /// Line number (if applicable)
    pub line: Option<u32>,
    /// Column number (if applicable)
    pub column: Option<u32>,
    /// Suggested fix
    pub suggested_fix: Option<String>,
    /// Whether this can be auto-fixed
    pub auto_fixable: bool,
    /// CWE IDs associated with this finding
    pub cwe_ids: Vec<String>,
    /// CVE IDs associated with this finding (for vulnerabilities)
    pub cve_ids: Vec<String>,
}

/// Finding severity levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum FindingSeverity {
    /// Informational finding
    Info,
    /// Low severity
    Low,
    /// Medium severity
    Medium,
    /// High severity
    High,
    /// Critical severity
    Critical,
}

impl FindingSeverity {
    /// Check if this severity requires immediate attention
    pub fn requires_attention(&self) -> bool {
        matches!(self, FindingSeverity::High | FindingSeverity::Critical)
    }
}

/// Finding category
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum FindingCategory {
    /// Workflow security issue
    WorkflowSecurity,
    /// Code security vulnerability
    CodeSecurity,
    /// Code quality issue
    CodeQuality,
    /// Dependency vulnerability
    DependencyVuln,
    /// Secret exposure
    SecretExposure,
    /// Configuration issue
    Configuration,
    /// License compliance issue
    License,
    /// Missing tests or coverage
    TestCoverage,
}

// ============================================================================
// SimulatedCI Trait
// ============================================================================

/// Core trait for simulated CI providers
///
/// All simulated CI providers must implement this trait to provide
/// a consistent interface for testing CI-related functionality.
#[async_trait]
pub trait SimulatedCI: Send + Sync {
    /// Get the CI provider type
    fn provider(&self) -> CIProvider;

    /// Trigger a new build
    async fn trigger_build(&mut self, config: BuildConfig) -> SimulationResult<String>;

    /// Get build status by ID
    async fn get_build(&self, build_id: &str) -> SimulationResult<SimulatedBuild>;

    /// Get build status
    async fn get_build_status(&self, build_id: &str) -> SimulationResult<BuildStatus>;

    /// Cancel a build
    async fn cancel_build(&mut self, build_id: &str) -> SimulationResult<()>;

    /// Complete a build with the given conclusion
    async fn complete_build(
        &mut self,
        build_id: &str,
        conclusion: BuildConclusion,
    ) -> SimulationResult<()>;

    /// Advance build to next state (for step-by-step simulation)
    async fn advance_build(&mut self, build_id: &str) -> SimulationResult<BuildStatus>;

    /// Get jobs for a build
    async fn get_jobs(&self, build_id: &str) -> SimulationResult<Vec<SimulatedJob>>;

    /// Complete a specific job
    async fn complete_job(
        &mut self,
        build_id: &str,
        job_id: &str,
        conclusion: BuildConclusion,
    ) -> SimulationResult<()>;

    /// Get artifacts for a build
    async fn get_artifacts(&self, build_id: &str) -> SimulationResult<Vec<SimulatedArtifact>>;

    /// Add an artifact to a build
    async fn add_artifact(
        &mut self,
        build_id: &str,
        artifact: SimulatedArtifact,
    ) -> SimulationResult<()>;

    /// Get logs for a build
    async fn get_logs(&self, build_id: &str) -> SimulationResult<Vec<LogEntry>>;

    /// Add a log entry
    async fn add_log(&mut self, build_id: &str, entry: LogEntry) -> SimulationResult<()>;

    /// Add a security finding to a build
    async fn add_security_finding(
        &mut self,
        build_id: &str,
        finding: SecurityFinding,
    ) -> SimulationResult<()>;

    /// Get security findings for a build
    async fn get_security_findings(&self, build_id: &str)
        -> SimulationResult<Vec<SecurityFinding>>;

    /// List all builds
    async fn list_builds(&self) -> SimulationResult<Vec<SimulatedBuild>>;

    /// Clear all simulation state
    async fn reset(&mut self) -> SimulationResult<()>;

    /// Get simulation statistics
    async fn stats(&self) -> SimulationStats;
}

/// Statistics about the simulation
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SimulationStats {
    /// Total builds triggered
    pub total_builds: u64,
    /// Successful builds
    pub successful_builds: u64,
    /// Failed builds
    pub failed_builds: u64,
    /// Cancelled builds
    pub cancelled_builds: u64,
    /// Currently running builds
    pub running_builds: u64,
    /// Total jobs executed
    pub total_jobs: u64,
    /// Total artifacts generated
    pub total_artifacts: u64,
    /// Total security findings
    pub total_security_findings: u64,
    /// High or critical findings
    pub critical_findings: u64,
}

// ============================================================================
// Shared Simulation Core
// ============================================================================

/// The build store every simulated provider shares.
///
/// Each provider keeps one of these and supplies only what differs between
/// providers (its name and the jobs a new build starts with, see
/// [`SimProvider`]). The build lifecycle itself is implemented once, for every
/// provider, by the blanket `impl<P: SimProvider> SimulatedCI for P`.
#[derive(Debug, Default)]
pub struct SimBuildStore {
    builds: Arc<RwLock<HashMap<String, SimulatedBuild>>>,
    build_counter: Arc<RwLock<u64>>,
}

impl SimBuildStore {
    /// Create an empty store
    pub fn new() -> Self {
        Self::default()
    }

    /// Run `change` on one build, then stamp its `updated_at`.
    async fn update<R>(
        &self,
        build_id: &str,
        change: impl FnOnce(&mut SimulatedBuild) -> SimulationResult<R> + Send,
    ) -> SimulationResult<R> {
        let mut builds = self.builds.write().await;
        let build = builds
            .get_mut(build_id)
            .ok_or_else(|| SimulationError::BuildNotFound(build_id.to_string()))?;
        let result = change(build)?;
        build.updated_at = Timestamp::now();
        Ok(result)
    }

    /// Run `read` on a copy of one build.
    async fn read<R>(
        &self,
        build_id: &str,
        read: impl FnOnce(SimulatedBuild) -> R + Send,
    ) -> SimulationResult<R> {
        self.builds
            .read()
            .await
            .get(build_id)
            .cloned()
            .map(read)
            .ok_or_else(|| SimulationError::BuildNotFound(build_id.to_string()))
    }
}

/// What distinguishes one simulated CI provider from another.
///
/// Implement this and the provider gets the whole [`SimulatedCI`] interface.
pub trait SimProvider: Send + Sync {
    /// The provider being simulated
    fn kind(&self) -> CIProvider;

    /// The provider's build store
    fn store(&self) -> &SimBuildStore;

    /// The jobs a new build starts with
    fn initial_jobs(&self, config: &BuildConfig) -> Vec<SimulatedJob>;
}

fn check_transition(build: &SimulatedBuild, to: BuildStatus) -> SimulationResult<()> {
    if build.status.can_transition_to(to) {
        Ok(())
    } else {
        Err(SimulationError::InvalidStateTransition {
            from: build.status,
            to,
        })
    }
}

/// One step of a running build: start the next job whose `needs` are met, or
/// finish the running one, or, once every job is terminal, conclude the build
/// (failure if any job failed).
fn advance_running(build: &mut SimulatedBuild) -> BuildStatus {
    if build.jobs.iter().all(|job| job.status.is_terminal()) {
        build.completed_at = Some(Timestamp::now());
        let has_failure = build
            .jobs
            .iter()
            .any(|job| job.conclusion.is_some_and(|c| c.is_failure()));
        build.conclusion = Some(if has_failure {
            BuildConclusion::Failure
        } else {
            BuildConclusion::Success
        });
        return BuildStatus::Completed;
    }

    let completed: Vec<String> = build
        .jobs
        .iter()
        .filter(|job| job.status.is_terminal())
        .map(|job| job.name.clone())
        .collect();

    for job in &mut build.jobs {
        if job.status == BuildStatus::Queued {
            if job.needs.iter().all(|need| completed.contains(need)) {
                job.status = BuildStatus::InProgress;
                job.started_at = Some(Timestamp::now());
                break;
            }
        } else if job.status == BuildStatus::InProgress {
            job.status = BuildStatus::Completed;
            job.conclusion = Some(BuildConclusion::Success);
            job.completed_at = Some(Timestamp::now());
            break;
        }
    }
    BuildStatus::InProgress
}

#[async_trait]
impl<P: SimProvider> SimulatedCI for P {
    fn provider(&self) -> CIProvider {
        self.kind()
    }

    async fn trigger_build(&mut self, config: BuildConfig) -> SimulationResult<String> {
        let store = self.store();
        let build_number = {
            let mut counter = store.build_counter.write().await;
            *counter += 1;
            *counter
        };

        let mut build = SimulatedBuild::from_config(&config, build_number);
        build.jobs = self.initial_jobs(&config);

        let build_id = build.id.clone();
        store.builds.write().await.insert(build_id.clone(), build);
        Ok(build_id)
    }

    async fn get_build(&self, build_id: &str) -> SimulationResult<SimulatedBuild> {
        self.store().read(build_id, |build| build).await
    }

    async fn get_build_status(&self, build_id: &str) -> SimulationResult<BuildStatus> {
        self.store().read(build_id, |build| build.status).await
    }

    async fn cancel_build(&mut self, build_id: &str) -> SimulationResult<()> {
        self.store()
            .update(build_id, |build| {
                check_transition(build, BuildStatus::Cancelled)?;
                build.status = BuildStatus::Cancelled;
                build.conclusion = Some(BuildConclusion::Cancelled);
                build.completed_at = Some(Timestamp::now());
                Ok(())
            })
            .await
    }

    async fn complete_build(
        &mut self,
        build_id: &str,
        conclusion: BuildConclusion,
    ) -> SimulationResult<()> {
        self.store()
            .update(build_id, |build| {
                if build.status == BuildStatus::Queued {
                    build.status = BuildStatus::InProgress;
                    build.started_at = Some(Timestamp::now());
                }
                check_transition(build, BuildStatus::Completed)?;

                build.status = BuildStatus::Completed;
                build.conclusion = Some(conclusion);
                build.completed_at = Some(Timestamp::now());
                for job in &mut build.jobs {
                    if job.status != BuildStatus::Completed {
                        job.status = BuildStatus::Completed;
                        job.conclusion = Some(conclusion);
                        job.completed_at = Some(Timestamp::now());
                    }
                }
                Ok(())
            })
            .await
    }

    async fn advance_build(&mut self, build_id: &str) -> SimulationResult<BuildStatus> {
        self.store()
            .update(build_id, |build| {
                let new_status = match build.status {
                    BuildStatus::Queued => {
                        build.started_at = Some(Timestamp::now());
                        BuildStatus::InProgress
                    }
                    BuildStatus::InProgress => advance_running(build),
                    terminal => terminal,
                };
                build.status = new_status;
                Ok(new_status)
            })
            .await
    }

    async fn get_jobs(&self, build_id: &str) -> SimulationResult<Vec<SimulatedJob>> {
        self.store().read(build_id, |build| build.jobs).await
    }

    async fn complete_job(
        &mut self,
        build_id: &str,
        job_id: &str,
        conclusion: BuildConclusion,
    ) -> SimulationResult<()> {
        self.store()
            .update(build_id, |build| {
                let job = build
                    .jobs
                    .iter_mut()
                    .find(|job| job.id == job_id)
                    .ok_or_else(|| SimulationError::JobNotFound(job_id.to_string()))?;
                job.status = BuildStatus::Completed;
                job.conclusion = Some(conclusion);
                job.completed_at = Some(Timestamp::now());
                Ok(())
            })
            .await
    }

    async fn get_artifacts(&self, build_id: &str) -> SimulationResult<Vec<SimulatedArtifact>> {
        self.store().read(build_id, |build| build.artifacts).await
    }

    async fn add_artifact(
        &mut self,
        build_id: &str,
        artifact: SimulatedArtifact,
    ) -> SimulationResult<()> {
        self.store()
            .update(build_id, |build| {
                build.artifacts.push(artifact);
                Ok(())
            })
            .await
    }

    async fn get_logs(&self, build_id: &str) -> SimulationResult<Vec<LogEntry>> {
        self.store().read(build_id, |build| build.logs).await
    }

    async fn add_log(&mut self, build_id: &str, entry: LogEntry) -> SimulationResult<()> {
        self.store()
            .update(build_id, |build| {
                build.logs.push(entry);
                Ok(())
            })
            .await
    }

    async fn add_security_finding(
        &mut self,
        build_id: &str,
        finding: SecurityFinding,
    ) -> SimulationResult<()> {
        self.store()
            .update(build_id, |build| {
                build.security_findings.push(finding);
                Ok(())
            })
            .await
    }

    async fn get_security_findings(
        &self,
        build_id: &str,
    ) -> SimulationResult<Vec<SecurityFinding>> {
        self.store()
            .read(build_id, |build| build.security_findings)
            .await
    }

    async fn list_builds(&self) -> SimulationResult<Vec<SimulatedBuild>> {
        Ok(self.store().builds.read().await.values().cloned().collect())
    }

    async fn reset(&mut self) -> SimulationResult<()> {
        let store = self.store();
        store.builds.write().await.clear();
        *store.build_counter.write().await = 0;
        Ok(())
    }

    async fn stats(&self) -> SimulationStats {
        let builds = self.store().builds.read().await;

        let mut stats = SimulationStats {
            total_builds: builds.len() as u64,
            ..Default::default()
        };

        for build in builds.values() {
            match build.status {
                BuildStatus::Completed => match build.conclusion {
                    Some(c) if c.is_success() => stats.successful_builds += 1,
                    Some(c) if c.is_failure() => stats.failed_builds += 1,
                    _ => {}
                },
                BuildStatus::Cancelled => stats.cancelled_builds += 1,
                BuildStatus::InProgress => stats.running_builds += 1,
                _ => {}
            }

            stats.total_jobs += build.jobs.len() as u64;
            stats.total_artifacts += build.artifacts.len() as u64;
            stats.total_security_findings += build.security_findings.len() as u64;
            stats.critical_findings += build
                .security_findings
                .iter()
                .filter(|finding| finding.severity.requires_attention())
                .count() as u64;
        }

        stats
    }
}

// ============================================================================
// Mock GitHub Actions Implementation
// ============================================================================

/// Mock implementation of GitHub Actions
#[derive(Debug)]
pub struct MockGitHubActions {
    store: SimBuildStore,
    workflows: HashMap<String, WorkflowDefinition>,
}

/// Workflow definition for GitHub Actions
#[derive(Debug, Clone)]
pub struct WorkflowDefinition {
    /// Workflow file name
    pub name: String,
    /// Jobs defined in the workflow
    pub jobs: Vec<JobDefinition>,
    /// Trigger events
    pub triggers: Vec<String>,
}

/// Job definition in a workflow
#[derive(Debug, Clone)]
pub struct JobDefinition {
    /// Job ID
    pub id: String,
    /// Job name
    pub name: String,
    /// Steps in the job
    pub steps: Vec<StepDefinition>,
    /// Job dependencies
    pub needs: Vec<String>,
    /// Runner type
    pub runs_on: String,
}

/// Step definition in a job
#[derive(Debug, Clone)]
pub struct StepDefinition {
    /// Step name
    pub name: String,
    /// Action to run (if using an action)
    pub uses: Option<String>,
    /// Shell command to run
    pub run: Option<String>,
}

impl Default for MockGitHubActions {
    fn default() -> Self {
        Self::new()
    }
}

impl MockGitHubActions {
    /// Create a new mock GitHub Actions instance
    pub fn new() -> Self {
        Self {
            store: SimBuildStore::new(),
            workflows: HashMap::new(),
        }
    }

    /// Register a workflow definition
    pub fn register_workflow(&mut self, workflow: WorkflowDefinition) {
        self.workflows.insert(workflow.name.clone(), workflow);
    }

    /// Create default CI workflow
    pub fn with_default_ci(mut self) -> Self {
        let ci_workflow = WorkflowDefinition {
            name: "ci.yml".to_string(),
            triggers: vec!["push".to_string(), "pull_request".to_string()],
            jobs: vec![
                JobDefinition {
                    id: "build".to_string(),
                    name: "Build".to_string(),
                    runs_on: "ubuntu-latest".to_string(),
                    needs: vec![],
                    steps: vec![
                        StepDefinition {
                            name: "Checkout".to_string(),
                            uses: Some("actions/checkout@v4".to_string()),
                            run: None,
                        },
                        StepDefinition {
                            name: "Build".to_string(),
                            uses: None,
                            run: Some("cargo build --release".to_string()),
                        },
                    ],
                },
                JobDefinition {
                    id: "test".to_string(),
                    name: "Test".to_string(),
                    runs_on: "ubuntu-latest".to_string(),
                    needs: vec!["build".to_string()],
                    steps: vec![
                        StepDefinition {
                            name: "Checkout".to_string(),
                            uses: Some("actions/checkout@v4".to_string()),
                            run: None,
                        },
                        StepDefinition {
                            name: "Run tests".to_string(),
                            uses: None,
                            run: Some("cargo test".to_string()),
                        },
                    ],
                },
            ],
        };
        self.workflows.insert("ci.yml".to_string(), ci_workflow);
        self
    }

    /// Create jobs from workflow definition
    fn create_jobs_from_workflow(
        &self,
        workflow_name: &str,
        config: &BuildConfig,
    ) -> Vec<SimulatedJob> {
        let workflow = match self.workflows.get(workflow_name) {
            Some(workflow) => workflow,
            None => return vec![SimulatedJob::new("default")],
        };

        workflow
            .jobs
            .iter()
            .map(|job_def| {
                let mut job = SimulatedJob::new(&job_def.name);
                // `needs` names job ids; the lifecycle tracks jobs by name.
                job.needs = job_def
                    .needs
                    .iter()
                    .map(|need| {
                        workflow
                            .jobs
                            .iter()
                            .find(|other| &other.id == need)
                            .map_or_else(|| need.clone(), |other| other.name.clone())
                    })
                    .collect();
                job.runner = Some(RunnerInfo {
                    runner_type: job_def.runs_on.clone(),
                    ..Default::default()
                });

                for (index, step_def) in job_def.steps.iter().enumerate() {
                    let step = SimulatedStep::new(&step_def.name, (index + 1) as u32);
                    job.steps.push(step);
                }

                // Mark job for failure if in failing_jobs list
                if config.failing_jobs.contains(&job_def.id) {
                    job.set_metadata_failing(true);
                }

                job
            })
            .collect()
    }
}

impl SimulatedJob {
    #[allow(dead_code)]
    fn metadata_failing(&self) -> bool {
        false
    }

    #[allow(dead_code)]
    fn set_metadata_failing(&mut self, _value: bool) {
        // This is a placeholder - in real implementation would use metadata field
    }
}

impl SimProvider for MockGitHubActions {
    fn kind(&self) -> CIProvider {
        CIProvider::GitHubActions
    }

    fn store(&self) -> &SimBuildStore {
        &self.store
    }

    fn initial_jobs(&self, config: &BuildConfig) -> Vec<SimulatedJob> {
        self.create_jobs_from_workflow(&config.workflow, config)
    }
}

// ============================================================================
// Mock GitLab CI and CircleCI Implementations
// ============================================================================

/// Mock implementation of GitLab CI: a pipeline of build, test and deploy jobs
#[derive(Debug, Default)]
pub struct MockGitLabCI {
    store: SimBuildStore,
}

impl MockGitLabCI {
    /// Create a new mock GitLab CI instance
    pub fn new() -> Self {
        Self::default()
    }
}

impl SimProvider for MockGitLabCI {
    fn kind(&self) -> CIProvider {
        CIProvider::GitLabCI
    }

    fn store(&self) -> &SimBuildStore {
        &self.store
    }

    fn initial_jobs(&self, _config: &BuildConfig) -> Vec<SimulatedJob> {
        ["build", "test", "deploy"].map(SimulatedJob::new).into()
    }
}

/// Mock implementation of CircleCI: a workflow of checkout, build and test jobs
#[derive(Debug, Default)]
pub struct MockCircleCI {
    store: SimBuildStore,
}

impl MockCircleCI {
    /// Create a new mock CircleCI instance
    pub fn new() -> Self {
        Self::default()
    }
}

impl SimProvider for MockCircleCI {
    fn kind(&self) -> CIProvider {
        CIProvider::CircleCI
    }

    fn store(&self) -> &SimBuildStore {
        &self.store
    }

    fn initial_jobs(&self, _config: &BuildConfig) -> Vec<SimulatedJob> {
        ["checkout", "build", "test"].map(SimulatedJob::new).into()
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Create a simulated CI provider by type
pub fn create_simulated_ci(provider: CIProvider) -> Box<dyn SimulatedCI> {
    match provider {
        CIProvider::GitHubActions => Box::new(MockGitHubActions::new().with_default_ci()),
        CIProvider::GitLabCI => Box::new(MockGitLabCI::new()),
        CIProvider::CircleCI => Box::new(MockCircleCI::new()),
        CIProvider::JenkinsCI => Box::new(MockGitLabCI::new()), // Use GitLab as fallback
        CIProvider::TravisCI => Box::new(MockGitLabCI::new()),  // Use GitLab as fallback
        CIProvider::AzurePipelines => Box::new(MockGitLabCI::new()), // Use GitLab as fallback
    }
}

/// Create a security finding for testing
pub fn create_test_finding(
    rule_id: &str,
    severity: FindingSeverity,
    category: FindingCategory,
    title: &str,
) -> SecurityFinding {
    SecurityFinding {
        id: Uuid::new_v4().to_string(),
        rule_id: rule_id.to_string(),
        severity,
        category,
        title: title.to_string(),
        description: format!("Test finding: {}", title),
        file: None,
        line: None,
        column: None,
        suggested_fix: None,
        auto_fixable: false,
        cwe_ids: vec![],
        cve_ids: vec![],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_github_actions_trigger_build() {
        let mut ci = MockGitHubActions::new().with_default_ci();

        let build_id = ci
            .trigger_build(BuildConfig::default())
            .await
            .expect("Failed to trigger build");

        let build = ci.get_build(&build_id).await.expect("Failed to get build");

        assert_eq!(build.status, BuildStatus::Queued);
        assert!(build.conclusion.is_none());
        assert!(!build.jobs.is_empty());
    }

    #[tokio::test]
    async fn test_github_actions_complete_build() {
        let mut ci = MockGitHubActions::new();

        let build_id = ci
            .trigger_build(BuildConfig::default())
            .await
            .expect("Failed to trigger build");

        ci.complete_build(&build_id, BuildConclusion::Success)
            .await
            .expect("Failed to complete build");

        let build = ci.get_build(&build_id).await.expect("Failed to get build");

        assert_eq!(build.status, BuildStatus::Completed);
        assert_eq!(build.conclusion, Some(BuildConclusion::Success));
    }

    #[tokio::test]
    async fn test_github_actions_advance_build() {
        let mut ci = MockGitHubActions::new().with_default_ci();

        let build_id = ci
            .trigger_build(BuildConfig::default())
            .await
            .expect("Failed to trigger build");

        // Advance from Queued to InProgress
        let status = ci
            .advance_build(&build_id)
            .await
            .expect("Failed to advance build");
        assert_eq!(status, BuildStatus::InProgress);

        // Get build and check started_at is set
        let build = ci.get_build(&build_id).await.expect("Failed to get build");
        assert!(build.started_at.is_some());
    }

    #[tokio::test]
    async fn test_gitlab_ci_trigger_build() {
        let mut ci = MockGitLabCI::new();

        let build_id = ci
            .trigger_build(BuildConfig::default())
            .await
            .expect("Failed to trigger build");

        let build = ci.get_build(&build_id).await.expect("Failed to get build");

        assert_eq!(build.status, BuildStatus::Queued);
        assert_eq!(ci.provider(), CIProvider::GitLabCI);
    }

    #[tokio::test]
    async fn test_circleci_trigger_build() {
        let mut ci = MockCircleCI::new();

        let build_id = ci
            .trigger_build(BuildConfig::default())
            .await
            .expect("Failed to trigger build");

        let build = ci.get_build(&build_id).await.expect("Failed to get build");

        assert_eq!(build.status, BuildStatus::Queued);
        assert_eq!(ci.provider(), CIProvider::CircleCI);
    }

    #[tokio::test]
    async fn test_add_artifact() {
        let mut ci = MockGitHubActions::new();

        let build_id = ci
            .trigger_build(BuildConfig::default())
            .await
            .expect("Failed to trigger build");

        let artifact = SimulatedArtifact::text("test.txt", "Hello, World!");
        ci.add_artifact(&build_id, artifact)
            .await
            .expect("Failed to add artifact");

        let artifacts = ci
            .get_artifacts(&build_id)
            .await
            .expect("Failed to get artifacts");

        assert_eq!(artifacts.len(), 1);
        assert_eq!(artifacts[0].name, "test.txt");
    }

    #[tokio::test]
    async fn test_add_security_finding() {
        let mut ci = MockGitHubActions::new();

        let build_id = ci
            .trigger_build(BuildConfig::default())
            .await
            .expect("Failed to trigger build");

        let finding = create_test_finding(
            "SEC001",
            FindingSeverity::High,
            FindingCategory::WorkflowSecurity,
            "Hardcoded credentials",
        );

        ci.add_security_finding(&build_id, finding)
            .await
            .expect("Failed to add finding");

        let findings = ci
            .get_security_findings(&build_id)
            .await
            .expect("Failed to get findings");

        assert_eq!(findings.len(), 1);
        assert_eq!(findings[0].rule_id, "SEC001");
        assert_eq!(findings[0].severity, FindingSeverity::High);
    }

    #[tokio::test]
    async fn test_stats() {
        let mut ci = MockGitHubActions::new();

        // Trigger multiple builds
        for _ in 0..3 {
            let build_id = ci
                .trigger_build(BuildConfig::default())
                .await
                .expect("Failed to trigger build");

            ci.complete_build(&build_id, BuildConclusion::Success)
                .await
                .expect("Failed to complete build");
        }

        let stats = ci.stats().await;

        assert_eq!(stats.total_builds, 3);
        assert_eq!(stats.successful_builds, 3);
    }

    #[test]
    fn test_build_status_transitions() {
        assert!(BuildStatus::Queued.can_transition_to(BuildStatus::InProgress));
        assert!(BuildStatus::Queued.can_transition_to(BuildStatus::Cancelled));
        assert!(BuildStatus::InProgress.can_transition_to(BuildStatus::Completed));
        assert!(!BuildStatus::Completed.can_transition_to(BuildStatus::InProgress));
        assert!(!BuildStatus::Cancelled.can_transition_to(BuildStatus::InProgress));
    }

    #[test]
    fn test_build_conclusion_predicates() {
        assert!(BuildConclusion::Success.is_success());
        assert!(BuildConclusion::Neutral.is_success());
        assert!(!BuildConclusion::Failure.is_success());

        assert!(BuildConclusion::Failure.is_failure());
        assert!(BuildConclusion::TimedOut.is_failure());
        assert!(!BuildConclusion::Success.is_failure());
    }

    /// The three providers, each fresh. Every lifecycle rule below must hold
    /// for each of them: the lifecycle is one implementation, not three.
    fn every_provider() -> Vec<Box<dyn SimulatedCI>> {
        vec![
            Box::new(MockGitHubActions::new().with_default_ci()),
            Box::new(MockGitLabCI::new()),
            Box::new(MockCircleCI::new()),
        ]
    }

    #[tokio::test]
    async fn every_provider_refuses_to_cancel_a_completed_build() {
        for mut ci in every_provider() {
            let id = ci.trigger_build(BuildConfig::default()).await.unwrap();
            ci.complete_build(&id, BuildConclusion::Success)
                .await
                .unwrap();

            let refused = ci.cancel_build(&id).await;
            assert!(
                matches!(
                    refused,
                    Err(SimulationError::InvalidStateTransition {
                        from: BuildStatus::Completed,
                        to: BuildStatus::Cancelled
                    })
                ),
                "{}: {refused:?}",
                ci.provider()
            );
            assert_eq!(
                ci.get_build(&id).await.unwrap().conclusion,
                Some(BuildConclusion::Success)
            );
        }
    }

    #[tokio::test]
    async fn every_provider_concludes_failure_when_a_job_failed() {
        for mut ci in every_provider() {
            let id = ci.trigger_build(BuildConfig::default()).await.unwrap();
            let jobs = ci.get_jobs(&id).await.unwrap();
            ci.advance_build(&id).await.unwrap();
            for (index, job) in jobs.iter().enumerate() {
                let conclusion = if index == 0 {
                    BuildConclusion::Failure
                } else {
                    BuildConclusion::Success
                };
                ci.complete_job(&id, &job.id, conclusion).await.unwrap();
            }

            assert_eq!(
                ci.advance_build(&id).await.unwrap(),
                BuildStatus::Completed,
                "{}",
                ci.provider()
            );
            assert_eq!(
                ci.get_build(&id).await.unwrap().conclusion,
                Some(BuildConclusion::Failure),
                "{}",
                ci.provider()
            );
        }
    }

    #[tokio::test]
    async fn every_provider_counts_critical_findings() {
        for mut ci in every_provider() {
            let id = ci.trigger_build(BuildConfig::default()).await.unwrap();
            for severity in [
                FindingSeverity::Critical,
                FindingSeverity::High,
                FindingSeverity::Low,
            ] {
                let finding = create_test_finding(
                    "SEC001",
                    severity,
                    FindingCategory::WorkflowSecurity,
                    "finding",
                );
                ci.add_security_finding(&id, finding).await.unwrap();
            }

            let stats = ci.stats().await;
            assert_eq!(stats.total_security_findings, 3, "{}", ci.provider());
            assert_eq!(stats.critical_findings, 2, "{}", ci.provider());
        }
    }

    #[tokio::test]
    async fn each_provider_keeps_its_own_job_layout() {
        let names = |jobs: Vec<SimulatedJob>| -> Vec<String> {
            jobs.into_iter().map(|job| job.name).collect()
        };
        let mut gitlab = MockGitLabCI::new();
        let id = gitlab.trigger_build(BuildConfig::default()).await.unwrap();
        assert_eq!(
            names(gitlab.get_jobs(&id).await.unwrap()),
            ["build", "test", "deploy"]
        );

        let mut circle = MockCircleCI::new();
        let id = circle.trigger_build(BuildConfig::default()).await.unwrap();
        assert_eq!(
            names(circle.get_jobs(&id).await.unwrap()),
            ["checkout", "build", "test"]
        );

        let mut github = MockGitHubActions::new().with_default_ci();
        let id = github.trigger_build(BuildConfig::default()).await.unwrap();
        assert_eq!(
            names(github.get_jobs(&id).await.unwrap()),
            ["Build", "Test"]
        );
    }

    #[tokio::test]
    async fn the_default_workflow_advances_to_completion_through_its_needs() {
        // Test `needs` the job id "build"; the job is named "Build".
        let mut ci = MockGitHubActions::new().with_default_ci();
        let id = ci.trigger_build(BuildConfig::default()).await.unwrap();

        let mut status = BuildStatus::Queued;
        for _ in 0..10 {
            status = ci.advance_build(&id).await.unwrap();
            if status == BuildStatus::Completed {
                break;
            }
        }

        let jobs = ci.get_jobs(&id).await.unwrap();
        assert_eq!(status, BuildStatus::Completed, "{jobs:?}");
        assert_eq!(jobs[1].needs, ["Build"]);
        assert!(jobs.iter().all(|job| job.status == BuildStatus::Completed));
        assert_eq!(
            ci.get_build(&id).await.unwrap().conclusion,
            Some(BuildConclusion::Success)
        );
    }
}
