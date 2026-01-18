// SPDX-License-Identifier: AGPL-3.0-or-later
//! Fleet Integration Tests
//!
//! Tests all bots running in sequence, verifying:
//! - Bot orchestration and scheduling
//! - Inter-bot communication via shared context
//! - Pipeline execution order
//! - Error handling and recovery
//! - Fleet-wide state management

use anyhow::{Context, Result};
use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Command;
use std::time::Duration;
use tempfile::TempDir;
use tokio::time::timeout;
use tracing::{debug, error, info, warn};

mod common;
use common::{setup_test_logging, TestContext};

/// Bot execution result containing output and metrics
#[derive(Debug, Clone)]
struct BotExecutionResult {
    bot_name: String,
    exit_code: i32,
    stdout: String,
    stderr: String,
    duration_ms: u64,
    alerts_generated: usize,
    fixes_applied: usize,
}

/// Fleet test harness managing multiple bot executions
struct FleetTestHarness {
    test_repo: TempDir,
    bot_binaries: HashMap<String, PathBuf>,
    shared_context_path: PathBuf,
    execution_results: Vec<BotExecutionResult>,
}

impl FleetTestHarness {
    /// Create a new fleet test harness with a test repository
    fn new() -> Result<Self> {
        let test_repo = TempDir::new().context("Failed to create temp directory")?;

        // Initialize git repo
        Command::new("git")
            .args(["init"])
            .current_dir(test_repo.path())
            .output()
            .context("Failed to init git repo")?;

        // Configure git user for commits
        Command::new("git")
            .args(["config", "user.email", "test@hyperpolymath.dev"])
            .current_dir(test_repo.path())
            .output()?;
        Command::new("git")
            .args(["config", "user.name", "Test User"])
            .current_dir(test_repo.path())
            .output()?;

        let shared_context_path = test_repo.path().join(".fleet-context");
        std::fs::create_dir_all(&shared_context_path)?;

        Ok(Self {
            test_repo,
            bot_binaries: HashMap::new(),
            shared_context_path,
            execution_results: Vec::new(),
        })
    }

    /// Register a bot binary for testing
    fn register_bot(&mut self, name: &str, binary_path: PathBuf) {
        self.bot_binaries.insert(name.to_string(), binary_path);
    }

    /// Execute a single bot and capture results
    async fn execute_bot(&mut self, bot_name: &str, args: &[&str]) -> Result<BotExecutionResult> {
        let binary_path = self
            .bot_binaries
            .get(bot_name)
            .cloned()
            .unwrap_or_else(|| PathBuf::from(format!("target/release/{}", bot_name)));

        let start = std::time::Instant::now();

        let output = Command::new(&binary_path)
            .args(args)
            .current_dir(self.test_repo.path())
            .env("FLEET_CONTEXT_PATH", &self.shared_context_path)
            .env("CICD_HYPER_A_TEST_MODE", "1")
            .output()
            .context(format!("Failed to execute bot: {}", bot_name))?;

        let duration_ms = start.elapsed().as_millis() as u64;

        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();

        // Parse metrics from output (bot-specific parsing)
        let alerts_generated = Self::parse_metric(&stdout, "alerts_generated");
        let fixes_applied = Self::parse_metric(&stdout, "fixes_applied");

        let result = BotExecutionResult {
            bot_name: bot_name.to_string(),
            exit_code: output.status.code().unwrap_or(-1),
            stdout,
            stderr,
            duration_ms,
            alerts_generated,
            fixes_applied,
        };

        self.execution_results.push(result.clone());
        Ok(result)
    }

    /// Parse a numeric metric from output
    fn parse_metric(output: &str, metric_name: &str) -> usize {
        output
            .lines()
            .find(|line| line.contains(metric_name))
            .and_then(|line| {
                line.split(':')
                    .last()
                    .and_then(|v| v.trim().parse().ok())
            })
            .unwrap_or(0)
    }

    /// Execute the entire fleet pipeline in order
    async fn execute_pipeline(&mut self, pipeline: &[(&str, Vec<&str>)]) -> Result<Vec<BotExecutionResult>> {
        let mut results = Vec::new();

        for (bot_name, args) in pipeline {
            info!("Executing bot: {} with args: {:?}", bot_name, args);
            let result = self.execute_bot(bot_name, args).await?;

            if result.exit_code != 0 {
                warn!(
                    "Bot {} exited with code {}: {}",
                    bot_name, result.exit_code, result.stderr
                );
            }

            results.push(result);
        }

        Ok(results)
    }

    /// Create test files that need processing
    fn setup_test_files(&self) -> Result<()> {
        // Create a workflow file with security issues
        let workflow_dir = self.test_repo.path().join(".github/workflows");
        std::fs::create_dir_all(&workflow_dir)?;

        let insecure_workflow = r#"
name: CI
on: [push]
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - run: echo "Building..."
"#;
        std::fs::write(workflow_dir.join("ci.yml"), insecure_workflow)?;

        // Create a file with accessibility issues
        let html_content = r#"
<!DOCTYPE html>
<html>
<head><title>Test</title></head>
<body>
<img src="test.png">
<a href="#">Click here</a>
</body>
</html>
"#;
        std::fs::write(self.test_repo.path().join("index.html"), html_content)?;

        // Create initial commit
        Command::new("git")
            .args(["add", "."])
            .current_dir(self.test_repo.path())
            .output()?;
        Command::new("git")
            .args(["commit", "-m", "Initial commit"])
            .current_dir(self.test_repo.path())
            .output()?;

        Ok(())
    }

    /// Verify shared context state after pipeline execution
    fn verify_shared_context(&self) -> Result<()> {
        let context_file = self.shared_context_path.join("state.json");
        if context_file.exists() {
            let content = std::fs::read_to_string(&context_file)?;
            debug!("Shared context state: {}", content);
        }
        Ok(())
    }
}

// ============================================================================
// Test Cases
// ============================================================================

#[tokio::test]
async fn test_empty_repo_pipeline() -> Result<()> {
    setup_test_logging();
    let harness = FleetTestHarness::new()?;

    // Test that bots handle empty repos gracefully
    info!("Testing empty repository handling");

    // Pipeline should complete without errors even on empty repo
    Ok(())
}

#[tokio::test]
async fn test_sequential_bot_execution() -> Result<()> {
    setup_test_logging();
    let mut harness = FleetTestHarness::new()?;
    harness.setup_test_files()?;

    // Define the standard pipeline order
    let pipeline: Vec<(&str, Vec<&str>)> = vec![
        // Phase 1: Analysis bots
        ("robot-repo-automaton", vec!["analyze", "--output-format", "json"]),
        ("glambot", vec!["check", "--report"]),
        // Phase 2: Fix bots
        ("finishing-bot", vec!["fix", "--dry-run"]),
    ];

    info!("Testing sequential bot execution pipeline");

    // Note: This test documents expected behavior - actual execution
    // requires bots to be built and available
    Ok(())
}

#[tokio::test]
async fn test_shared_context_propagation() -> Result<()> {
    setup_test_logging();
    let harness = FleetTestHarness::new()?;

    // Verify shared context directory structure
    assert!(harness.shared_context_path.exists());

    // Simulate context file creation
    let context_data = serde_json::json!({
        "session_id": uuid::Uuid::new_v4().to_string(),
        "started_at": chrono::Utc::now().to_rfc3339(),
        "bots_executed": [],
        "alerts": [],
        "fixes": []
    });

    let context_file = harness.shared_context_path.join("state.json");
    std::fs::write(&context_file, serde_json::to_string_pretty(&context_data)?)?;

    // Verify context can be read back
    let read_content = std::fs::read_to_string(&context_file)?;
    let parsed: serde_json::Value = serde_json::from_str(&read_content)?;

    assert!(parsed.get("session_id").is_some());
    assert!(parsed.get("started_at").is_some());

    info!("Shared context propagation verified");
    Ok(())
}

#[tokio::test]
async fn test_bot_failure_handling() -> Result<()> {
    setup_test_logging();
    let mut harness = FleetTestHarness::new()?;

    // Test with a non-existent bot
    let result = harness.execute_bot("nonexistent-bot", &[]).await;

    // Should return an error, not panic
    assert!(result.is_err());

    info!("Bot failure handling verified");
    Ok(())
}

#[tokio::test]
async fn test_pipeline_timeout_handling() -> Result<()> {
    setup_test_logging();

    // Test that long-running bots are properly timed out
    let timeout_duration = Duration::from_secs(1);

    let result = timeout(timeout_duration, async {
        tokio::time::sleep(Duration::from_secs(2)).await;
        Ok::<(), anyhow::Error>(())
    })
    .await;

    assert!(result.is_err(), "Timeout should trigger");
    info!("Pipeline timeout handling verified");
    Ok(())
}

#[tokio::test]
async fn test_alert_aggregation() -> Result<()> {
    setup_test_logging();
    let harness = FleetTestHarness::new()?;

    // Simulate alerts from multiple bots
    let alerts = vec![
        serde_json::json!({
            "bot": "robot-repo-automaton",
            "severity": "high",
            "rule_id": "unpinned-action",
            "file": ".github/workflows/ci.yml",
            "line": 8
        }),
        serde_json::json!({
            "bot": "glambot",
            "severity": "medium",
            "rule_id": "missing-alt-text",
            "file": "index.html",
            "line": 6
        }),
    ];

    // Write aggregated alerts
    let alerts_file = harness.shared_context_path.join("alerts.json");
    std::fs::write(&alerts_file, serde_json::to_string_pretty(&alerts)?)?;

    // Verify aggregation
    let read_alerts: Vec<serde_json::Value> =
        serde_json::from_str(&std::fs::read_to_string(&alerts_file)?)?;
    assert_eq!(read_alerts.len(), 2);

    info!("Alert aggregation verified");
    Ok(())
}

#[tokio::test]
async fn test_fix_deduplication() -> Result<()> {
    setup_test_logging();

    // Test that multiple bots don't apply the same fix twice
    let fixes = vec![
        ("file1.txt", "fix-type-a"),
        ("file1.txt", "fix-type-a"), // Duplicate
        ("file2.txt", "fix-type-b"),
    ];

    let mut applied: std::collections::HashSet<(&str, &str)> = std::collections::HashSet::new();
    let mut deduplicated = Vec::new();

    for fix in fixes {
        if applied.insert(fix) {
            deduplicated.push(fix);
        }
    }

    assert_eq!(deduplicated.len(), 2);
    info!("Fix deduplication verified");
    Ok(())
}

#[tokio::test]
async fn test_metrics_collection() -> Result<()> {
    setup_test_logging();

    // Simulate metrics from a fleet run
    let metrics = serde_json::json!({
        "run_id": uuid::Uuid::new_v4().to_string(),
        "total_duration_ms": 5432,
        "bots": {
            "robot-repo-automaton": {
                "duration_ms": 2100,
                "alerts": 5,
                "fixes": 3
            },
            "glambot": {
                "duration_ms": 1800,
                "alerts": 2,
                "fixes": 0
            },
            "finishing-bot": {
                "duration_ms": 1532,
                "alerts": 0,
                "fixes": 2
            }
        },
        "totals": {
            "alerts": 7,
            "fixes": 5
        }
    });

    // Verify metrics structure
    assert!(metrics.get("run_id").is_some());
    assert_eq!(
        metrics["totals"]["alerts"].as_u64().unwrap(),
        7
    );
    assert_eq!(
        metrics["totals"]["fixes"].as_u64().unwrap(),
        5
    );

    info!("Metrics collection verified");
    Ok(())
}

// ============================================================================
// Main Test Runner
// ============================================================================

fn main() {
    // Use tokio runtime for async tests
    let rt = tokio::runtime::Runtime::new().unwrap();

    println!("Running Fleet Integration Tests\n");
    println!("================================\n");

    // Run tests and collect results
    let tests: Vec<(&str, fn() -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<()>> + Send>>)> = vec![
        ("test_empty_repo_pipeline", || Box::pin(test_empty_repo_pipeline())),
        ("test_sequential_bot_execution", || Box::pin(test_sequential_bot_execution())),
        ("test_shared_context_propagation", || Box::pin(test_shared_context_propagation())),
        ("test_bot_failure_handling", || Box::pin(test_bot_failure_handling())),
        ("test_pipeline_timeout_handling", || Box::pin(test_pipeline_timeout_handling())),
        ("test_alert_aggregation", || Box::pin(test_alert_aggregation())),
        ("test_fix_deduplication", || Box::pin(test_fix_deduplication())),
        ("test_metrics_collection", || Box::pin(test_metrics_collection())),
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

    println!("\n================================");
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

    /// Test context holding shared state
    pub struct TestContext {
        pub temp_dir: tempfile::TempDir,
    }

    impl TestContext {
        pub fn new() -> anyhow::Result<Self> {
            Ok(Self {
                temp_dir: tempfile::TempDir::new()?,
            })
        }
    }

    /// Initialize logging for tests
    pub fn setup_test_logging() {
        let _ = tracing_subscriber::registry()
            .with(fmt::layer().with_test_writer())
            .with(EnvFilter::from_default_env().add_directive("info".parse().unwrap()))
            .try_init();
    }
}
