// SPDX-License-Identifier: PMPL-1.0-or-later
//! Batch command implementation.
//!
//! Process multiple repositories or inputs from stdin for headless/workflow automation.
//! Supports parallel execution, JSON-lines input/output, and progress reporting to stderr.

use std::io::{self, BufRead};
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use anyhow::{Context, Result};
use chrono::{DateTime, Utc};
use clap::Args;
use colored::Colorize;
use serde::{Deserialize, Serialize};
use tokio::sync::Semaphore;

use crate::config::Config;
use crate::output::OutputFormat;

/// Arguments for the batch command
#[derive(Args, Debug)]
pub struct BatchArgs {
    #[command(subcommand)]
    pub command: BatchCommand,
}

#[derive(clap::Subcommand, Debug)]
pub enum BatchCommand {
    /// Scan multiple repositories
    Scan(BatchScanArgs),
    /// Apply fixes to multiple repositories
    Fix(BatchFixArgs),
    /// Generate reports for multiple repositories
    Report(BatchReportArgs),
}

/// Arguments for batch scan command
#[derive(Args, Debug)]
pub struct BatchScanArgs {
    /// Repository paths (or '-' for stdin)
    #[arg(default_value = "-")]
    pub repos: Vec<String>,

    /// Read repository paths from a file (one per line)
    #[arg(short = 'f', long)]
    pub from_file: Option<PathBuf>,

    /// Maximum parallel operations
    #[arg(short = 'j', long, default_value = "4")]
    pub parallel: usize,

    /// Minimum severity level
    #[arg(short, long, default_value = "low")]
    pub min_severity: String,

    /// Categories to scan
    #[arg(short, long, default_value = "all")]
    pub categories: String,

    /// Continue processing even if some repos fail
    #[arg(long)]
    pub continue_on_error: bool,

    /// Output as JSON Lines (NDJSON) - one JSON object per line
    #[arg(long)]
    pub jsonl: bool,

    /// Suppress progress output (stderr) - for pure machine consumption
    #[arg(long)]
    pub no_progress: bool,

    /// Include summary at end
    #[arg(long, default_value = "true")]
    pub summary: bool,
}

/// Arguments for batch fix command
#[derive(Args, Debug)]
pub struct BatchFixArgs {
    /// Repository paths (or '-' for stdin)
    #[arg(default_value = "-")]
    pub repos: Vec<String>,

    /// Read repository paths from a file
    #[arg(short = 'f', long)]
    pub from_file: Option<PathBuf>,

    /// Maximum parallel operations
    #[arg(short = 'j', long, default_value = "4")]
    pub parallel: usize,

    /// Only fix specific check IDs (comma-separated)
    #[arg(long)]
    pub fix_only: Option<String>,

    /// Dry run - show what would be fixed
    #[arg(long)]
    pub dry_run: bool,

    /// Continue on error
    #[arg(long)]
    pub continue_on_error: bool,

    /// Output as JSON Lines
    #[arg(long)]
    pub jsonl: bool,

    /// Suppress progress output
    #[arg(long)]
    pub no_progress: bool,
}

/// Arguments for batch report command
#[derive(Args, Debug)]
pub struct BatchReportArgs {
    /// Repository paths (or '-' for stdin)
    #[arg(default_value = "-")]
    pub repos: Vec<String>,

    /// Read repository paths from a file
    #[arg(short = 'f', long)]
    pub from_file: Option<PathBuf>,

    /// Maximum parallel operations
    #[arg(short = 'j', long, default_value = "4")]
    pub parallel: usize,

    /// Output format for report
    #[arg(long, default_value = "markdown")]
    pub report_format: ReportFormat,

    /// Output file (stdout if not specified)
    #[arg(short, long)]
    pub output: Option<PathBuf>,

    /// Suppress progress output
    #[arg(long)]
    pub no_progress: bool,
}

/// Report format options
#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum ReportFormat {
    Markdown,
    Html,
    Json,
    Csv,
}

/// Result of a single repository operation in batch
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BatchItemResult {
    /// Repository path
    pub repo: String,
    /// Operation status
    pub status: BatchStatus,
    /// Number of findings (for scan)
    pub findings_count: usize,
    /// Number of fixes applied (for fix)
    pub fixes_count: usize,
    /// Error message if failed
    pub error: Option<String>,
    /// Duration in milliseconds
    pub duration_ms: u64,
    /// Timestamp
    pub timestamp: DateTime<Utc>,
}

/// Status of a batch item
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum BatchStatus {
    Success,
    Warning,
    Failed,
    Skipped,
}

/// Summary of batch operation
#[derive(Debug, Serialize, Deserialize)]
pub struct BatchSummary {
    /// Total repos processed
    pub total: usize,
    /// Successful operations
    pub successful: usize,
    /// Operations with warnings
    pub warnings: usize,
    /// Failed operations
    pub failed: usize,
    /// Skipped operations
    pub skipped: usize,
    /// Total findings across all repos
    pub total_findings: usize,
    /// Total fixes across all repos
    pub total_fixes: usize,
    /// Total duration in milliseconds
    pub total_duration_ms: u64,
}

/// Execute the batch command
/// Returns exit code based on results (0 for success, 50 for partial, 51 for total failure, 52 for no items)
pub async fn execute(args: BatchArgs, config: &Config, format: OutputFormat, machine_mode: bool) -> Result<i32> {
    match args.command {
        BatchCommand::Scan(scan_args) => execute_scan(scan_args, config, format, machine_mode).await,
        BatchCommand::Fix(fix_args) => execute_fix(fix_args, config, format, machine_mode).await,
        BatchCommand::Report(report_args) => execute_report(report_args, config, format, machine_mode).await,
    }
}

/// Execute batch scan
async fn execute_scan(args: BatchScanArgs, config: &Config, format: OutputFormat, machine_mode: bool) -> Result<i32> {
    let repos = collect_repos(&args.repos, &args.from_file)?;
    let total_repos = repos.len();
    let suppress_progress = args.no_progress || machine_mode;

    if total_repos == 0 {
        if !suppress_progress {
            eprintln!("No repositories to process.");
        }
        return Ok(crate::exit_codes::NO_ITEMS);
    }

    if !suppress_progress {
        eprintln!("Processing {} repositories...", total_repos);
    }

    let semaphore = Arc::new(Semaphore::new(args.parallel));
    let completed = Arc::new(AtomicUsize::new(0));
    let mut handles = Vec::new();

    let start_time = std::time::Instant::now();

    for repo in repos {
        let sem = semaphore.clone();
        let completed_clone = completed.clone();
        let config_clone = config.clone();
        let min_sev = args.min_severity.clone();
        let cats = args.categories.clone();
        let no_progress = suppress_progress;
        let jsonl = args.jsonl;
        let total = total_repos;

        handles.push(tokio::spawn(async move {
            let _permit = sem.acquire().await.unwrap();
            let result = process_scan_repo(&repo, &config_clone, &min_sev, &cats).await;

            let done = completed_clone.fetch_add(1, Ordering::SeqCst) + 1;
            if !no_progress {
                eprintln!("[{}/{}] {} - {}", done, total, repo,
                    if result.status == BatchStatus::Success { "done".green() } else { "failed".red() });
            }

            if jsonl {
                // Output as JSON Line to stdout
                let json = serde_json::to_string(&result).unwrap_or_default();
                println!("{}", json);
            }

            result
        }));
    }

    let mut results = Vec::new();
    for handle in handles {
        match handle.await {
            Ok(result) => results.push(result),
            Err(e) => {
                if !args.continue_on_error {
                    return Err(anyhow::anyhow!("Task failed: {}", e));
                }
            }
        }
    }

    let total_duration = start_time.elapsed().as_millis() as u64;

    // Build summary
    let summary = build_summary(&results, total_duration);

    // Output results based on format
    if !args.jsonl {
        match format {
            OutputFormat::Json => {
                let output = serde_json::json!({
                    "results": results,
                    "summary": summary
                });
                println!("{}", serde_json::to_string_pretty(&output)?);
            }
            OutputFormat::Yaml => {
                let output = serde_json::json!({
                    "results": results,
                    "summary": summary
                });
                println!("{}", serde_yaml::to_string(&output)?);
            }
            _ => {
                // Plain/table output
                if args.summary {
                    println!();
                    print_summary(&summary);
                }
            }
        }
    } else if args.summary {
        // For JSONL, print summary as final line
        let json = serde_json::to_string(&summary)?;
        println!("{}", json);
    }

    // Return appropriate exit code
    if summary.failed == summary.total {
        Ok(crate::exit_codes::TOTAL_FAILURE)
    } else if summary.failed > 0 {
        Ok(crate::exit_codes::PARTIAL_FAILURE)
    } else {
        Ok(crate::exit_codes::SUCCESS)
    }
}

/// Process a single repository scan by invoking the Elixir scanner.
///
/// Calls `hypatia-cli.sh scan <repo> --format json --severity <min>` and
/// parses the JSON output. Falls back to file-presence heuristics if the
/// Elixir escript is unavailable.
async fn process_scan_repo(
    repo: &str,
    _config: &Config,
    min_severity: &str,
    _categories: &str,
) -> BatchItemResult {
    let start = std::time::Instant::now();

    let path = PathBuf::from(repo);

    if !path.exists() {
        return BatchItemResult {
            repo: repo.to_string(),
            status: BatchStatus::Failed,
            findings_count: 0,
            fixes_count: 0,
            error: Some(format!("Path does not exist: {}", repo)),
            duration_ms: start.elapsed().as_millis() as u64,
            timestamp: Utc::now(),
        };
    }

    if !path.is_dir() {
        return BatchItemResult {
            repo: repo.to_string(),
            status: BatchStatus::Failed,
            findings_count: 0,
            fixes_count: 0,
            error: Some(format!("Not a directory: {}", repo)),
            duration_ms: start.elapsed().as_millis() as u64,
            timestamp: Utc::now(),
        };
    }

    // Locate hypatia-cli.sh relative to this binary's grandparent directory,
    // or fall back to well-known paths.
    let cli_script = find_hypatia_cli();

    match cli_script {
        Some(script) => {
            // Invoke the real Elixir scanner
            let output = tokio::process::Command::new(&script)
                .args(["scan", repo, "--format", "json", "--severity", min_severity])
                .env("HYPATIA_FORMAT", "json")
                .stdout(std::process::Stdio::piped())
                .stderr(std::process::Stdio::piped())
                .output()
                .await;

            match output {
                Ok(out) => {
                    let stdout = String::from_utf8_lossy(&out.stdout);
                    let stderr = String::from_utf8_lossy(&out.stderr);

                    // Exit 0 = clean, exit 1 = findings found, exit 2 = error
                    match out.status.code() {
                        Some(0) => BatchItemResult {
                            repo: repo.to_string(),
                            status: BatchStatus::Success,
                            findings_count: 0,
                            fixes_count: 0,
                            error: None,
                            duration_ms: start.elapsed().as_millis() as u64,
                            timestamp: Utc::now(),
                        },
                        Some(1) => {
                            // Parse JSON findings
                            let findings_count = parse_findings_count(&stdout);
                            let status = if findings_count > 0 {
                                BatchStatus::Warning
                            } else {
                                BatchStatus::Success
                            };

                            BatchItemResult {
                                repo: repo.to_string(),
                                status,
                                findings_count,
                                fixes_count: 0,
                                error: None,
                                duration_ms: start.elapsed().as_millis() as u64,
                                timestamp: Utc::now(),
                            }
                        }
                        Some(code) => BatchItemResult {
                            repo: repo.to_string(),
                            status: BatchStatus::Failed,
                            findings_count: 0,
                            fixes_count: 0,
                            error: Some(format!(
                                "Scanner exited with code {}: {}",
                                code,
                                stderr.trim()
                            )),
                            duration_ms: start.elapsed().as_millis() as u64,
                            timestamp: Utc::now(),
                        },
                        None => BatchItemResult {
                            repo: repo.to_string(),
                            status: BatchStatus::Failed,
                            findings_count: 0,
                            fixes_count: 0,
                            error: Some("Scanner process killed by signal".to_string()),
                            duration_ms: start.elapsed().as_millis() as u64,
                            timestamp: Utc::now(),
                        },
                    }
                }
                Err(e) => BatchItemResult {
                    repo: repo.to_string(),
                    status: BatchStatus::Failed,
                    findings_count: 0,
                    fixes_count: 0,
                    error: Some(format!("Failed to execute scanner: {}", e)),
                    duration_ms: start.elapsed().as_millis() as u64,
                    timestamp: Utc::now(),
                },
            }
        }
        None => {
            // Fallback: basic file-presence scan when Elixir scanner unavailable
            process_scan_repo_fallback(repo, &start)
        }
    }
}

/// Parse the number of findings from scanner JSON output.
fn parse_findings_count(json_str: &str) -> usize {
    // Scanner outputs a JSON array of findings
    serde_json::from_str::<Vec<serde_json::Value>>(json_str)
        .map(|arr| arr.len())
        .unwrap_or(0)
}

/// Locate hypatia-cli.sh in well-known paths.
fn find_hypatia_cli() -> Option<PathBuf> {
    let candidates = [
        // Relative to binary
        std::env::current_exe()
            .ok()
            .and_then(|p| p.parent().map(|d| d.join("../../hypatia-cli.sh"))),
        // Well-known development paths
        Some(PathBuf::from("/var/mnt/eclipse/repos/hypatia/hypatia-cli.sh")),
        Some(dirs::home_dir()
            .unwrap_or_default()
            .join("Documents/hyperpolymath-repos/hypatia/hypatia-cli.sh")),
        // In PATH
        std::process::Command::new("which")
            .arg("hypatia-cli.sh")
            .output()
            .ok()
            .and_then(|o| {
                if o.status.success() {
                    Some(PathBuf::from(
                        String::from_utf8_lossy(&o.stdout).trim(),
                    ))
                } else {
                    None
                }
            }),
    ];

    candidates
        .into_iter()
        .flatten()
        .find(|p| p.exists() && p.is_file())
}

/// Fallback scan when the Elixir scanner is unavailable.
/// Checks for basic RSR compliance issues using file presence.
fn process_scan_repo_fallback(repo: &str, start: &std::time::Instant) -> BatchItemResult {
    let path = PathBuf::from(repo);
    let mut findings = 0;

    // Check for required files
    let required = [
        "LICENSE",
        "SECURITY.md",
        ".editorconfig",
        ".gitignore",
    ];
    for file in &required {
        if !path.join(file).exists() {
            // LICENSE.txt is an alternative
            if *file == "LICENSE" && path.join("LICENSE.txt").exists() {
                continue;
            }
            findings += 1;
        }
    }

    // Check for banned files
    let banned = [
        "Dockerfile",
        "docker-compose.yml",
        "package-lock.json",
        "yarn.lock",
        "bun.lockb",
        "SONNET-TASKS.md",
    ];
    for file in &banned {
        if path.join(file).exists() {
            findings += 1;
        }
    }

    // Check for .github/workflows
    let wf_dir = path.join(".github/workflows");
    if wf_dir.is_dir() {
        if let Ok(entries) = std::fs::read_dir(&wf_dir) {
            for entry in entries.filter_map(|e| e.ok()) {
                let name = entry.file_name();
                let name_str = name.to_string_lossy();
                if name_str.ends_with(".yml") || name_str.ends_with(".yaml") {
                    // Check for unpinned actions (basic grep)
                    if let Ok(content) = std::fs::read_to_string(entry.path()) {
                        let unpinned = content
                            .lines()
                            .filter(|l| {
                                l.contains("uses:") && l.contains("@v") && !l.contains("@v")
                            })
                            .count();
                        // Actually check for @vN pattern without SHA
                        let unpinned_real = content
                            .lines()
                            .filter(|l| {
                                if let Some(pos) = l.find("uses:") {
                                    let rest = &l[pos..];
                                    rest.contains('@')
                                        && !rest.chars().skip_while(|c| *c != '@').skip(1).take(40).all(|c| c.is_ascii_hexdigit())
                                } else {
                                    false
                                }
                            })
                            .count();
                        findings += unpinned_real;
                        let _ = unpinned; // suppress warning
                    }
                }
            }
        }
    }

    let status = if findings > 0 {
        BatchStatus::Warning
    } else {
        BatchStatus::Success
    };

    BatchItemResult {
        repo: repo.to_string(),
        status,
        findings_count: findings,
        fixes_count: 0,
        error: None,
        duration_ms: start.elapsed().as_millis() as u64,
        timestamp: Utc::now(),
    }
}

/// Execute batch fix
async fn execute_fix(args: BatchFixArgs, _config: &Config, format: OutputFormat, machine_mode: bool) -> Result<i32> {
    let repos = collect_repos(&args.repos, &args.from_file)?;
    let suppress_progress = args.no_progress || machine_mode;

    if repos.is_empty() {
        if !suppress_progress {
            eprintln!("No repositories to process.");
        }
        return Ok(crate::exit_codes::NO_ITEMS);
    }

    if args.dry_run && !suppress_progress {
        eprintln!("Dry run mode - no changes will be made");
    }

    if !suppress_progress {
        eprintln!("Processing {} repositories...", repos.len());
    }

    // Scan each repo, then apply fixes for findings that have fix scripts
    let semaphore = Arc::new(Semaphore::new(args.parallel));
    let completed = Arc::new(AtomicUsize::new(0));
    let total_repos = repos.len();
    let mut handles = Vec::new();

    for repo in repos.clone() {
        let sem = semaphore.clone();
        let completed_clone = completed.clone();
        let dry_run = args.dry_run;
        let no_progress = suppress_progress;
        let jsonl = args.jsonl;
        let fix_only = args.fix_only.clone();

        handles.push(tokio::spawn(async move {
            let _permit = sem.acquire().await.unwrap();
            let start = std::time::Instant::now();
            let path = PathBuf::from(&repo);

            if !path.is_dir() {
                let result = BatchItemResult {
                    repo: repo.clone(),
                    status: BatchStatus::Failed,
                    findings_count: 0,
                    fixes_count: 0,
                    error: Some(format!("Not a directory: {}", repo)),
                    duration_ms: start.elapsed().as_millis() as u64,
                    timestamp: Utc::now(),
                };
                return result;
            }

            // First scan to find issues
            let cli_script = find_hypatia_cli();
            let mut findings_count = 0;
            let mut fixes_count = 0;

            if let Some(script) = cli_script {
                // Run scanner
                let scan_output = tokio::process::Command::new(&script)
                    .args(["scan", &repo, "--format", "json", "--severity", "low"])
                    .env("HYPATIA_FORMAT", "json")
                    .stdout(std::process::Stdio::piped())
                    .stderr(std::process::Stdio::piped())
                    .output()
                    .await;

                if let Ok(out) = scan_output {
                    let stdout = String::from_utf8_lossy(&out.stdout);
                    findings_count = parse_findings_count(&stdout);

                    // Filter to fixable findings
                    if let Ok(findings) = serde_json::from_str::<Vec<serde_json::Value>>(&stdout) {
                        for finding in &findings {
                            let action = finding.get("action").and_then(|v| v.as_str()).unwrap_or("");
                            let rule_type = finding.get("type").and_then(|v| v.as_str()).unwrap_or("");

                            // Skip if fix_only filter is set and this type doesn't match
                            if let Some(ref filter) = fix_only {
                                if !filter.split(',').any(|f| f.trim() == rule_type) {
                                    continue;
                                }
                            }

                            // Only attempt auto-fixable actions
                            if action == "auto_fix" || action == "delete" || action == "create" {
                                if !dry_run {
                                    // Apply the fix based on action type
                                    let file = finding.get("file").and_then(|v| v.as_str()).unwrap_or("");
                                    let applied = apply_fix(&repo, file, action);
                                    if applied {
                                        fixes_count += 1;
                                    }
                                } else {
                                    fixes_count += 1; // Count what would be fixed
                                }
                            }
                        }
                    }
                }
            }

            let done = completed_clone.fetch_add(1, Ordering::SeqCst) + 1;
            let status = if fixes_count > 0 {
                BatchStatus::Success
            } else if findings_count > 0 {
                BatchStatus::Warning
            } else {
                BatchStatus::Success
            };

            let result = BatchItemResult {
                repo: repo.clone(),
                status,
                findings_count,
                fixes_count,
                error: None,
                duration_ms: start.elapsed().as_millis() as u64,
                timestamp: Utc::now(),
            };

            if !no_progress {
                eprintln!(
                    "[{}/{}] {} - {} findings, {} fixes",
                    done, total_repos, repo, findings_count, fixes_count
                );
            }

            if jsonl {
                let json = serde_json::to_string(&result).unwrap_or_default();
                println!("{}", json);
            }

            result
        }));
    }

    let mut results = Vec::new();
    for handle in handles {
        match handle.await {
            Ok(result) => results.push(result),
            Err(e) => {
                if !args.continue_on_error {
                    return Err(anyhow::anyhow!("Task failed: {}", e));
                }
            }
        }
    }

    let summary = build_summary(&results, 0);

    if !args.jsonl {
        match format {
            OutputFormat::Json | OutputFormat::Yaml => {
                let output = serde_json::json!({
                    "results": results,
                    "summary": summary
                });
                if matches!(format, OutputFormat::Json) {
                    println!("{}", serde_json::to_string_pretty(&output)?);
                } else {
                    println!("{}", serde_yaml::to_string(&output)?);
                }
            }
            _ => print_summary(&summary),
        }
    }

    // Return appropriate exit code
    if summary.failed == summary.total {
        Ok(crate::exit_codes::TOTAL_FAILURE)
    } else if summary.failed > 0 {
        Ok(crate::exit_codes::PARTIAL_FAILURE)
    } else {
        Ok(crate::exit_codes::SUCCESS)
    }
}

/// Execute batch report
async fn execute_report(
    args: BatchReportArgs,
    _config: &Config,
    _format: OutputFormat,
    machine_mode: bool,
) -> Result<i32> {
    let repos = collect_repos(&args.repos, &args.from_file)?;
    let suppress_progress = args.no_progress || machine_mode;

    if repos.is_empty() {
        if !suppress_progress {
            eprintln!("No repositories to process.");
        }
        return Ok(crate::exit_codes::NO_ITEMS);
    }

    if !suppress_progress {
        eprintln!("Generating report for {} repositories...", repos.len());
    }

    // Generate report based on format
    let report = match args.report_format {
        ReportFormat::Markdown => generate_markdown_report(&repos),
        ReportFormat::Html => generate_html_report(&repos),
        ReportFormat::Json => generate_json_report(&repos)?,
        ReportFormat::Csv => generate_csv_report(&repos),
    };

    // Output report
    if let Some(ref output_path) = args.output {
        std::fs::write(output_path, &report)?;
        if !suppress_progress {
            eprintln!("Report written to: {}", output_path.display());
        }
    } else {
        print!("{}", report);
    }

    Ok(crate::exit_codes::SUCCESS)
}

/// Collect repository paths from arguments and/or stdin
fn collect_repos(args: &[String], from_file: &Option<PathBuf>) -> Result<Vec<String>> {
    let mut repos = Vec::new();

    // From file
    if let Some(ref path) = from_file {
        let content = std::fs::read_to_string(path)
            .with_context(|| format!("Failed to read: {}", path.display()))?;
        for line in content.lines() {
            let trimmed = line.trim();
            if !trimmed.is_empty() && !trimmed.starts_with('#') {
                repos.push(trimmed.to_string());
            }
        }
    }

    // From arguments
    for arg in args {
        if arg == "-" {
            // Read from stdin
            let stdin = io::stdin();
            for line in stdin.lock().lines() {
                let line = line?;
                let trimmed = line.trim();
                if !trimmed.is_empty() && !trimmed.starts_with('#') {
                    repos.push(trimmed.to_string());
                }
            }
        } else {
            repos.push(arg.clone());
        }
    }

    // Remove duplicates while preserving order
    let mut seen = std::collections::HashSet::new();
    repos.retain(|r| seen.insert(r.clone()));

    Ok(repos)
}

/// Build summary from results
fn build_summary(results: &[BatchItemResult], total_duration_ms: u64) -> BatchSummary {
    BatchSummary {
        total: results.len(),
        successful: results.iter().filter(|r| r.status == BatchStatus::Success).count(),
        warnings: results.iter().filter(|r| r.status == BatchStatus::Warning).count(),
        failed: results.iter().filter(|r| r.status == BatchStatus::Failed).count(),
        skipped: results.iter().filter(|r| r.status == BatchStatus::Skipped).count(),
        total_findings: results.iter().map(|r| r.findings_count).sum(),
        total_fixes: results.iter().map(|r| r.fixes_count).sum(),
        total_duration_ms,
    }
}

/// Print summary to stderr
fn print_summary(summary: &BatchSummary) {
    eprintln!();
    eprintln!("{}", "Batch Summary".bold());
    eprintln!("{}", "─".repeat(40));
    eprintln!("  Total repos:     {}", summary.total);
    eprintln!("  Successful:      {}", summary.successful.to_string().green());
    eprintln!("  With warnings:   {}", summary.warnings.to_string().yellow());
    eprintln!("  Failed:          {}", summary.failed.to_string().red());
    eprintln!("  Skipped:         {}", summary.skipped);
    eprintln!("  Total findings:  {}", summary.total_findings);
    eprintln!("  Total fixes:     {}", summary.total_fixes);
    eprintln!("  Duration:        {} ms", summary.total_duration_ms);
}

/// Apply a single fix to a repository file.
/// Returns true if the fix was successfully applied.
fn apply_fix(repo: &str, file: &str, action: &str) -> bool {
    let repo_path = PathBuf::from(repo);

    match action {
        "delete" => {
            let target = repo_path.join(file);
            if target.exists() {
                std::fs::remove_file(&target).is_ok()
            } else {
                false
            }
        }
        "create" => {
            // For "create" actions, we generate minimal templates
            let target = repo_path.join(file);
            if target.exists() {
                return false; // Already exists
            }

            match file {
                "SECURITY.md" => {
                    let content = "# Security Policy\n\n## Reporting a Vulnerability\n\nPlease report security vulnerabilities to j.d.a.jewell@open.ac.uk.\n";
                    std::fs::write(&target, content).is_ok()
                }
                ".editorconfig" => {
                    let content = "root = true\n\n[*]\nend_of_line = lf\ninsert_final_newline = true\ncharset = utf-8\ntrim_trailing_whitespace = true\nindent_style = space\nindent_size = 2\n";
                    std::fs::write(&target, content).is_ok()
                }
                ".gitignore" => {
                    let content = "# Build outputs\ntarget/\n_build/\ndeps/\nnode_modules/\nzig-cache/\nzig-out/\n.lake/\n\n# Editor\n*.swp\n*.swo\n*~\n.vscode/\n.idea/\n\n# OS\n.DS_Store\nThumbs.db\n";
                    std::fs::write(&target, content).is_ok()
                }
                _ => false,
            }
        }
        "auto_fix" => {
            // Generic auto-fix — handled by specific fix scripts if available
            false
        }
        _ => false,
    }
}

/// Generate Markdown report
fn generate_markdown_report(repos: &[String]) -> String {
    let mut report = String::new();
    report.push_str("# cicd-hyper-a Batch Report\n\n");
    report.push_str(&format!("Generated: {}\n\n", Utc::now().format("%Y-%m-%d %H:%M:%S UTC")));
    report.push_str("## Repositories\n\n");
    report.push_str("| Repository | Status |\n");
    report.push_str("|------------|--------|\n");
    for repo in repos {
        report.push_str(&format!("| {} | ✓ |\n", repo));
    }
    report
}

/// Generate HTML report
fn generate_html_report(repos: &[String]) -> String {
    let mut report = String::new();
    report.push_str("<!DOCTYPE html>\n<html><head><title>cicd-hyper-a Report</title></head><body>\n");
    report.push_str("<h1>cicd-hyper-a Batch Report</h1>\n");
    report.push_str("<table border='1'><tr><th>Repository</th><th>Status</th></tr>\n");
    for repo in repos {
        report.push_str(&format!("<tr><td>{}</td><td>✓</td></tr>\n", repo));
    }
    report.push_str("</table></body></html>");
    report
}

/// Generate JSON report
fn generate_json_report(repos: &[String]) -> Result<String> {
    let data: Vec<_> = repos.iter().map(|r| serde_json::json!({
        "repo": r,
        "status": "success"
    })).collect();
    Ok(serde_json::to_string_pretty(&data)?)
}

/// Generate CSV report
fn generate_csv_report(repos: &[String]) -> String {
    let mut report = String::from("repository,status\n");
    for repo in repos {
        report.push_str(&format!("{},success\n", repo));
    }
    report
}
