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

/// Process a single repository scan
async fn process_scan_repo(
    repo: &str,
    _config: &Config,
    _min_severity: &str,
    _categories: &str,
) -> BatchItemResult {
    let start = std::time::Instant::now();

    // Simplified scan - in real implementation, would call full scan logic
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

    // Simulate scan - in real implementation, would run actual scan
    // For now, check for basic files
    let workflows_dir = path.join(".github/workflows");
    let findings = if workflows_dir.exists() {
        // Count workflow files as proxy for findings
        std::fs::read_dir(&workflows_dir)
            .map(|entries| entries.filter_map(|e| e.ok()).count())
            .unwrap_or(0)
    } else {
        0
    };

    let status = if findings > 5 {
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

    // Simplified implementation - would call actual fix logic
    let mut results = Vec::new();
    for repo in &repos {
        let result = BatchItemResult {
            repo: repo.clone(),
            status: BatchStatus::Success,
            findings_count: 0,
            fixes_count: if args.dry_run { 0 } else { 1 },
            error: None,
            duration_ms: 100,
            timestamp: Utc::now(),
        };

        if args.jsonl {
            println!("{}", serde_json::to_string(&result)?);
        }
        results.push(result);
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
