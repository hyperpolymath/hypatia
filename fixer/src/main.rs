// SPDX-License-Identifier: AGPL-3.0-or-later
//! cicd-fixer CLI - Programmatic CI/CD workflow fixer
//!
//! Usage:
//!   cicd-fixer scan <repo-path>        Scan a repo for CI/CD issues
//!   cicd-fixer fix <repo-path>         Fix auto-fixable issues
//!   cicd-fixer fix <repo-path> --dry-run  Preview fixes without applying
//!   cicd-fixer batch <repos-dir>       Scan/fix multiple repos

use cicd_fixer::{CicdFixer, IssueSeverity};
use clap::{Parser, Subcommand};
use std::path::PathBuf;
use tracing::{error, info, warn};
use tracing_subscriber::EnvFilter;
use walkdir::WalkDir;

#[derive(Parser)]
#[command(name = "cicd-fixer")]
#[command(about = "Programmatic CI/CD workflow fixer for gitbot-fleet")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Scan a repository for CI/CD issues
    Scan {
        /// Path to the repository
        repo_path: PathBuf,
        /// Output format (text, json)
        #[arg(short, long, default_value = "text")]
        format: String,
    },
    /// Fix auto-fixable issues in a repository
    Fix {
        /// Path to the repository
        repo_path: PathBuf,
        /// Preview fixes without applying
        #[arg(long)]
        dry_run: bool,
    },
    /// Scan/fix multiple repositories
    Batch {
        /// Directory containing repositories
        repos_dir: PathBuf,
        /// Fix issues (default: scan only)
        #[arg(long)]
        fix: bool,
        /// Preview fixes without applying
        #[arg(long)]
        dry_run: bool,
        /// Maximum repos to process
        #[arg(long, default_value = "100")]
        limit: usize,
    },
    /// List known SHA pins
    Pins,
    /// List error patterns
    Catalog,
}

fn main() {
    // Initialize logging
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env().add_directive("cicd_fixer=info".parse().unwrap()))
        .init();

    let cli = Cli::parse();
    let fixer = CicdFixer::new();

    match cli.command {
        Commands::Scan { repo_path, format } => {
            scan_repo(&fixer, &repo_path, &format);
        }
        Commands::Fix { repo_path, dry_run } => {
            fix_repo(&fixer, &repo_path, dry_run);
        }
        Commands::Batch { repos_dir, fix, dry_run, limit } => {
            batch_process(&fixer, &repos_dir, fix, dry_run, limit);
        }
        Commands::Pins => {
            list_pins(&fixer);
        }
        Commands::Catalog => {
            list_catalog(&fixer);
        }
    }
}

fn scan_repo(fixer: &CicdFixer, repo_path: &PathBuf, format: &str) {
    info!("Scanning repository: {}", repo_path.display());

    match fixer.scan_repo(repo_path) {
        Ok(result) => {
            if format == "json" {
                // JSON output for programmatic use
                let json = serde_json::json!({
                    "repo_path": result.repo_path,
                    "workflows_scanned": result.workflows_scanned,
                    "issues_count": result.issues.len(),
                    "auto_fixable_count": result.auto_fixable_count,
                    "issues": result.issues.iter().map(|i| serde_json::json!({
                        "id": i.id,
                        "severity": format!("{:?}", i.severity),
                        "file_path": i.file_path,
                        "line_number": i.line_number,
                        "description": i.description,
                        "fix_suggestion": i.fix_suggestion,
                        "auto_fixable": i.auto_fixable,
                    })).collect::<Vec<_>>()
                });
                println!("{}", serde_json::to_string_pretty(&json).unwrap());
            } else {
                // Human-readable output
                println!("\n{}", result.summary());
                println!();

                for issue in &result.issues {
                    let severity_icon = match issue.severity {
                        IssueSeverity::Critical => "🔴",
                        IssueSeverity::High => "🟠",
                        IssueSeverity::Medium => "🟡",
                        IssueSeverity::Low => "🔵",
                        IssueSeverity::Info => "⚪",
                    };

                    let fixable = if issue.auto_fixable { "✓" } else { "✗" };

                    println!(
                        "{} [{}] {} {}",
                        severity_icon,
                        fixable,
                        issue.id,
                        issue.file_path
                    );
                    if let Some(line) = issue.line_number {
                        println!("   Line {}: {}", line, issue.description);
                    } else {
                        println!("   {}", issue.description);
                    }
                    println!("   Fix: {}", issue.fix_suggestion);
                    println!();
                }
            }
        }
        Err(e) => {
            error!("Scan failed: {}", e);
        }
    }
}

fn fix_repo(fixer: &CicdFixer, repo_path: &PathBuf, dry_run: bool) {
    let mode = if dry_run { "(dry-run)" } else { "" };
    info!("Fixing repository: {} {}", repo_path.display(), mode);

    match fixer.fix_repo(repo_path, dry_run) {
        Ok(results) => {
            let applied = results.iter().filter(|r| r.applied).count();
            let success = results.iter().filter(|r| r.success).count();

            println!("\nFix Results:");
            for result in &results {
                let status = if result.applied {
                    "✓ Applied"
                } else if result.success {
                    "○ Would apply"
                } else {
                    "✗ Failed"
                };
                println!("  {} [{}] {}", status, result.issue_id, result.file_path);
                println!("    {}", result.message);
            }

            println!("\nSummary: {} fixes {}, {} successful",
                applied,
                if dry_run { "would be applied" } else { "applied" },
                success
            );
        }
        Err(e) => {
            error!("Fix failed: {}", e);
        }
    }
}

fn batch_process(fixer: &CicdFixer, repos_dir: &PathBuf, fix: bool, dry_run: bool, limit: usize) {
    info!("Batch processing: {}", repos_dir.display());

    let mut processed = 0;
    let mut total_issues = 0;
    let mut total_fixed = 0;

    for entry in WalkDir::new(repos_dir)
        .min_depth(1)
        .max_depth(1)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_dir())
        .take(limit)
    {
        let repo_path = entry.path();
        let git_dir = repo_path.join(".git");
        let workflows_dir = repo_path.join(".github/workflows");

        // Only process git repos with workflows
        if !git_dir.exists() || !workflows_dir.exists() {
            continue;
        }

        processed += 1;
        let repo_name = repo_path.file_name().unwrap().to_string_lossy();

        match fixer.scan_repo(repo_path) {
            Ok(result) => {
                total_issues += result.issues.len();

                if result.issues.is_empty() {
                    println!("✓ {} - No issues", repo_name);
                } else if fix {
                    match fixer.fix_repo(repo_path, dry_run) {
                        Ok(fix_results) => {
                            let fixed = fix_results.iter().filter(|r| r.success).count();
                            total_fixed += fixed;
                            println!("⚡ {} - {} issues, {} fixed", repo_name, result.issues.len(), fixed);
                        }
                        Err(e) => {
                            warn!("Fix failed for {}: {}", repo_name, e);
                        }
                    }
                } else {
                    println!("⚠ {} - {} issues ({} auto-fixable)",
                        repo_name, result.issues.len(), result.auto_fixable_count);
                }
            }
            Err(e) => {
                warn!("Scan failed for {}: {}", repo_name, e);
            }
        }
    }

    println!("\n=== Batch Summary ===");
    println!("Repos processed: {}", processed);
    println!("Total issues found: {}", total_issues);
    if fix {
        println!("Total issues fixed: {}", total_fixed);
    }
}

fn list_pins(fixer: &CicdFixer) {
    println!("Known SHA Pins:");
    println!("==============\n");

    for action in fixer.sha_pins.all_actions() {
        if let Some((sha, version)) = fixer.sha_pins.get_pin(action) {
            println!("{}@{} # {}", action, sha, version);
        }
    }
}

fn list_catalog(fixer: &CicdFixer) {
    println!("Error Catalog:");
    println!("==============\n");

    println!("Auto-fixable patterns:");
    for pattern in fixer.catalog.auto_fixable() {
        println!("  [{:?}] {} - {}", pattern.severity, pattern.id, pattern.description);
        println!("         Fix: {}", pattern.fix_pattern);
    }
}
