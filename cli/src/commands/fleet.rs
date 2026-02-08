// SPDX-License-Identifier: PLMP-1.0-or-later
//! Fleet command implementation.
//!
//! Runs bot fleets on a repository in dependency order.
//! Coordinates multiple bots to analyze and fix issues.

use std::collections::HashMap;
use std::path::PathBuf;
use std::time::Duration;

use anyhow::Result;
use chrono::{DateTime, Utc};
use clap::Args;
use colored::Colorize;
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use serde::{Deserialize, Serialize};
use tokio::time::sleep;
use tracing::info;

use super::validate_git_repository;
use crate::config::Config;
use crate::output::{OutputFormat, Outputter};

/// Arguments for the fleet command
#[derive(Args, Debug)]
pub struct FleetArgs {
    #[command(subcommand)]
    pub command: FleetCommand,
}

#[derive(clap::Subcommand, Debug)]
pub enum FleetCommand {
    /// Run all configured bots on a repository
    Run(FleetRunArgs),
    /// List available bots in the fleet
    List,
    /// Show status of a running fleet operation
    Status {
        /// Operation ID to check
        operation_id: String,
    },
    /// Stop a running fleet operation
    Stop {
        /// Operation ID to stop
        operation_id: String,
    },
}

/// Arguments for fleet run command
#[derive(Args, Debug)]
pub struct FleetRunArgs {
    /// Path to the repository
    #[arg(default_value = ".")]
    pub repo_path: PathBuf,

    /// Run only specific bots (comma-separated)
    #[arg(short, long)]
    pub bots: Option<String>,

    /// Exclude specific bots (comma-separated)
    #[arg(short, long)]
    pub exclude: Option<String>,

    /// Dry run - show what would be done without executing
    #[arg(long)]
    pub dry_run: bool,

    /// Run bots in parallel where possible
    #[arg(long)]
    pub parallel: bool,

    /// Continue even if a bot fails
    #[arg(long)]
    pub continue_on_error: bool,

    /// Maximum time to wait for fleet completion (seconds)
    #[arg(long, default_value = "600")]
    pub timeout: u64,

    /// Apply fixes automatically
    #[arg(long)]
    pub auto_fix: bool,
}

/// Bot definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Bot {
    /// Unique identifier
    pub id: String,
    /// Human-readable name
    pub name: String,
    /// Description of what the bot does
    pub description: String,
    /// Bot category
    pub category: BotCategory,
    /// Dependencies - other bots that must run first
    pub dependencies: Vec<String>,
    /// Checks performed by this bot
    pub checks: Vec<String>,
    /// Whether this bot can apply fixes
    pub can_fix: bool,
    /// Estimated runtime in seconds
    pub estimated_runtime: u32,
}

/// Bot category
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum BotCategory {
    /// Security-focused bot
    Security,
    /// Quality assurance bot
    Quality,
    /// Documentation bot
    Documentation,
    /// Compliance and policy bot
    Compliance,
    /// Performance analysis bot
    Performance,
    /// Release preparation bot
    Release,
}

impl std::fmt::Display for BotCategory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BotCategory::Security => write!(f, "security"),
            BotCategory::Quality => write!(f, "quality"),
            BotCategory::Documentation => write!(f, "documentation"),
            BotCategory::Compliance => write!(f, "compliance"),
            BotCategory::Performance => write!(f, "performance"),
            BotCategory::Release => write!(f, "release"),
        }
    }
}

/// Status of a bot execution
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum BotStatus {
    Pending,
    Running,
    Success,
    Failed,
    Skipped,
}

impl std::fmt::Display for BotStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BotStatus::Pending => write!(f, "pending"),
            BotStatus::Running => write!(f, "running"),
            BotStatus::Success => write!(f, "success"),
            BotStatus::Failed => write!(f, "failed"),
            BotStatus::Skipped => write!(f, "skipped"),
        }
    }
}

/// Result of a single bot execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BotResult {
    pub bot_id: String,
    pub status: BotStatus,
    pub started_at: Option<DateTime<Utc>>,
    pub completed_at: Option<DateTime<Utc>>,
    pub duration_ms: Option<u64>,
    pub findings_count: usize,
    pub fixes_applied: usize,
    pub error_message: Option<String>,
    pub output: Option<String>,
}

/// Results of a fleet run
#[derive(Debug, Serialize, Deserialize)]
pub struct FleetResults {
    pub operation_id: String,
    pub repository_path: PathBuf,
    pub started_at: DateTime<Utc>,
    pub completed_at: DateTime<Utc>,
    pub total_duration_ms: u64,
    pub bot_results: Vec<BotResult>,
    pub summary: FleetSummary,
}

/// Summary of fleet execution
#[derive(Debug, Serialize, Deserialize)]
pub struct FleetSummary {
    pub total_bots: usize,
    pub successful: usize,
    pub failed: usize,
    pub skipped: usize,
    pub total_findings: usize,
    pub total_fixes: usize,
}

/// Execute the fleet command
pub async fn execute(args: FleetArgs, config: &Config, format: OutputFormat) -> Result<()> {
    match args.command {
        FleetCommand::Run(run_args) => execute_run(run_args, config, format).await,
        FleetCommand::List => execute_list(config, format).await,
        FleetCommand::Status { operation_id } => execute_status(&operation_id, config, format).await,
        FleetCommand::Stop { operation_id } => execute_stop(&operation_id, config, format).await,
    }
}

async fn execute_run(args: FleetRunArgs, _config: &Config, format: OutputFormat) -> Result<()> {
    let outputter = Outputter::new(format);
    let start_time = std::time::Instant::now();
    let started_at = Utc::now();

    // Validate repository
    let repo = validate_git_repository(&args.repo_path)?;
    let repo_path = repo.workdir().unwrap_or(repo.path()).to_path_buf();

    info!("Running fleet on repository: {}", repo_path.display());

    // Get configured bots
    let mut bots = get_available_bots();

    // Filter bots based on arguments
    if let Some(ref include) = args.bots {
        let include_set: Vec<&str> = include.split(',').map(|s| s.trim()).collect();
        bots.retain(|b| include_set.contains(&b.id.as_str()));
    }

    if let Some(ref exclude) = args.exclude {
        let exclude_set: Vec<&str> = exclude.split(',').map(|s| s.trim()).collect();
        bots.retain(|b| !exclude_set.contains(&b.id.as_str()));
    }

    if bots.is_empty() {
        anyhow::bail!("No bots to run after filtering");
    }

    // Sort bots by dependency order
    let ordered_bots = topological_sort(&bots)?;

    // Generate operation ID
    let operation_id = uuid::Uuid::new_v4().to_string();

    if args.dry_run {
        outputter.info("Dry run mode - no changes will be made")?;
        println!();
        println!("{}", "Execution order:".bold());
        for (i, bot) in ordered_bots.iter().enumerate() {
            println!(
                "  {}. {} ({}) - {}",
                i + 1,
                bot.name.bold(),
                bot.id.dimmed(),
                bot.description
            );
        }
        return Ok(());
    }

    // Create progress display
    let multi_progress = MultiProgress::new();
    let overall_pb = multi_progress.add(ProgressBar::new(ordered_bots.len() as u64));
    overall_pb.set_style(
        ProgressStyle::default_bar()
            .template("{spinner:.green} [{bar:40.cyan/blue}] {pos}/{len} bots ({msg})")
            .unwrap()
            .progress_chars("#>-"),
    );
    overall_pb.set_message("starting...");

    let mut bot_results = Vec::new();

    // Execute bots
    for (idx, bot) in ordered_bots.iter().enumerate() {
        overall_pb.set_message(format!("running {}", bot.id));

        let bot_pb = multi_progress.add(ProgressBar::new_spinner());
        bot_pb.set_style(
            ProgressStyle::default_spinner()
                .template("  {spinner:.blue} {msg}")
                .unwrap(),
        );
        bot_pb.set_message(format!("{}: starting...", bot.name));

        let bot_start = std::time::Instant::now();
        let bot_started_at = Utc::now();

        // Simulate bot execution (in real implementation, this would call the actual bot)
        let result = run_bot(bot, &repo_path, args.auto_fix, &bot_pb).await;

        let bot_duration = bot_start.elapsed().as_millis() as u64;
        let bot_completed_at = Utc::now();

        let bot_result = match result {
            Ok((findings, fixes)) => {
                bot_pb.finish_with_message(format!(
                    "{}: {} ({} findings, {} fixes)",
                    bot.name,
                    "done".green(),
                    findings,
                    fixes
                ));
                BotResult {
                    bot_id: bot.id.clone(),
                    status: BotStatus::Success,
                    started_at: Some(bot_started_at),
                    completed_at: Some(bot_completed_at),
                    duration_ms: Some(bot_duration),
                    findings_count: findings,
                    fixes_applied: fixes,
                    error_message: None,
                    output: None,
                }
            }
            Err(e) => {
                bot_pb.finish_with_message(format!("{}: {} - {}", bot.name, "failed".red(), e));

                if !args.continue_on_error {
                    // Mark remaining bots as skipped
                    for remaining_bot in ordered_bots.iter().skip(idx + 1) {
                        bot_results.push(BotResult {
                            bot_id: remaining_bot.id.clone(),
                            status: BotStatus::Skipped,
                            started_at: None,
                            completed_at: None,
                            duration_ms: None,
                            findings_count: 0,
                            fixes_applied: 0,
                            error_message: Some("Skipped due to previous bot failure".to_string()),
                            output: None,
                        });
                    }
                }

                BotResult {
                    bot_id: bot.id.clone(),
                    status: BotStatus::Failed,
                    started_at: Some(bot_started_at),
                    completed_at: Some(bot_completed_at),
                    duration_ms: Some(bot_duration),
                    findings_count: 0,
                    fixes_applied: 0,
                    error_message: Some(e.to_string()),
                    output: None,
                }
            }
        };

        let should_stop = bot_result.status == BotStatus::Failed && !args.continue_on_error;
        bot_results.push(bot_result);

        overall_pb.inc(1);

        if should_stop {
            break;
        }
    }

    overall_pb.finish_with_message("complete");

    // Build summary
    let summary = FleetSummary {
        total_bots: bot_results.len(),
        successful: bot_results.iter().filter(|r| matches!(r.status, BotStatus::Success)).count(),
        failed: bot_results.iter().filter(|r| matches!(r.status, BotStatus::Failed)).count(),
        skipped: bot_results.iter().filter(|r| matches!(r.status, BotStatus::Skipped)).count(),
        total_findings: bot_results.iter().map(|r| r.findings_count).sum(),
        total_fixes: bot_results.iter().map(|r| r.fixes_applied).sum(),
    };

    let completed_at = Utc::now();
    let total_duration = start_time.elapsed().as_millis() as u64;

    let results = FleetResults {
        operation_id: operation_id.clone(),
        repository_path: repo_path,
        started_at,
        completed_at,
        total_duration_ms: total_duration,
        bot_results,
        summary,
    };

    // Output results
    println!();
    match format {
        OutputFormat::Json | OutputFormat::Yaml => {
            outputter.output(&results)?;
        }
        _ => {
            print_fleet_results(&results)?;
        }
    }

    // Exit with error if any bots failed
    if results.summary.failed > 0 {
        std::process::exit(1);
    }

    Ok(())
}

async fn execute_list(_config: &Config, format: OutputFormat) -> Result<()> {
    let outputter = Outputter::new(format);
    let bots = get_available_bots();

    match format {
        OutputFormat::Json | OutputFormat::Yaml => {
            outputter.output(&bots)?;
        }
        OutputFormat::Table => {
            use tabled::{settings::Style, Table, Tabled};

            #[derive(Tabled)]
            struct BotRow {
                #[tabled(rename = "ID")]
                id: String,
                #[tabled(rename = "Name")]
                name: String,
                #[tabled(rename = "Category")]
                category: String,
                #[tabled(rename = "Can Fix")]
                can_fix: String,
                #[tabled(rename = "Est. Time")]
                est_time: String,
            }

            let rows: Vec<BotRow> = bots
                .iter()
                .map(|b| BotRow {
                    id: b.id.clone(),
                    name: b.name.clone(),
                    category: b.category.to_string(),
                    can_fix: if b.can_fix { "Yes" } else { "No" }.to_string(),
                    est_time: format!("{}s", b.estimated_runtime),
                })
                .collect();

            let table = Table::new(rows).with(Style::rounded()).to_string();
            println!("{}", table);
        }
        OutputFormat::Plain => {
            println!("{}", "Available Bots:".bold());
            println!("{}", "─".repeat(60));
            for bot in &bots {
                println!(
                    "{} ({}) - {}",
                    bot.name.bold(),
                    bot.id.dimmed(),
                    bot.category
                );
                println!("  {}", bot.description);
                if !bot.dependencies.is_empty() {
                    println!("  Dependencies: {}", bot.dependencies.join(", ").dimmed());
                }
                if bot.can_fix {
                    println!("  {}", "[can-fix]".green());
                }
                println!();
            }
        }
    }

    Ok(())
}

async fn execute_status(operation_id: &str, _config: &Config, format: OutputFormat) -> Result<()> {
    let outputter = Outputter::new(format);

    // In a real implementation, this would check a database or file for operation status
    outputter.warn(&format!(
        "Operation status lookup not yet implemented for: {}",
        operation_id
    ))?;

    Ok(())
}

async fn execute_stop(operation_id: &str, _config: &Config, format: OutputFormat) -> Result<()> {
    let outputter = Outputter::new(format);

    // In a real implementation, this would signal the operation to stop
    outputter.warn(&format!(
        "Operation stop not yet implemented for: {}",
        operation_id
    ))?;

    Ok(())
}

/// Get the list of available bots
fn get_available_bots() -> Vec<Bot> {
    vec![
        Bot {
            id: "robot-repo-automaton".to_string(),
            name: "Robot Repo Automaton".to_string(),
            description: "Automated workflow cleanup and security fixes".to_string(),
            category: BotCategory::Security,
            dependencies: vec![],
            checks: vec![
                "unpinned-actions".to_string(),
                "missing-permissions".to_string(),
                "missing-spdx".to_string(),
            ],
            can_fix: true,
            estimated_runtime: 30,
        },
        Bot {
            id: "glambot".to_string(),
            name: "Glambot".to_string(),
            description: "Presentation quality and aesthetic checks".to_string(),
            category: BotCategory::Quality,
            dependencies: vec![],
            checks: vec![
                "readme-quality".to_string(),
                "badges".to_string(),
                "formatting".to_string(),
            ],
            can_fix: true,
            estimated_runtime: 15,
        },
        Bot {
            id: "finishing-bot".to_string(),
            name: "Finishing Bot".to_string(),
            description: "Release readiness and completeness checks".to_string(),
            category: BotCategory::Release,
            dependencies: vec!["glambot".to_string()],
            checks: vec![
                "changelog".to_string(),
                "version-consistency".to_string(),
                "release-notes".to_string(),
            ],
            can_fix: true,
            estimated_runtime: 20,
        },
        Bot {
            id: "echidnabot".to_string(),
            name: "Echidnabot".to_string(),
            description: "Mathematical verification and proofs".to_string(),
            category: BotCategory::Quality,
            dependencies: vec![],
            checks: vec!["proof-validity".to_string(), "theorem-coverage".to_string()],
            can_fix: false,
            estimated_runtime: 60,
        },
        Bot {
            id: "seambot".to_string(),
            name: "Seambot".to_string(),
            description: "Architectural seam analysis - drift detection, hidden channels, forge integration".to_string(),
            category: BotCategory::Quality,
            dependencies: vec!["robot-repo-automaton".to_string(), "echidnabot".to_string()],
            checks: vec![
                "seam-analysis".to_string(),
                "drift-detection".to_string(),
                "hidden-channels".to_string(),
                "forge-integration".to_string(),
                "api-compatibility".to_string(),
                "integration-tests".to_string(),
            ],
            can_fix: false,
            estimated_runtime: 45,
        },
        Bot {
            id: "compliance-bot".to_string(),
            name: "Compliance Bot".to_string(),
            description: "License and policy compliance checking".to_string(),
            category: BotCategory::Compliance,
            dependencies: vec![],
            checks: vec![
                "license-headers".to_string(),
                "dependency-licenses".to_string(),
                "policy-adherence".to_string(),
            ],
            can_fix: true,
            estimated_runtime: 25,
        },
    ]
}

/// Topologically sort bots based on dependencies
fn topological_sort(bots: &[Bot]) -> Result<Vec<Bot>> {
    let mut result = Vec::new();
    let mut visited: HashMap<String, bool> = HashMap::new();
    let bot_map: HashMap<String, &Bot> = bots.iter().map(|b| (b.id.clone(), b)).collect();

    fn visit(
        bot_id: &str,
        bot_map: &HashMap<String, &Bot>,
        visited: &mut HashMap<String, bool>,
        result: &mut Vec<Bot>,
    ) -> Result<()> {
        if let Some(&in_progress) = visited.get(bot_id) {
            if in_progress {
                anyhow::bail!("Circular dependency detected involving: {}", bot_id);
            }
            return Ok(()); // Already fully visited
        }

        if let Some(bot) = bot_map.get(bot_id) {
            visited.insert(bot_id.to_string(), true); // Mark as in progress

            for dep_id in &bot.dependencies {
                if bot_map.contains_key(dep_id) {
                    visit(dep_id, bot_map, visited, result)?;
                }
            }

            visited.insert(bot_id.to_string(), false); // Mark as complete
            result.push((*bot).clone());
        }

        Ok(())
    }

    for bot in bots {
        if !visited.contains_key(&bot.id) {
            visit(&bot.id, &bot_map, &mut visited, &mut result)?;
        }
    }

    Ok(result)
}

/// Run a single bot (simulated for now)
async fn run_bot(
    bot: &Bot,
    _repo_path: &PathBuf,
    auto_fix: bool,
    progress: &ProgressBar,
) -> Result<(usize, usize)> {
    // Simulate bot execution with progress updates
    let steps = 5;
    let step_duration = Duration::from_millis((bot.estimated_runtime as u64 * 1000) / steps / 10);

    for i in 0..steps {
        progress.set_message(format!("{}: step {}/{}", bot.name, i + 1, steps));
        sleep(step_duration).await;
    }

    // Simulate findings and fixes
    let findings = rand_findings();
    let fixes = if auto_fix && bot.can_fix {
        findings / 2
    } else {
        0
    };

    Ok((findings, fixes))
}

/// Generate random number of findings for simulation
fn rand_findings() -> usize {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    use std::time::SystemTime;

    let mut hasher = DefaultHasher::new();
    SystemTime::now().hash(&mut hasher);
    (hasher.finish() % 10) as usize
}

fn print_fleet_results(results: &FleetResults) -> Result<()> {
    println!("{}", "Fleet Execution Results".bold());
    println!("{}", "═".repeat(60));
    println!(
        "Operation ID: {}",
        results.operation_id.dimmed()
    );
    println!("Repository: {}", results.repository_path.display());
    println!("Duration: {} ms", results.total_duration_ms);
    println!();

    println!("{}", "Bot Results:".bold());
    println!("{}", "─".repeat(60));

    for result in &results.bot_results {
        let status_colored = match result.status {
            BotStatus::Success => "SUCCESS".green().bold(),
            BotStatus::Failed => "FAILED".red().bold(),
            BotStatus::Skipped => "SKIPPED".yellow(),
            BotStatus::Running => "RUNNING".blue(),
            BotStatus::Pending => "PENDING".dimmed(),
        };

        println!(
            "{}: {} ({} findings, {} fixes)",
            result.bot_id.bold(),
            status_colored,
            result.findings_count,
            result.fixes_applied
        );

        if let Some(ref error) = result.error_message {
            println!("  Error: {}", error.red());
        }
    }

    println!();
    println!("{}", "Summary:".bold());
    println!("  Total bots: {}", results.summary.total_bots);
    println!("  Successful: {}", results.summary.successful.to_string().green());
    println!("  Failed: {}", results.summary.failed.to_string().red());
    println!("  Skipped: {}", results.summary.skipped.to_string().yellow());
    println!("  Total findings: {}", results.summary.total_findings);
    println!("  Total fixes: {}", results.summary.total_fixes);

    Ok(())
}
