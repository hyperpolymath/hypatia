// SPDX-License-Identifier: PMPL-1.0-or-later
//! cicd-hyper-a CLI - Command-line interface for the neurosymbolic CI/CD platform
//!
//! This CLI provides commands for:
//! - Scanning repositories for CI/CD issues
//! - Running bot fleets for automated fixes
//! - Managing rulesets (deposit, withdraw, search)
//! - Installing and managing git hooks

use anyhow::Result;
use clap::{Parser, Subcommand};
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

mod commands;
mod completions;
mod config;
mod exit_codes;
mod output;

use commands::{batch, deposit, fleet, hooks, scan, search, withdraw};
use config::Config;
use output::OutputFormat;

/// cicd-hyper-a CLI - Neurosymbolic CI/CD Intelligence Platform
///
/// Combines neural learning with symbolic reasoning to provide
/// intelligent CI/CD automation and issue detection.
#[derive(Parser)]
#[command(
    name = "hyper",
    author = "hyperpolymath",
    version,
    about = "Neurosymbolic CI/CD Intelligence Platform CLI",
    long_about = None,
    propagate_version = true,
    after_help = "Use 'hyper <command> --help' for more information about a command."
)]
struct Cli {
    /// Output format (json, yaml, table, or plain)
    #[arg(short, long, global = true, default_value = "plain", env = "HYPER_OUTPUT")]
    output: OutputFormat,

    /// Enable verbose output
    #[arg(short, long, global = true, action = clap::ArgAction::Count)]
    verbose: u8,

    /// Suppress all non-error output
    #[arg(short, long, global = true)]
    quiet: bool,

    /// Configuration file path
    #[arg(short, long, global = true, env = "HYPER_CONFIG")]
    config: Option<std::path::PathBuf>,

    /// Disable colored output
    #[arg(long, global = true, env = "NO_COLOR")]
    no_color: bool,

    /// Machine-readable output mode (implies --no-color, disables progress)
    #[arg(long, global = true, env = "HYPER_MACHINE")]
    machine: bool,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Scan a repository for CI/CD issues and policy violations
    ///
    /// Analyzes the repository structure, workflows, and configurations
    /// to detect security issues, anti-patterns, and policy violations.
    Scan(scan::ScanArgs),

    /// Run bot fleet operations on a repository
    ///
    /// Executes configured bots in dependency order to analyze
    /// and fix issues in the repository.
    Fleet(fleet::FleetArgs),

    /// Submit a ruleset to the registry
    ///
    /// Deposits a validated ruleset into the central registry
    /// for distribution and reuse across repositories.
    Deposit(deposit::DepositArgs),

    /// Pull a ruleset from the registry
    ///
    /// Withdraws a ruleset by name or ID from the registry
    /// for local use or inspection.
    Withdraw(withdraw::WithdrawArgs),

    /// Search for rulesets in the registry
    ///
    /// Find rulesets by effect, language, category, or text search.
    Search(search::SearchArgs),

    /// Manage git hooks in a repository
    ///
    /// Install, remove, or update git hooks for automated
    /// policy enforcement and quality checks.
    Hooks(hooks::HooksArgs),

    /// Process multiple repositories in batch mode
    ///
    /// Scan, fix, or report on multiple repositories at once.
    /// Supports stdin input, parallel processing, and JSON Lines output.
    Batch(batch::BatchArgs),

    /// Generate shell completion scripts
    ///
    /// Outputs completion scripts for bash, zsh, fish, elvish, or PowerShell.
    Completions(completions::CompletionsArgs),

    /// Show or manage configuration
    #[command(subcommand)]
    Config(ConfigCommands),

    /// Show version and build information
    Version,

    /// Show exit code documentation
    #[command(name = "exit-codes")]
    ExitCodes,
}

#[derive(Subcommand)]
enum ConfigCommands {
    /// Show current configuration
    Show,
    /// Initialize configuration file
    Init,
    /// Set a configuration value
    Set {
        /// Configuration key (e.g., registry.url)
        key: String,
        /// Configuration value
        value: String,
    },
    /// Get a configuration value
    Get {
        /// Configuration key
        key: String,
    },
}

fn init_logging(verbose: u8, quiet: bool) {
    let filter = match (quiet, verbose) {
        (true, _) => "error",
        (false, 0) => "warn",
        (false, 1) => "info",
        (false, 2) => "debug",
        (false, _) => "trace",
    };

    let env_filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new(filter));

    tracing_subscriber::registry()
        .with(fmt::layer().with_target(verbose >= 2))
        .with(env_filter)
        .init();
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize logging
    init_logging(cli.verbose, cli.quiet);

    // Disable colors if requested or in machine mode
    if cli.no_color || cli.machine {
        colored::control::set_override(false);
    }

    // Load configuration
    let config = match &cli.config {
        Some(path) => Config::from_file(path)?,
        None => Config::load()?,
    };

    // Dispatch to command handlers
    match cli.command {
        Commands::Scan(args) => {
            scan::execute(args, &config, cli.output).await?;
        }
        Commands::Fleet(args) => {
            fleet::execute(args, &config, cli.output).await?;
        }
        Commands::Deposit(args) => {
            deposit::execute(args, &config, cli.output).await?;
        }
        Commands::Withdraw(args) => {
            withdraw::execute(args, &config, cli.output).await?;
        }
        Commands::Search(args) => {
            search::execute(args, &config, cli.output).await?;
        }
        Commands::Hooks(args) => {
            hooks::execute(args, &config, cli.output).await?;
        }
        Commands::Batch(args) => {
            let code = batch::execute(args, &config, cli.output, cli.machine).await?;
            std::process::exit(code);
        }
        Commands::Completions(args) => {
            completions::generate_completions(&args)?;
        }
        Commands::Config(cmd) => {
            handle_config_command(cmd, &config, cli.output)?;
        }
        Commands::Version => {
            print_version_info(cli.output)?;
        }
        Commands::ExitCodes => {
            print_exit_codes(cli.output)?;
        }
    }

    Ok(())
}

fn handle_config_command(cmd: ConfigCommands, config: &Config, format: OutputFormat) -> Result<()> {
    use output::Outputter;

    let outputter = Outputter::new(format);

    match cmd {
        ConfigCommands::Show => {
            outputter.output_config(config)?;
        }
        ConfigCommands::Init => {
            let path = Config::init_default()?;
            outputter.success(&format!("Configuration initialized at: {}", path.display()))?;
        }
        ConfigCommands::Set { key, value } => {
            let mut new_config = config.clone();
            new_config.set(&key, &value)?;
            new_config.save()?;
            outputter.success(&format!("Set {} = {}", key, value))?;
        }
        ConfigCommands::Get { key } => {
            if let Some(value) = config.get(&key) {
                outputter.plain(&value)?;
            } else {
                anyhow::bail!("Configuration key '{}' not found", key);
            }
        }
    }

    Ok(())
}

fn print_version_info(format: OutputFormat) -> Result<()> {
    use output::Outputter;
    use serde::Serialize;

    #[derive(Serialize)]
    struct VersionInfo {
        name: &'static str,
        version: &'static str,
        git_commit: Option<&'static str>,
        build_date: Option<&'static str>,
        rust_version: &'static str,
        target: &'static str,
    }

    let info = VersionInfo {
        name: env!("CARGO_PKG_NAME"),
        version: env!("CARGO_PKG_VERSION"),
        git_commit: option_env!("GIT_COMMIT"),
        build_date: option_env!("BUILD_DATE"),
        rust_version: env!("CARGO_PKG_RUST_VERSION"),
        target: env!("TARGET"),
    };

    let outputter = Outputter::new(format);

    match format {
        OutputFormat::Json | OutputFormat::Yaml => {
            outputter.output(&info)?;
        }
        _ => {
            println!("{} {}", info.name, info.version);
            if let Some(commit) = info.git_commit {
                println!("Git commit: {}", commit);
            }
            if let Some(date) = info.build_date {
                println!("Build date: {}", date);
            }
            println!("Rust version: {}", info.rust_version);
            println!("Target: {}", info.target);
        }
    }

    Ok(())
}

fn print_exit_codes(format: OutputFormat) -> Result<()> {
    use output::Outputter;
    use serde::Serialize;

    #[derive(Serialize)]
    struct ExitCodeInfo {
        code: i32,
        name: &'static str,
        description: &'static str,
        category: &'static str,
    }

    let codes = vec![
        ExitCodeInfo { code: 0, name: "SUCCESS", description: "Operation completed successfully", category: "Success" },
        ExitCodeInfo { code: 1, name: "GENERAL_ERROR", description: "General/unspecified error", category: "General Errors" },
        ExitCodeInfo { code: 2, name: "INVALID_ARGUMENTS", description: "Invalid command line arguments", category: "General Errors" },
        ExitCodeInfo { code: 3, name: "CONFIG_ERROR", description: "Configuration file error", category: "General Errors" },
        ExitCodeInfo { code: 4, name: "IO_ERROR", description: "IO error (file read/write, network)", category: "General Errors" },
        ExitCodeInfo { code: 5, name: "CANCELLED", description: "Operation cancelled by user", category: "General Errors" },
        ExitCodeInfo { code: 6, name: "TIMEOUT", description: "Operation timed out", category: "General Errors" },
        ExitCodeInfo { code: 7, name: "NOT_IMPLEMENTED", description: "Feature not implemented", category: "General Errors" },
        ExitCodeInfo { code: 10, name: "CRITICAL_FINDINGS", description: "Critical severity findings detected", category: "Scan Results" },
        ExitCodeInfo { code: 11, name: "HIGH_FINDINGS", description: "High severity findings detected", category: "Scan Results" },
        ExitCodeInfo { code: 12, name: "MEDIUM_FINDINGS", description: "Medium severity findings detected", category: "Scan Results" },
        ExitCodeInfo { code: 13, name: "LOW_FINDINGS", description: "Low severity findings detected", category: "Scan Results" },
        ExitCodeInfo { code: 14, name: "INFO_FINDINGS", description: "Info level findings only", category: "Scan Results" },
        ExitCodeInfo { code: 15, name: "SCAN_FAILED", description: "Scan failed to complete", category: "Scan Results" },
        ExitCodeInfo { code: 20, name: "REGISTRY_CONNECTION_ERROR", description: "Registry connection failed", category: "Registry Errors" },
        ExitCodeInfo { code: 21, name: "REGISTRY_AUTH_ERROR", description: "Registry authentication failed", category: "Registry Errors" },
        ExitCodeInfo { code: 22, name: "RULESET_NOT_FOUND", description: "Ruleset not found", category: "Registry Errors" },
        ExitCodeInfo { code: 23, name: "RULESET_CONFLICT", description: "Ruleset version conflict", category: "Registry Errors" },
        ExitCodeInfo { code: 24, name: "RULESET_INVALID", description: "Ruleset validation failed", category: "Registry Errors" },
        ExitCodeInfo { code: 25, name: "REGISTRY_RATE_LIMIT", description: "Registry rate limit exceeded", category: "Registry Errors" },
        ExitCodeInfo { code: 30, name: "NOT_A_REPO", description: "Not a git repository", category: "Repository Errors" },
        ExitCodeInfo { code: 31, name: "REPO_NOT_FOUND", description: "Repository not found", category: "Repository Errors" },
        ExitCodeInfo { code: 32, name: "REPO_ACCESS_DENIED", description: "Repository access denied", category: "Repository Errors" },
        ExitCodeInfo { code: 33, name: "GIT_ERROR", description: "Git operation failed", category: "Repository Errors" },
        ExitCodeInfo { code: 34, name: "REPO_DIRTY", description: "Repository has uncommitted changes", category: "Repository Errors" },
        ExitCodeInfo { code: 35, name: "HOOK_ERROR", description: "Hook installation/execution failed", category: "Repository Errors" },
        ExitCodeInfo { code: 40, name: "BOT_FAILED", description: "Bot execution failed", category: "Fleet Errors" },
        ExitCodeInfo { code: 41, name: "BOT_NOT_FOUND", description: "Bot not found", category: "Fleet Errors" },
        ExitCodeInfo { code: 42, name: "FLEET_PARTIAL_FAILURE", description: "Fleet operation partially failed", category: "Fleet Errors" },
        ExitCodeInfo { code: 43, name: "FLEET_TOTAL_FAILURE", description: "Fleet operation completely failed", category: "Fleet Errors" },
        ExitCodeInfo { code: 44, name: "BOT_DEPENDENCY_ERROR", description: "Bot dependency error", category: "Fleet Errors" },
        ExitCodeInfo { code: 50, name: "PARTIAL_FAILURE", description: "Batch operation partially succeeded", category: "Batch Results" },
        ExitCodeInfo { code: 51, name: "TOTAL_FAILURE", description: "Batch operation completely failed", category: "Batch Results" },
        ExitCodeInfo { code: 52, name: "NO_ITEMS", description: "No items to process", category: "Batch Results" },
        ExitCodeInfo { code: 100, name: "INTERNAL_ERROR", description: "Internal error (panic, unexpected state)", category: "Internal Errors" },
    ];

    let outputter = Outputter::new(format);

    match format {
        OutputFormat::Json | OutputFormat::Yaml => {
            outputter.output(&codes)?;
        }
        _ => {
            println!("Exit Codes for hyper CLI");
            println!("========================\n");

            let mut current_category = "";
            for info in &codes {
                if info.category != current_category {
                    if !current_category.is_empty() {
                        println!();
                    }
                    println!("{}:", info.category);
                    current_category = info.category;
                }
                println!("  {:3}  {:<30}  {}", info.code, info.name, info.description);
            }
            println!("\nFor detailed information: man hyper-exit-codes(7)");
        }
    }

    Ok(())
}
