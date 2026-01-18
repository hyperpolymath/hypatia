// SPDX-License-Identifier: PLMP-1.0-or-later
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
mod config;
mod output;

use commands::{deposit, fleet, hooks, scan, search, withdraw};
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

    /// Show or manage configuration
    #[command(subcommand)]
    Config(ConfigCommands),

    /// Show version and build information
    Version,
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

    // Disable colors if requested
    if cli.no_color {
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
        Commands::Config(cmd) => {
            handle_config_command(cmd, &config, cli.output)?;
        }
        Commands::Version => {
            print_version_info(cli.output)?;
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
