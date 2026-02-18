// SPDX-License-Identifier: PMPL-1.0-or-later
//! Hooks command implementation.
//!
//! Install, remove, and manage git hooks for automated policy enforcement.

#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use chrono::{DateTime, Utc};
use clap::Args;
use colored::Colorize;
use serde::{Deserialize, Serialize};
use tracing::{debug, info};

use super::validate_git_repository;
use crate::config::Config;
use crate::output::{OutputFormat, Outputter};

/// Arguments for the hooks command
#[derive(Args, Debug)]
pub struct HooksArgs {
    #[command(subcommand)]
    pub command: HooksCommand,
}

#[derive(clap::Subcommand, Debug)]
pub enum HooksCommand {
    /// Install git hooks in a repository
    Install(HooksInstallArgs),
    /// Remove git hooks from a repository
    Remove(HooksRemoveArgs),
    /// List installed hooks
    List(HooksListArgs),
    /// Check hook status
    Status(HooksStatusArgs),
    /// Update hooks to latest version
    Update(HooksUpdateArgs),
}

/// Arguments for hooks install command
#[derive(Args, Debug)]
pub struct HooksInstallArgs {
    /// Path to the repository
    #[arg(default_value = ".")]
    pub repo_path: PathBuf,

    /// Specific hooks to install (comma-separated)
    #[arg(short, long)]
    pub hooks: Option<String>,

    /// Force overwrite existing hooks
    #[arg(long)]
    pub force: bool,

    /// Create backup of existing hooks
    #[arg(long, default_value = "true")]
    pub backup: bool,

    /// Skip confirmation prompt
    #[arg(short, long)]
    pub yes: bool,
}

/// Arguments for hooks remove command
#[derive(Args, Debug)]
pub struct HooksRemoveArgs {
    /// Path to the repository
    #[arg(default_value = ".")]
    pub repo_path: PathBuf,

    /// Specific hooks to remove (comma-separated, or 'all')
    #[arg(short, long, default_value = "all")]
    pub hooks: String,

    /// Skip confirmation prompt
    #[arg(short, long)]
    pub yes: bool,
}

/// Arguments for hooks list command
#[derive(Args, Debug)]
pub struct HooksListArgs {
    /// Path to the repository
    #[arg(default_value = ".")]
    pub repo_path: PathBuf,

    /// Show available hooks (not just installed)
    #[arg(long)]
    pub available: bool,
}

/// Arguments for hooks status command
#[derive(Args, Debug)]
pub struct HooksStatusArgs {
    /// Path to the repository
    #[arg(default_value = ".")]
    pub repo_path: PathBuf,
}

/// Arguments for hooks update command
#[derive(Args, Debug)]
pub struct HooksUpdateArgs {
    /// Path to the repository
    #[arg(default_value = ".")]
    pub repo_path: PathBuf,

    /// Skip confirmation prompt
    #[arg(short, long)]
    pub yes: bool,
}

/// Hook type enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum HookType {
    PreCommit,
    PrePush,
    PostReceive,
    CommitMsg,
    PrepareCommitMsg,
    PostCommit,
    PostCheckout,
    PostMerge,
}

impl std::fmt::Display for HookType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HookType::PreCommit => write!(f, "pre-commit"),
            HookType::PrePush => write!(f, "pre-push"),
            HookType::PostReceive => write!(f, "post-receive"),
            HookType::CommitMsg => write!(f, "commit-msg"),
            HookType::PrepareCommitMsg => write!(f, "prepare-commit-msg"),
            HookType::PostCommit => write!(f, "post-commit"),
            HookType::PostCheckout => write!(f, "post-checkout"),
            HookType::PostMerge => write!(f, "post-merge"),
        }
    }
}

impl std::str::FromStr for HookType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "pre-commit" | "precommit" => Ok(HookType::PreCommit),
            "pre-push" | "prepush" => Ok(HookType::PrePush),
            "post-receive" | "postreceive" => Ok(HookType::PostReceive),
            "commit-msg" | "commitmsg" => Ok(HookType::CommitMsg),
            "prepare-commit-msg" | "preparecommitmsg" => Ok(HookType::PrepareCommitMsg),
            "post-commit" | "postcommit" => Ok(HookType::PostCommit),
            "post-checkout" | "postcheckout" => Ok(HookType::PostCheckout),
            "post-merge" | "postmerge" => Ok(HookType::PostMerge),
            _ => Err(format!("Unknown hook type: {}", s)),
        }
    }
}

/// Hook definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HookDefinition {
    /// Hook type
    pub hook_type: HookType,
    /// Description
    pub description: String,
    /// Checks performed by this hook
    pub checks: Vec<String>,
    /// Whether this hook can block commits/pushes
    pub blocking: bool,
    /// Version of the hook
    pub version: String,
}

/// Hook installation status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HookStatus {
    /// Hook type
    pub hook_type: HookType,
    /// Whether installed
    pub installed: bool,
    /// Whether it's a hyper-managed hook
    pub managed: bool,
    /// Installed version (if managed)
    pub version: Option<String>,
    /// Last modified timestamp
    pub modified_at: Option<DateTime<Utc>>,
    /// File path
    pub path: PathBuf,
}

/// Result of hooks operation
#[derive(Debug, Serialize, Deserialize)]
pub struct HooksResult {
    /// Operation performed
    pub operation: String,
    /// Repository path
    pub repository_path: PathBuf,
    /// Hooks affected
    pub hooks: Vec<String>,
    /// Success status
    pub success: bool,
    /// Messages
    pub messages: Vec<String>,
    /// Warnings
    pub warnings: Vec<String>,
}

/// Execute the hooks command
pub async fn execute(args: HooksArgs, config: &Config, format: OutputFormat) -> Result<()> {
    match args.command {
        HooksCommand::Install(install_args) => execute_install(install_args, config, format).await,
        HooksCommand::Remove(remove_args) => execute_remove(remove_args, config, format).await,
        HooksCommand::List(list_args) => execute_list(list_args, config, format).await,
        HooksCommand::Status(status_args) => execute_status(status_args, config, format).await,
        HooksCommand::Update(update_args) => execute_update(update_args, config, format).await,
    }
}

async fn execute_install(
    args: HooksInstallArgs,
    _config: &Config,
    format: OutputFormat,
) -> Result<()> {
    let outputter = Outputter::new(format);

    // Validate repository
    let repo = validate_git_repository(&args.repo_path)?;
    let repo_path = repo.workdir().unwrap_or(repo.path()).to_path_buf();
    let hooks_dir = get_hooks_dir(&repo_path)?;

    info!("Installing hooks in: {}", repo_path.display());

    // Get hooks to install
    let hooks_to_install = if let Some(ref hooks_str) = args.hooks {
        parse_hook_list(hooks_str)?
    } else {
        get_default_hooks()
    };

    // Check for existing hooks
    let mut existing_hooks = Vec::new();
    for hook_type in &hooks_to_install {
        let hook_path = hooks_dir.join(hook_type.to_string());
        if hook_path.exists() && !is_managed_hook(&hook_path)? {
            existing_hooks.push(hook_type);
        }
    }

    // Warn about existing hooks
    if !existing_hooks.is_empty() && !args.force {
        if !matches!(format, OutputFormat::Json | OutputFormat::Yaml) {
            println!("{}", "Warning: The following hooks already exist:".yellow());
            for hook in &existing_hooks {
                println!("  - {}", hook);
            }
            println!();
            println!("Use --force to overwrite, or --backup to create backups.");
        }

        if !args.yes {
            anyhow::bail!("Existing hooks found. Use --force to overwrite.");
        }
    }

    // Create backups if requested
    if args.backup {
        for hook_type in &existing_hooks {
            let hook_path = hooks_dir.join(hook_type.to_string());
            if hook_path.exists() {
                let backup_path = hooks_dir.join(format!("{}.backup", hook_type));
                std::fs::copy(&hook_path, &backup_path)
                    .with_context(|| format!("Failed to backup hook: {}", hook_type))?;
                debug!("Backed up {} to {}", hook_type, backup_path.display());
            }
        }
    }

    // Install hooks
    let mut installed = Vec::new();
    let mut messages = Vec::new();
    let warnings: Vec<String> = Vec::new();

    for hook_type in &hooks_to_install {
        let hook_content = generate_hook_script(*hook_type);
        let hook_path = hooks_dir.join(hook_type.to_string());

        // Write hook file
        std::fs::write(&hook_path, &hook_content)
            .with_context(|| format!("Failed to write hook: {}", hook_path.display()))?;

        // Make executable (Unix only)
        #[cfg(unix)]
        {
            let mut perms = std::fs::metadata(&hook_path)?.permissions();
            perms.set_mode(0o755);
            std::fs::set_permissions(&hook_path, perms)?;
        }

        installed.push(hook_type.to_string());
        messages.push(format!("Installed {}", hook_type));
    }

    let result = HooksResult {
        operation: "install".to_string(),
        repository_path: repo_path,
        hooks: installed.clone(),
        success: true,
        messages,
        warnings,
    };

    // Output result
    match format {
        OutputFormat::Json | OutputFormat::Yaml => {
            outputter.output(&result)?;
        }
        _ => {
            println!("{}", "Hooks installed successfully!".green().bold());
            println!();
            for hook in &installed {
                println!("  {} {}", "✓".green(), hook);
            }
            println!();
            println!(
                "Hooks are now active and will run on git operations."
            );
        }
    }

    Ok(())
}

async fn execute_remove(
    args: HooksRemoveArgs,
    _config: &Config,
    format: OutputFormat,
) -> Result<()> {
    let outputter = Outputter::new(format);

    // Validate repository
    let repo = validate_git_repository(&args.repo_path)?;
    let repo_path = repo.workdir().unwrap_or(repo.path()).to_path_buf();
    let hooks_dir = get_hooks_dir(&repo_path)?;

    info!("Removing hooks from: {}", repo_path.display());

    // Get hooks to remove
    let hooks_to_remove = if args.hooks.to_lowercase() == "all" {
        get_default_hooks()
    } else {
        parse_hook_list(&args.hooks)?
    };

    // Confirm removal
    if !args.yes && !matches!(format, OutputFormat::Json | OutputFormat::Yaml) {
        println!("The following hooks will be removed:");
        for hook in &hooks_to_remove {
            println!("  - {}", hook);
        }
        println!();
        println!("This action cannot be undone.");
        // In a real implementation, we'd prompt for confirmation here
    }

    // Remove hooks
    let mut removed = Vec::new();
    let mut messages = Vec::new();
    let mut warnings = Vec::new();

    for hook_type in &hooks_to_remove {
        let hook_path = hooks_dir.join(hook_type.to_string());

        if hook_path.exists() {
            // Check if it's a managed hook
            if !is_managed_hook(&hook_path)? {
                warnings.push(format!(
                    "{} is not a hyper-managed hook, skipping",
                    hook_type
                ));
                continue;
            }

            std::fs::remove_file(&hook_path)
                .with_context(|| format!("Failed to remove hook: {}", hook_path.display()))?;

            removed.push(hook_type.to_string());
            messages.push(format!("Removed {}", hook_type));
        } else {
            messages.push(format!("{} not installed", hook_type));
        }
    }

    let result = HooksResult {
        operation: "remove".to_string(),
        repository_path: repo_path,
        hooks: removed.clone(),
        success: true,
        messages,
        warnings,
    };

    // Output result
    match format {
        OutputFormat::Json | OutputFormat::Yaml => {
            outputter.output(&result)?;
        }
        _ => {
            if removed.is_empty() {
                println!("No hooks were removed.");
            } else {
                println!("{}", "Hooks removed successfully!".green().bold());
                println!();
                for hook in &removed {
                    println!("  {} {}", "✓".green(), hook);
                }
            }
        }
    }

    Ok(())
}

async fn execute_list(args: HooksListArgs, _config: &Config, format: OutputFormat) -> Result<()> {
    let outputter = Outputter::new(format);

    if args.available {
        // List all available hooks
        let definitions = get_hook_definitions();

        match format {
            OutputFormat::Json | OutputFormat::Yaml => {
                outputter.output(&definitions)?;
            }
            OutputFormat::Table => {
                use tabled::{settings::Style, Table, Tabled};

                #[derive(Tabled)]
                struct HookRow {
                    #[tabled(rename = "Hook")]
                    hook: String,
                    #[tabled(rename = "Description")]
                    description: String,
                    #[tabled(rename = "Blocking")]
                    blocking: String,
                    #[tabled(rename = "Version")]
                    version: String,
                }

                let rows: Vec<HookRow> = definitions
                    .iter()
                    .map(|d| HookRow {
                        hook: d.hook_type.to_string(),
                        description: d.description.clone(),
                        blocking: if d.blocking { "Yes" } else { "No" }.to_string(),
                        version: d.version.clone(),
                    })
                    .collect();

                let table = Table::new(rows).with(Style::rounded()).to_string();
                println!("{}", table);
            }
            OutputFormat::Plain => {
                println!("{}", "Available Hooks:".bold());
                println!("{}", "─".repeat(60));
                for def in &definitions {
                    let blocking_str = if def.blocking {
                        "[blocking]".red()
                    } else {
                        "[non-blocking]".green()
                    };
                    println!(
                        "{} {} (v{})",
                        def.hook_type.to_string().bold(),
                        blocking_str,
                        def.version.dimmed()
                    );
                    println!("  {}", def.description);
                    println!("  Checks: {}", def.checks.join(", ").dimmed());
                    println!();
                }
            }
        }
    } else {
        // List installed hooks
        let repo = validate_git_repository(&args.repo_path)?;
        let repo_path = repo.workdir().unwrap_or(repo.path()).to_path_buf();

        let statuses = get_hook_statuses(&repo_path)?;

        match format {
            OutputFormat::Json | OutputFormat::Yaml => {
                outputter.output(&statuses)?;
            }
            OutputFormat::Table => {
                use tabled::{settings::Style, Table, Tabled};

                #[derive(Tabled)]
                struct StatusRow {
                    #[tabled(rename = "Hook")]
                    hook: String,
                    #[tabled(rename = "Installed")]
                    installed: String,
                    #[tabled(rename = "Managed")]
                    managed: String,
                    #[tabled(rename = "Version")]
                    version: String,
                }

                let rows: Vec<StatusRow> = statuses
                    .iter()
                    .map(|s| StatusRow {
                        hook: s.hook_type.to_string(),
                        installed: if s.installed { "Yes" } else { "No" }.to_string(),
                        managed: if s.managed { "Yes" } else { "No" }.to_string(),
                        version: s.version.clone().unwrap_or_else(|| "-".to_string()),
                    })
                    .collect();

                let table = Table::new(rows).with(Style::rounded()).to_string();
                println!("{}", table);
            }
            OutputFormat::Plain => {
                println!("{}", "Installed Hooks:".bold());
                println!("{}", "─".repeat(60));

                let installed: Vec<_> = statuses.iter().filter(|s| s.installed).collect();

                if installed.is_empty() {
                    println!("No hooks installed.");
                    println!();
                    println!("Use 'hyper hooks install' to install hooks.");
                } else {
                    for status in installed {
                        let managed_str = if status.managed {
                            "[managed]".green()
                        } else {
                            "[custom]".yellow()
                        };
                        println!(
                            "{} {} {}",
                            status.hook_type.to_string().bold(),
                            managed_str,
                            status
                                .version
                                .as_ref()
                                .map(|v| format!("v{}", v).dimmed())
                                .unwrap_or_default()
                        );
                    }
                }
            }
        }
    }

    Ok(())
}

async fn execute_status(
    args: HooksStatusArgs,
    _config: &Config,
    format: OutputFormat,
) -> Result<()> {
    let outputter = Outputter::new(format);

    let repo = validate_git_repository(&args.repo_path)?;
    let repo_path = repo.workdir().unwrap_or(repo.path()).to_path_buf();

    let statuses = get_hook_statuses(&repo_path)?;

    match format {
        OutputFormat::Json | OutputFormat::Yaml => {
            outputter.output(&statuses)?;
        }
        _ => {
            println!("{}", "Hook Status:".bold());
            println!("{}", "─".repeat(60));

            for status in &statuses {
                let status_icon = if status.installed {
                    if status.managed {
                        "✓".green()
                    } else {
                        "⚠".yellow()
                    }
                } else {
                    "○".dimmed()
                };

                let status_text = if status.installed {
                    if status.managed {
                        "installed (managed)".green()
                    } else {
                        "installed (custom)".yellow()
                    }
                } else {
                    "not installed".dimmed()
                };

                println!("{} {} - {}", status_icon, status.hook_type, status_text);
            }
        }
    }

    Ok(())
}

async fn execute_update(
    args: HooksUpdateArgs,
    _config: &Config,
    format: OutputFormat,
) -> Result<()> {
    let outputter = Outputter::new(format);

    let repo = validate_git_repository(&args.repo_path)?;
    let repo_path = repo.workdir().unwrap_or(repo.path()).to_path_buf();
    let hooks_dir = get_hooks_dir(&repo_path)?;

    info!("Updating hooks in: {}", repo_path.display());

    let statuses = get_hook_statuses(&repo_path)?;
    let managed_hooks: Vec<_> = statuses
        .iter()
        .filter(|s| s.installed && s.managed)
        .collect();

    if managed_hooks.is_empty() {
        outputter.info("No managed hooks to update.")?;
        return Ok(());
    }

    let mut updated = Vec::new();
    let mut messages = Vec::new();

    for status in managed_hooks {
        let hook_content = generate_hook_script(status.hook_type);
        let hook_path = hooks_dir.join(status.hook_type.to_string());

        std::fs::write(&hook_path, &hook_content)?;

        updated.push(status.hook_type.to_string());
        messages.push(format!("Updated {}", status.hook_type));
    }

    let result = HooksResult {
        operation: "update".to_string(),
        repository_path: repo_path,
        hooks: updated.clone(),
        success: true,
        messages,
        warnings: vec![],
    };

    match format {
        OutputFormat::Json | OutputFormat::Yaml => {
            outputter.output(&result)?;
        }
        _ => {
            println!("{}", "Hooks updated successfully!".green().bold());
            for hook in &updated {
                println!("  {} {}", "✓".green(), hook);
            }
        }
    }

    Ok(())
}

/// Get the git hooks directory for a repository
fn get_hooks_dir(repo_path: &Path) -> Result<PathBuf> {
    let hooks_dir = repo_path.join(".git/hooks");
    if !hooks_dir.exists() {
        std::fs::create_dir_all(&hooks_dir)
            .with_context(|| format!("Failed to create hooks directory: {}", hooks_dir.display()))?;
    }
    Ok(hooks_dir)
}

/// Parse a comma-separated list of hook types
fn parse_hook_list(list: &str) -> Result<Vec<HookType>> {
    list.split(',')
        .map(|s| {
            s.trim()
                .parse::<HookType>()
                .map_err(|e| anyhow::anyhow!(e))
        })
        .collect()
}

/// Get the default hooks to install
fn get_default_hooks() -> Vec<HookType> {
    vec![HookType::PreCommit, HookType::PrePush]
}

/// Check if a hook file is managed by hyper
fn is_managed_hook(path: &Path) -> Result<bool> {
    if !path.exists() {
        return Ok(false);
    }

    let content = std::fs::read_to_string(path)?;
    Ok(content.contains("# Managed by cicd-hyper-a"))
}

/// Get hook definitions
fn get_hook_definitions() -> Vec<HookDefinition> {
    vec![
        HookDefinition {
            hook_type: HookType::PreCommit,
            description: "Run checks before committing".to_string(),
            checks: vec![
                "spdx-headers".to_string(),
                "trailing-whitespace".to_string(),
                "merge-conflicts".to_string(),
            ],
            blocking: true,
            version: "1.0.0".to_string(),
        },
        HookDefinition {
            hook_type: HookType::PrePush,
            description: "Run checks before pushing".to_string(),
            checks: vec![
                "workflow-validation".to_string(),
                "sha-pins".to_string(),
                "permissions".to_string(),
            ],
            blocking: true,
            version: "1.0.0".to_string(),
        },
        HookDefinition {
            hook_type: HookType::CommitMsg,
            description: "Validate commit message format".to_string(),
            checks: vec!["conventional-commits".to_string()],
            blocking: true,
            version: "1.0.0".to_string(),
        },
        HookDefinition {
            hook_type: HookType::PostReceive,
            description: "Trigger after receiving pushes (server-side)".to_string(),
            checks: vec!["notification".to_string(), "deploy-trigger".to_string()],
            blocking: false,
            version: "1.0.0".to_string(),
        },
    ]
}

/// Get status of all hooks for a repository
fn get_hook_statuses(repo_path: &Path) -> Result<Vec<HookStatus>> {
    let hooks_dir = get_hooks_dir(repo_path)?;
    let all_hooks = vec![
        HookType::PreCommit,
        HookType::PrePush,
        HookType::CommitMsg,
        HookType::PostReceive,
        HookType::PostCommit,
        HookType::PostCheckout,
        HookType::PostMerge,
    ];

    let mut statuses = Vec::new();

    for hook_type in all_hooks {
        let hook_path = hooks_dir.join(hook_type.to_string());
        let installed = hook_path.exists();
        let managed = installed && is_managed_hook(&hook_path).unwrap_or(false);

        let modified_at = if installed {
            std::fs::metadata(&hook_path)
                .ok()
                .and_then(|m| m.modified().ok())
                .map(|t| DateTime::<Utc>::from(t))
        } else {
            None
        };

        // Extract version from managed hooks
        let version = if managed {
            extract_hook_version(&hook_path).ok().flatten()
        } else {
            None
        };

        statuses.push(HookStatus {
            hook_type,
            installed,
            managed,
            version,
            modified_at,
            path: hook_path,
        });
    }

    Ok(statuses)
}

/// Extract version from a managed hook file
fn extract_hook_version(path: &Path) -> Result<Option<String>> {
    let content = std::fs::read_to_string(path)?;
    for line in content.lines() {
        if line.starts_with("# Version:") {
            return Ok(Some(line.trim_start_matches("# Version:").trim().to_string()));
        }
    }
    Ok(None)
}

/// Generate hook script content
fn generate_hook_script(hook_type: HookType) -> String {
    let checks = match hook_type {
        HookType::PreCommit => r#"
# Check for SPDX headers in modified files
for file in $(git diff --cached --name-only --diff-filter=ACMR); do
    if [[ "$file" =~ \.(rs|js|ts|py|go|sh)$ ]]; then
        if ! head -10 "$file" | grep -q "SPDX-License-Identifier"; then
            echo "Missing SPDX header: $file"
            missing_spdx=1
        fi
    fi
done

if [ "$missing_spdx" = "1" ]; then
    echo ""
    echo "Add SPDX headers with: hyper scan --fix"
    exit 1
fi

# Check for trailing whitespace
if git diff --cached --check; then
    :
else
    echo "Trailing whitespace detected. Fix and try again."
    exit 1
fi

# Check for merge conflicts
if git diff --cached --name-only | xargs grep -l "<<<<<<< HEAD" 2>/dev/null; then
    echo "Merge conflict markers found!"
    exit 1
fi
"#,
        HookType::PrePush => r#"
# Validate GitHub workflow files
if [ -d ".github/workflows" ]; then
    for workflow in .github/workflows/*.yml .github/workflows/*.yaml; do
        [ -f "$workflow" ] || continue

        # Check for permissions declaration
        if ! grep -q "^permissions:" "$workflow"; then
            echo "Missing permissions in: $workflow"
            missing_perms=1
        fi

        # Check for unpinned actions (excluding first-party)
        if grep -E "uses:.*@(v[0-9]+|main|master|latest)" "$workflow" | grep -v "actions/"; then
            echo "Unpinned actions in: $workflow"
            unpinned=1
        fi
    done

    if [ "$missing_perms" = "1" ] || [ "$unpinned" = "1" ]; then
        echo ""
        echo "Fix workflow issues with: hyper scan --fix"
        exit 1
    fi
fi
"#,
        HookType::CommitMsg => r#"
# Validate conventional commit format
commit_msg_file=$1
commit_msg=$(cat "$commit_msg_file")

# Pattern for conventional commits
pattern="^(feat|fix|docs|style|refactor|perf|test|build|ci|chore|revert)(\(.+\))?: .+"

if ! echo "$commit_msg" | head -1 | grep -qE "$pattern"; then
    echo "Invalid commit message format!"
    echo ""
    echo "Expected format: <type>(<scope>): <description>"
    echo ""
    echo "Types: feat, fix, docs, style, refactor, perf, test, build, ci, chore, revert"
    echo ""
    echo "Examples:"
    echo "  feat(cli): add scan command"
    echo "  fix: resolve workflow validation issue"
    echo "  docs: update README"
    exit 1
fi
"#,
        _ => "",
    };

    format!(
        r#"#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Managed by cicd-hyper-a
# Version: 1.0.0
# Hook: {hook_type}
#
# This hook is managed by the cicd-hyper-a CLI.
# To update: hyper hooks update
# To remove: hyper hooks remove {hook_type}

set -e

{checks}

exit 0
"#,
        hook_type = hook_type,
        checks = checks.trim()
    )
}
