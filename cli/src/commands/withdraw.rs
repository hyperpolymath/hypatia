// SPDX-License-Identifier: PLMP-1.0-or-later
//! Withdraw command implementation.
//!
//! Pulls rulesets from the registry for local use or inspection.

use std::path::PathBuf;

use anyhow::{Context, Result};
use chrono::{DateTime, Utc};
use clap::Args;
use colored::Colorize;
use serde::{Deserialize, Serialize};
use tracing::{debug, info};

use super::deposit::{Ruleset, RulesetMetadata, RulesetCategory, Rule, RuleEffect};
use crate::config::Config;
use crate::output::{OutputFormat, Outputter};

/// Arguments for the withdraw command
#[derive(Args, Debug)]
pub struct WithdrawArgs {
    /// Ruleset name or ID to withdraw (e.g., "security/workflow-pins/1.0.0")
    pub ruleset_name: String,

    /// Specific version to withdraw (latest if not specified)
    #[arg(short, long)]
    pub version: Option<String>,

    /// Output directory for the ruleset file
    #[arg(short, long)]
    pub output: Option<PathBuf>,

    /// Output format for the ruleset file
    #[arg(long, default_value = "yaml")]
    pub file_format: RulesetFormat,

    /// Just display the ruleset without saving
    #[arg(long)]
    pub show_only: bool,

    /// Include the ruleset content in JSON output
    #[arg(long)]
    pub include_content: bool,

    /// Registry URL to withdraw from
    #[arg(long)]
    pub registry: Option<String>,

    /// Force overwrite if file exists
    #[arg(long)]
    pub force: bool,
}

/// Output format for ruleset files
#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum RulesetFormat {
    Json,
    Yaml,
    Toml,
}

impl std::fmt::Display for RulesetFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RulesetFormat::Json => write!(f, "json"),
            RulesetFormat::Yaml => write!(f, "yaml"),
            RulesetFormat::Toml => write!(f, "toml"),
        }
    }
}

/// Result of a withdraw operation
#[derive(Debug, Serialize, Deserialize)]
pub struct WithdrawResult {
    /// Whether the withdraw was successful
    pub success: bool,
    /// Ruleset ID that was withdrawn
    pub ruleset_id: String,
    /// Version that was withdrawn
    pub version: String,
    /// Path where the ruleset was saved (if applicable)
    pub saved_to: Option<PathBuf>,
    /// Registry URL
    pub registry_url: String,
    /// Timestamp of withdrawal
    pub withdrawn_at: DateTime<Utc>,
    /// Ruleset metadata
    pub metadata: RulesetMetadata,
    /// Full ruleset content (if include_content is true)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content: Option<Ruleset>,
}

/// Execute the withdraw command
pub async fn execute(args: WithdrawArgs, config: &Config, format: OutputFormat) -> Result<()> {
    let outputter = Outputter::new(format);

    let registry_url = args
        .registry
        .clone()
        .or_else(|| config.registry_url.clone())
        .unwrap_or_else(|| "https://registry.cicd-hyper-a.dev".to_string());

    info!(
        "Withdrawing ruleset '{}' from {}",
        args.ruleset_name, registry_url
    );

    // Fetch ruleset from registry
    let (ruleset, version) = fetch_ruleset(&args.ruleset_name, &args.version, &registry_url).await?;

    // If show_only, just display and return
    if args.show_only {
        match format {
            OutputFormat::Json | OutputFormat::Yaml => {
                outputter.output(&ruleset)?;
            }
            _ => {
                print_ruleset_details(&ruleset)?;
            }
        }
        return Ok(());
    }

    // Determine output path
    let output_path = determine_output_path(&args, &ruleset)?;

    // Check if file exists
    if output_path.exists() && !args.force {
        anyhow::bail!(
            "File already exists: {}. Use --force to overwrite.",
            output_path.display()
        );
    }

    // Serialize ruleset to chosen format
    let content = serialize_ruleset(&ruleset, args.file_format)?;

    // Write to file
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent)
            .with_context(|| format!("Failed to create directory: {}", parent.display()))?;
    }

    std::fs::write(&output_path, &content)
        .with_context(|| format!("Failed to write ruleset to: {}", output_path.display()))?;

    // Build result
    let result = WithdrawResult {
        success: true,
        ruleset_id: args.ruleset_name.clone(),
        version: version.clone(),
        saved_to: Some(output_path.clone()),
        registry_url,
        withdrawn_at: Utc::now(),
        metadata: ruleset.metadata.clone(),
        content: if args.include_content {
            Some(ruleset.clone())
        } else {
            None
        },
    };

    // Output result
    match format {
        OutputFormat::Json | OutputFormat::Yaml => {
            outputter.output(&result)?;
        }
        _ => {
            println!("{}", "Ruleset withdrawn successfully!".green().bold());
            println!();
            println!("  {}: {}", "Name".dimmed(), ruleset.metadata.name);
            println!("  {}: {}", "Version".dimmed(), version);
            println!("  {}: {}", "Category".dimmed(), ruleset.metadata.category);
            println!("  {}: {} rules", "Rules".dimmed(), ruleset.rules.len());
            println!("  {}: {}", "Saved to".dimmed(), output_path.display());
            println!();
            println!(
                "Use with: {}",
                format!("hyper scan --rules {}", output_path.display()).cyan()
            );
        }
    }

    Ok(())
}

/// Fetch a ruleset from the registry
async fn fetch_ruleset(
    name: &str,
    version: &Option<String>,
    registry_url: &str,
) -> Result<(Ruleset, String)> {
    debug!(
        "Fetching ruleset: {} (version: {:?}) from {}",
        name, version, registry_url
    );

    // In a real implementation, this would make an HTTP request
    // For now, return a mock ruleset based on the name

    // Simulate network delay
    tokio::time::sleep(std::time::Duration::from_millis(150)).await;

    let (category, ruleset_name, default_version) = parse_ruleset_id(name)?;
    let resolved_version = version.clone().unwrap_or(default_version);

    // Generate a mock ruleset based on the category
    let ruleset = generate_mock_ruleset(&category, &ruleset_name, &resolved_version);

    Ok((ruleset, resolved_version))
}

/// Parse a ruleset ID like "security/workflow-pins/1.0.0"
fn parse_ruleset_id(id: &str) -> Result<(String, String, String)> {
    let parts: Vec<&str> = id.split('/').collect();

    match parts.len() {
        1 => Ok((
            "general".to_string(),
            parts[0].to_string(),
            "latest".to_string(),
        )),
        2 => Ok((
            parts[0].to_string(),
            parts[1].to_string(),
            "latest".to_string(),
        )),
        3 => Ok((
            parts[0].to_string(),
            parts[1].to_string(),
            parts[2].to_string(),
        )),
        _ => anyhow::bail!(
            "Invalid ruleset ID format. Use: name, category/name, or category/name/version"
        ),
    }
}

/// Generate a mock ruleset for demonstration
fn generate_mock_ruleset(category: &str, name: &str, version: &str) -> Ruleset {
    let category_enum = match category {
        "security" => RulesetCategory::Security,
        "quality" => RulesetCategory::Quality,
        "policy" => RulesetCategory::Policy,
        "performance" => RulesetCategory::Performance,
        "documentation" => RulesetCategory::Documentation,
        "workflow" => RulesetCategory::Workflow,
        "dependencies" => RulesetCategory::Dependencies,
        "accessibility" => RulesetCategory::Accessibility,
        _ => RulesetCategory::Quality,
    };

    let rules = match category {
        "security" => vec![
            Rule {
                id: "pin-github-actions".to_string(),
                name: "Pin GitHub Actions".to_string(),
                description: "Ensure all GitHub Actions are pinned to SHA".to_string(),
                severity: "high".to_string(),
                effect: RuleEffect::Both,
                pattern: Some(r"uses:\s*([^@]+)@(v\d+|main|master|latest)".to_string()),
                fix_template: Some("uses: $1@SHA # $2".to_string()),
                conditions: Some(vec![]),
            },
            Rule {
                id: "workflow-permissions".to_string(),
                name: "Workflow Permissions".to_string(),
                description: "Ensure workflows declare explicit permissions".to_string(),
                severity: "medium".to_string(),
                effect: RuleEffect::Both,
                pattern: Some(r"^permissions:\s*".to_string()),
                fix_template: Some("permissions: read-all".to_string()),
                conditions: None,
            },
        ],
        "policy" => vec![
            Rule {
                id: "spdx-header".to_string(),
                name: "SPDX License Header".to_string(),
                description: "Ensure files have SPDX license identifiers".to_string(),
                severity: "low".to_string(),
                effect: RuleEffect::Both,
                pattern: Some(r"SPDX-License-Identifier:".to_string()),
                fix_template: Some("// SPDX-License-Identifier: PLMP-1.0-or-later".to_string()),
                conditions: None,
            },
            Rule {
                id: "security-policy".to_string(),
                name: "Security Policy".to_string(),
                description: "Ensure repository has SECURITY.md".to_string(),
                severity: "medium".to_string(),
                effect: RuleEffect::Check,
                pattern: None,
                fix_template: None,
                conditions: None,
            },
        ],
        _ => vec![Rule {
            id: format!("{}-check", name),
            name: format!("{} Check", name.replace('-', " ").to_uppercase()),
            description: format!("Generic check for {} ruleset", name),
            severity: "info".to_string(),
            effect: RuleEffect::Check,
            pattern: None,
            fix_template: None,
            conditions: None,
        }],
    };

    Ruleset {
        metadata: RulesetMetadata {
            name: name.to_string(),
            version: version.to_string(),
            description: format!("{} ruleset for {} category", name, category),
            category: category_enum,
            authors: vec!["cicd-hyper-a".to_string()],
            languages: vec!["all".to_string()],
            tags: vec![category.to_string(), name.to_string()],
            license: "PLMP-1.0-or-later".to_string(),
            repository: Some("https://github.com/hyperpolymath/cicd-hyper-a".to_string()),
            private: false,
        },
        rules,
    }
}

/// Determine output path for the ruleset
fn determine_output_path(args: &WithdrawArgs, ruleset: &Ruleset) -> Result<PathBuf> {
    let extension = match args.file_format {
        RulesetFormat::Json => "json",
        RulesetFormat::Yaml => "yaml",
        RulesetFormat::Toml => "toml",
    };

    let filename = format!(
        "{}-{}.{}",
        ruleset.metadata.name, ruleset.metadata.version, extension
    );

    let path = match &args.output {
        Some(dir) => {
            if dir.is_dir() || dir.to_string_lossy().ends_with('/') {
                dir.join(filename)
            } else {
                dir.clone()
            }
        }
        None => PathBuf::from(".").join(filename),
    };

    Ok(path)
}

/// Serialize ruleset to the chosen format
fn serialize_ruleset(ruleset: &Ruleset, format: RulesetFormat) -> Result<String> {
    match format {
        RulesetFormat::Json => {
            serde_json::to_string_pretty(ruleset).context("Failed to serialize as JSON")
        }
        RulesetFormat::Yaml => {
            serde_yaml::to_string(ruleset).context("Failed to serialize as YAML")
        }
        RulesetFormat::Toml => {
            toml::to_string_pretty(ruleset).context("Failed to serialize as TOML")
        }
    }
}

/// Print detailed ruleset information
fn print_ruleset_details(ruleset: &Ruleset) -> Result<()> {
    println!("{}", "Ruleset Details".bold());
    println!("{}", "═".repeat(60));
    println!();

    println!("{}", "Metadata:".bold());
    println!("  {}: {}", "Name".dimmed(), ruleset.metadata.name);
    println!("  {}: {}", "Version".dimmed(), ruleset.metadata.version);
    println!("  {}: {}", "Category".dimmed(), ruleset.metadata.category);
    println!(
        "  {}: {}",
        "Description".dimmed(),
        ruleset.metadata.description
    );
    println!(
        "  {}: {}",
        "Authors".dimmed(),
        ruleset.metadata.authors.join(", ")
    );
    println!(
        "  {}: {}",
        "Languages".dimmed(),
        ruleset.metadata.languages.join(", ")
    );
    println!(
        "  {}: {}",
        "Tags".dimmed(),
        ruleset.metadata.tags.join(", ")
    );
    println!("  {}: {}", "License".dimmed(), ruleset.metadata.license);
    if let Some(ref repo) = ruleset.metadata.repository {
        println!("  {}: {}", "Repository".dimmed(), repo);
    }
    println!();

    println!("{} ({}):", "Rules".bold(), ruleset.rules.len());
    println!("{}", "─".repeat(60));

    for rule in &ruleset.rules {
        let effect_str = match rule.effect {
            RuleEffect::Check => "[check]".cyan(),
            RuleEffect::Fix => "[fix]".green(),
            RuleEffect::Both => "[both]".yellow(),
        };

        let severity_str = match rule.severity.as_str() {
            "critical" => rule.severity.magenta().bold(),
            "high" => rule.severity.red().bold(),
            "medium" => rule.severity.yellow(),
            "low" => rule.severity.cyan(),
            _ => rule.severity.blue(),
        };

        println!();
        println!(
            "  {} ({}) {} {}",
            rule.name.bold(),
            rule.id.dimmed(),
            effect_str,
            severity_str
        );
        println!("    {}", rule.description);

        if let Some(ref pattern) = rule.pattern {
            println!("    {}: {}", "Pattern".dimmed(), pattern.dimmed());
        }

        if let Some(ref fix) = rule.fix_template {
            println!("    {}: {}", "Fix".dimmed(), fix.green());
        }
    }

    println!();
    Ok(())
}
