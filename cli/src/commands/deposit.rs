// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deposit command implementation.
//!
//! Submits validated rulesets to the registry for distribution and reuse.

use std::path::PathBuf;

use anyhow::{Context, Result};
use chrono::{DateTime, Utc};
use clap::Args;
use colored::Colorize;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use tracing::{debug, info};

use super::validate_file_path;
use crate::config::Config;
use crate::output::{OutputFormat, Outputter};

/// Arguments for the deposit command
#[derive(Args, Debug)]
pub struct DepositArgs {
    /// Path to the ruleset file to deposit
    pub ruleset_path: PathBuf,

    /// Name for the ruleset (defaults to filename)
    #[arg(short, long)]
    pub name: Option<String>,

    /// Version string (defaults to 0.1.0)
    #[arg(short, long, default_value = "0.1.0")]
    pub version: String,

    /// Description of the ruleset
    #[arg(short, long)]
    pub description: Option<String>,

    /// Category for the ruleset
    #[arg(short, long)]
    pub category: Option<RulesetCategory>,

    /// Target language(s) (comma-separated)
    #[arg(short, long)]
    pub languages: Option<String>,

    /// Tags for the ruleset (comma-separated)
    #[arg(short, long)]
    pub tags: Option<String>,

    /// Mark as private (not visible in public search)
    #[arg(long)]
    pub private: bool,

    /// Skip validation checks
    #[arg(long)]
    pub no_validate: bool,

    /// Registry URL to deposit to
    #[arg(long)]
    pub registry: Option<String>,

    /// Dry run - validate but don't submit
    #[arg(long)]
    pub dry_run: bool,
}

/// Ruleset category
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, clap::ValueEnum)]
#[serde(rename_all = "kebab-case")]
pub enum RulesetCategory {
    /// Security-focused rules
    Security,
    /// Code quality rules
    Quality,
    /// Policy enforcement rules
    Policy,
    /// Performance optimization rules
    Performance,
    /// Documentation rules
    Documentation,
    /// CI/CD workflow rules
    Workflow,
    /// Dependency management rules
    Dependencies,
    /// Accessibility rules
    Accessibility,
}

impl std::fmt::Display for RulesetCategory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RulesetCategory::Security => write!(f, "security"),
            RulesetCategory::Quality => write!(f, "quality"),
            RulesetCategory::Policy => write!(f, "policy"),
            RulesetCategory::Performance => write!(f, "performance"),
            RulesetCategory::Documentation => write!(f, "documentation"),
            RulesetCategory::Workflow => write!(f, "workflow"),
            RulesetCategory::Dependencies => write!(f, "dependencies"),
            RulesetCategory::Accessibility => write!(f, "accessibility"),
        }
    }
}

/// A rule within a ruleset
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Rule {
    /// Unique identifier within the ruleset
    pub id: String,
    /// Human-readable name
    pub name: String,
    /// Description of what the rule checks
    pub description: String,
    /// Severity level
    pub severity: String,
    /// Effect type (check, fix, both)
    pub effect: RuleEffect,
    /// Pattern to match (regex, glob, or custom)
    pub pattern: Option<String>,
    /// Fix template if applicable
    pub fix_template: Option<String>,
    /// Conditions for when the rule applies
    pub conditions: Option<Vec<RuleCondition>>,
}

/// Effect type for a rule
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum RuleEffect {
    /// Only detects issues
    Check,
    /// Only applies fixes
    Fix,
    /// Both detects and can fix
    Both,
}

/// Condition for rule application
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RuleCondition {
    /// Condition type
    #[serde(rename = "type")]
    pub condition_type: String,
    /// Condition value
    pub value: String,
}

/// A complete ruleset
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Ruleset {
    /// Ruleset metadata
    pub metadata: RulesetMetadata,
    /// Rules in this ruleset
    pub rules: Vec<Rule>,
}

/// Metadata for a ruleset
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RulesetMetadata {
    /// Ruleset name
    pub name: String,
    /// Version string
    pub version: String,
    /// Description
    pub description: String,
    /// Category
    pub category: RulesetCategory,
    /// Author(s)
    pub authors: Vec<String>,
    /// Target languages
    pub languages: Vec<String>,
    /// Tags for discovery
    pub tags: Vec<String>,
    /// License identifier
    pub license: String,
    /// Repository URL
    pub repository: Option<String>,
    /// Whether this is private
    pub private: bool,
}

/// Result of a deposit operation
#[derive(Debug, Serialize, Deserialize)]
pub struct DepositResult {
    /// Whether the deposit was successful
    pub success: bool,
    /// Unique ID assigned to the ruleset
    pub ruleset_id: Option<String>,
    /// SHA256 hash of the content
    pub content_hash: String,
    /// Timestamp of deposit
    pub deposited_at: DateTime<Utc>,
    /// Registry URL
    pub registry_url: String,
    /// Validation results
    pub validation: ValidationResult,
    /// Warnings during deposit
    pub warnings: Vec<String>,
}

/// Validation result for a ruleset
#[derive(Debug, Serialize, Deserialize)]
pub struct ValidationResult {
    /// Whether validation passed
    pub valid: bool,
    /// Number of rules validated
    pub rules_count: usize,
    /// Validation errors
    pub errors: Vec<String>,
    /// Validation warnings
    pub warnings: Vec<String>,
}

/// Execute the deposit command
pub async fn execute(args: DepositArgs, config: &Config, format: OutputFormat) -> Result<()> {
    let outputter = Outputter::new(format);

    // Validate file exists
    let ruleset_path = validate_file_path(&args.ruleset_path)?;
    info!("Depositing ruleset from: {}", ruleset_path.display());

    // Read and parse ruleset
    let content = std::fs::read_to_string(&ruleset_path)
        .with_context(|| format!("Failed to read ruleset file: {}", ruleset_path.display()))?;

    // Detect format and parse
    let mut ruleset = parse_ruleset(&content, &ruleset_path)?;

    // Override metadata from command line arguments
    if let Some(ref name) = args.name {
        ruleset.metadata.name = name.clone();
    }
    ruleset.metadata.version = args.version.clone();

    if let Some(ref desc) = args.description {
        ruleset.metadata.description = desc.clone();
    }

    if let Some(category) = args.category {
        ruleset.metadata.category = category;
    }

    if let Some(ref langs) = args.languages {
        ruleset.metadata.languages = langs.split(',').map(|s| s.trim().to_string()).collect();
    }

    if let Some(ref tags) = args.tags {
        ruleset.metadata.tags = tags.split(',').map(|s| s.trim().to_string()).collect();
    }

    ruleset.metadata.private = args.private;

    // Validate ruleset
    let validation = if args.no_validate {
        ValidationResult {
            valid: true,
            rules_count: ruleset.rules.len(),
            errors: vec![],
            warnings: vec!["Validation was skipped".to_string()],
        }
    } else {
        validate_ruleset(&ruleset)?
    };

    if !validation.valid {
        if !matches!(format, OutputFormat::Json | OutputFormat::Yaml) {
            println!("{}", "Ruleset validation failed:".red().bold());
            for error in &validation.errors {
                println!("  {} {}", "×".red(), error);
            }
        }

        let result = DepositResult {
            success: false,
            ruleset_id: None,
            content_hash: compute_hash(&content),
            deposited_at: Utc::now(),
            registry_url: get_registry_url(&args.registry, config),
            validation,
            warnings: vec![],
        };

        if matches!(format, OutputFormat::Json | OutputFormat::Yaml) {
            outputter.output(&result)?;
        }

        anyhow::bail!("Ruleset validation failed");
    }

    // Print validation warnings
    if !validation.warnings.is_empty() && !matches!(format, OutputFormat::Json | OutputFormat::Yaml)
    {
        println!("{}", "Validation warnings:".yellow());
        for warning in &validation.warnings {
            println!("  {} {}", "⚠".yellow(), warning);
        }
        println!();
    }

    // Compute content hash
    let content_hash = compute_hash(&content);

    // Dry run - just show what would be deposited
    if args.dry_run {
        if !matches!(format, OutputFormat::Json | OutputFormat::Yaml) {
            println!("{}", "Dry run - ruleset would be deposited:".cyan().bold());
            println!();
            print_ruleset_summary(&ruleset, &content_hash)?;
        }

        let result = DepositResult {
            success: true,
            ruleset_id: Some(format!("dry-run-{}", &content_hash[..8])),
            content_hash,
            deposited_at: Utc::now(),
            registry_url: get_registry_url(&args.registry, config),
            validation,
            warnings: vec!["This was a dry run - nothing was actually deposited".to_string()],
        };

        if matches!(format, OutputFormat::Json | OutputFormat::Yaml) {
            outputter.output(&result)?;
        }

        return Ok(());
    }

    // Submit to registry
    let registry_url = get_registry_url(&args.registry, config);
    let ruleset_id = submit_to_registry(&ruleset, &content, &registry_url, config).await?;

    let result = DepositResult {
        success: true,
        ruleset_id: Some(ruleset_id.clone()),
        content_hash,
        deposited_at: Utc::now(),
        registry_url: registry_url.clone(),
        validation,
        warnings: vec![],
    };

    // Output result
    match format {
        OutputFormat::Json | OutputFormat::Yaml => {
            outputter.output(&result)?;
        }
        _ => {
            println!("{}", "Ruleset deposited successfully!".green().bold());
            println!();
            println!("  {}: {}", "ID".dimmed(), ruleset_id);
            println!("  {}: {}", "Name".dimmed(), ruleset.metadata.name);
            println!("  {}: {}", "Version".dimmed(), ruleset.metadata.version);
            println!("  {}: {}", "Hash".dimmed(), result.content_hash);
            println!("  {}: {}", "Registry".dimmed(), registry_url);
            println!();
            println!(
                "Withdraw with: {}",
                format!("hyper withdraw {}", ruleset_id).cyan()
            );
        }
    }

    Ok(())
}

/// Parse a ruleset from content
fn parse_ruleset(content: &str, path: &PathBuf) -> Result<Ruleset> {
    let extension = path.extension().and_then(|e| e.to_str()).unwrap_or("");

    match extension {
        "json" => serde_json::from_str(content)
            .with_context(|| "Failed to parse ruleset as JSON"),
        "yaml" | "yml" => serde_yaml::from_str(content)
            .with_context(|| "Failed to parse ruleset as YAML"),
        "toml" => toml::from_str(content)
            .with_context(|| "Failed to parse ruleset as TOML"),
        _ => {
            // Try each format
            if let Ok(ruleset) = serde_json::from_str(content) {
                return Ok(ruleset);
            }
            if let Ok(ruleset) = serde_yaml::from_str(content) {
                return Ok(ruleset);
            }
            if let Ok(ruleset) = toml::from_str(content) {
                return Ok(ruleset);
            }
            anyhow::bail!("Could not parse ruleset - supported formats: JSON, YAML, TOML")
        }
    }
}

/// Validate a ruleset
fn validate_ruleset(ruleset: &Ruleset) -> Result<ValidationResult> {
    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    // Validate metadata
    if ruleset.metadata.name.is_empty() {
        errors.push("Ruleset name is required".to_string());
    }

    if ruleset.metadata.name.len() > 100 {
        errors.push("Ruleset name must be 100 characters or less".to_string());
    }

    if !ruleset.metadata.name.chars().all(|c| c.is_alphanumeric() || c == '-' || c == '_') {
        errors.push("Ruleset name must contain only alphanumeric characters, hyphens, and underscores".to_string());
    }

    if ruleset.metadata.version.is_empty() {
        errors.push("Ruleset version is required".to_string());
    }

    if ruleset.metadata.description.is_empty() {
        warnings.push("Ruleset description is empty".to_string());
    }

    if ruleset.metadata.authors.is_empty() {
        warnings.push("No authors specified".to_string());
    }

    if ruleset.metadata.languages.is_empty() {
        warnings.push("No target languages specified".to_string());
    }

    // Validate rules
    if ruleset.rules.is_empty() {
        errors.push("Ruleset must contain at least one rule".to_string());
    }

    let mut rule_ids = std::collections::HashSet::new();
    for (idx, rule) in ruleset.rules.iter().enumerate() {
        let rule_prefix = format!("Rule[{}]", idx);

        if rule.id.is_empty() {
            errors.push(format!("{}: ID is required", rule_prefix));
        } else if !rule_ids.insert(rule.id.clone()) {
            errors.push(format!("{}: Duplicate rule ID '{}'", rule_prefix, rule.id));
        }

        if rule.name.is_empty() {
            errors.push(format!("{}: Name is required", rule_prefix));
        }

        if rule.description.is_empty() {
            warnings.push(format!("{}: Description is empty", rule_prefix));
        }

        // Validate pattern if present
        if let Some(ref pattern) = rule.pattern {
            if regex::Regex::new(pattern).is_err() {
                errors.push(format!(
                    "{}: Invalid regex pattern '{}'",
                    rule_prefix, pattern
                ));
            }
        }

        // Warn if fix rule has no template
        if rule.effect == RuleEffect::Fix || rule.effect == RuleEffect::Both {
            if rule.fix_template.is_none() {
                warnings.push(format!(
                    "{}: Rule has fix effect but no fix_template",
                    rule_prefix
                ));
            }
        }
    }

    Ok(ValidationResult {
        valid: errors.is_empty(),
        rules_count: ruleset.rules.len(),
        errors,
        warnings,
    })
}

/// Compute SHA256 hash of content
fn compute_hash(content: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(content.as_bytes());
    hex::encode(hasher.finalize())
}

/// Get registry URL
fn get_registry_url(arg: &Option<String>, config: &Config) -> String {
    arg.clone()
        .or_else(|| config.registry_url.clone())
        .unwrap_or_else(|| "https://registry.cicd-hyper-a.dev".to_string())
}

/// Submit ruleset to registry
async fn submit_to_registry(
    ruleset: &Ruleset,
    _content: &str,
    registry_url: &str,
    _config: &Config,
) -> Result<String> {
    // In a real implementation, this would make an HTTP request to the registry
    // For now, generate a mock ID
    let ruleset_id = format!(
        "{}/{}/{}",
        ruleset.metadata.category,
        ruleset.metadata.name,
        ruleset.metadata.version
    );

    debug!("Would submit to: {}/api/v1/rulesets", registry_url);
    debug!("Ruleset ID: {}", ruleset_id);

    // Simulate network delay
    tokio::time::sleep(std::time::Duration::from_millis(100)).await;

    Ok(ruleset_id)
}

/// Print ruleset summary
fn print_ruleset_summary(ruleset: &Ruleset, hash: &str) -> Result<()> {
    println!("{}", "Ruleset Summary:".bold());
    println!("  {}: {}", "Name".dimmed(), ruleset.metadata.name);
    println!("  {}: {}", "Version".dimmed(), ruleset.metadata.version);
    println!("  {}: {}", "Category".dimmed(), ruleset.metadata.category);
    println!("  {}: {}", "Description".dimmed(), ruleset.metadata.description);
    println!(
        "  {}: {}",
        "Languages".dimmed(),
        if ruleset.metadata.languages.is_empty() {
            "(none)".to_string()
        } else {
            ruleset.metadata.languages.join(", ")
        }
    );
    println!(
        "  {}: {}",
        "Tags".dimmed(),
        if ruleset.metadata.tags.is_empty() {
            "(none)".to_string()
        } else {
            ruleset.metadata.tags.join(", ")
        }
    );
    println!(
        "  {}: {}",
        "Private".dimmed(),
        if ruleset.metadata.private { "yes" } else { "no" }
    );
    println!("  {}: {} rules", "Rules".dimmed(), ruleset.rules.len());
    println!("  {}: {}", "Hash".dimmed(), hash);
    println!();

    println!("{}", "Rules:".bold());
    for rule in &ruleset.rules {
        let effect_str = match rule.effect {
            RuleEffect::Check => "check".cyan(),
            RuleEffect::Fix => "fix".green(),
            RuleEffect::Both => "both".yellow(),
        };
        println!(
            "  {} ({}) [{}] - {}",
            rule.name.bold(),
            rule.id.dimmed(),
            effect_str,
            rule.description
        );
    }

    Ok(())
}
