// SPDX-License-Identifier: PMPL-1.0-or-later
//! Repository scanning command implementation.
//!
//! Scans a repository for CI/CD issues, policy violations, and anti-patterns.
//! Detects security issues in workflows, missing configurations, and more.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use chrono::{DateTime, Utc};
use clap::Args;
use colored::Colorize;
use indicatif::{ProgressBar, ProgressStyle};
use regex::Regex;
use serde::{Deserialize, Serialize};
use tracing::{debug, info};
use walkdir::WalkDir;

use super::{validate_git_repository, Category, Severity};
use crate::config::Config;
use crate::output::{OutputFormat, Outputter};

/// Arguments for the scan command
#[derive(Args, Debug)]
pub struct ScanArgs {
    /// Path to the repository to scan
    #[arg(default_value = ".")]
    pub repo_path: PathBuf,

    /// Minimum severity level to report
    #[arg(short, long, default_value = "low")]
    pub min_severity: SeverityFilter,

    /// Categories to scan (comma-separated, or 'all')
    #[arg(short, long, default_value = "all")]
    pub categories: String,

    /// Include auto-fixable issues only
    #[arg(long)]
    pub fixable_only: bool,

    /// Generate a detailed report
    #[arg(long)]
    pub detailed: bool,

    /// Save report to file
    #[arg(long)]
    pub report_file: Option<PathBuf>,

    /// Skip specific checks (comma-separated check IDs)
    #[arg(long)]
    pub skip: Option<String>,

    /// Include hidden files in scan
    #[arg(long)]
    pub include_hidden: bool,
}

/// Severity filter for command line
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SeverityFilter {
    Info,
    Low,
    Medium,
    High,
    Critical,
}

impl std::str::FromStr for SeverityFilter {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "info" => Ok(SeverityFilter::Info),
            "low" => Ok(SeverityFilter::Low),
            "medium" | "med" => Ok(SeverityFilter::Medium),
            "high" => Ok(SeverityFilter::High),
            "critical" | "crit" => Ok(SeverityFilter::Critical),
            _ => Err(format!("Invalid severity: {}", s)),
        }
    }
}

impl SeverityFilter {
    fn to_severity(self) -> Severity {
        match self {
            SeverityFilter::Info => Severity::Info,
            SeverityFilter::Low => Severity::Low,
            SeverityFilter::Medium => Severity::Medium,
            SeverityFilter::High => Severity::High,
            SeverityFilter::Critical => Severity::Critical,
        }
    }
}

/// A single finding from the scan
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Finding {
    /// Unique identifier for this finding type
    pub check_id: String,
    /// Human-readable title
    pub title: String,
    /// Detailed description
    pub description: String,
    /// Severity level
    pub severity: Severity,
    /// Category of the finding
    pub category: Category,
    /// File path where the issue was found
    pub file_path: Option<PathBuf>,
    /// Line number in the file
    pub line_number: Option<usize>,
    /// The problematic code or configuration
    pub snippet: Option<String>,
    /// Suggested fix
    pub suggestion: Option<String>,
    /// Whether this can be auto-fixed
    pub auto_fixable: bool,
    /// Related documentation URL
    pub docs_url: Option<String>,
    /// Rule that triggered this finding
    pub rule_id: Option<String>,
}

/// Complete scan results
#[derive(Debug, Serialize, Deserialize)]
pub struct ScanResults {
    /// Repository path that was scanned
    pub repository_path: PathBuf,
    /// Timestamp of the scan
    pub timestamp: DateTime<Utc>,
    /// Duration of the scan in milliseconds
    pub duration_ms: u64,
    /// All findings
    pub findings: Vec<Finding>,
    /// Summary statistics
    pub summary: ScanSummary,
    /// Metadata about the scan
    pub metadata: ScanMetadata,
}

/// Summary statistics for scan results
#[derive(Debug, Serialize, Deserialize)]
pub struct ScanSummary {
    /// Total number of files scanned
    pub files_scanned: usize,
    /// Total number of findings
    pub total_findings: usize,
    /// Findings by severity
    pub by_severity: HashMap<String, usize>,
    /// Findings by category
    pub by_category: HashMap<String, usize>,
    /// Number of auto-fixable issues
    pub auto_fixable: usize,
}

/// Metadata about the scan configuration
#[derive(Debug, Serialize, Deserialize)]
pub struct ScanMetadata {
    /// CLI version
    pub cli_version: String,
    /// Categories scanned
    pub categories: Vec<String>,
    /// Minimum severity used
    pub min_severity: String,
    /// Checks that were skipped
    pub skipped_checks: Vec<String>,
}

/// Execute the scan command
pub async fn execute(args: ScanArgs, _config: &Config, format: OutputFormat) -> Result<()> {
    let outputter = Outputter::new(format);
    let start_time = std::time::Instant::now();

    // Validate repository path
    let repo = validate_git_repository(&args.repo_path)?;
    let repo_path = repo.workdir().unwrap_or(repo.path()).to_path_buf();

    info!("Scanning repository: {}", repo_path.display());

    // Parse categories
    let categories = parse_categories(&args.categories);
    let skipped_checks = parse_skipped_checks(&args.skip);

    // Create progress bar
    let progress = if !matches!(format, OutputFormat::Json | OutputFormat::Yaml) {
        let pb = ProgressBar::new_spinner();
        pb.set_style(
            ProgressStyle::default_spinner()
                .template("{spinner:.green} {msg}")
                .unwrap(),
        );
        pb.set_message("Scanning repository...");
        Some(pb)
    } else {
        None
    };

    // Run all scanners
    let mut findings = Vec::new();
    let mut files_scanned = 0;

    // Scan GitHub workflows
    if categories.contains(&"security".to_string()) || categories.contains(&"all".to_string()) {
        if let Some(pb) = &progress {
            pb.set_message("Scanning GitHub workflows...");
        }
        let (workflow_findings, workflow_files) =
            scan_github_workflows(&repo_path, &skipped_checks)?;
        findings.extend(workflow_findings);
        files_scanned += workflow_files;
    }

    // Scan for security policy
    if categories.contains(&"policy".to_string()) || categories.contains(&"all".to_string()) {
        if let Some(pb) = &progress {
            pb.set_message("Checking security policies...");
        }
        findings.extend(scan_security_policy(&repo_path, &skipped_checks)?);
    }

    // Scan for license compliance
    if categories.contains(&"policy".to_string()) || categories.contains(&"all".to_string()) {
        if let Some(pb) = &progress {
            pb.set_message("Checking license compliance...");
        }
        findings.extend(scan_license_compliance(&repo_path, &skipped_checks)?);
    }

    // Scan for SPDX headers
    if categories.contains(&"policy".to_string()) || categories.contains(&"all".to_string()) {
        if let Some(pb) = &progress {
            pb.set_message("Checking SPDX headers...");
        }
        let (spdx_findings, spdx_files) =
            scan_spdx_headers(&repo_path, &skipped_checks, args.include_hidden)?;
        findings.extend(spdx_findings);
        files_scanned += spdx_files;
    }

    // Scan for configuration issues
    if categories.contains(&"configuration".to_string())
        || categories.contains(&"all".to_string())
    {
        if let Some(pb) = &progress {
            pb.set_message("Checking configurations...");
        }
        findings.extend(scan_configurations(&repo_path, &skipped_checks)?);
    }

    if let Some(pb) = &progress {
        pb.finish_with_message("Scan complete!");
    }

    // Filter findings by severity
    let min_severity = args.min_severity.to_severity();
    findings.retain(|f| f.severity >= min_severity);

    // Filter by fixable only if requested
    if args.fixable_only {
        findings.retain(|f| f.auto_fixable);
    }

    // Sort findings by severity (highest first)
    findings.sort_by(|a, b| b.severity.cmp(&a.severity));

    // Build summary
    let summary = build_summary(&findings, files_scanned);

    // Build metadata
    let metadata = ScanMetadata {
        cli_version: env!("CARGO_PKG_VERSION").to_string(),
        categories: categories.clone(),
        min_severity: format!("{:?}", args.min_severity),
        skipped_checks,
    };

    // Build results
    let results = ScanResults {
        repository_path: repo_path.clone(),
        timestamp: Utc::now(),
        duration_ms: start_time.elapsed().as_millis() as u64,
        findings,
        summary,
        metadata,
    };

    // Output results
    match format {
        OutputFormat::Json | OutputFormat::Yaml => {
            outputter.output(&results)?;
        }
        OutputFormat::Table => {
            output_table_format(&results, args.detailed)?;
        }
        OutputFormat::Plain => {
            output_plain_format(&results, args.detailed)?;
        }
    }

    // Save to file if requested
    if let Some(report_path) = args.report_file {
        let json_output = serde_json::to_string_pretty(&results)?;
        std::fs::write(&report_path, json_output)
            .with_context(|| format!("Failed to write report to: {}", report_path.display()))?;
        outputter.success(&format!("Report saved to: {}", report_path.display()))?;
    }

    // Exit with error if critical findings
    if results.summary.by_severity.get("critical").copied().unwrap_or(0) > 0 {
        std::process::exit(1);
    }

    Ok(())
}

fn parse_categories(input: &str) -> Vec<String> {
    if input.to_lowercase() == "all" {
        vec!["all".to_string()]
    } else {
        input
            .split(',')
            .map(|s| s.trim().to_lowercase())
            .filter(|s| !s.is_empty())
            .collect()
    }
}

fn parse_skipped_checks(input: &Option<String>) -> Vec<String> {
    input
        .as_ref()
        .map(|s| {
            s.split(',')
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .collect()
        })
        .unwrap_or_default()
}

fn build_summary(findings: &[Finding], files_scanned: usize) -> ScanSummary {
    let mut by_severity: HashMap<String, usize> = HashMap::new();
    let mut by_category: HashMap<String, usize> = HashMap::new();
    let mut auto_fixable = 0;

    for finding in findings {
        *by_severity
            .entry(finding.severity.to_string())
            .or_insert(0) += 1;
        *by_category
            .entry(finding.category.to_string())
            .or_insert(0) += 1;
        if finding.auto_fixable {
            auto_fixable += 1;
        }
    }

    ScanSummary {
        files_scanned,
        total_findings: findings.len(),
        by_severity,
        by_category,
        auto_fixable,
    }
}

/// Scan GitHub workflows for security issues
fn scan_github_workflows(
    repo_path: &Path,
    skipped_checks: &[String],
) -> Result<(Vec<Finding>, usize)> {
    let mut findings = Vec::new();
    let mut files_scanned = 0;
    let workflows_dir = repo_path.join(".github/workflows");

    if !workflows_dir.exists() {
        return Ok((findings, 0));
    }

    // Regex patterns for common issues
    let unpinned_action_regex = Regex::new(r"uses:\s*([^@]+)@(v\d+|main|master|latest)")?;
    let permissions_regex = Regex::new(r"^\s*permissions:\s*")?;
    let spdx_regex = Regex::new(r"^#\s*SPDX-License-Identifier:")?;

    for entry in WalkDir::new(&workflows_dir)
        .max_depth(1)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        let path = entry.path();
        if !path.is_file() {
            continue;
        }

        let extension = path.extension().and_then(|e| e.to_str());
        if extension != Some("yml") && extension != Some("yaml") {
            continue;
        }

        files_scanned += 1;
        let content = std::fs::read_to_string(path)?;
        let relative_path = path.strip_prefix(repo_path).unwrap_or(path);

        // Check for unpinned actions
        if !skipped_checks.contains(&"unpinned-actions".to_string()) {
            for (line_num, line) in content.lines().enumerate() {
                if let Some(captures) = unpinned_action_regex.captures(line) {
                    let action = captures.get(1).map(|m| m.as_str()).unwrap_or("");
                    let version = captures.get(2).map(|m| m.as_str()).unwrap_or("");

                    // Skip first-party GitHub actions at v3/v4
                    if action.starts_with("actions/") && (version == "v3" || version == "v4") {
                        debug!("Skipping first-party action: {}@{}", action, version);
                        continue;
                    }

                    findings.push(Finding {
                        check_id: "unpinned-actions".to_string(),
                        title: "Unpinned GitHub Action".to_string(),
                        description: format!(
                            "Action '{}' uses version '{}' instead of a SHA pin",
                            action, version
                        ),
                        severity: Severity::High,
                        category: Category::Security,
                        file_path: Some(relative_path.to_path_buf()),
                        line_number: Some(line_num + 1),
                        snippet: Some(line.trim().to_string()),
                        suggestion: Some(format!(
                            "Pin to a specific SHA: {}@<SHA> # {}",
                            action, version
                        )),
                        auto_fixable: true,
                        docs_url: Some(
                            "https://docs.github.com/en/actions/security-guides/security-hardening-for-github-actions#using-third-party-actions".to_string()
                        ),
                        rule_id: Some("SEC-WF-001".to_string()),
                    });
                }
            }
        }

        // Check for missing permissions
        if !skipped_checks.contains(&"missing-permissions".to_string()) {
            if !permissions_regex.is_match(&content) {
                findings.push(Finding {
                    check_id: "missing-permissions".to_string(),
                    title: "Missing Workflow Permissions".to_string(),
                    description: "Workflow does not declare explicit permissions".to_string(),
                    severity: Severity::Medium,
                    category: Category::Security,
                    file_path: Some(relative_path.to_path_buf()),
                    line_number: None,
                    snippet: None,
                    suggestion: Some("Add 'permissions: read-all' at workflow level".to_string()),
                    auto_fixable: true,
                    docs_url: Some(
                        "https://docs.github.com/en/actions/security-guides/automatic-token-authentication#modifying-the-permissions-for-the-github_token".to_string()
                    ),
                    rule_id: Some("SEC-WF-002".to_string()),
                });
            }
        }

        // Check for missing SPDX header
        if !skipped_checks.contains(&"missing-spdx".to_string()) {
            if !spdx_regex.is_match(&content) {
                findings.push(Finding {
                    check_id: "missing-spdx".to_string(),
                    title: "Missing SPDX License Header".to_string(),
                    description: "Workflow file does not have SPDX license identifier".to_string(),
                    severity: Severity::Low,
                    category: Category::Policy,
                    file_path: Some(relative_path.to_path_buf()),
                    line_number: Some(1),
                    snippet: None,
                    suggestion: Some(
                        "Add '# SPDX-License-Identifier: PMPL-1.0-or-later' as first line"
                            .to_string(),
                    ),
                    auto_fixable: true,
                    docs_url: Some("https://spdx.dev/learn/handling-license-info/".to_string()),
                    rule_id: Some("LIC-001".to_string()),
                });
            }
        }
    }

    Ok((findings, files_scanned))
}

/// Scan for security policy (SECURITY.md)
fn scan_security_policy(repo_path: &Path, skipped_checks: &[String]) -> Result<Vec<Finding>> {
    let mut findings = Vec::new();

    if skipped_checks.contains(&"missing-security-policy".to_string()) {
        return Ok(findings);
    }

    let security_paths = ["SECURITY.md", "security.md", ".github/SECURITY.md"];

    let has_security = security_paths
        .iter()
        .any(|p| repo_path.join(p).exists());

    if !has_security {
        findings.push(Finding {
            check_id: "missing-security-policy".to_string(),
            title: "Missing SECURITY.md".to_string(),
            description: "Repository does not have a security policy file".to_string(),
            severity: Severity::Medium,
            category: Category::Policy,
            file_path: None,
            line_number: None,
            snippet: None,
            suggestion: Some("Create a SECURITY.md file with vulnerability reporting instructions".to_string()),
            auto_fixable: true,
            docs_url: Some("https://docs.github.com/en/code-security/getting-started/adding-a-security-policy-to-your-repository".to_string()),
            rule_id: Some("POL-SEC-001".to_string()),
        });
    }

    Ok(findings)
}

/// Scan for license compliance
fn scan_license_compliance(repo_path: &Path, skipped_checks: &[String]) -> Result<Vec<Finding>> {
    let mut findings = Vec::new();

    if skipped_checks.contains(&"missing-license".to_string()) {
        return Ok(findings);
    }

    let license_paths = [
        "LICENSE",
        "LICENSE.md",
        "LICENSE.txt",
        "COPYING",
        "COPYING.md",
    ];

    let has_license = license_paths.iter().any(|p| repo_path.join(p).exists());

    if !has_license {
        findings.push(Finding {
            check_id: "missing-license".to_string(),
            title: "Missing LICENSE File".to_string(),
            description: "Repository does not have a license file".to_string(),
            severity: Severity::High,
            category: Category::Policy,
            file_path: None,
            line_number: None,
            snippet: None,
            suggestion: Some("Add a LICENSE file to specify the project's license".to_string()),
            auto_fixable: false,
            docs_url: Some("https://choosealicense.com/".to_string()),
            rule_id: Some("POL-LIC-001".to_string()),
        });
    }

    Ok(findings)
}

/// Scan source files for SPDX headers
fn scan_spdx_headers(
    repo_path: &Path,
    skipped_checks: &[String],
    include_hidden: bool,
) -> Result<(Vec<Finding>, usize)> {
    let mut findings = Vec::new();
    let mut files_scanned = 0;

    if skipped_checks.contains(&"missing-spdx-source".to_string()) {
        return Ok((findings, 0));
    }

    let source_extensions = [
        "rs", "js", "ts", "jsx", "tsx", "res", "py", "go", "java", "kt", "rb", "ml", "hs", "ex",
        "exs", "gleam", "jl", "sh", "bash", "lgt", "pl", "pro",
    ];

    let spdx_pattern = Regex::new(r"SPDX-License-Identifier:")?;

    for entry in WalkDir::new(repo_path)
        .into_iter()
        .filter_entry(|e| {
            let name = e.file_name().to_str().unwrap_or("");
            // Skip common non-source directories
            !name.starts_with('.') || include_hidden
        })
        .filter_map(|e| e.ok())
    {
        let path = entry.path();

        // Skip common directories
        let path_str = path.to_string_lossy();
        if path_str.contains("/target/")
            || path_str.contains("/node_modules/")
            || path_str.contains("/dist/")
            || path_str.contains("/.git/")
            || path_str.contains("/vendor/")
        {
            continue;
        }

        if !path.is_file() {
            continue;
        }

        let extension = path.extension().and_then(|e| e.to_str());
        if !source_extensions.iter().any(|ext| extension == Some(ext)) {
            continue;
        }

        files_scanned += 1;

        // Read first few lines to check for SPDX
        let file_content = match std::fs::read_to_string(path) {
            Ok(c) => c,
            Err(_) => continue, // Skip unreadable files
        };

        // Check first 10 lines for SPDX header
        let first_lines: String = file_content.lines().take(10).collect::<Vec<_>>().join("\n");

        if !spdx_pattern.is_match(&first_lines) {
            let relative_path = path.strip_prefix(repo_path).unwrap_or(path);
            findings.push(Finding {
                check_id: "missing-spdx-source".to_string(),
                title: "Missing SPDX Header in Source File".to_string(),
                description: "Source file does not have SPDX license identifier".to_string(),
                severity: Severity::Info,
                category: Category::Policy,
                file_path: Some(relative_path.to_path_buf()),
                line_number: Some(1),
                snippet: None,
                suggestion: Some(
                    "Add SPDX-License-Identifier comment at the top of the file".to_string(),
                ),
                auto_fixable: true,
                docs_url: Some("https://spdx.dev/learn/handling-license-info/".to_string()),
                rule_id: Some("LIC-002".to_string()),
            });
        }
    }

    Ok((findings, files_scanned))
}

/// Scan for configuration issues
fn scan_configurations(repo_path: &Path, skipped_checks: &[String]) -> Result<Vec<Finding>> {
    let mut findings = Vec::new();

    // Check for .gitignore
    if !skipped_checks.contains(&"missing-gitignore".to_string()) {
        if !repo_path.join(".gitignore").exists() {
            findings.push(Finding {
                check_id: "missing-gitignore".to_string(),
                title: "Missing .gitignore".to_string(),
                description: "Repository does not have a .gitignore file".to_string(),
                severity: Severity::Low,
                category: Category::Configuration,
                file_path: None,
                line_number: None,
                snippet: None,
                suggestion: Some("Add a .gitignore file appropriate for your project".to_string()),
                auto_fixable: false,
                docs_url: Some("https://git-scm.com/docs/gitignore".to_string()),
                rule_id: Some("CFG-001".to_string()),
            });
        }
    }

    // Check for README
    if !skipped_checks.contains(&"missing-readme".to_string()) {
        let readme_paths = [
            "README.md",
            "README.adoc",
            "README.rst",
            "README.txt",
            "README",
        ];
        let has_readme = readme_paths.iter().any(|p| repo_path.join(p).exists());

        if !has_readme {
            findings.push(Finding {
                check_id: "missing-readme".to_string(),
                title: "Missing README".to_string(),
                description: "Repository does not have a README file".to_string(),
                severity: Severity::Low,
                category: Category::Documentation,
                file_path: None,
                line_number: None,
                snippet: None,
                suggestion: Some("Add a README.md or README.adoc file".to_string()),
                auto_fixable: false,
                docs_url: None,
                rule_id: Some("DOC-001".to_string()),
            });
        }
    }

    // Check for CONTRIBUTING
    if !skipped_checks.contains(&"missing-contributing".to_string()) {
        let contrib_paths = ["CONTRIBUTING.md", "CONTRIBUTING.adoc", ".github/CONTRIBUTING.md"];
        let has_contrib = contrib_paths.iter().any(|p| repo_path.join(p).exists());

        if !has_contrib {
            findings.push(Finding {
                check_id: "missing-contributing".to_string(),
                title: "Missing CONTRIBUTING Guide".to_string(),
                description: "Repository does not have contribution guidelines".to_string(),
                severity: Severity::Info,
                category: Category::Documentation,
                file_path: None,
                line_number: None,
                snippet: None,
                suggestion: Some("Add CONTRIBUTING.md with contribution guidelines".to_string()),
                auto_fixable: false,
                docs_url: Some("https://docs.github.com/en/communities/setting-up-your-project-for-healthy-contributions/setting-guidelines-for-repository-contributors".to_string()),
                rule_id: Some("DOC-002".to_string()),
            });
        }
    }

    Ok(findings)
}

fn output_plain_format(results: &ScanResults, detailed: bool) -> Result<()> {
    println!(
        "\n{} {}",
        "Scan Results for:".bold(),
        results.repository_path.display()
    );
    println!(
        "{} {} ms\n",
        "Scan duration:".dimmed(),
        results.duration_ms
    );

    if results.findings.is_empty() {
        println!("{}", "No issues found.".green().bold());
        return Ok(());
    }

    // Print summary
    println!("{}", "Summary:".bold());
    println!("  Total findings: {}", results.summary.total_findings);
    println!("  Files scanned: {}", results.summary.files_scanned);
    println!("  Auto-fixable: {}", results.summary.auto_fixable);
    println!();

    // Print by severity
    let severity_order = ["critical", "high", "medium", "low", "info"];
    for sev in severity_order {
        if let Some(count) = results.summary.by_severity.get(sev) {
            let colored_sev = match sev {
                "critical" => sev.magenta().bold(),
                "high" => sev.red().bold(),
                "medium" => sev.yellow(),
                "low" => sev.cyan(),
                "info" => sev.blue(),
                _ => sev.normal(),
            };
            println!("  {}: {}", colored_sev, count);
        }
    }
    println!();

    // Print findings
    println!("{}", "Findings:".bold());
    println!("{}", "─".repeat(60));

    for finding in &results.findings {
        let severity_colored = match finding.severity {
            Severity::Critical => finding.severity.to_string().magenta().bold(),
            Severity::High => finding.severity.to_string().red().bold(),
            Severity::Medium => finding.severity.to_string().yellow(),
            Severity::Low => finding.severity.to_string().cyan(),
            Severity::Info => finding.severity.to_string().blue(),
        };

        println!(
            "[{}] {} ({})",
            severity_colored,
            finding.title.bold(),
            finding.check_id.dimmed()
        );

        if let Some(path) = &finding.file_path {
            let location = match finding.line_number {
                Some(line) => format!("{}:{}", path.display(), line),
                None => path.display().to_string(),
            };
            println!("  {}: {}", "Location".dimmed(), location);
        }

        println!("  {}", finding.description);

        if detailed {
            if let Some(snippet) = &finding.snippet {
                println!("  {}: {}", "Code".dimmed(), snippet.dimmed());
            }
            if let Some(suggestion) = &finding.suggestion {
                println!("  {}: {}", "Fix".green(), suggestion);
            }
            if let Some(url) = &finding.docs_url {
                println!("  {}: {}", "Docs".dimmed(), url);
            }
        } else if let Some(suggestion) = &finding.suggestion {
            println!("  {}: {}", "Fix".green(), suggestion);
        }

        if finding.auto_fixable {
            println!("  {}", "[auto-fixable]".green());
        }
        println!();
    }

    Ok(())
}

fn output_table_format(results: &ScanResults, _detailed: bool) -> Result<()> {
    use tabled::{settings::Style, Table, Tabled};

    #[derive(Tabled)]
    struct FindingRow {
        #[tabled(rename = "Severity")]
        severity: String,
        #[tabled(rename = "Check ID")]
        check_id: String,
        #[tabled(rename = "Title")]
        title: String,
        #[tabled(rename = "File")]
        file: String,
        #[tabled(rename = "Fixable")]
        fixable: String,
    }

    let rows: Vec<FindingRow> = results
        .findings
        .iter()
        .map(|f| FindingRow {
            severity: f.severity.to_string().to_uppercase(),
            check_id: f.check_id.clone(),
            title: if f.title.len() > 40 {
                format!("{}...", &f.title[..37])
            } else {
                f.title.clone()
            },
            file: f
                .file_path
                .as_ref()
                .map(|p| {
                    let s = p.display().to_string();
                    if s.len() > 30 {
                        format!("...{}", &s[s.len() - 27..])
                    } else {
                        s
                    }
                })
                .unwrap_or_else(|| "-".to_string()),
            fixable: if f.auto_fixable { "Yes" } else { "No" }.to_string(),
        })
        .collect();

    if rows.is_empty() {
        println!("No issues found.");
        return Ok(());
    }

    let table = Table::new(rows).with(Style::rounded()).to_string();
    println!("{}", table);

    println!(
        "\nTotal: {} findings ({} auto-fixable)",
        results.summary.total_findings, results.summary.auto_fixable
    );

    Ok(())
}
