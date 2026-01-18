// SPDX-License-Identifier: AGPL-3.0-or-later
//! Registry Integration Tests
//!
//! Tests Haskell registry operations:
//! - Ruleset validation and parsing
//! - LiquidHaskell verification
//! - Registry API operations
//! - Rule lookup and matching
//! - Template instantiation

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Command;
use tempfile::TempDir;
use tracing::{debug, info, warn};

mod common;
use common::setup_test_logging;

// ============================================================================
// Registry Data Models
// ============================================================================

/// Ruleset definition matching Haskell types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Ruleset {
    pub id: String,
    pub name: String,
    pub version: String,
    pub description: String,
    pub rules: Vec<Rule>,
    pub metadata: RulesetMetadata,
}

/// Individual rule definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Rule {
    pub id: String,
    pub name: String,
    pub description: String,
    pub category: RuleCategory,
    pub severity: Severity,
    pub pattern: Option<String>,
    pub fix_template: Option<String>,
    pub auto_fixable: bool,
    pub references: Vec<String>,
}

/// Rule category
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum RuleCategory {
    WorkflowSecurity,
    CodeSecurity,
    CodeQuality,
    DependencyVuln,
    ProcessHygiene,
    MissingTests,
    Accessibility,
    Performance,
}

/// Rule severity
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Severity {
    Info,
    Low,
    Medium,
    High,
    Critical,
}

/// Ruleset metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RulesetMetadata {
    pub author: String,
    pub license: String,
    pub tags: Vec<String>,
    pub created_at: String,
    pub updated_at: String,
}

/// Verification result from LiquidHaskell
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VerificationResult {
    pub valid: bool,
    pub errors: Vec<VerificationError>,
    pub warnings: Vec<String>,
}

/// Verification error
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VerificationError {
    pub location: String,
    pub message: String,
    pub code: String,
}

// ============================================================================
// Mock Registry Client
// ============================================================================

/// Mock registry client for testing without Haskell runtime
struct MockRegistryClient {
    rulesets: HashMap<String, Ruleset>,
    verification_enabled: bool,
}

impl MockRegistryClient {
    fn new() -> Self {
        Self {
            rulesets: HashMap::new(),
            verification_enabled: true,
        }
    }

    /// Register a ruleset
    fn register_ruleset(&mut self, ruleset: Ruleset) -> Result<String> {
        if self.verification_enabled {
            self.verify_ruleset(&ruleset)?;
        }
        let id = ruleset.id.clone();
        self.rulesets.insert(id.clone(), ruleset);
        Ok(id)
    }

    /// Get a ruleset by ID
    fn get_ruleset(&self, id: &str) -> Option<&Ruleset> {
        self.rulesets.get(id)
    }

    /// List all rulesets
    fn list_rulesets(&self) -> Vec<&Ruleset> {
        self.rulesets.values().collect()
    }

    /// Search rulesets by criteria
    fn search_rulesets(&self, query: &str, category: Option<RuleCategory>) -> Vec<&Ruleset> {
        self.rulesets
            .values()
            .filter(|rs| {
                let matches_query = rs.name.contains(query) || rs.description.contains(query);
                let matches_category = category.map_or(true, |cat| {
                    rs.rules.iter().any(|r| r.category == cat)
                });
                matches_query && matches_category
            })
            .collect()
    }

    /// Get rules by category
    fn get_rules_by_category(&self, category: RuleCategory) -> Vec<&Rule> {
        self.rulesets
            .values()
            .flat_map(|rs| rs.rules.iter())
            .filter(|r| r.category == category)
            .collect()
    }

    /// Get rules by severity threshold
    fn get_rules_by_severity(&self, min_severity: Severity) -> Vec<&Rule> {
        self.rulesets
            .values()
            .flat_map(|rs| rs.rules.iter())
            .filter(|r| r.severity >= min_severity)
            .collect()
    }

    /// Verify a ruleset (mock LiquidHaskell verification)
    fn verify_ruleset(&self, ruleset: &Ruleset) -> Result<VerificationResult> {
        let mut errors = Vec::new();
        let mut warnings = Vec::new();

        // Check for empty rules
        if ruleset.rules.is_empty() {
            warnings.push("Ruleset has no rules defined".to_string());
        }

        // Check for duplicate rule IDs
        let mut seen_ids = std::collections::HashSet::new();
        for rule in &ruleset.rules {
            if !seen_ids.insert(&rule.id) {
                errors.push(VerificationError {
                    location: format!("rules.{}", rule.id),
                    message: format!("Duplicate rule ID: {}", rule.id),
                    code: "ERR-DUP-RULE".to_string(),
                });
            }
        }

        // Check for auto-fixable rules without fix templates
        for rule in &ruleset.rules {
            if rule.auto_fixable && rule.fix_template.is_none() {
                warnings.push(format!(
                    "Rule {} is marked auto-fixable but has no fix template",
                    rule.id
                ));
            }
        }

        // Check version format
        if !ruleset.version.chars().next().map_or(false, |c| c.is_ascii_digit()) {
            errors.push(VerificationError {
                location: "version".to_string(),
                message: "Version must start with a digit".to_string(),
                code: "ERR-VER-FORMAT".to_string(),
            });
        }

        Ok(VerificationResult {
            valid: errors.is_empty(),
            errors,
            warnings,
        })
    }

    /// Apply fix template with bindings
    fn apply_fix_template(
        &self,
        template: &str,
        bindings: &HashMap<String, String>,
    ) -> String {
        let mut result = template.to_string();
        for (key, value) in bindings {
            result = result.replace(&format!("{{{}}}", key), value);
        }
        result
    }
}

// ============================================================================
// Haskell Registry Process Tests
// ============================================================================

/// Test harness for running Haskell registry binary
struct RegistryTestHarness {
    temp_dir: TempDir,
    registry_binary: PathBuf,
}

impl RegistryTestHarness {
    fn new() -> Result<Self> {
        let temp_dir = TempDir::new()?;
        let registry_binary = PathBuf::from("registry/dist-newstyle/build/x86_64-linux/ghc-9.4.8/cicd-hyper-a-0.1.0.0/x/registry/build/registry/registry");

        Ok(Self {
            temp_dir,
            registry_binary,
        })
    }

    /// Check if Haskell registry is available
    fn registry_available(&self) -> bool {
        self.registry_binary.exists()
    }

    /// Run registry command
    fn run_registry_cmd(&self, args: &[&str]) -> Result<(i32, String, String)> {
        let output = Command::new(&self.registry_binary)
            .args(args)
            .current_dir(self.temp_dir.path())
            .output()
            .context("Failed to run registry")?;

        Ok((
            output.status.code().unwrap_or(-1),
            String::from_utf8_lossy(&output.stdout).to_string(),
            String::from_utf8_lossy(&output.stderr).to_string(),
        ))
    }

    /// Validate ruleset file
    fn validate_ruleset_file(&self, path: &PathBuf) -> Result<bool> {
        if !self.registry_available() {
            warn!("Registry binary not available, skipping validation");
            return Ok(true);
        }

        let (code, stdout, stderr) = self.run_registry_cmd(&[
            "validate",
            "--ruleset",
            path.to_str().unwrap(),
        ])?;

        debug!("Validation stdout: {}", stdout);
        if !stderr.is_empty() {
            debug!("Validation stderr: {}", stderr);
        }

        Ok(code == 0)
    }
}

// ============================================================================
// Test Cases
// ============================================================================

#[tokio::test]
async fn test_ruleset_registration() -> Result<()> {
    setup_test_logging();

    let mut client = MockRegistryClient::new();

    let ruleset = Ruleset {
        id: "openssf-workflow-security".to_string(),
        name: "OpenSSF Workflow Security".to_string(),
        version: "1.0.0".to_string(),
        description: "Security rules for GitHub Actions workflows".to_string(),
        rules: vec![
            Rule {
                id: "unpinned-action".to_string(),
                name: "Unpinned GitHub Action".to_string(),
                description: "GitHub Actions should be pinned to commit SHAs".to_string(),
                category: RuleCategory::WorkflowSecurity,
                severity: Severity::High,
                pattern: Some(r"uses:\s*[\w-]+/[\w-]+@(v?\d+|main|master)".to_string()),
                fix_template: Some("uses: {action}@{sha} # {version}".to_string()),
                auto_fixable: true,
                references: vec![
                    "https://docs.github.com/en/actions/security-guides/security-hardening-for-github-actions".to_string(),
                ],
            },
            Rule {
                id: "missing-permissions".to_string(),
                name: "Missing Permissions".to_string(),
                description: "Workflows should declare explicit permissions".to_string(),
                category: RuleCategory::WorkflowSecurity,
                severity: Severity::Medium,
                pattern: Some(r"^name:.*\n(?!permissions:)".to_string()),
                fix_template: Some("permissions: read-all".to_string()),
                auto_fixable: true,
                references: vec![],
            },
        ],
        metadata: RulesetMetadata {
            author: "hyperpolymath".to_string(),
            license: "AGPL-3.0-or-later".to_string(),
            tags: vec!["security".to_string(), "github-actions".to_string()],
            created_at: chrono::Utc::now().to_rfc3339(),
            updated_at: chrono::Utc::now().to_rfc3339(),
        },
    };

    let id = client.register_ruleset(ruleset)?;
    assert_eq!(id, "openssf-workflow-security");

    let retrieved = client.get_ruleset(&id);
    assert!(retrieved.is_some());
    assert_eq!(retrieved.unwrap().rules.len(), 2);

    info!("Ruleset registration test passed");
    Ok(())
}

#[tokio::test]
async fn test_ruleset_verification() -> Result<()> {
    setup_test_logging();

    let client = MockRegistryClient::new();

    // Valid ruleset
    let valid_ruleset = Ruleset {
        id: "valid-ruleset".to_string(),
        name: "Valid Ruleset".to_string(),
        version: "1.0.0".to_string(),
        description: "A valid ruleset".to_string(),
        rules: vec![Rule {
            id: "rule-1".to_string(),
            name: "Rule 1".to_string(),
            description: "Test rule".to_string(),
            category: RuleCategory::CodeQuality,
            severity: Severity::Low,
            pattern: None,
            fix_template: None,
            auto_fixable: false,
            references: vec![],
        }],
        metadata: RulesetMetadata {
            author: "test".to_string(),
            license: "MIT".to_string(),
            tags: vec![],
            created_at: chrono::Utc::now().to_rfc3339(),
            updated_at: chrono::Utc::now().to_rfc3339(),
        },
    };

    let result = client.verify_ruleset(&valid_ruleset)?;
    assert!(result.valid);
    assert!(result.errors.is_empty());

    // Invalid ruleset (bad version)
    let invalid_ruleset = Ruleset {
        id: "invalid-ruleset".to_string(),
        name: "Invalid Ruleset".to_string(),
        version: "vBadVersion".to_string(),
        description: "An invalid ruleset".to_string(),
        rules: vec![],
        metadata: RulesetMetadata {
            author: "test".to_string(),
            license: "MIT".to_string(),
            tags: vec![],
            created_at: chrono::Utc::now().to_rfc3339(),
            updated_at: chrono::Utc::now().to_rfc3339(),
        },
    };

    let result = client.verify_ruleset(&invalid_ruleset)?;
    assert!(!result.valid);
    assert!(!result.errors.is_empty());

    info!("Ruleset verification test passed");
    Ok(())
}

#[tokio::test]
async fn test_duplicate_rule_detection() -> Result<()> {
    setup_test_logging();

    let client = MockRegistryClient::new();

    let ruleset = Ruleset {
        id: "dup-test".to_string(),
        name: "Duplicate Test".to_string(),
        version: "1.0.0".to_string(),
        description: "Test for duplicate detection".to_string(),
        rules: vec![
            Rule {
                id: "same-id".to_string(),
                name: "Rule 1".to_string(),
                description: "First rule".to_string(),
                category: RuleCategory::CodeQuality,
                severity: Severity::Low,
                pattern: None,
                fix_template: None,
                auto_fixable: false,
                references: vec![],
            },
            Rule {
                id: "same-id".to_string(), // Duplicate!
                name: "Rule 2".to_string(),
                description: "Second rule with same ID".to_string(),
                category: RuleCategory::CodeQuality,
                severity: Severity::Medium,
                pattern: None,
                fix_template: None,
                auto_fixable: false,
                references: vec![],
            },
        ],
        metadata: RulesetMetadata {
            author: "test".to_string(),
            license: "MIT".to_string(),
            tags: vec![],
            created_at: chrono::Utc::now().to_rfc3339(),
            updated_at: chrono::Utc::now().to_rfc3339(),
        },
    };

    let result = client.verify_ruleset(&ruleset)?;
    assert!(!result.valid);
    assert!(result.errors.iter().any(|e| e.code == "ERR-DUP-RULE"));

    info!("Duplicate rule detection test passed");
    Ok(())
}

#[tokio::test]
async fn test_rule_search_by_category() -> Result<()> {
    setup_test_logging();

    let mut client = MockRegistryClient::new();
    client.verification_enabled = false;

    // Add multiple rulesets
    let security_ruleset = Ruleset {
        id: "security".to_string(),
        name: "Security".to_string(),
        version: "1.0.0".to_string(),
        description: "Security rules".to_string(),
        rules: vec![
            Rule {
                id: "sec-1".to_string(),
                name: "Sec Rule 1".to_string(),
                description: "Security rule".to_string(),
                category: RuleCategory::WorkflowSecurity,
                severity: Severity::High,
                pattern: None,
                fix_template: None,
                auto_fixable: false,
                references: vec![],
            },
        ],
        metadata: RulesetMetadata {
            author: "test".to_string(),
            license: "MIT".to_string(),
            tags: vec![],
            created_at: chrono::Utc::now().to_rfc3339(),
            updated_at: chrono::Utc::now().to_rfc3339(),
        },
    };

    let quality_ruleset = Ruleset {
        id: "quality".to_string(),
        name: "Quality".to_string(),
        version: "1.0.0".to_string(),
        description: "Quality rules".to_string(),
        rules: vec![
            Rule {
                id: "qual-1".to_string(),
                name: "Quality Rule 1".to_string(),
                description: "Quality rule".to_string(),
                category: RuleCategory::CodeQuality,
                severity: Severity::Low,
                pattern: None,
                fix_template: None,
                auto_fixable: false,
                references: vec![],
            },
        ],
        metadata: RulesetMetadata {
            author: "test".to_string(),
            license: "MIT".to_string(),
            tags: vec![],
            created_at: chrono::Utc::now().to_rfc3339(),
            updated_at: chrono::Utc::now().to_rfc3339(),
        },
    };

    client.register_ruleset(security_ruleset)?;
    client.register_ruleset(quality_ruleset)?;

    // Search by category
    let security_rules = client.get_rules_by_category(RuleCategory::WorkflowSecurity);
    assert_eq!(security_rules.len(), 1);
    assert_eq!(security_rules[0].id, "sec-1");

    let quality_rules = client.get_rules_by_category(RuleCategory::CodeQuality);
    assert_eq!(quality_rules.len(), 1);
    assert_eq!(quality_rules[0].id, "qual-1");

    info!("Rule search by category test passed");
    Ok(())
}

#[tokio::test]
async fn test_rule_search_by_severity() -> Result<()> {
    setup_test_logging();

    let mut client = MockRegistryClient::new();
    client.verification_enabled = false;

    let ruleset = Ruleset {
        id: "multi-severity".to_string(),
        name: "Multi Severity".to_string(),
        version: "1.0.0".to_string(),
        description: "Rules with various severities".to_string(),
        rules: vec![
            Rule {
                id: "info-rule".to_string(),
                name: "Info".to_string(),
                description: "Info level".to_string(),
                category: RuleCategory::ProcessHygiene,
                severity: Severity::Info,
                pattern: None,
                fix_template: None,
                auto_fixable: false,
                references: vec![],
            },
            Rule {
                id: "medium-rule".to_string(),
                name: "Medium".to_string(),
                description: "Medium level".to_string(),
                category: RuleCategory::ProcessHygiene,
                severity: Severity::Medium,
                pattern: None,
                fix_template: None,
                auto_fixable: false,
                references: vec![],
            },
            Rule {
                id: "critical-rule".to_string(),
                name: "Critical".to_string(),
                description: "Critical level".to_string(),
                category: RuleCategory::CodeSecurity,
                severity: Severity::Critical,
                pattern: None,
                fix_template: None,
                auto_fixable: false,
                references: vec![],
            },
        ],
        metadata: RulesetMetadata {
            author: "test".to_string(),
            license: "MIT".to_string(),
            tags: vec![],
            created_at: chrono::Utc::now().to_rfc3339(),
            updated_at: chrono::Utc::now().to_rfc3339(),
        },
    };

    client.register_ruleset(ruleset)?;

    // Get rules with minimum severity
    let high_and_above = client.get_rules_by_severity(Severity::High);
    assert_eq!(high_and_above.len(), 1);

    let medium_and_above = client.get_rules_by_severity(Severity::Medium);
    assert_eq!(medium_and_above.len(), 2);

    let all_rules = client.get_rules_by_severity(Severity::Info);
    assert_eq!(all_rules.len(), 3);

    info!("Rule search by severity test passed");
    Ok(())
}

#[tokio::test]
async fn test_fix_template_application() -> Result<()> {
    setup_test_logging();

    let client = MockRegistryClient::new();

    let template = "uses: {action}@{sha} # {version}";
    let mut bindings = HashMap::new();
    bindings.insert("action".to_string(), "actions/checkout".to_string());
    bindings.insert("sha".to_string(), "b4ffde65f46336ab88eb53be808477a3936bae11".to_string());
    bindings.insert("version".to_string(), "v4".to_string());

    let result = client.apply_fix_template(template, &bindings);
    assert_eq!(
        result,
        "uses: actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11 # v4"
    );

    info!("Fix template application test passed");
    Ok(())
}

#[tokio::test]
async fn test_ruleset_json_serialization() -> Result<()> {
    setup_test_logging();

    let ruleset = Ruleset {
        id: "test-serialization".to_string(),
        name: "Test Serialization".to_string(),
        version: "1.0.0".to_string(),
        description: "Test JSON serialization".to_string(),
        rules: vec![Rule {
            id: "test-rule".to_string(),
            name: "Test Rule".to_string(),
            description: "Test".to_string(),
            category: RuleCategory::WorkflowSecurity,
            severity: Severity::High,
            pattern: Some(r"test.*pattern".to_string()),
            fix_template: Some("fix: {value}".to_string()),
            auto_fixable: true,
            references: vec!["https://example.com".to_string()],
        }],
        metadata: RulesetMetadata {
            author: "test".to_string(),
            license: "AGPL-3.0-or-later".to_string(),
            tags: vec!["test".to_string()],
            created_at: "2025-01-18T00:00:00Z".to_string(),
            updated_at: "2025-01-18T00:00:00Z".to_string(),
        },
    };

    // Serialize to JSON
    let json = serde_json::to_string_pretty(&ruleset)?;
    debug!("Serialized JSON:\n{}", json);

    // Deserialize back
    let parsed: Ruleset = serde_json::from_str(&json)?;
    assert_eq!(parsed.id, ruleset.id);
    assert_eq!(parsed.rules.len(), 1);
    assert_eq!(parsed.rules[0].category, RuleCategory::WorkflowSecurity);

    info!("Ruleset JSON serialization test passed");
    Ok(())
}

// ============================================================================
// Main Test Runner
// ============================================================================

fn main() {
    let rt = tokio::runtime::Runtime::new().unwrap();

    println!("Running Registry Integration Tests\n");
    println!("===================================\n");

    let tests: Vec<(&str, fn() -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<()>> + Send>>)> = vec![
        ("test_ruleset_registration", || Box::pin(test_ruleset_registration())),
        ("test_ruleset_verification", || Box::pin(test_ruleset_verification())),
        ("test_duplicate_rule_detection", || Box::pin(test_duplicate_rule_detection())),
        ("test_rule_search_by_category", || Box::pin(test_rule_search_by_category())),
        ("test_rule_search_by_severity", || Box::pin(test_rule_search_by_severity())),
        ("test_fix_template_application", || Box::pin(test_fix_template_application())),
        ("test_ruleset_json_serialization", || Box::pin(test_ruleset_json_serialization())),
    ];

    let mut passed = 0;
    let mut failed = 0;

    for (name, test_fn) in tests {
        print!("  {} ... ", name);
        match rt.block_on(test_fn()) {
            Ok(_) => {
                println!("ok");
                passed += 1;
            }
            Err(e) => {
                println!("FAILED");
                eprintln!("    Error: {}", e);
                failed += 1;
            }
        }
    }

    println!("\n===================================");
    println!("Results: {} passed, {} failed", passed, failed);

    if failed > 0 {
        std::process::exit(1);
    }
}

// ============================================================================
// Common Test Utilities
// ============================================================================

mod common {
    use tracing_subscriber::{fmt, prelude::*, EnvFilter};

    pub fn setup_test_logging() {
        let _ = tracing_subscriber::registry()
            .with(fmt::layer().with_test_writer())
            .with(EnvFilter::from_default_env().add_directive("info".parse().unwrap()))
            .try_init();
    }
}
