// SPDX-License-Identifier: AGPL-3.0-or-later
//! cicd-hyper-a-fixer: Programmatic CI/CD workflow fixer
//!
//! This crate provides:
//! - Workflow scanning for common CI/CD issues
//! - Automatic fix application based on ERROR-CATALOG.scm patterns
//! - SHA pin management using SHA-PINS.scm reference
//! - Integration points for gitbot-fleet bots

pub mod catalog;
pub mod fixer;
pub mod scanner;
pub mod sha_pins;

use std::path::Path;
use thiserror::Error;

pub use catalog::ErrorCatalog;
pub use fixer::WorkflowFixer;
pub use scanner::{Issue, IssueCategory, IssueSeverity, Scanner, ScanResult};
pub use sha_pins::ShaPins;

#[derive(Error, Debug)]
pub enum FixerError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("YAML parse error: {0}")]
    Yaml(#[from] serde_yaml::Error),
    #[error("Regex error: {0}")]
    Regex(#[from] regex::Error),
    #[error("No workflows found in {0}")]
    NoWorkflows(String),
    #[error("Fix failed: {0}")]
    FixFailed(String),
}

pub type Result<T> = std::result::Result<T, FixerError>;

/// Main entry point for the fixer library
/// Bots can use this to scan and fix repos programmatically
pub struct CicdFixer {
    pub catalog: ErrorCatalog,
    pub sha_pins: ShaPins,
    pub scanner: Scanner,
    pub fixer: WorkflowFixer,
}

impl CicdFixer {
    /// Create a new fixer with default catalog and SHA pins
    pub fn new() -> Self {
        Self {
            catalog: ErrorCatalog::default(),
            sha_pins: ShaPins::default(),
            scanner: Scanner::new(),
            fixer: WorkflowFixer::new(),
        }
    }

    /// Scan a repository for CI/CD issues
    pub fn scan_repo(&self, repo_path: &Path) -> Result<ScanResult> {
        self.scanner.scan_repo(repo_path, &self.catalog, &self.sha_pins)
    }

    /// Fix all auto-fixable issues in a repository
    pub fn fix_repo(&self, repo_path: &Path, dry_run: bool) -> Result<Vec<FixResult>> {
        let scan_result = self.scan_repo(repo_path)?;
        let mut results = Vec::new();

        for issue in scan_result.issues {
            if issue.auto_fixable {
                let result = self.fixer.fix_issue(repo_path, &issue, &self.sha_pins, dry_run)?;
                results.push(result);
            }
        }

        Ok(results)
    }
}

impl Default for CicdFixer {
    fn default() -> Self {
        Self::new()
    }
}

/// Result of a fix operation
#[derive(Debug, Clone)]
pub struct FixResult {
    pub issue_id: String,
    pub file_path: String,
    pub success: bool,
    pub applied: bool,
    pub message: String,
}
