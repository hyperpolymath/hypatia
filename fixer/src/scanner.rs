// SPDX-License-Identifier: AGPL-3.0-or-later
//! Workflow scanner for detecting CI/CD issues

use crate::{ErrorCatalog, FixerError, Result, ShaPins};
use regex::Regex;
use std::fs;
use std::path::Path;
use walkdir::WalkDir;

/// Issue severity levels (aligned with CVSS)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum IssueSeverity {
    Critical,
    High,
    Medium,
    Low,
    Info,
}

impl IssueSeverity {
    pub fn from_str(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "critical" => Self::Critical,
            "high" => Self::High,
            "medium" => Self::Medium,
            "low" => Self::Low,
            _ => Self::Info,
        }
    }
}

/// A detected CI/CD issue
#[derive(Debug, Clone)]
pub struct Issue {
    pub id: String,
    pub severity: IssueSeverity,
    pub file_path: String,
    pub line_number: Option<usize>,
    pub description: String,
    pub fix_suggestion: String,
    pub auto_fixable: bool,
    pub category: IssueCategory,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IssueCategory {
    UnpinnedAction,
    MissingPermissions,
    MissingSpdx,
    MissingToolchainInput,
    CodeqlLanguageMismatch,
    WorkflowLinterSelfDetection,
    Other,
}

/// Result of scanning a repository
#[derive(Debug, Clone)]
pub struct ScanResult {
    pub repo_path: String,
    pub workflows_scanned: usize,
    pub issues: Vec<Issue>,
    pub auto_fixable_count: usize,
}

impl ScanResult {
    pub fn summary(&self) -> String {
        let critical = self.issues.iter().filter(|i| i.severity == IssueSeverity::Critical).count();
        let high = self.issues.iter().filter(|i| i.severity == IssueSeverity::High).count();
        let medium = self.issues.iter().filter(|i| i.severity == IssueSeverity::Medium).count();
        let low = self.issues.iter().filter(|i| i.severity == IssueSeverity::Low).count();

        format!(
            "Scanned {} workflows: {} issues ({} critical, {} high, {} medium, {} low), {} auto-fixable",
            self.workflows_scanned,
            self.issues.len(),
            critical,
            high,
            medium,
            low,
            self.auto_fixable_count
        )
    }
}

/// Workflow scanner
pub struct Scanner {
    unpinned_action_re: Regex,
    sha_pinned_re: Regex,
    permissions_re: Regex,
    spdx_re: Regex,
    rust_toolchain_re: Regex,
    toolchain_with_re: Regex,
    codeql_language_re: Regex,
    uses_re: Regex,
}

impl Scanner {
    pub fn new() -> Self {
        Self {
            // Match uses: action/name@v1 or @main (not SHA-pinned)
            unpinned_action_re: Regex::new(r"uses:\s*([a-zA-Z0-9_-]+/[a-zA-Z0-9_/-]+)@(v[0-9]+[a-zA-Z0-9.-]*|main|master)").unwrap(),
            // Match uses: action/name@SHA (40 hex chars)
            sha_pinned_re: Regex::new(r"uses:\s*[a-zA-Z0-9_-]+/[a-zA-Z0-9_/-]+@[a-f0-9]{40}").unwrap(),
            // Match permissions: at workflow level
            permissions_re: Regex::new(r"(?m)^permissions:").unwrap(),
            // Match SPDX header
            spdx_re: Regex::new(r"#\s*SPDX-License-Identifier:").unwrap(),
            // Match dtolnay/rust-toolchain without with: toolchain:
            rust_toolchain_re: Regex::new(r"uses:\s*dtolnay/rust-toolchain@").unwrap(),
            toolchain_with_re: Regex::new(r"with:\s*\n\s*toolchain:").unwrap(),
            // Match CodeQL language matrix
            codeql_language_re: Regex::new(r"language:\s*\[([^\]]+)\]").unwrap(),
            // Match any uses: line
            uses_re: Regex::new(r"uses:\s*([a-zA-Z0-9_-]+/[a-zA-Z0-9_/-]+)@([a-zA-Z0-9.-]+)").unwrap(),
        }
    }

    /// Scan a repository for CI/CD issues
    pub fn scan_repo(&self, repo_path: &Path, _catalog: &ErrorCatalog, sha_pins: &ShaPins) -> Result<ScanResult> {
        let workflows_dir = repo_path.join(".github/workflows");
        let mut issues = Vec::new();
        let mut workflows_scanned = 0;

        if !workflows_dir.exists() {
            return Err(FixerError::NoWorkflows(repo_path.display().to_string()));
        }

        for entry in WalkDir::new(&workflows_dir)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| {
                e.path().extension().map_or(false, |ext| ext == "yml" || ext == "yaml")
            })
        {
            let path = entry.path();
            let content = fs::read_to_string(path)?;
            workflows_scanned += 1;

            // Check for missing SPDX header
            if !self.spdx_re.is_match(&content) {
                issues.push(Issue {
                    id: "missing-spdx-header".to_string(),
                    severity: IssueSeverity::Low,
                    file_path: path.display().to_string(),
                    line_number: Some(1),
                    description: "Workflow file missing SPDX license header".to_string(),
                    fix_suggestion: "Add '# SPDX-License-Identifier: AGPL-3.0-or-later' as first line".to_string(),
                    auto_fixable: true,
                    category: IssueCategory::MissingSpdx,
                });
            }

            // Check for missing permissions
            if !self.permissions_re.is_match(&content) {
                issues.push(Issue {
                    id: "missing-workflow-permissions".to_string(),
                    severity: IssueSeverity::High,
                    file_path: path.display().to_string(),
                    line_number: None,
                    description: "Workflow missing explicit permissions declaration".to_string(),
                    fix_suggestion: "Add 'permissions: read-all' after 'on:' block".to_string(),
                    auto_fixable: true,
                    category: IssueCategory::MissingPermissions,
                });
            }

            // Check for unpinned actions
            for cap in self.unpinned_action_re.captures_iter(&content) {
                let action = cap.get(1).map_or("", |m| m.as_str());
                let version = cap.get(2).map_or("", |m| m.as_str());

                // Find line number and check if it's commented
                let line_result = content.lines()
                    .enumerate()
                    .find(|(_, line)| line.contains(action) && line.contains(version));

                // Skip commented lines
                if let Some((_, line)) = line_result {
                    if line.trim().starts_with('#') {
                        continue;
                    }
                }

                let line_num = line_result.map(|(i, _)| i + 1);

                // Get SHA pin suggestion if available
                let sha_suggestion = sha_pins.get_pin(action)
                    .map(|(sha, ver)| format!("@{} # {}", sha, ver))
                    .unwrap_or_else(|| format!("@SHA # {}", version));

                issues.push(Issue {
                    id: "unpinned-action".to_string(),
                    severity: IssueSeverity::Medium,
                    file_path: path.display().to_string(),
                    line_number: line_num,
                    description: format!("GitHub Action '{}' using version tag '{}' instead of SHA pin", action, version),
                    fix_suggestion: format!("Replace @{} with {}", version, sha_suggestion),
                    auto_fixable: true,
                    category: IssueCategory::UnpinnedAction,
                });
            }

            // Check for dtolnay/rust-toolchain missing toolchain input
            if self.rust_toolchain_re.is_match(&content) {
                // Find each occurrence and check if it has with: toolchain:
                let lines: Vec<&str> = content.lines().collect();
                for (i, line) in lines.iter().enumerate() {
                    if line.contains("dtolnay/rust-toolchain@") {
                        // Check next few lines for with: toolchain:
                        let has_toolchain = lines.iter()
                            .skip(i + 1)
                            .take(3)
                            .any(|l| l.trim().starts_with("with:") || l.contains("toolchain:"));

                        if !has_toolchain {
                            issues.push(Issue {
                                id: "missing-toolchain-input".to_string(),
                                severity: IssueSeverity::Medium,
                                file_path: path.display().to_string(),
                                line_number: Some(i + 1),
                                description: "dtolnay/rust-toolchain missing required 'toolchain' input".to_string(),
                                fix_suggestion: "Add 'with: toolchain: stable' (or nightly/beta)".to_string(),
                                auto_fixable: true,
                                category: IssueCategory::MissingToolchainInput,
                            });
                        }
                    }
                }
            }
        }

        let auto_fixable_count = issues.iter().filter(|i| i.auto_fixable).count();

        Ok(ScanResult {
            repo_path: repo_path.display().to_string(),
            workflows_scanned,
            issues,
            auto_fixable_count,
        })
    }
}

impl Default for Scanner {
    fn default() -> Self {
        Self::new()
    }
}
