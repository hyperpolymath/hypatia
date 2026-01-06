// SPDX-License-Identifier: AGPL-3.0-or-later
//! Error catalog for CI/CD issues
//! Generated from .audittraining/ERROR-CATALOG.scm

use std::collections::HashMap;

/// Error severity levels (aligned with CVSS)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    Critical,  // 9.0+ - Immediate action required
    High,      // 7.0+ - Fix within 24 hours
    Medium,    // 4.0+ - Fix within 1 week
    Low,       // 1.0+ - Fix when convenient
    Info,      // 0.0  - Informational only
}

/// Error type categories
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorType {
    CodeSecurity,
    WorkflowSecurity,
    DependencyVuln,
    ProcessHygiene,
    CodeQuality,
    MissingTests,
    MissingSast,
    MissingFuzzing,
}

/// An error pattern from the catalog
#[derive(Debug, Clone)]
pub struct ErrorPattern {
    pub id: String,
    pub error_type: ErrorType,
    pub severity: Severity,
    pub auto_fixable: bool,
    pub description: String,
    pub fix_pattern: String,
}

/// Error catalog loaded from ERROR-CATALOG.scm
pub struct ErrorCatalog {
    patterns: HashMap<String, ErrorPattern>,
}

impl ErrorCatalog {
    pub fn new() -> Self {
        let mut patterns = HashMap::new();

        // Workflow Security
        patterns.insert("token-permissions-id".to_string(), ErrorPattern {
            id: "token-permissions-id".to_string(),
            error_type: ErrorType::WorkflowSecurity,
            severity: Severity::High,
            auto_fixable: true,
            description: "Workflow missing explicit permissions declaration".to_string(),
            fix_pattern: "Add `permissions: read-all` at workflow level".to_string(),
        });

        patterns.insert("pinned-dependencies-id".to_string(), ErrorPattern {
            id: "pinned-dependencies-id".to_string(),
            error_type: ErrorType::WorkflowSecurity,
            severity: Severity::Medium,
            auto_fixable: true,
            description: "GitHub Actions not SHA-pinned".to_string(),
            fix_pattern: "Replace @vN with @SHA # vN format".to_string(),
        });

        patterns.insert("workflow-linter-self-detection".to_string(), ErrorPattern {
            id: "workflow-linter-self-detection".to_string(),
            error_type: ErrorType::WorkflowSecurity,
            severity: Severity::Low,
            auto_fixable: true,
            description: "Workflow linter grep matches its own comments containing 'uses:'".to_string(),
            fix_pattern: "Add grep -v filters for comment lines and specific patterns".to_string(),
        });

        patterns.insert("missing-action-input".to_string(), ErrorPattern {
            id: "missing-action-input".to_string(),
            error_type: ErrorType::WorkflowSecurity,
            severity: Severity::Medium,
            auto_fixable: true,
            description: "GitHub Action missing required input parameter".to_string(),
            fix_pattern: "Add 'with:' section with required inputs (e.g., toolchain: stable)".to_string(),
        });

        patterns.insert("codeql-language-mismatch".to_string(), ErrorPattern {
            id: "codeql-language-mismatch".to_string(),
            error_type: ErrorType::WorkflowSecurity,
            severity: Severity::Medium,
            auto_fixable: true,
            description: "CodeQL configured for languages not present in repository".to_string(),
            fix_pattern: "Update language matrix to match repo contents; use 'actions' for workflow scanning".to_string(),
        });

        patterns.insert("missing-workflow-permissions".to_string(), ErrorPattern {
            id: "missing-workflow-permissions".to_string(),
            error_type: ErrorType::WorkflowSecurity,
            severity: Severity::High,
            auto_fixable: true,
            description: "Workflow does not contain permissions".to_string(),
            fix_pattern: "Add 'permissions: read-all' at workflow level".to_string(),
        });

        patterns.insert("missing-spdx-header".to_string(), ErrorPattern {
            id: "missing-spdx-header".to_string(),
            error_type: ErrorType::WorkflowSecurity,
            severity: Severity::Low,
            auto_fixable: true,
            description: "Workflow file missing SPDX license header".to_string(),
            fix_pattern: "Add '# SPDX-License-Identifier: AGPL-3.0-or-later' as first line".to_string(),
        });

        // Code Security
        patterns.insert("hard-coded-cryptographic-value".to_string(), ErrorPattern {
            id: "hard-coded-cryptographic-value".to_string(),
            error_type: ErrorType::CodeSecurity,
            severity: Severity::Critical,
            auto_fixable: false,
            description: "Hard-coded secret, key, or token in source code".to_string(),
            fix_pattern: "Use environment variables or secrets manager".to_string(),
        });

        patterns.insert("remote-property-injection".to_string(), ErrorPattern {
            id: "remote-property-injection".to_string(),
            error_type: ErrorType::CodeSecurity,
            severity: Severity::High,
            auto_fixable: false,
            description: "Dynamic property access without validation".to_string(),
            fix_pattern: "Add allowlist validation for property names".to_string(),
        });

        // Dependency Vulnerabilities
        patterns.insert("vulnerabilities-id".to_string(), ErrorPattern {
            id: "vulnerabilities-id".to_string(),
            error_type: ErrorType::DependencyVuln,
            severity: Severity::High,
            auto_fixable: false,
            description: "Known vulnerabilities in dependencies".to_string(),
            fix_pattern: "Run cargo audit / npm audit and update deps".to_string(),
        });

        // Process Hygiene
        patterns.insert("security-policy-id".to_string(), ErrorPattern {
            id: "security-policy-id".to_string(),
            error_type: ErrorType::ProcessHygiene,
            severity: Severity::Medium,
            auto_fixable: true,
            description: "Missing SECURITY.md file".to_string(),
            fix_pattern: "Add SECURITY.md with reporting instructions".to_string(),
        });

        patterns.insert("branch-protection-id".to_string(), ErrorPattern {
            id: "branch-protection-id".to_string(),
            error_type: ErrorType::ProcessHygiene,
            severity: Severity::Medium,
            auto_fixable: true,
            description: "Branch protection not enabled".to_string(),
            fix_pattern: "Enable via GitHub API or UI".to_string(),
        });

        patterns.insert("code-review-id".to_string(), ErrorPattern {
            id: "code-review-id".to_string(),
            error_type: ErrorType::ProcessHygiene,
            severity: Severity::Medium,
            auto_fixable: true,
            description: "Pull requests merged without review".to_string(),
            fix_pattern: "Enable branch protection with required reviews".to_string(),
        });

        // Testing & Analysis
        patterns.insert("fuzzing-id".to_string(), ErrorPattern {
            id: "fuzzing-id".to_string(),
            error_type: ErrorType::MissingFuzzing,
            severity: Severity::Medium,
            auto_fixable: true,
            description: "No fuzzing infrastructure configured".to_string(),
            fix_pattern: "Add ClusterFuzzLite or cargo-fuzz setup".to_string(),
        });

        patterns.insert("sast-id".to_string(), ErrorPattern {
            id: "sast-id".to_string(),
            error_type: ErrorType::MissingSast,
            severity: Severity::Medium,
            auto_fixable: true,
            description: "No static analysis configured".to_string(),
            fix_pattern: "Add CodeQL workflow with correct language matrix".to_string(),
        });

        Self { patterns }
    }

    /// Get an error pattern by ID
    pub fn get(&self, id: &str) -> Option<&ErrorPattern> {
        self.patterns.get(id)
    }

    /// Get all auto-fixable patterns
    pub fn auto_fixable(&self) -> Vec<&ErrorPattern> {
        self.patterns.values()
            .filter(|p| p.auto_fixable)
            .collect()
    }

    /// Get all patterns by severity
    pub fn by_severity(&self, severity: Severity) -> Vec<&ErrorPattern> {
        self.patterns.values()
            .filter(|p| p.severity == severity)
            .collect()
    }

    /// Get all patterns
    pub fn all(&self) -> Vec<&ErrorPattern> {
        self.patterns.values().collect()
    }
}

impl Default for ErrorCatalog {
    fn default() -> Self {
        Self::new()
    }
}
