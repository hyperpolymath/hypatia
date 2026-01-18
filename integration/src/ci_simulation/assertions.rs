// SPDX-License-Identifier: PLMP-1.0-or-later
//! Custom Assertions for CI Simulation Testing
//!
//! This module provides specialized assertion functions and macros for testing
//! the CI/CD intelligence platform against simulated CI environments.
//!
//! # Assertion Categories
//!
//! - **Rule Assertions** - Verify that specific rules were triggered
//! - **Severity Assertions** - Verify that findings have correct severity
//! - **Fix Assertions** - Verify that appropriate fixes were suggested
//! - **False Positive Assertions** - Verify absence of false positives
//!
//! # Example
//!
//! ```rust
//! use integration::ci_simulation::assertions::*;
//! use integration::ci_simulation::scenarios::security_scan_findings;
//!
//! #[tokio::test]
//! async fn test_security_findings() {
//!     let result = security_scan_findings().await;
//!
//!     assert_rule_triggered(&result.findings, "SEC-CRIT-001");
//!     assert_severity_correct(&result.findings, "SEC-CRIT-001", FindingSeverity::Critical);
//!     assert_fix_suggested(&result.findings, "SEC-CRIT-001");
//!     assert_no_false_positives(&result.findings, &known_false_positives);
//! }
//! ```

use super::{
    BuildStatus, FindingCategory, FindingSeverity, LogEntry, LogLevel, SecurityFinding,
    SimulatedBuild, SimulatedJob,
};
use std::collections::HashSet;

// ============================================================================
// Assertion Result Type
// ============================================================================

/// Result of an assertion with detailed information
#[derive(Debug)]
pub struct AssertionResult {
    /// Whether the assertion passed
    pub passed: bool,
    /// Description of the assertion
    pub description: String,
    /// Detailed message explaining the result
    pub message: String,
    /// Additional context for debugging
    pub context: Option<String>,
}

impl AssertionResult {
    /// Create a passing assertion result
    pub fn pass(description: impl Into<String>) -> Self {
        Self {
            passed: true,
            description: description.into(),
            message: "Assertion passed".to_string(),
            context: None,
        }
    }

    /// Create a failing assertion result
    pub fn fail(description: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            passed: false,
            description: description.into(),
            message: message.into(),
            context: None,
        }
    }

    /// Add context to the result
    pub fn with_context(mut self, context: impl Into<String>) -> Self {
        self.context = Some(context.into());
        self
    }

    /// Assert that this result passed, panicking with a message if it failed
    pub fn assert(self) {
        if !self.passed {
            let mut message = format!(
                "Assertion failed: {}\n  Message: {}",
                self.description, self.message
            );
            if let Some(context) = &self.context {
                message.push_str(&format!("\n  Context: {}", context));
            }
            panic!("{}", message);
        }
    }
}

// ============================================================================
// Rule Assertions
// ============================================================================

/// Assert that a specific rule was triggered in the findings
///
/// # Arguments
/// * `findings` - The list of security findings
/// * `rule_id` - The rule ID to check for
///
/// # Panics
/// Panics if the rule was not triggered
pub fn assert_rule_triggered(findings: &[SecurityFinding], rule_id: &str) {
    check_rule_triggered(findings, rule_id).assert();
}

/// Check if a specific rule was triggered (non-panicking version)
pub fn check_rule_triggered(findings: &[SecurityFinding], rule_id: &str) -> AssertionResult {
    let triggered = findings.iter().any(|finding| finding.rule_id == rule_id);

    if triggered {
        AssertionResult::pass(format!("Rule {} was triggered", rule_id))
    } else {
        let available_rules: Vec<&str> = findings.iter().map(|f| f.rule_id.as_str()).collect();
        AssertionResult::fail(
            format!("Rule {} should be triggered", rule_id),
            "Rule was not found in findings",
        )
        .with_context(format!("Available rules: {:?}", available_rules))
    }
}

/// Assert that a specific rule was NOT triggered
pub fn assert_rule_not_triggered(findings: &[SecurityFinding], rule_id: &str) {
    check_rule_not_triggered(findings, rule_id).assert();
}

/// Check if a specific rule was NOT triggered (non-panicking version)
pub fn check_rule_not_triggered(findings: &[SecurityFinding], rule_id: &str) -> AssertionResult {
    let triggered = findings.iter().any(|finding| finding.rule_id == rule_id);

    if !triggered {
        AssertionResult::pass(format!("Rule {} was not triggered", rule_id))
    } else {
        AssertionResult::fail(
            format!("Rule {} should not be triggered", rule_id),
            "Rule was unexpectedly found in findings",
        )
    }
}

/// Assert that all specified rules were triggered
pub fn assert_all_rules_triggered(findings: &[SecurityFinding], rule_ids: &[&str]) {
    for rule_id in rule_ids {
        assert_rule_triggered(findings, rule_id);
    }
}

/// Assert that exactly the specified rules were triggered (no more, no less)
pub fn assert_exact_rules_triggered(findings: &[SecurityFinding], expected_rules: &[&str]) {
    let expected: HashSet<&str> = expected_rules.iter().copied().collect();
    let actual: HashSet<&str> = findings.iter().map(|f| f.rule_id.as_str()).collect();

    let missing: Vec<&&str> = expected.difference(&actual).collect();
    let unexpected: Vec<&&str> = actual.difference(&expected).collect();

    if !missing.is_empty() {
        panic!(
            "Missing expected rules: {:?}\nActual rules: {:?}",
            missing, actual
        );
    }

    if !unexpected.is_empty() {
        panic!(
            "Unexpected rules triggered: {:?}\nExpected rules: {:?}",
            unexpected, expected
        );
    }
}

// ============================================================================
// Severity Assertions
// ============================================================================

/// Assert that a finding has the correct severity
pub fn assert_severity_correct(
    findings: &[SecurityFinding],
    rule_id: &str,
    expected_severity: FindingSeverity,
) {
    check_severity_correct(findings, rule_id, expected_severity).assert();
}

/// Check if a finding has the correct severity (non-panicking version)
pub fn check_severity_correct(
    findings: &[SecurityFinding],
    rule_id: &str,
    expected_severity: FindingSeverity,
) -> AssertionResult {
    let finding = findings.iter().find(|finding| finding.rule_id == rule_id);

    match finding {
        Some(finding) => {
            if finding.severity == expected_severity {
                AssertionResult::pass(format!(
                    "Rule {} has correct severity {:?}",
                    rule_id, expected_severity
                ))
            } else {
                AssertionResult::fail(
                    format!(
                        "Rule {} should have severity {:?}",
                        rule_id, expected_severity
                    ),
                    format!("Actual severity: {:?}", finding.severity),
                )
            }
        }
        None => AssertionResult::fail(
            format!("Rule {} should exist to check severity", rule_id),
            "Rule not found in findings",
        ),
    }
}

/// Assert that there are no findings above a certain severity
pub fn assert_max_severity(findings: &[SecurityFinding], max_severity: FindingSeverity) {
    check_max_severity(findings, max_severity).assert();
}

/// Check max severity (non-panicking version)
pub fn check_max_severity(
    findings: &[SecurityFinding],
    max_severity: FindingSeverity,
) -> AssertionResult {
    let violations: Vec<&SecurityFinding> = findings
        .iter()
        .filter(|f| f.severity > max_severity)
        .collect();

    if violations.is_empty() {
        AssertionResult::pass(format!(
            "No findings above {:?} severity",
            max_severity
        ))
    } else {
        let violation_details: Vec<String> = violations
            .iter()
            .map(|f| format!("{}: {:?}", f.rule_id, f.severity))
            .collect();
        AssertionResult::fail(
            format!("No findings should exceed {:?} severity", max_severity),
            format!(
                "Found {} violations: {}",
                violations.len(),
                violation_details.join(", ")
            ),
        )
    }
}

/// Assert that there are findings at a minimum severity level
pub fn assert_min_severity_present(
    findings: &[SecurityFinding],
    min_severity: FindingSeverity,
) {
    check_min_severity_present(findings, min_severity).assert();
}

/// Check min severity present (non-panicking version)
pub fn check_min_severity_present(
    findings: &[SecurityFinding],
    min_severity: FindingSeverity,
) -> AssertionResult {
    let has_severity = findings.iter().any(|finding| finding.severity >= min_severity);

    if has_severity {
        AssertionResult::pass(format!(
            "Found findings at or above {:?} severity",
            min_severity
        ))
    } else {
        AssertionResult::fail(
            format!("Should have findings at or above {:?} severity", min_severity),
            "No findings at required severity level",
        )
    }
}

/// Assert severity distribution matches expected counts
pub fn assert_severity_distribution(
    findings: &[SecurityFinding],
    expected: &[(FindingSeverity, usize)],
) {
    for (severity, expected_count) in expected {
        let actual_count = findings.iter().filter(|f| f.severity == *severity).count();

        if actual_count != *expected_count {
            panic!(
                "Expected {} findings at {:?} severity, found {}",
                expected_count, severity, actual_count
            );
        }
    }
}

// ============================================================================
// Fix Assertions
// ============================================================================

/// Assert that a fix was suggested for a rule
pub fn assert_fix_suggested(findings: &[SecurityFinding], rule_id: &str) {
    check_fix_suggested(findings, rule_id).assert();
}

/// Check if a fix was suggested (non-panicking version)
pub fn check_fix_suggested(findings: &[SecurityFinding], rule_id: &str) -> AssertionResult {
    let finding = findings.iter().find(|finding| finding.rule_id == rule_id);

    match finding {
        Some(finding) => {
            if finding.suggested_fix.is_some() {
                AssertionResult::pass(format!("Rule {} has a suggested fix", rule_id))
            } else {
                AssertionResult::fail(
                    format!("Rule {} should have a suggested fix", rule_id),
                    "No suggested_fix provided",
                )
            }
        }
        None => AssertionResult::fail(
            format!("Rule {} should exist to check fix", rule_id),
            "Rule not found in findings",
        ),
    }
}

/// Assert that a finding is auto-fixable
pub fn assert_auto_fixable(findings: &[SecurityFinding], rule_id: &str) {
    check_auto_fixable(findings, rule_id).assert();
}

/// Check if a finding is auto-fixable (non-panicking version)
pub fn check_auto_fixable(findings: &[SecurityFinding], rule_id: &str) -> AssertionResult {
    let finding = findings.iter().find(|finding| finding.rule_id == rule_id);

    match finding {
        Some(finding) => {
            if finding.auto_fixable {
                AssertionResult::pass(format!("Rule {} is auto-fixable", rule_id))
            } else {
                AssertionResult::fail(
                    format!("Rule {} should be auto-fixable", rule_id),
                    "auto_fixable is false",
                )
            }
        }
        None => AssertionResult::fail(
            format!("Rule {} should exist to check auto-fix", rule_id),
            "Rule not found in findings",
        ),
    }
}

/// Assert that a finding is NOT auto-fixable (requires manual intervention)
pub fn assert_manual_fix_required(findings: &[SecurityFinding], rule_id: &str) {
    let finding = findings.iter().find(|finding| finding.rule_id == rule_id);

    match finding {
        Some(finding) => {
            if !finding.auto_fixable {
                // Pass
            } else {
                panic!(
                    "Rule {} should require manual fix, but is marked as auto-fixable",
                    rule_id
                );
            }
        }
        None => panic!("Rule {} not found in findings", rule_id),
    }
}

/// Assert that all critical/high findings have suggested fixes
pub fn assert_critical_findings_have_fixes(findings: &[SecurityFinding]) {
    for finding in findings {
        if finding.severity.requires_attention() && finding.suggested_fix.is_none() {
            panic!(
                "Critical/High finding {} should have a suggested fix",
                finding.rule_id
            );
        }
    }
}

// ============================================================================
// False Positive Assertions
// ============================================================================

/// Assert that there are no false positives
///
/// # Arguments
/// * `findings` - The actual findings from the scan
/// * `known_false_positives` - List of rule IDs known to be false positives in this context
pub fn assert_no_false_positives(
    findings: &[SecurityFinding],
    known_false_positives: &[&str],
) {
    check_no_false_positives(findings, known_false_positives).assert();
}

/// Check for false positives (non-panicking version)
pub fn check_no_false_positives(
    findings: &[SecurityFinding],
    known_false_positives: &[&str],
) -> AssertionResult {
    let false_positive_set: HashSet<&str> = known_false_positives.iter().copied().collect();

    let false_positives: Vec<&SecurityFinding> = findings
        .iter()
        .filter(|f| false_positive_set.contains(f.rule_id.as_str()))
        .collect();

    if false_positives.is_empty() {
        AssertionResult::pass("No known false positives found")
    } else {
        let fp_rules: Vec<&str> = false_positives.iter().map(|f| f.rule_id.as_str()).collect();
        AssertionResult::fail(
            "Should not contain known false positives",
            format!("Found false positives: {:?}", fp_rules),
        )
    }
}

/// Assert that a finding is a true positive (has valid file/line info)
pub fn assert_true_positive(findings: &[SecurityFinding], rule_id: &str) {
    let finding = findings.iter().find(|finding| finding.rule_id == rule_id);

    match finding {
        Some(finding) => {
            if finding.file.is_none() && finding.line.is_none() {
                panic!(
                    "True positive {} should have file/line information",
                    rule_id
                );
            }
        }
        None => panic!("Rule {} not found in findings", rule_id),
    }
}

/// Assert the false positive rate is below a threshold
pub fn assert_false_positive_rate(
    total_findings: usize,
    false_positives: usize,
    max_rate: f64,
) {
    if total_findings == 0 {
        return; // Can't calculate rate with no findings
    }

    let rate = false_positives as f64 / total_findings as f64;
    if rate > max_rate {
        panic!(
            "False positive rate {:.2}% exceeds maximum {:.2}%",
            rate * 100.0,
            max_rate * 100.0
        );
    }
}

// ============================================================================
// Build Assertions
// ============================================================================

/// Assert that a build completed successfully
pub fn assert_build_success(build: &SimulatedBuild) {
    assert_eq!(
        build.status,
        BuildStatus::Completed,
        "Build should be completed"
    );
    assert!(
        build.conclusion.map(|c| c.is_success()).unwrap_or(false),
        "Build conclusion should indicate success, got: {:?}",
        build.conclusion
    );
}

/// Assert that a build failed
pub fn assert_build_failure(build: &SimulatedBuild) {
    assert_eq!(
        build.status,
        BuildStatus::Completed,
        "Build should be completed"
    );
    assert!(
        build.conclusion.map(|c| c.is_failure()).unwrap_or(false),
        "Build conclusion should indicate failure, got: {:?}",
        build.conclusion
    );
}

/// Assert that a build was cancelled
pub fn assert_build_cancelled(build: &SimulatedBuild) {
    assert_eq!(
        build.status,
        BuildStatus::Cancelled,
        "Build should be cancelled"
    );
}

/// Assert build duration is within expected range
pub fn assert_build_duration_range(
    build: &SimulatedBuild,
    min_seconds: i64,
    max_seconds: i64,
) {
    let duration = build.duration().expect("Build should have duration");
    let seconds = duration.num_seconds();

    if seconds < min_seconds || seconds > max_seconds {
        panic!(
            "Build duration {}s should be between {}s and {}s",
            seconds, min_seconds, max_seconds
        );
    }
}

// ============================================================================
// Job Assertions
// ============================================================================

/// Assert that all jobs completed successfully
pub fn assert_all_jobs_success(jobs: &[SimulatedJob]) {
    for job in jobs {
        if !job
            .conclusion
            .map(|c| c.is_success())
            .unwrap_or(false)
        {
            panic!(
                "Job '{}' should have succeeded, got: {:?}",
                job.name, job.conclusion
            );
        }
    }
}

/// Assert that a specific job failed
pub fn assert_job_failed(jobs: &[SimulatedJob], job_name: &str) {
    let job = jobs.iter().find(|j| j.name == job_name);

    match job {
        Some(job) => {
            if !job.conclusion.map(|c| c.is_failure()).unwrap_or(false) {
                panic!(
                    "Job '{}' should have failed, got: {:?}",
                    job_name, job.conclusion
                );
            }
        }
        None => panic!("Job '{}' not found", job_name),
    }
}

/// Assert job count matches expected
pub fn assert_job_count(jobs: &[SimulatedJob], expected: usize) {
    assert_eq!(
        jobs.len(),
        expected,
        "Expected {} jobs, found {}",
        expected,
        jobs.len()
    );
}

/// Assert that jobs ran in parallel (started at similar times)
pub fn assert_jobs_parallel(jobs: &[SimulatedJob], tolerance_seconds: i64) {
    if jobs.len() < 2 {
        return; // Can't check parallelism with fewer than 2 jobs
    }

    let start_times: Vec<_> = jobs
        .iter()
        .filter_map(|j| j.started_at)
        .collect();

    if start_times.len() < 2 {
        panic!("Not enough jobs with start times to verify parallelism");
    }

    let first = start_times[0];
    for time in &start_times[1..] {
        let diff = (*time - first).num_seconds().abs();
        if diff > tolerance_seconds {
            panic!(
                "Jobs should start within {}s of each other for parallel execution, but diff was {}s",
                tolerance_seconds, diff
            );
        }
    }
}

// ============================================================================
// Log Assertions
// ============================================================================

/// Assert that a log message exists
pub fn assert_log_contains(logs: &[LogEntry], substring: &str) {
    let found = logs.iter().any(|log| log.message.contains(substring));
    if !found {
        panic!(
            "Expected log containing '{}', but not found.\nAvailable logs: {:?}",
            substring,
            logs.iter().map(|l| &l.message).collect::<Vec<_>>()
        );
    }
}

/// Assert that no error logs exist
pub fn assert_no_error_logs(logs: &[LogEntry]) {
    let error_logs: Vec<&LogEntry> = logs.iter().filter(|l| l.level == LogLevel::Error).collect();

    if !error_logs.is_empty() {
        let messages: Vec<&str> = error_logs.iter().map(|l| l.message.as_str()).collect();
        panic!("Expected no error logs, found: {:?}", messages);
    }
}

/// Assert that at least one error log exists
pub fn assert_has_error_logs(logs: &[LogEntry]) {
    let has_errors = logs.iter().any(|l| l.level == LogLevel::Error);
    if !has_errors {
        panic!("Expected at least one error log, but found none");
    }
}

/// Assert log count at a specific level
pub fn assert_log_count(logs: &[LogEntry], level: LogLevel, expected: usize) {
    let count = logs.iter().filter(|l| l.level == level).count();
    assert_eq!(
        count, expected,
        "Expected {} {:?} logs, found {}",
        expected, level, count
    );
}

// ============================================================================
// Category Assertions
// ============================================================================

/// Assert that findings include a specific category
pub fn assert_category_present(findings: &[SecurityFinding], category: FindingCategory) {
    let found = findings.iter().any(|f| f.category == category);
    if !found {
        panic!("Expected findings in category {:?}", category);
    }
}

/// Assert that findings do NOT include a specific category
pub fn assert_category_absent(findings: &[SecurityFinding], category: FindingCategory) {
    let found = findings.iter().any(|f| f.category == category);
    if found {
        panic!(
            "Did not expect findings in category {:?}, but found some",
            category
        );
    }
}

/// Assert category distribution
pub fn assert_category_count(
    findings: &[SecurityFinding],
    category: FindingCategory,
    expected: usize,
) {
    let count = findings.iter().filter(|f| f.category == category).count();
    assert_eq!(
        count, expected,
        "Expected {} findings in category {:?}, found {}",
        expected, category, count
    );
}

// ============================================================================
// Composite Assertions
// ============================================================================

/// Run a suite of assertions and collect all failures
pub struct AssertionCollector {
    results: Vec<AssertionResult>,
}

impl AssertionCollector {
    /// Create a new assertion collector
    pub fn new() -> Self {
        Self {
            results: Vec::new(),
        }
    }

    /// Add an assertion result
    pub fn add(&mut self, result: AssertionResult) {
        self.results.push(result);
    }

    /// Check if all assertions passed
    pub fn all_passed(&self) -> bool {
        self.results.iter().all(|r| r.passed)
    }

    /// Get all failed assertions
    pub fn failures(&self) -> Vec<&AssertionResult> {
        self.results.iter().filter(|r| !r.passed).collect()
    }

    /// Assert that all collected assertions passed
    pub fn assert_all(&self) {
        let failures = self.failures();
        if !failures.is_empty() {
            let messages: Vec<String> = failures
                .iter()
                .map(|f| format!("- {}: {}", f.description, f.message))
                .collect();
            panic!(
                "{} assertion(s) failed:\n{}",
                failures.len(),
                messages.join("\n")
            );
        }
    }
}

impl Default for AssertionCollector {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Macros for Common Assertions
// ============================================================================

/// Macro to assert that a rule was triggered
#[macro_export]
macro_rules! assert_rule {
    ($findings:expr, $rule_id:expr) => {
        $crate::ci_simulation::assertions::assert_rule_triggered($findings, $rule_id)
    };
}

/// Macro to assert severity of a finding
#[macro_export]
macro_rules! assert_severity {
    ($findings:expr, $rule_id:expr, $severity:expr) => {
        $crate::ci_simulation::assertions::assert_severity_correct($findings, $rule_id, $severity)
    };
}

/// Macro to assert a fix exists
#[macro_export]
macro_rules! assert_fix {
    ($findings:expr, $rule_id:expr) => {
        $crate::ci_simulation::assertions::assert_fix_suggested($findings, $rule_id)
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use uuid::Uuid;

    fn create_test_finding(
        rule_id: &str,
        severity: FindingSeverity,
        category: FindingCategory,
    ) -> SecurityFinding {
        SecurityFinding {
            id: Uuid::new_v4().to_string(),
            rule_id: rule_id.to_string(),
            severity,
            category,
            title: format!("Test finding {}", rule_id),
            description: "Test description".to_string(),
            file: Some("src/test.rs".to_string()),
            line: Some(10),
            column: Some(5),
            suggested_fix: Some("Fix it".to_string()),
            auto_fixable: severity <= FindingSeverity::Low,
            cwe_ids: vec![],
            cve_ids: vec![],
        }
    }

    #[test]
    fn test_assert_rule_triggered() {
        let findings = vec![
            create_test_finding("SEC-001", FindingSeverity::High, FindingCategory::CodeSecurity),
            create_test_finding("SEC-002", FindingSeverity::Medium, FindingCategory::CodeQuality),
        ];

        assert_rule_triggered(&findings, "SEC-001");
        assert_rule_triggered(&findings, "SEC-002");
    }

    #[test]
    #[should_panic(expected = "Rule SEC-003 should be triggered")]
    fn test_assert_rule_triggered_fails() {
        let findings = vec![create_test_finding(
            "SEC-001",
            FindingSeverity::High,
            FindingCategory::CodeSecurity,
        )];

        assert_rule_triggered(&findings, "SEC-003");
    }

    #[test]
    fn test_assert_rule_not_triggered() {
        let findings = vec![create_test_finding(
            "SEC-001",
            FindingSeverity::High,
            FindingCategory::CodeSecurity,
        )];

        assert_rule_not_triggered(&findings, "SEC-999");
    }

    #[test]
    fn test_assert_severity_correct() {
        let findings = vec![create_test_finding(
            "SEC-001",
            FindingSeverity::High,
            FindingCategory::CodeSecurity,
        )];

        assert_severity_correct(&findings, "SEC-001", FindingSeverity::High);
    }

    #[test]
    #[should_panic(expected = "should have severity Critical")]
    fn test_assert_severity_correct_fails() {
        let findings = vec![create_test_finding(
            "SEC-001",
            FindingSeverity::High,
            FindingCategory::CodeSecurity,
        )];

        assert_severity_correct(&findings, "SEC-001", FindingSeverity::Critical);
    }

    #[test]
    fn test_assert_fix_suggested() {
        let findings = vec![create_test_finding(
            "SEC-001",
            FindingSeverity::High,
            FindingCategory::CodeSecurity,
        )];

        assert_fix_suggested(&findings, "SEC-001");
    }

    #[test]
    fn test_assert_no_false_positives() {
        let findings = vec![
            create_test_finding("SEC-001", FindingSeverity::High, FindingCategory::CodeSecurity),
            create_test_finding("SEC-002", FindingSeverity::Medium, FindingCategory::CodeQuality),
        ];

        let known_fps = vec!["FP-001", "FP-002"];
        assert_no_false_positives(&findings, &known_fps);
    }

    #[test]
    #[should_panic(expected = "Should not contain known false positives")]
    fn test_assert_no_false_positives_fails() {
        let findings = vec![create_test_finding(
            "FP-001",
            FindingSeverity::High,
            FindingCategory::CodeSecurity,
        )];

        let known_fps = vec!["FP-001"];
        assert_no_false_positives(&findings, &known_fps);
    }

    #[test]
    fn test_assert_max_severity() {
        let findings = vec![
            create_test_finding("SEC-001", FindingSeverity::Medium, FindingCategory::CodeSecurity),
            create_test_finding("SEC-002", FindingSeverity::Low, FindingCategory::CodeQuality),
        ];

        assert_max_severity(&findings, FindingSeverity::Medium);
    }

    #[test]
    #[should_panic(expected = "should exceed")]
    fn test_assert_max_severity_fails() {
        let findings = vec![create_test_finding(
            "SEC-001",
            FindingSeverity::Critical,
            FindingCategory::CodeSecurity,
        )];

        assert_max_severity(&findings, FindingSeverity::High);
    }

    #[test]
    fn test_assertion_collector() {
        let mut collector = AssertionCollector::new();

        collector.add(AssertionResult::pass("Test 1"));
        collector.add(AssertionResult::pass("Test 2"));

        assert!(collector.all_passed());
        assert!(collector.failures().is_empty());
    }

    #[test]
    fn test_assertion_collector_with_failures() {
        let mut collector = AssertionCollector::new();

        collector.add(AssertionResult::pass("Test 1"));
        collector.add(AssertionResult::fail("Test 2", "Failed"));

        assert!(!collector.all_passed());
        assert_eq!(collector.failures().len(), 1);
    }

    #[test]
    fn test_log_assertions() {
        let logs = vec![
            LogEntry::info("Build started"),
            LogEntry::info("Build completed"),
        ];

        assert_log_contains(&logs, "Build started");
        assert_no_error_logs(&logs);
    }

    #[test]
    fn test_log_assertions_with_errors() {
        let logs = vec![
            LogEntry::info("Build started"),
            LogEntry::error("Build failed"),
        ];

        assert_log_contains(&logs, "failed");
        assert_has_error_logs(&logs);
    }

    #[test]
    fn test_category_assertions() {
        let findings = vec![
            create_test_finding("SEC-001", FindingSeverity::High, FindingCategory::CodeSecurity),
            create_test_finding("SEC-002", FindingSeverity::Medium, FindingCategory::CodeQuality),
        ];

        assert_category_present(&findings, FindingCategory::CodeSecurity);
        assert_category_present(&findings, FindingCategory::CodeQuality);
        assert_category_absent(&findings, FindingCategory::DependencyVuln);
    }

    #[test]
    fn test_auto_fixable_assertions() {
        let findings = vec![
            create_test_finding("SEC-001", FindingSeverity::Low, FindingCategory::CodeQuality),
        ];

        assert_auto_fixable(&findings, "SEC-001");
    }

    #[test]
    fn test_severity_distribution() {
        let findings = vec![
            create_test_finding("SEC-001", FindingSeverity::High, FindingCategory::CodeSecurity),
            create_test_finding("SEC-002", FindingSeverity::High, FindingCategory::CodeSecurity),
            create_test_finding("SEC-003", FindingSeverity::Medium, FindingCategory::CodeQuality),
        ];

        assert_severity_distribution(
            &findings,
            &[
                (FindingSeverity::High, 2),
                (FindingSeverity::Medium, 1),
                (FindingSeverity::Critical, 0),
            ],
        );
    }

    #[test]
    fn test_exact_rules_triggered() {
        let findings = vec![
            create_test_finding("SEC-001", FindingSeverity::High, FindingCategory::CodeSecurity),
            create_test_finding("SEC-002", FindingSeverity::Medium, FindingCategory::CodeQuality),
        ];

        assert_exact_rules_triggered(&findings, &["SEC-001", "SEC-002"]);
    }
}
