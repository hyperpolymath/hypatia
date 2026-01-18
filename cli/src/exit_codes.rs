// SPDX-License-Identifier: PLMP-1.0-or-later
//! Exit codes for the cicd-hyper-a CLI.
//!
//! Standardized exit codes for headless/workflow automation.
//! These codes allow scripts and CI systems to detect specific failure modes.
//!
//! # Exit Code Categories
//!
//! | Range     | Category                  | Description                                    |
//! |-----------|---------------------------|------------------------------------------------|
//! | 0         | Success                   | Operation completed successfully               |
//! | 1-9       | General errors            | Configuration, argument, IO errors             |
//! | 10-19     | Scan/analysis results     | Findings at various severity levels            |
//! | 20-29     | Registry errors           | Registry communication, auth, not found        |
//! | 30-39     | Repository errors         | Git, file access, permission errors            |
//! | 40-49     | Fleet/bot errors          | Bot execution failures                         |
//! | 50-59     | Batch operation results   | Partial failures in batch operations           |
//! | 100+      | Internal errors           | Unexpected errors, panics                      |

#![allow(dead_code)] // Exit codes are defined for documentation and future use

// ============================================================================
// SUCCESS
// ============================================================================

/// Operation completed successfully
pub const SUCCESS: i32 = 0;

// ============================================================================
// GENERAL ERRORS (1-9)
// ============================================================================

/// General/unspecified error
pub const GENERAL_ERROR: i32 = 1;

/// Invalid command line arguments
pub const INVALID_ARGUMENTS: i32 = 2;

/// Configuration file error (missing, invalid, or inaccessible)
pub const CONFIG_ERROR: i32 = 3;

/// IO error (file read/write, network, etc.)
pub const IO_ERROR: i32 = 4;

/// Operation cancelled by user
pub const CANCELLED: i32 = 5;

/// Operation timed out
pub const TIMEOUT: i32 = 6;

/// Feature not implemented
pub const NOT_IMPLEMENTED: i32 = 7;

// ============================================================================
// SCAN/ANALYSIS RESULTS (10-19)
// ============================================================================

/// Scan found critical severity findings
pub const CRITICAL_FINDINGS: i32 = 10;

/// Scan found high severity findings
pub const HIGH_FINDINGS: i32 = 11;

/// Scan found medium severity findings
pub const MEDIUM_FINDINGS: i32 = 12;

/// Scan found low severity findings
pub const LOW_FINDINGS: i32 = 13;

/// Scan found info level findings only
pub const INFO_FINDINGS: i32 = 14;

/// Scan failed to complete
pub const SCAN_FAILED: i32 = 15;

// ============================================================================
// REGISTRY ERRORS (20-29)
// ============================================================================

/// Registry connection failed
pub const REGISTRY_CONNECTION_ERROR: i32 = 20;

/// Registry authentication failed
pub const REGISTRY_AUTH_ERROR: i32 = 21;

/// Ruleset not found in registry
pub const RULESET_NOT_FOUND: i32 = 22;

/// Ruleset version conflict
pub const RULESET_CONFLICT: i32 = 23;

/// Ruleset validation failed
pub const RULESET_INVALID: i32 = 24;

/// Registry rate limit exceeded
pub const REGISTRY_RATE_LIMIT: i32 = 25;

// ============================================================================
// REPOSITORY ERRORS (30-39)
// ============================================================================

/// Not a git repository
pub const NOT_A_REPO: i32 = 30;

/// Repository path does not exist
pub const REPO_NOT_FOUND: i32 = 31;

/// Repository access denied
pub const REPO_ACCESS_DENIED: i32 = 32;

/// Git operation failed
pub const GIT_ERROR: i32 = 33;

/// Repository is dirty (uncommitted changes)
pub const REPO_DIRTY: i32 = 34;

/// Hook installation/execution failed
pub const HOOK_ERROR: i32 = 35;

// ============================================================================
// FLEET/BOT ERRORS (40-49)
// ============================================================================

/// Bot execution failed
pub const BOT_FAILED: i32 = 40;

/// Bot not found
pub const BOT_NOT_FOUND: i32 = 41;

/// Fleet operation partially failed
pub const FLEET_PARTIAL_FAILURE: i32 = 42;

/// Fleet operation completely failed
pub const FLEET_TOTAL_FAILURE: i32 = 43;

/// Bot dependency error
pub const BOT_DEPENDENCY_ERROR: i32 = 44;

// ============================================================================
// BATCH OPERATION RESULTS (50-59)
// ============================================================================

/// Batch operation partially succeeded (some failures)
pub const PARTIAL_FAILURE: i32 = 50;

/// Batch operation completely failed (all items failed)
pub const TOTAL_FAILURE: i32 = 51;

/// No items to process
pub const NO_ITEMS: i32 = 52;

// ============================================================================
// INTERNAL ERRORS (100+)
// ============================================================================

/// Internal error (panic, unexpected state)
pub const INTERNAL_ERROR: i32 = 100;

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/// Get a human-readable description of an exit code
pub fn describe(code: i32) -> &'static str {
    match code {
        SUCCESS => "Success",
        GENERAL_ERROR => "General error",
        INVALID_ARGUMENTS => "Invalid command line arguments",
        CONFIG_ERROR => "Configuration error",
        IO_ERROR => "IO error",
        CANCELLED => "Operation cancelled",
        TIMEOUT => "Operation timed out",
        NOT_IMPLEMENTED => "Feature not implemented",
        CRITICAL_FINDINGS => "Critical severity findings detected",
        HIGH_FINDINGS => "High severity findings detected",
        MEDIUM_FINDINGS => "Medium severity findings detected",
        LOW_FINDINGS => "Low severity findings detected",
        INFO_FINDINGS => "Info level findings detected",
        SCAN_FAILED => "Scan failed to complete",
        REGISTRY_CONNECTION_ERROR => "Registry connection error",
        REGISTRY_AUTH_ERROR => "Registry authentication error",
        RULESET_NOT_FOUND => "Ruleset not found",
        RULESET_CONFLICT => "Ruleset version conflict",
        RULESET_INVALID => "Ruleset validation failed",
        REGISTRY_RATE_LIMIT => "Registry rate limit exceeded",
        NOT_A_REPO => "Not a git repository",
        REPO_NOT_FOUND => "Repository not found",
        REPO_ACCESS_DENIED => "Repository access denied",
        GIT_ERROR => "Git operation failed",
        REPO_DIRTY => "Repository has uncommitted changes",
        HOOK_ERROR => "Hook installation/execution failed",
        BOT_FAILED => "Bot execution failed",
        BOT_NOT_FOUND => "Bot not found",
        FLEET_PARTIAL_FAILURE => "Fleet operation partially failed",
        FLEET_TOTAL_FAILURE => "Fleet operation completely failed",
        BOT_DEPENDENCY_ERROR => "Bot dependency error",
        PARTIAL_FAILURE => "Batch operation partially succeeded",
        TOTAL_FAILURE => "Batch operation completely failed",
        NO_ITEMS => "No items to process",
        INTERNAL_ERROR => "Internal error",
        _ => "Unknown error",
    }
}

/// Convert findings severity to exit code
pub fn from_severity(severity: &str, count: usize) -> i32 {
    if count == 0 {
        return SUCCESS;
    }

    match severity.to_lowercase().as_str() {
        "critical" | "crit" => CRITICAL_FINDINGS,
        "high" => HIGH_FINDINGS,
        "medium" | "med" => MEDIUM_FINDINGS,
        "low" => LOW_FINDINGS,
        "info" => INFO_FINDINGS,
        _ => GENERAL_ERROR,
    }
}

/// Get the highest severity exit code from a list of counts
pub fn highest_severity(critical: usize, high: usize, medium: usize, low: usize, info: usize) -> i32 {
    if critical > 0 {
        CRITICAL_FINDINGS
    } else if high > 0 {
        HIGH_FINDINGS
    } else if medium > 0 {
        MEDIUM_FINDINGS
    } else if low > 0 {
        LOW_FINDINGS
    } else if info > 0 {
        INFO_FINDINGS
    } else {
        SUCCESS
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_describe() {
        assert_eq!(describe(SUCCESS), "Success");
        assert_eq!(describe(CRITICAL_FINDINGS), "Critical severity findings detected");
        assert_eq!(describe(999), "Unknown error");
    }

    #[test]
    fn test_from_severity() {
        assert_eq!(from_severity("critical", 1), CRITICAL_FINDINGS);
        assert_eq!(from_severity("high", 5), HIGH_FINDINGS);
        assert_eq!(from_severity("low", 0), SUCCESS);
    }

    #[test]
    fn test_highest_severity() {
        assert_eq!(highest_severity(0, 0, 0, 0, 0), SUCCESS);
        assert_eq!(highest_severity(0, 0, 0, 1, 5), LOW_FINDINGS);
        assert_eq!(highest_severity(1, 5, 10, 15, 20), CRITICAL_FINDINGS);
    }
}
