// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Hypatia client types — the typed surface that replaces the
// V-lang client at the deleted `api/v/hypatia.v`. Mirrors the
// Idris2 ABI types in `src/abi/Types.idr` and the Zig ABI types
// in `ffi/zig/src/main.zig`.

use serde::{Deserialize, Serialize};

/// Severity of a finding. Wire ordering matches the Zig
/// `Severity` enum and the Idris2 `Severity` data type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
#[repr(u8)]
pub enum Severity {
    Critical = 0,
    High = 1,
    Medium = 2,
    Low = 3,
    Info = 4,
}

/// One finding produced by a Hypatia scan rule.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Finding {
    pub rule_id: String,
    pub severity: Severity,
    pub file: String,
    #[serde(default)]
    pub line: Option<usize>,
    pub message: String,
    /// Safety-triangle tier when known: `"Eliminate"`, `"Substitute"`,
    /// `"Control"`. Hypatia populates this when the rule maps to a
    /// recipe.
    #[serde(default)]
    pub triangle_tier: Option<String>,
}

/// Status of an entire scan.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ScanResult {
    Pass,
    Fail,
    Skip,
    Error,
}

/// Request shape for `Client::scan`. The repo path is required;
/// rules is the list of Hypatia rule ids to apply (empty = all).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScanRequest {
    pub repo_path: String,
    #[serde(default)]
    pub rules: Vec<String>,
}

/// Response shape from `Client::scan`. `score` is the bounded
/// integer score (0..=100) used by Hypatia's CRG layer.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScanResponse {
    pub result: ScanResult,
    #[serde(default)]
    pub findings: Vec<Finding>,
    pub score: i32,
}

// ============================================================================
// Safety-triangle, dispatch, outcome — for the 6 extra FFI bindings
// ============================================================================

/// Safety-triangle tier. Mirrors `TriangleTier` in
/// `hypatia/ffi/zig/src/main.zig` and the Idris2 ABI. Wire ordering
/// is load-bearing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
#[repr(u8)]
pub enum TriangleTier {
    Eliminate = 0,
    Substitute = 1,
    Control = 2,
}

/// Dispatch strategy. Mirrors `DispatchStrategy` in main.zig.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
#[repr(u8)]
pub enum DispatchStrategy {
    AutoExecute = 0,
    Review = 1,
    ReportOnly = 2,
}

/// Outcome of a dispatched fix. Mirrors `Outcome` in main.zig.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
#[repr(u8)]
pub enum Outcome {
    Success = 0,
    Failure = 1,
    FalsePositive = 2,
}

/// Bot identifier in the gitbot fleet. Mirrors `BotId` in main.zig.
/// Wire ordering is load-bearing — see `gitbot-fleet/` for the
/// canonical bot list.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
#[repr(u8)]
pub enum BotId {
    Rhodibot = 0,
    Echidnabot = 1,
    Sustainabot = 2,
    Glambot = 3,
    Seambot = 4,
    Cipherbot = 5,
    Finishbot = 6,
    Accessibilitybot = 7,
    RobotRepoAutomaton = 8,
}

/// Health status of the Hypatia stack as reported by
/// `hypatia_health_check`. Parsed from the JSON `status` field.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum HealthStatus {
    Pass,
    Warn,
    Fail,
}

/// Health-check report — what the FFI returns from
/// `hypatia_health_check`. Pulled from the JSON shape produced by
/// the Zig side (status / stores / total_files / data_path).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthReport {
    pub status: HealthStatus,
    #[serde(default)]
    pub stores: u32,
    #[serde(default)]
    pub total_files: u64,
    #[serde(default)]
    pub data_path: String,
}

/// One dispatch entry to send to the fleet. The FFI wraps this as a
/// C `extern struct` (`DispatchEntry` in main.zig) — the Rust client
/// owns the strings and the FFI module reborrows their pointers.
#[derive(Debug, Clone)]
pub struct DispatchEntry {
    pub bot: BotId,
    pub repo: String,
    pub file: String,
    pub recipe_id: String,
    pub tier: TriangleTier,
    pub strategy: DispatchStrategy,
}

/// One outcome record for the learning loop. Same ownership rule as
/// `DispatchEntry`.
#[derive(Debug, Clone)]
pub struct OutcomeRecord {
    pub recipe_id: String,
    pub repo: String,
    pub file: String,
    pub outcome: Outcome,
    pub timestamp: String,
    pub bot: String,
}

impl DispatchStrategy {
    /// Map a confidence score (0.0..=1.0) to a dispatch strategy
    /// using Hypatia's standard thresholds. Mirrors
    /// `hypatia_dispatch_strategy` in main.zig — kept here so
    /// callers without an active FFI can still classify.
    pub fn from_confidence(confidence: f64) -> Self {
        if confidence >= 0.95 { DispatchStrategy::AutoExecute }
        else if confidence >= 0.85 { DispatchStrategy::Review }
        else { DispatchStrategy::ReportOnly }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dispatch_strategy_thresholds() {
        assert_eq!(DispatchStrategy::from_confidence(0.99), DispatchStrategy::AutoExecute);
        assert_eq!(DispatchStrategy::from_confidence(0.95), DispatchStrategy::AutoExecute);
        assert_eq!(DispatchStrategy::from_confidence(0.90), DispatchStrategy::Review);
        assert_eq!(DispatchStrategy::from_confidence(0.85), DispatchStrategy::Review);
        assert_eq!(DispatchStrategy::from_confidence(0.50), DispatchStrategy::ReportOnly);
    }

    #[test]
    fn enum_wire_ids_are_stable() {
        // Pinned to match main.zig and Idris2 ABI. Renumbering is an
        // ABI break that the producer side will catch first.
        assert_eq!(TriangleTier::Eliminate as u8, 0);
        assert_eq!(TriangleTier::Control as u8, 2);
        assert_eq!(DispatchStrategy::AutoExecute as u8, 0);
        assert_eq!(DispatchStrategy::ReportOnly as u8, 2);
        assert_eq!(Outcome::Success as u8, 0);
        assert_eq!(Outcome::FalsePositive as u8, 2);
        assert_eq!(BotId::Rhodibot as u8, 0);
        assert_eq!(BotId::RobotRepoAutomaton as u8, 8);
    }
}
