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
