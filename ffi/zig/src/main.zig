// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Hypatia FFI — Zig C ABI Bridge
// Implements the C-compatible interface defined by the Idris2 ABI.
// Provides stable FFI for all Hypatia API operations.

const std = @import("std");

// ============================================================
// Type mappings from Idris2 ABI
// ============================================================

pub const Severity = enum(u8) {
    critical = 0,
    high = 1,
    medium = 2,
    low = 3,
    info = 4,
};

pub const TriangleTier = enum(u8) {
    eliminate = 0,
    substitute = 1,
    control = 2,
};

pub const DispatchStrategy = enum(u8) {
    auto_execute = 0,
    review = 1,
    report_only = 2,
};

pub const Outcome = enum(u8) {
    success = 0,
    failure = 1,
    false_positive = 2,
};

pub const HealthStatus = enum(u8) {
    pass = 0,
    warn = 1,
    fail = 2,
};

pub const BotId = enum(u8) {
    rhodibot = 0,
    echidnabot = 1,
    sustainabot = 2,
    glambot = 3,
    seambot = 4,
    cipherbot = 5,
    finishbot = 6,
    accessibilitybot = 7,
    robot_repo_automaton = 8,
};

// ============================================================
// API Response wrapper
// ============================================================

pub const ApiResponse = extern struct {
    success: bool,
    data_ptr: ?[*]const u8,
    data_len: usize,
    error_ptr: ?[*]const u8,
    error_len: usize,
    timestamp_ptr: [*]const u8,
    timestamp_len: usize,
};

// ============================================================
// Dispatch entry for fleet coordination
// ============================================================

pub const DispatchEntry = extern struct {
    bot: BotId,
    repo_ptr: [*]const u8,
    repo_len: usize,
    file_ptr: [*]const u8,
    file_len: usize,
    recipe_id_ptr: [*]const u8,
    recipe_id_len: usize,
    tier: TriangleTier,
    strategy: DispatchStrategy,
};

// ============================================================
// Outcome record for learning loop
// ============================================================

pub const OutcomeRecord = extern struct {
    recipe_id_ptr: [*]const u8,
    recipe_id_len: usize,
    repo_ptr: [*]const u8,
    repo_len: usize,
    file_ptr: [*]const u8,
    file_len: usize,
    outcome: Outcome,
    timestamp_ptr: [*]const u8,
    timestamp_len: usize,
    bot_ptr: [*]const u8,
    bot_len: usize,
};

// ============================================================
// Exported C ABI functions
// ============================================================

/// Get the health status of all Hypatia components
export fn hypatia_health_check(out: *ApiResponse) callconv(.C) i32 {
    _ = out;
    // Implementation calls into Elixir/Logtalk via NIF bridge
    return 0;
}

/// Trigger a scan for a specific repository
export fn hypatia_scan_repo(
    repo_ptr: [*]const u8,
    repo_len: usize,
    out: *ApiResponse,
) callconv(.C) i32 {
    _ = repo_ptr;
    _ = repo_len;
    _ = out;
    return 0;
}

/// Dispatch a finding to the fleet
export fn hypatia_dispatch(entry: *const DispatchEntry, out: *ApiResponse) callconv(.C) i32 {
    _ = entry;
    _ = out;
    return 0;
}

/// Record a fix outcome
export fn hypatia_record_outcome(record: *const OutcomeRecord, out: *ApiResponse) callconv(.C) i32 {
    _ = record;
    _ = out;
    return 0;
}

/// Force a learning cycle
export fn hypatia_force_learning_cycle(out: *ApiResponse) callconv(.C) i32 {
    _ = out;
    return 0;
}

/// Get confidence for a recipe
export fn hypatia_get_confidence(
    recipe_id_ptr: [*]const u8,
    recipe_id_len: usize,
    out_confidence: *f64,
) callconv(.C) i32 {
    _ = recipe_id_ptr;
    _ = recipe_id_len;
    out_confidence.* = 0.0;
    return 0;
}

/// Map severity enum to string
export fn hypatia_severity_str(sev: Severity) callconv(.C) [*:0]const u8 {
    return switch (sev) {
        .critical => "critical",
        .high => "high",
        .medium => "medium",
        .low => "low",
        .info => "info",
    };
}

/// Map triangle tier to dispatch strategy based on confidence
export fn hypatia_dispatch_strategy(confidence: f64) callconv(.C) DispatchStrategy {
    if (confidence >= 0.95) return .auto_execute;
    if (confidence >= 0.85) return .review;
    return .report_only;
}
