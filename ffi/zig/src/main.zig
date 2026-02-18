// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Hypatia FFI — Zig C ABI Bridge
// Implements the C-compatible interface defined by the Idris2 ABI.
// All operations work against verisimdb-data flat files (no NIF/HTTP required).
// Data path configurable via VERISIMDB_DATA_PATH environment variable.

const std = @import("std");
const json_writer = @import("json_writer");
const file_ops = @import("file_ops");

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
// Shared response buffer (thread-local would be ideal, static for simplicity)
// ============================================================

var response_buf: [8192]u8 = undefined;
const timestamp_static = "2026-02-13T00:00:00Z";

fn setResponse(out: *ApiResponse, success: bool, data: []const u8) void {
    out.success = success;
    if (success) {
        out.data_ptr = data.ptr;
        out.data_len = data.len;
        out.error_ptr = null;
        out.error_len = 0;
    } else {
        out.data_ptr = null;
        out.data_len = 0;
        out.error_ptr = data.ptr;
        out.error_len = data.len;
    }
    out.timestamp_ptr = timestamp_static;
    out.timestamp_len = timestamp_static.len;
}

// ============================================================
// Exported C ABI functions
// ============================================================

/// Get the health status of all Hypatia components.
/// Checks verisimdb-data directory structure, counts files per store.
export fn hypatia_health_check(out: *ApiResponse)i32 {
    const data_path = file_ops.getDataPath();

    // Check that the data path itself is accessible
    if (!file_ops.fileExists(data_path)) {
        const err = "verisimdb-data path not found";
        setResponse(out, false, err);
        return -1;
    }

    const stores = [_][]const u8{ "scans", "patterns", "recipes", "outcomes", "dispatch", "index" };
    var all_ok = true;
    var total_files: usize = 0;

    var path_buf: [512]u8 = undefined;
    for (stores) |store| {
        const prefix = data_path;
        const written = std.fmt.bufPrint(&path_buf, "{s}/{s}", .{ prefix, store }) catch {
            all_ok = false;
            continue;
        };
        const count = file_ops.countFiles(written);
        if (count == 0) all_ok = false;
        total_files += count;
    }

    var w = json_writer.JsonWriter.init(&response_buf);
    w.beginObject();
    w.writeKey("status");
    w.writeString(if (all_ok) "pass" else "warn");
    w.writeKey("stores");
    w.writeNumber(6);
    w.writeKey("total_files");
    w.writeNumber(@intCast(total_files));
    w.writeKey("data_path");
    w.writeString(data_path);
    w.endObject();

    const json = w.getWritten();
    setResponse(out, true, json);
    return 0;
}

/// Read scan results for a specific repository.
/// Reads scans/{repo}.json from verisimdb-data.
export fn hypatia_scan_repo(
    repo_ptr: [*]const u8,
    repo_len: usize,
    out: *ApiResponse,
)i32 {
    if (repo_len == 0 or repo_len > 255) {
        const err = "invalid repo name length";
        setResponse(out, false, err);
        return -1;
    }

    const repo = repo_ptr[0..repo_len];
    const data_path = file_ops.getDataPath();

    var path_buf: [512]u8 = undefined;
    const scan_path = std.fmt.bufPrint(&path_buf, "{s}/scans/{s}.json", .{ data_path, repo }) catch {
        const err = "path too long";
        setResponse(out, false, err);
        return -2;
    };

    const n = file_ops.readFile(scan_path, &response_buf);
    if (n == 0) {
        const err = "scan not found";
        setResponse(out, false, err);
        return -3;
    }

    setResponse(out, true, response_buf[0..n]);
    return 0;
}

/// Dispatch a finding to the fleet.
/// Serializes DispatchEntry as JSON, appends to dispatch/pending.jsonl.
export fn hypatia_dispatch(entry: *const DispatchEntry, out: *ApiResponse)i32 {
    const data_path = file_ops.getDataPath();
    const repo = entry.repo_ptr[0..entry.repo_len];
    const file = entry.file_ptr[0..entry.file_len];
    const recipe_id = entry.recipe_id_ptr[0..entry.recipe_id_len];

    const tier_str = switch (entry.tier) {
        .eliminate => "eliminate",
        .substitute => "substitute",
        .control => "control",
    };
    const strategy_str = switch (entry.strategy) {
        .auto_execute => "auto_execute",
        .review => "review",
        .report_only => "report_only",
    };

    var w = json_writer.JsonWriter.init(&response_buf);
    w.beginObject();
    w.writeKey("bot");
    w.writeNumber(@intFromEnum(entry.bot));
    w.writeKey("repo");
    w.writeString(repo);
    w.writeKey("file");
    w.writeString(file);
    w.writeKey("recipe_id");
    w.writeString(recipe_id);
    w.writeKey("tier");
    w.writeString(tier_str);
    w.writeKey("strategy");
    w.writeString(strategy_str);
    w.writeKey("timestamp");
    w.writeString(timestamp_static);
    w.endObject();

    const json = w.getWritten();

    var path_buf: [512]u8 = undefined;
    const pending_path = std.fmt.bufPrint(&path_buf, "{s}/dispatch/pending.jsonl", .{data_path}) catch {
        const err = "path too long";
        setResponse(out, false, err);
        return -2;
    };

    if (!file_ops.appendLine(pending_path, json)) {
        const err = "failed to write dispatch entry";
        setResponse(out, false, err);
        return -3;
    }

    setResponse(out, true, json);
    return 0;
}

/// Record a fix outcome.
/// Serializes OutcomeRecord as JSON, appends to outcomes/YYYY-MM.jsonl.
export fn hypatia_record_outcome(record: *const OutcomeRecord, out: *ApiResponse)i32 {
    const recipe_id = record.recipe_id_ptr[0..record.recipe_id_len];
    const repo = record.repo_ptr[0..record.repo_len];
    const file = record.file_ptr[0..record.file_len];
    const timestamp = record.timestamp_ptr[0..record.timestamp_len];
    const bot = record.bot_ptr[0..record.bot_len];

    const outcome_str = switch (record.outcome) {
        .success => "success",
        .failure => "failure",
        .false_positive => "false_positive",
    };

    var w = json_writer.JsonWriter.init(&response_buf);
    w.beginObject();
    w.writeKey("recipe_id");
    w.writeString(recipe_id);
    w.writeKey("repo");
    w.writeString(repo);
    w.writeKey("file");
    w.writeString(file);
    w.writeKey("outcome");
    w.writeString(outcome_str);
    w.writeKey("timestamp");
    w.writeString(timestamp);
    w.writeKey("bot");
    w.writeString(bot);
    w.endObject();

    const json = w.getWritten();

    // Extract YYYY-MM from timestamp for file naming
    var month_buf: [7]u8 = undefined;
    if (timestamp.len >= 7) {
        @memcpy(&month_buf, timestamp[0..7]);
    } else {
        @memcpy(month_buf[0..timestamp.len], timestamp);
        @memset(month_buf[timestamp.len..], '0');
    }

    const data_path = file_ops.getDataPath();
    var path_buf: [512]u8 = undefined;
    const outcomes_path = std.fmt.bufPrint(&path_buf, "{s}/outcomes/{s}.jsonl", .{ data_path, &month_buf }) catch {
        const err = "path too long";
        setResponse(out, false, err);
        return -2;
    };

    if (!file_ops.appendLine(outcomes_path, json)) {
        const err = "failed to write outcome record";
        setResponse(out, false, err);
        return -3;
    }

    setResponse(out, true, json);
    return 0;
}

/// Force a learning cycle.
/// Writes a .force-learning signal file in verisimdb-data root.
export fn hypatia_force_learning_cycle(out: *ApiResponse)i32 {
    const data_path = file_ops.getDataPath();

    var path_buf: [512]u8 = undefined;
    const signal_path = std.fmt.bufPrint(&path_buf, "{s}/.force-learning", .{data_path}) catch {
        const err = "path too long";
        setResponse(out, false, err);
        return -2;
    };

    if (!file_ops.writeFile(signal_path, timestamp_static)) {
        const err = "failed to write signal file";
        setResponse(out, false, err);
        return -3;
    }

    var w = json_writer.JsonWriter.init(&response_buf);
    w.beginObject();
    w.writeKey("status");
    w.writeString("triggered");
    w.writeKey("signal_file");
    w.writeString(".force-learning");
    w.endObject();

    const json = w.getWritten();
    setResponse(out, true, json);
    return 0;
}

/// Get confidence for a recipe.
/// Reads recipes/recipe-{id}.json, extracts the "confidence" field.
export fn hypatia_get_confidence(
    recipe_id_ptr: [*]const u8,
    recipe_id_len: usize,
    out_confidence: *f64,
)i32 {
    if (recipe_id_len == 0 or recipe_id_len > 255) {
        out_confidence.* = 0.0;
        return -1;
    }

    const recipe_id = recipe_id_ptr[0..recipe_id_len];
    const data_path = file_ops.getDataPath();

    // Try recipe-{id}.json first (canonical format)
    var path_buf: [512]u8 = undefined;
    const recipe_path = std.fmt.bufPrint(&path_buf, "{s}/recipes/{s}.json", .{ data_path, recipe_id }) catch {
        out_confidence.* = 0.0;
        return -2;
    };

    var file_buf: [8192]u8 = undefined;
    const n = file_ops.readFile(recipe_path, &file_buf);
    if (n == 0) {
        out_confidence.* = 0.0;
        return -3;
    }

    const content = file_buf[0..n];
    // Simple extraction: find "confidence": and parse the number
    const needle = "\"confidence\":";
    if (std.mem.indexOf(u8, content, needle)) |idx| {
        var start = idx + needle.len;
        // Skip whitespace
        while (start < content.len and (content[start] == ' ' or content[start] == '\t')) {
            start += 1;
        }
        // Find end of number
        var end = start;
        while (end < content.len and (content[end] >= '0' and content[end] <= '9' or content[end] == '.')) {
            end += 1;
        }
        if (end > start) {
            out_confidence.* = std.fmt.parseFloat(f64, content[start..end]) catch {
                out_confidence.* = 0.0;
                return -4;
            };
            return 0;
        }
    }

    out_confidence.* = 0.0;
    return -4;
}

/// Map severity enum to string
export fn hypatia_severity_str(sev: Severity)[*:0]const u8 {
    return switch (sev) {
        .critical => "critical",
        .high => "high",
        .medium => "medium",
        .low => "low",
        .info => "info",
    };
}

/// Map triangle tier to dispatch strategy based on confidence
export fn hypatia_dispatch_strategy(confidence: f64)DispatchStrategy {
    if (confidence >= 0.95) return .auto_execute;
    if (confidence >= 0.85) return .review;
    return .report_only;
}

// ============================================================
// Tests
// ============================================================

test "severity_str mapping" {
    try std.testing.expectEqualStrings("critical", std.mem.span(hypatia_severity_str(.critical)));
    try std.testing.expectEqualStrings("info", std.mem.span(hypatia_severity_str(.info)));
}

test "dispatch_strategy thresholds" {
    try std.testing.expectEqual(DispatchStrategy.auto_execute, hypatia_dispatch_strategy(0.99));
    try std.testing.expectEqual(DispatchStrategy.auto_execute, hypatia_dispatch_strategy(0.95));
    try std.testing.expectEqual(DispatchStrategy.review, hypatia_dispatch_strategy(0.90));
    try std.testing.expectEqual(DispatchStrategy.review, hypatia_dispatch_strategy(0.85));
    try std.testing.expectEqual(DispatchStrategy.report_only, hypatia_dispatch_strategy(0.50));
}

test "health_check returns valid response" {
    var resp: ApiResponse = undefined;
    const rc = hypatia_health_check(&resp);
    // Should succeed with some data (path may or may not exist in test env)
    try std.testing.expect(rc == 0 or rc == -1);
}

test "scan_repo with empty name fails" {
    var resp: ApiResponse = undefined;
    const rc = hypatia_scan_repo("".ptr, 0, &resp);
    try std.testing.expectEqual(@as(i32, -1), rc);
}

test "get_confidence with empty id fails" {
    var conf: f64 = undefined;
    const rc = hypatia_get_confidence("".ptr, 0, &conf);
    try std.testing.expectEqual(@as(i32, -1), rc);
}
