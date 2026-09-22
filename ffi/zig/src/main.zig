// SPDX-License-Identifier: MPL-2.0
// HYPATIA FFI Implementation
//
// This module implements the C-compatible FFI declared in src/Hypatia/ABI/FFI.idr
// All types and layouts must match the Idris2 ABI definitions.
//
// SPDX-License-Identifier: MPL-2.0

const std = @import("std");
const jw = @import("json_writer");
const file_ops = @import("file_ops");
const runtime = @import("runtime");

const Dir = std.Io.Dir;

// Version information (keep in sync with project)
const VERSION = "0.1.0";
const BUILD_INFO = "HYPATIA built with Zig " ++ @import("builtin").zig_version_string;

/// Thread-local error storage
threadlocal var last_error: ?[]const u8 = null;

/// Set the last error message
fn setError(msg: []const u8) void {
    last_error = msg;
}

/// Clear the last error
fn clearError() void {
    last_error = null;
}

//==============================================================================
// Core Types (must match src/Hypatia/ABI/Types.idr)
//==============================================================================

/// Result codes (must match Idris2 Result type)
pub const Result = enum(c_int) {
    ok = 0,
    @"error" = 1,
    invalid_param = 2,
    out_of_memory = 3,
    null_pointer = 4,
};

/// Library handle (opaque to prevent direct access).
///
/// C sees only a pointer. The state lives in `HandleState` because an `opaque`
/// type cannot have fields -- the previous declaration gave this one two, which
/// is a compile error under every Zig version, not a 0.16 regression.
pub const Handle = opaque {};

/// The state behind a `*Handle`. Never named in the C header.
const HandleState = struct {
    allocator: std.mem.Allocator,
    initialized: bool,
};

fn state(handle: *Handle) *HandleState {
    return @ptrCast(@alignCast(handle));
}

//==============================================================================
// Library Lifecycle
//==============================================================================

/// Initialize the library
/// Returns a handle, or null on failure
export fn hypatia_init() ?*Handle {
    const allocator = std.heap.c_allocator;

    const handle = allocator.create(HandleState) catch {
        setError("Failed to allocate handle");
        return null;
    };

    // Initialize handle
    handle.* = .{
        .allocator = allocator,
        .initialized = true,
    };

    clearError();
    return @ptrCast(handle);
}

/// Free the library handle
export fn hypatia_free(handle: ?*Handle) void {
    const h = state(handle orelse return);
    const allocator = h.allocator;

    // Clean up resources
    h.initialized = false;

    allocator.destroy(h);
    clearError();
}

//==============================================================================
// Core Operations
//==============================================================================

/// Process data (example operation)
export fn hypatia_process(handle: ?*Handle, input: u32) Result {
    const h = state(handle orelse {
        setError("Null handle");
        return .null_pointer;
    });

    if (!h.initialized) {
        setError("Handle not initialized");
        return .@"error";
    }

    // Example processing logic
    _ = input;

    clearError();
    return .ok;
}

//==============================================================================
// String Operations
//==============================================================================

/// Get a string result (example)
/// Caller must free the returned string
export fn hypatia_get_string(handle: ?*Handle) ?[*:0]const u8 {
    const h = state(handle orelse {
        setError("Null handle");
        return null;
    });

    if (!h.initialized) {
        setError("Handle not initialized");
        return null;
    }

    // Example: allocate and return a string
    const result = h.allocator.dupeZ(u8, "Example result") catch {
        setError("Failed to allocate string");
        return null;
    };

    clearError();
    return result.ptr;
}

/// Free a string allocated by the library
export fn hypatia_free_string(str: ?[*:0]const u8) void {
    const s = str orelse return;
    const allocator = std.heap.c_allocator;

    const slice = std.mem.span(s);
    allocator.free(slice);
}

//==============================================================================
// Array/Buffer Operations
//==============================================================================

/// Process an array of data
export fn hypatia_process_array(
    handle: ?*Handle,
    buffer: ?[*]const u8,
    len: u32,
) Result {
    const h = state(handle orelse {
        setError("Null handle");
        return .null_pointer;
    });

    const buf = buffer orelse {
        setError("Null buffer");
        return .null_pointer;
    };

    if (!h.initialized) {
        setError("Handle not initialized");
        return .@"error";
    }

    // Access the buffer
    const data = buf[0..len];
    _ = data;

    // Process data here

    clearError();
    return .ok;
}

//==============================================================================
// Error Handling
//==============================================================================

/// Get the last error message
/// Returns null if no error
export fn hypatia_last_error() ?[*:0]const u8 {
    const err = last_error orelse return null;

    // Return C string (static storage, no need to free)
    const allocator = std.heap.c_allocator;
    const c_str = allocator.dupeZ(u8, err) catch return null;
    return c_str.ptr;
}

//==============================================================================
// Version Information
//==============================================================================

/// Get the library version
export fn hypatia_version() [*:0]const u8 {
    return VERSION.ptr;
}

/// Get build information
export fn hypatia_build_info() [*:0]const u8 {
    return BUILD_INFO.ptr;
}

//==============================================================================
// Callback Support
//==============================================================================

/// Callback function type (C ABI)
pub const Callback = *const fn (u64, u32) callconv(.c) u32;

/// Register a callback
export fn hypatia_register_callback(
    handle: ?*Handle,
    callback: ?Callback,
) Result {
    const h = state(handle orelse {
        setError("Null handle");
        return .null_pointer;
    });

    const cb = callback orelse {
        setError("Null callback");
        return .null_pointer;
    };

    if (!h.initialized) {
        setError("Handle not initialized");
        return .@"error";
    }

    // Store callback for later use
    _ = cb;

    clearError();
    return .ok;
}

//==============================================================================
// Utility Functions
//==============================================================================

/// Check if handle is initialized
export fn hypatia_is_initialized(handle: ?*Handle) u32 {
    const h = state(handle orelse return 0);
    return if (h.initialized) 1 else 0;
}

//==============================================================================
// Hypatia Domain Functions (7 documented C ABI exports)
// Spec: .claude/CLAUDE.md §"Zig FFI (ffi/zig/src/)"
//==============================================================================

fn hypatiaDataPath() []const u8 {
    return runtime.getEnv("HYPATIA_DATA_PATH") orelse
        runtime.getEnv("VERISIMDB_DATA_PATH") orelse
        "data/verisim";
}

// Append data + newline to a file (cwd-relative; creates if missing).
fn appendLine(io: std.Io, path: []const u8, data: []const u8) bool {
    const file = Dir.cwd().createFile(io, path, .{ .truncate = false }) catch return false;
    defer file.close(io);
    // 0.16 removed `seekFromEnd`/`writeAll`; a positional write at the current
    // length is the direct equivalent of seek-to-end-then-write.
    const end = file.length(io) catch return false;
    file.writePositionalAll(io, data, end) catch return false;
    file.writePositionalAll(io, "\n", end + data.len) catch return false;
    return true;
}

/// Health check — queries verisim-data store directories.
/// Returns {"ok":bool,"stores":{"scans":"ok/missing",...}} or null on OOM.
/// Caller must free with hypatia_free_string.
export fn hypatia_health_check() ?[*:0]const u8 {
    const io = runtime.io();
    const allocator = std.heap.c_allocator;
    const base = hypatiaDataPath();
    const stores = [_][]const u8{ "scans", "patterns", "recipes", "outcomes", "dispatch" };
    var ok_flags: [5]bool = undefined;
    var all_ok = true;

    for (stores, 0..) |store, i| {
        const path = std.fmt.allocPrint(allocator, "{s}/{s}", .{ base, store }) catch return null;
        defer allocator.free(path);
        const accessible: bool = blk: {
            Dir.cwd().access(io, path, .{}) catch break :blk false;
            break :blk true;
        };
        if (!accessible) all_ok = false;
        ok_flags[i] = accessible;
    }

    var buf: [512]u8 = undefined;
    var w = jw.JsonWriter.init(&buf);
    w.beginObject();
    w.writeKey("ok");
    w.writeBool(all_ok);
    w.writeKey("stores");
    w.beginObject();
    for (stores, 0..) |store, i| {
        w.writeKey(store);
        w.writeString(if (ok_flags[i]) "ok" else "missing");
    }
    w.endObject();
    w.endObject();
    return (allocator.dupeZ(u8, w.getWritten()) catch return null).ptr;
}

/// Scan repo — reads scans/{repo}.json from verisim-data.
/// Returns file contents or {"error":"scan not found","repo":"..."} on miss.
/// Caller must free with hypatia_free_string.
export fn hypatia_scan_repo(repo: ?[*:0]const u8) ?[*:0]const u8 {
    const io = runtime.io();
    const allocator = std.heap.c_allocator;
    const repo_str = std.mem.span(repo orelse {
        setError("Null repo parameter");
        return null;
    });
    const base = hypatiaDataPath();
    const path = std.fmt.allocPrint(allocator, "{s}/scans/{s}.json", .{ base, repo_str }) catch return null;
    defer allocator.free(path);

    const content = Dir.cwd().readFileAlloc(io, path, allocator, .limited(1024 * 1024)) catch {
        var err_buf: [256]u8 = undefined;
        var ew = jw.JsonWriter.init(&err_buf);
        ew.beginObject();
        ew.writeKey("error");
        ew.writeString("scan not found");
        ew.writeKey("repo");
        ew.writeString(repo_str);
        ew.endObject();
        return (allocator.dupeZ(u8, ew.getWritten()) catch return null).ptr;
    };
    defer allocator.free(content);
    return (allocator.dupeZ(u8, content) catch return null).ptr;
}

/// Dispatch finding — appends entry_json as a line to dispatch/pending.jsonl.
/// Returns 0 on success, 1 on error.
export fn hypatia_dispatch(entry_json: ?[*:0]const u8) c_int {
    const io = runtime.io();
    const entry = entry_json orelse {
        setError("Null entry_json");
        return 1;
    };
    const allocator = std.heap.c_allocator;
    const base = hypatiaDataPath();
    const path = std.fmt.allocPrint(allocator, "{s}/dispatch/pending.jsonl", .{base}) catch {
        setError("OOM");
        return 1;
    };
    defer allocator.free(path);
    return if (appendLine(io, path, std.mem.span(entry))) 0 else 1;
}

/// Record outcome — appends record_json to outcomes/YYYY-MM.jsonl.
/// Returns 0 on success, 1 on error.
export fn hypatia_record_outcome(record_json: ?[*:0]const u8) c_int {
    const io = runtime.io();
    const record = record_json orelse {
        setError("Null record_json");
        return 1;
    };
    const allocator = std.heap.c_allocator;
    const base = hypatiaDataPath();

    // 0.16 removed `std.time.timestamp`. `Clock.real` is documented as Unix time
    // (seconds since 1970-01-01T00:00:00Z, leap seconds ignored), and `toSeconds`
    // converts the nanosecond `Io.Timestamp` back to the same i64 the 0.15 call
    // returned -- so `epoch.EpochSeconds` below still receives seconds.
    const ts: i64 = std.Io.Clock.now(.real, io).toSeconds();
    const epoch_secs = std.time.epoch.EpochSeconds{ .secs = @intCast(ts) };
    const epoch_day = epoch_secs.getEpochDay();
    const year_day = epoch_day.calculateYearDay();
    const month_day = year_day.calculateMonthDay();

    const path = std.fmt.allocPrint(allocator, "{s}/outcomes/{d:0>4}-{d:0>2}.jsonl", .{
        base,
        year_day.year,
        @intFromEnum(month_day.month),
    }) catch {
        setError("OOM");
        return 1;
    };
    defer allocator.free(path);
    return if (appendLine(io, path, std.mem.span(record))) 0 else 1;
}

/// Force learning cycle — writes .force-learning signal file to verisim-data root.
/// Returns 0 on success, 1 on error.
export fn hypatia_force_learning_cycle() c_int {
    const io = runtime.io();
    const allocator = std.heap.c_allocator;
    const base = hypatiaDataPath();
    const path = std.fmt.allocPrint(allocator, "{s}/.force-learning", .{base}) catch {
        setError("OOM");
        return 1;
    };
    defer allocator.free(path);
    const f = Dir.cwd().createFile(io, path, .{}) catch return 1;
    f.close(io);
    return 0;
}

/// Get recipe confidence — reads recipes/recipe-{id}.json, extracts "confidence" field.
/// Returns confidence as f64, or -1.0 on missing recipe or parse error.
export fn hypatia_get_confidence(recipe_id: ?[*:0]const u8) f64 {
    const io = runtime.io();
    const id_str = std.mem.span(recipe_id orelse return -1.0);
    const allocator = std.heap.c_allocator;
    const base = hypatiaDataPath();
    const path = std.fmt.allocPrint(allocator, "{s}/recipes/recipe-{s}.json", .{ base, id_str }) catch return -1.0;
    defer allocator.free(path);

    const content = Dir.cwd().readFileAlloc(io, path, allocator, .limited(64 * 1024)) catch return -1.0;
    defer allocator.free(content);

    const key = "\"confidence\":";
    const idx = std.mem.indexOf(u8, content, key) orelse return -1.0;
    const after_key = std.mem.trimStart(u8, content[idx + key.len ..], " \t");

    var end: usize = 0;
    while (end < after_key.len) : (end += 1) {
        const c = after_key[end];
        if (!std.ascii.isDigit(c) and c != '.' and c != '-') break;
    }
    return std.fmt.parseFloat(f64, after_key[0..end]) catch -1.0;
}

/// Map confidence float to dispatch strategy integer.
/// Returns: 0=auto_execute (>=0.95), 1=review (0.85-0.94), 2=report_only (<0.85)
export fn hypatia_dispatch_strategy(confidence: f64) c_int {
    if (confidence >= 0.95) return 0;
    if (confidence >= 0.85) return 1;
    return 2;
}

//==============================================================================
// Tests
//==============================================================================

test "lifecycle" {
    const handle = hypatia_init() orelse return error.InitFailed;
    defer hypatia_free(handle);

    try std.testing.expect(hypatia_is_initialized(handle) == 1);
}

test "error handling" {
    const result = hypatia_process(null, 0);
    try std.testing.expectEqual(Result.null_pointer, result);

    const err = hypatia_last_error();
    try std.testing.expect(err != null);
}

test "version" {
    const ver = hypatia_version();
    const ver_str = std.mem.span(ver);
    try std.testing.expectEqualStrings(VERSION, ver_str);
}
