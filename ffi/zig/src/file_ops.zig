// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// File operations for verisimdb-data access.
// Reads/writes the flat-file stores used by the canonical data layer.

const std = @import("std");

const default_data_path = "/var/mnt/eclipse/repos/verisimdb-data";

/// Get the verisimdb-data path from VERISIMDB_DATA_PATH env or use default.
pub fn getDataPath() []const u8 {
    return std.posix.getenv("VERISIMDB_DATA_PATH") orelse default_data_path;
}

/// Build a full path into the data directory. Caller owns returned memory.
pub fn buildPath(allocator: std.mem.Allocator, segments: []const []const u8) ![]u8 {
    const base = getDataPath();
    var parts = try allocator.alloc([]const u8, segments.len + 1);
    defer allocator.free(parts);
    parts[0] = base;
    for (segments, 0..) |seg, i| {
        parts[i + 1] = seg;
    }
    return std.fs.path.join(allocator, parts);
}

/// Read an entire file into a buffer. Returns the number of bytes read.
/// Returns 0 if the file does not exist.
pub fn readFile(path: []const u8, buf: []u8) usize {
    const file = std.fs.openFileAbsolute(path, .{}) catch return 0;
    defer file.close();
    return file.readAll(buf) catch return 0;
}

/// Check if a file exists at the given absolute path.
pub fn fileExists(path: []const u8) bool {
    const file = std.fs.openFileAbsolute(path, .{}) catch return false;
    file.close();
    return true;
}

/// Append a line to a file (creates if missing). Returns success.
pub fn appendLine(path: []const u8, data: []const u8) bool {
    const file = std.fs.createFileAbsolute(path, .{
        .truncate = false,
    }) catch return false;
    defer file.close();
    file.seekFromEnd(0) catch return false;
    _ = file.write(data) catch return false;
    _ = file.write("\n") catch return false;
    return true;
}

/// Count files in a directory. Returns 0 on error.
pub fn countFiles(dir_path: []const u8) usize {
    var dir = std.fs.openDirAbsolute(dir_path, .{ .iterate = true }) catch return 0;
    defer dir.close();
    var count: usize = 0;
    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        if (entry.kind == .file) count += 1;
    }
    return count;
}

/// Write an entire buffer to a file (creates/truncates).
pub fn writeFile(path: []const u8, data: []const u8) bool {
    const file = std.fs.createFileAbsolute(path, .{}) catch return false;
    defer file.close();
    _ = file.writeAll(data) catch return false;
    return true;
}

test "getDataPath returns default" {
    const path = getDataPath();
    try std.testing.expect(path.len > 0);
}
