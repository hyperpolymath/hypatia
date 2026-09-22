// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// File operations for verisimdb-data access.
// Reads/writes the flat-file stores used by the canonical data layer.
//
// Zig 0.16 moved the filesystem API from `std.fs` to `std.Io.Dir` and made the
// I/O capability an explicit parameter. Every function here takes `io` as its
// first argument so it can be exercised with an injected `Io`; the `export fn`
// boundary in `main.zig` supplies the process-wide one from `runtime.zig`.

const std = @import("std");
const runtime = @import("runtime");

const Dir = std.Io.Dir;

/// Get the verisimdb-data path from VERISIMDB_DATA_PATH env or working dir fallback.
pub fn getDataPath() []const u8 {
    return runtime.getEnv("VERISIMDB_DATA_PATH") orelse "verisimdb-data";
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
pub fn readFile(io: std.Io, path: []const u8, buf: []u8) usize {
    const file = Dir.openFileAbsolute(io, path, .{}) catch return 0;
    defer file.close(io);
    return file.readPositionalAll(io, buf, 0) catch return 0;
}

/// Check if a file exists at the given absolute path.
pub fn fileExists(io: std.Io, path: []const u8) bool {
    const file = Dir.openFileAbsolute(io, path, .{}) catch return false;
    file.close(io);
    return true;
}

/// Append a line to a file (creates if missing). Returns success.
pub fn appendLine(io: std.Io, path: []const u8, data: []const u8) bool {
    const file = Dir.createFileAbsolute(io, path, .{
        .truncate = false,
    }) catch return false;
    defer file.close(io);
    // 0.16 removed `seekFromEnd`; a positional write at the current length is
    // the direct equivalent of seek-to-end-then-write.
    const end = file.length(io) catch return false;
    file.writePositionalAll(io, data, end) catch return false;
    file.writePositionalAll(io, "\n", end + data.len) catch return false;
    return true;
}

/// Count files in a directory. Returns 0 on error.
pub fn countFiles(io: std.Io, dir_path: []const u8) usize {
    var dir = Dir.openDirAbsolute(io, dir_path, .{ .iterate = true }) catch return 0;
    defer dir.close(io);
    var count: usize = 0;
    var iter = dir.iterate();
    while (iter.next(io) catch null) |entry| {
        if (entry.kind == .file) count += 1;
    }
    return count;
}

/// Write an entire buffer to a file (creates/truncates).
pub fn writeFile(io: std.Io, path: []const u8, data: []const u8) bool {
    const file = Dir.createFileAbsolute(io, path, .{}) catch return false;
    defer file.close(io);
    file.writePositionalAll(io, data, 0) catch return false;
    return true;
}

extern "c" fn setenv(name: [*:0]const u8, value: [*:0]const u8, overwrite: c_int) c_int;
extern "c" fn unsetenv(name: [*:0]const u8) c_int;

test "getDataPath returns default" {
    _ = unsetenv("VERISIMDB_DATA_PATH");
    const path = getDataPath();
    try std.testing.expect(path.len > 0);
    try std.testing.expectEqualStrings("verisimdb-data", path);
}

// Positive control for the environment lookup. The `returns default` test above
// passes whether or not the lookup works at all -- which is exactly how a
// migration that silently broke `getenv` would go unnoticed. This one fails
// unless the variable is actually read.
test "getDataPath reads VERISIMDB_DATA_PATH when it is set" {
    try std.testing.expectEqual(@as(c_int, 0), setenv("VERISIMDB_DATA_PATH", "/tmp/hypatia-positive-control", 1));
    defer _ = unsetenv("VERISIMDB_DATA_PATH");
    try std.testing.expectEqualStrings("/tmp/hypatia-positive-control", getDataPath());
}

test "writeFile then readFile round-trips through an injected Io" {
    var threaded: std.Io.Threaded = .init_single_threaded;
    const io = threaded.io();

    const path = "/tmp/hypatia-file-ops-roundtrip.txt";
    defer Dir.deleteFileAbsolute(io, path) catch {};

    try std.testing.expect(writeFile(io, path, "hello"));
    try std.testing.expect(fileExists(io, path));

    var buf: [64]u8 = undefined;
    try std.testing.expectEqual(@as(usize, 5), readFile(io, path, &buf));
    try std.testing.expectEqualStrings("hello", buf[0..5]);
}
