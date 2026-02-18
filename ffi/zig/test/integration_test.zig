// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Integration tests for Hypatia Zig FFI against verisimdb-data.

const std = @import("std");
const json_writer = @import("json_writer");
const file_ops = @import("file_ops");

test "json_writer produces valid JSON object" {
    var buf: [512]u8 = undefined;
    var w = json_writer.JsonWriter.init(&buf);

    w.beginObject();
    w.writeKey("success");
    w.writeBool(true);
    w.writeKey("components");
    w.writeNumber(7);
    w.writeKey("confidence");
    w.writeFloat(0.95);
    w.endObject();

    const result = w.getWritten();
    try std.testing.expectEqualStrings(
        "{\"success\":true,\"components\":7,\"confidence\":0.9500}",
        result,
    );
}

test "json_writer nested object" {
    var buf: [512]u8 = undefined;
    var w = json_writer.JsonWriter.init(&buf);

    w.beginObject();
    w.writeKey("data");
    w.beginObject();
    w.writeKey("repo");
    w.writeString("echidna");
    w.writeKey("weak_points");
    w.writeNumber(15);
    w.endObject();
    w.endObject();

    const result = w.getWritten();
    try std.testing.expectEqualStrings(
        "{\"data\":{\"repo\":\"echidna\",\"weak_points\":15}}",
        result,
    );
}

test "json_writer array of strings" {
    var buf: [256]u8 = undefined;
    var w = json_writer.JsonWriter.init(&buf);

    w.beginArray();
    w.writeString("scans");
    w.writeString("patterns");
    w.writeString("recipes");
    w.endArray();

    const result = w.getWritten();
    try std.testing.expectEqualStrings(
        "[\"scans\",\"patterns\",\"recipes\"]",
        result,
    );
}

test "file_ops data path" {
    const path = file_ops.getDataPath();
    try std.testing.expect(path.len > 0);
}

test "file_ops read nonexistent file returns 0" {
    var buf: [64]u8 = undefined;
    const n = file_ops.readFile("/tmp/nonexistent-hypatia-test-file-zig", &buf);
    try std.testing.expectEqual(@as(usize, 0), n);
}

test "file_ops fileExists for nonexistent" {
    try std.testing.expect(!file_ops.fileExists("/tmp/nonexistent-hypatia-test-zig"));
}

test "file_ops write and read roundtrip" {
    const test_path = "/tmp/hypatia-zig-ffi-test-roundtrip.txt";
    const data = "hello from zig ffi test";

    // Write
    try std.testing.expect(file_ops.writeFile(test_path, data));

    // Verify exists
    try std.testing.expect(file_ops.fileExists(test_path));

    // Read back
    var buf: [256]u8 = undefined;
    const n = file_ops.readFile(test_path, &buf);
    try std.testing.expectEqual(data.len, n);
    try std.testing.expectEqualStrings(data, buf[0..n]);

    // Cleanup
    std.fs.deleteFileAbsolute(test_path) catch {};
}

test "file_ops appendLine creates and appends" {
    const test_path = "/tmp/hypatia-zig-ffi-test-append.jsonl";

    // Clean up from previous runs
    std.fs.deleteFileAbsolute(test_path) catch {};

    // Append two lines
    try std.testing.expect(file_ops.appendLine(test_path, "{\"line\":1}"));
    try std.testing.expect(file_ops.appendLine(test_path, "{\"line\":2}"));

    // Read back and verify
    var buf: [256]u8 = undefined;
    const n = file_ops.readFile(test_path, &buf);
    try std.testing.expect(n > 0);
    const content = buf[0..n];
    // Should contain both lines separated by newlines
    try std.testing.expect(std.mem.indexOf(u8, content, "{\"line\":1}") != null);
    try std.testing.expect(std.mem.indexOf(u8, content, "{\"line\":2}") != null);

    // Cleanup
    std.fs.deleteFileAbsolute(test_path) catch {};
}
