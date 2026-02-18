// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Minimal JSON writer for FFI response serialization.
// No allocator dependency — writes to a fixed buffer.

const std = @import("std");

pub const JsonWriter = struct {
    buf: []u8,
    pos: usize = 0,
    depth: usize = 0,
    needs_comma: bool = false,

    pub fn init(buffer: []u8) JsonWriter {
        return .{ .buf = buffer };
    }

    pub fn beginObject(self: *JsonWriter) void {
        self.maybeComma();
        self.write("{");
        self.depth += 1;
        self.needs_comma = false;
    }

    pub fn endObject(self: *JsonWriter) void {
        self.depth -= 1;
        self.write("}");
        self.needs_comma = true;
    }

    pub fn beginArray(self: *JsonWriter) void {
        self.maybeComma();
        self.write("[");
        self.depth += 1;
        self.needs_comma = false;
    }

    pub fn endArray(self: *JsonWriter) void {
        self.depth -= 1;
        self.write("]");
        self.needs_comma = true;
    }

    pub fn writeKey(self: *JsonWriter, key: []const u8) void {
        self.maybeComma();
        self.write("\"");
        self.write(key);
        self.write("\":");
        self.needs_comma = false;
    }

    pub fn writeString(self: *JsonWriter, value: []const u8) void {
        self.maybeComma();
        self.write("\"");
        self.write(value);
        self.write("\"");
        self.needs_comma = true;
    }

    pub fn writeNumber(self: *JsonWriter, value: i64) void {
        self.maybeComma();
        var num_buf: [20]u8 = undefined;
        const slice = std.fmt.bufPrint(&num_buf, "{d}", .{value}) catch return;
        self.write(slice);
        self.needs_comma = true;
    }

    pub fn writeFloat(self: *JsonWriter, value: f64) void {
        self.maybeComma();
        var num_buf: [32]u8 = undefined;
        const slice = std.fmt.bufPrint(&num_buf, "{d:.4}", .{value}) catch return;
        self.write(slice);
        self.needs_comma = true;
    }

    pub fn writeBool(self: *JsonWriter, value: bool) void {
        self.maybeComma();
        self.write(if (value) "true" else "false");
        self.needs_comma = true;
    }

    pub fn getWritten(self: *const JsonWriter) []const u8 {
        return self.buf[0..self.pos];
    }

    fn maybeComma(self: *JsonWriter) void {
        if (self.needs_comma) {
            self.write(",");
        }
    }

    fn write(self: *JsonWriter, data: []const u8) void {
        const remaining = self.buf.len - self.pos;
        const to_copy = @min(data.len, remaining);
        @memcpy(self.buf[self.pos..][0..to_copy], data[0..to_copy]);
        self.pos += to_copy;
    }
};

test "json object" {
    var buf: [256]u8 = undefined;
    var w = JsonWriter.init(&buf);
    w.beginObject();
    w.writeKey("status");
    w.writeString("pass");
    w.writeKey("count");
    w.writeNumber(42);
    w.endObject();
    const result = w.getWritten();
    try std.testing.expectEqualStrings("{\"status\":\"pass\",\"count\":42}", result);
}

test "json array" {
    var buf: [256]u8 = undefined;
    var w = JsonWriter.init(&buf);
    w.beginArray();
    w.writeString("a");
    w.writeString("b");
    w.endArray();
    const result = w.getWritten();
    try std.testing.expectEqualStrings("[\"a\",\"b\"]", result);
}
