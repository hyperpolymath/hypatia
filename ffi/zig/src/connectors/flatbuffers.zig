// SPDX-License-Identifier: MPL-2.0
// Connector 4/16 — FlatBuffers. Stub fidelity; see `hexadeca.zig`.

const std = @import("std");

pub fn start(port: u16) void {
    std.debug.print("V-FlatBuffers (Hypatia connector) starting on port {d}...\n", .{port});
}
