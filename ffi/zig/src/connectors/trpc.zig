// SPDX-License-Identifier: MPL-2.0
// Connector 9/16 — tRPC. Stub fidelity; see `hexadeca.zig`.

const std = @import("std");

pub fn start(port: u16) void {
    std.debug.print("V-tRPC (Hypatia connector) starting on port {d}...\n", .{port});
}
