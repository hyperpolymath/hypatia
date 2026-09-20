// SPDX-License-Identifier: MPL-2.0
// Connector 6/16 — JSON-RPC. Stub fidelity; see `unified-api-adapter.zig`.

const std = @import("std");

pub fn start(port: u16) void {
    std.debug.print("V-JSON-RPC (Hypatia connector) starting on port {d}...\n", .{port});
}
