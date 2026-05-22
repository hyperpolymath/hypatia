// SPDX-License-Identifier: MPL-2.0
// Connector 11/16 — SOAP. Stub fidelity; see `hexadeca.zig`.

const std = @import("std");

pub fn start(port: u16) void {
    std.debug.print("V-SOAP (Hypatia connector) starting on port {d}...\n", .{port});
}
