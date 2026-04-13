// SPDX-License-Identifier: PMPL-1.0-or-later
// Connector 11/16 — SOAP. Stub fidelity; see `hexadeca.zig`.

const std = @import("std");

pub fn start(port: u16) void {
    std.debug.print("V-SOAP (Hypatia connector) starting on port {d}...\n", .{port});
}
