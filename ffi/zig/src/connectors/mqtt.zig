// SPDX-License-Identifier: PMPL-1.0-or-later
// Connector 8/16 — MQTT. Stub fidelity; see `hexadeca.zig`.

const std = @import("std");

pub fn start(port: u16) void {
    std.debug.print("V-MQTT (Hypatia connector) starting on port {d}...\n", .{port});
}
