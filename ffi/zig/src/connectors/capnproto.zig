// SPDX-License-Identifier: PMPL-1.0-or-later
// Connector 10/16 — Cap'n Proto. Stub fidelity; see `hexadeca.zig`.

const std = @import("std");

pub fn start(port: u16) void {
    std.debug.print("V-CapnProto (Hypatia connector) starting on port {d}...\n", .{port});
}
