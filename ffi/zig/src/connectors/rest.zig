// SPDX-License-Identifier: PMPL-1.0-or-later
// Connector 3/16 — REST. Stub fidelity; see `hexadeca.zig` for the substrate.

const std = @import("std");

pub fn start(port: u16) void {
    std.debug.print("V-REST (Hypatia connector) starting on port {d}...\n", .{port});
}
