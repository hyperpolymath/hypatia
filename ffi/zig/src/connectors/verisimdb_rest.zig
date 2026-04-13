// SPDX-License-Identifier: PMPL-1.0-or-later
// Connector 12/16 — VeriSimDB REST. Stub fidelity; see `hexadeca.zig`.
//
// Distinct from the generic REST connector (3/16) because VeriSimDB
// has its own modality semantics — when this is fleshed out it will
// route hexad-modality reads/writes through the existing
// `hypatia_record_outcome` / scan storage path.

const std = @import("std");

pub fn start(port: u16) void {
    std.debug.print("V-VeriSimDB-REST (Hypatia connector) starting on port {d}...\n", .{port});
}
