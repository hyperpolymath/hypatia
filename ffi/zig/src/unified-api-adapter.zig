// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Hypatia UnifiedApiAdapter — sixteen-protocol unified API surface.
//
// Replaces the V-lang `api/v/hypatia.v` client (deleted 2026-04-13)
// with a Zig-side multi-transport surface that wraps the existing
// Hypatia C ABI core (`hypatia_health_check`, `hypatia_scan_repo`,
// `hypatia_dispatch`, `hypatia_record_outcome`, `hypatia_force_learning_cycle`,
// `hypatia_get_confidence`, `hypatia_dispatch_strategy`).
//
// The unified-api-adapter pattern follows
// `developer-ecosystem/.../v-ecosystem/v_api_interfaces/v_api_interfaces.v`:
//
//   Core 12 protocols
//     1.  grpc
//     2.  graphql
//     3.  rest
//     4.  flatbuffers
//     5.  bebop
//     6.  jsonrpc
//     7.  websocket
//     8.  mqtt
//     9.  trpc
//     10. capnproto
//     11. soap
//     12. verisimdb-rest
//   Umoja-substrate 4
//     13. bsp           (Build Server Protocol)
//     14. scip          (Source Code Index Protocol)
//     15. ipfs
//     16. arrow-flight
//
// Each connector is a thin module (`connectors/*.zig`) exposing
// `fn start(port: u16) void` and `fn dispatch(req: []const u8, out: *ApiResponse) i32`.
// All sixteen route requests through the same core ABI — they differ
// only in transport. At the V-lang reference fidelity each `start`
// is a port-binding stub; sockets/encoding live in the per-protocol
// modules and can be filled in incrementally without changing this
// file or the C ABI.

const std = @import("std");

// ============================================================
// 16-variant tagged Connector enum — GENERATED
// ============================================================
//
// Stable wire ordering — DO NOT renumber, and do not edit by hand.
// The normative source is the Idris2 ABI `src/Hypatia/ABI/Types.idr`;
// `connector_generated.zig` is emitted from it by `just abi-gen`, as are
// the Rust client's enum and `ffi/connectors.json`.
//
// This file re-exports the generated declarations and keeps its own
// hand-written `comptime` assertion and tests below. A generated test
// would re-assert its own source and prove nothing.

const gen = @import("connector_generated.zig");

pub const Connector = gen.Connector;
pub const CONNECTOR_COUNT = gen.CONNECTOR_COUNT;

// ============================================================
// Per-connector stub modules
// ============================================================
//
// Each module matches the fidelity of the V-lang reference
// (`v_api_interfaces.v`): a `start(port)` that announces the bind
// and a `dispatch(req, out)` that calls into the Hypatia core.
// Real socket/encoding work belongs in the connector modules; this
// file only routes.


// ============================================================
// Suite — port-table for the sixteen connectors
// ============================================================

pub const UnifiedApiAdapter = struct {
    base_port: u16,

    pub fn init(base_port: u16) UnifiedApiAdapter {
        return .{ .base_port = base_port };
    }

    /// Returns the bound port for a given connector. Layout matches
    /// the V reference: `base + 1` … `base + 16`.
    pub fn portFor(self: UnifiedApiAdapter, c: Connector) u16 {
        return self.base_port + @as(u16, @intFromEnum(c)) + 1;
    }

    /// Start every connector. Each module's `start` is currently a
    /// log-only stub matching the V reference; real bind happens
    /// when each module is fleshed out individually.
    pub fn startAll(self: UnifiedApiAdapter) void {
        // Dispatch is generated: `gen.ALL` and `Connector.module()` come from
        // the Idris2 ABI, so a new connector needs no edit here.
        inline for (gen.ALL) |c| {
            c.module().start(self.portFor(c));
        }
    }
};

// ============================================================
// Dispatch — single entry point used by every connector
// ============================================================
//
// Connectors decode their wire format into a `CoreRequest`, hand it
// to `coreDispatch`, then re-encode the response back into their wire
// format. The C ABI exported in `main.zig` calls `coreDispatch`
// directly so the Rust client and other in-process consumers skip
// transport encoding altogether.

pub const CoreOp = enum(u8) {
    health_check = 0,
    scan_repo = 1,
    get_confidence = 2,
    force_learning = 3,
};

pub const CoreRequest = extern struct {
    op: CoreOp,
    arg_ptr: ?[*]const u8,
    arg_len: usize,
};

/// Dispatch table — sixteen entries, one per connector. The function
/// pointers are the per-connector decode/encode trampolines; today
/// they all delegate to the core, but the table is the seam where
/// per-protocol logic will land.
pub const ConnectorDispatchFn = *const fn (req: []const u8, out: *anyopaque) i32;

// Compile-time assertion: the connector table has exactly 16 entries.
comptime {
    if (CONNECTOR_COUNT != 16) {
        @compileError("UnifiedApiAdapter requires exactly 16 connectors");
    }
}

// ============================================================
// Tests
// ============================================================

test "exactly sixteen connectors" {
    try std.testing.expectEqual(@as(usize, 16), CONNECTOR_COUNT);
}

test "connector names are stable" {
    try std.testing.expectEqualStrings("grpc", std.mem.span(Connector.grpc.name()));
    try std.testing.expectEqualStrings("arrow-flight", std.mem.span(Connector.arrow_flight.name()));
    try std.testing.expectEqualStrings("verisimdb-rest", std.mem.span(Connector.verisimdb_rest.name()));
}

test "port layout matches v-lang reference" {
    const suite = UnifiedApiAdapter.init(8000);
    try std.testing.expectEqual(@as(u16, 8001), suite.portFor(.grpc));
    try std.testing.expectEqual(@as(u16, 8012), suite.portFor(.verisimdb_rest));
    try std.testing.expectEqual(@as(u16, 8013), suite.portFor(.bsp));
    try std.testing.expectEqual(@as(u16, 8016), suite.portFor(.arrow_flight));
}

test "wire ordering is stable" {
    // Renumbering breaks the Idris2 ABI and the Rust client. This
    // test pins the ordering by exact integer value.
    try std.testing.expectEqual(@as(u8, 0), @intFromEnum(Connector.grpc));
    try std.testing.expectEqual(@as(u8, 11), @intFromEnum(Connector.verisimdb_rest));
    try std.testing.expectEqual(@as(u8, 12), @intFromEnum(Connector.bsp));
    try std.testing.expectEqual(@as(u8, 15), @intFromEnum(Connector.arrow_flight));
}
