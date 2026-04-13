// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Hypatia Hexadeca-Connector — sixteen-protocol unified API surface.
//
// Replaces the V-lang `api/v/hypatia.v` client (deleted 2026-04-13)
// with a Zig-side multi-transport surface that wraps the existing
// Hypatia C ABI core (`hypatia_health_check`, `hypatia_scan_repo`,
// `hypatia_dispatch`, `hypatia_record_outcome`, `hypatia_force_learning_cycle`,
// `hypatia_get_confidence`, `hypatia_dispatch_strategy`).
//
// The hexadeca pattern follows
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
// 16-variant tagged Connector enum
// ============================================================
//
// Stable wire ordering — DO NOT renumber. The Idris2 ABI mirrors this
// in `src/abi/Types.idr` and the Rust client in
// `clients/rust/hypatia-client/src/connector.rs`. All three must agree.

pub const Connector = enum(u8) {
    // Core 12
    grpc = 0,
    graphql = 1,
    rest = 2,
    flatbuffers = 3,
    bebop = 4,
    jsonrpc = 5,
    websocket = 6,
    mqtt = 7,
    trpc = 8,
    capnproto = 9,
    soap = 10,
    verisimdb_rest = 11,
    // Umoja-substrate 4
    bsp = 12,
    scip = 13,
    ipfs = 14,
    arrow_flight = 15,

    pub fn name(self: Connector) [*:0]const u8 {
        return switch (self) {
            .grpc => "grpc",
            .graphql => "graphql",
            .rest => "rest",
            .flatbuffers => "flatbuffers",
            .bebop => "bebop",
            .jsonrpc => "jsonrpc",
            .websocket => "websocket",
            .mqtt => "mqtt",
            .trpc => "trpc",
            .capnproto => "capnproto",
            .soap => "soap",
            .verisimdb_rest => "verisimdb-rest",
            .bsp => "bsp",
            .scip => "scip",
            .ipfs => "ipfs",
            .arrow_flight => "arrow-flight",
        };
    }
};

/// The total number of connectors. Compile-time constant; tests
/// assert that it equals 16.
pub const CONNECTOR_COUNT: usize = @typeInfo(Connector).@"enum".fields.len;

// ============================================================
// Per-connector stub modules
// ============================================================
//
// Each module matches the fidelity of the V-lang reference
// (`v_api_interfaces.v`): a `start(port)` that announces the bind
// and a `dispatch(req, out)` that calls into the Hypatia core.
// Real socket/encoding work belongs in the connector modules; this
// file only routes.

const grpc = @import("connectors/grpc.zig");
const graphql = @import("connectors/graphql.zig");
const rest = @import("connectors/rest.zig");
const flatbuffers = @import("connectors/flatbuffers.zig");
const bebop = @import("connectors/bebop.zig");
const jsonrpc = @import("connectors/jsonrpc.zig");
const websocket = @import("connectors/websocket.zig");
const mqtt = @import("connectors/mqtt.zig");
const trpc = @import("connectors/trpc.zig");
const capnproto = @import("connectors/capnproto.zig");
const soap = @import("connectors/soap.zig");
const verisimdb_rest = @import("connectors/verisimdb_rest.zig");
const bsp = @import("connectors/bsp.zig");
const scip = @import("connectors/scip.zig");
const ipfs = @import("connectors/ipfs.zig");
const arrow_flight = @import("connectors/arrow_flight.zig");

// ============================================================
// Suite — port-table for the sixteen connectors
// ============================================================

pub const HexadecaSuite = struct {
    base_port: u16,

    pub fn init(base_port: u16) HexadecaSuite {
        return .{ .base_port = base_port };
    }

    /// Returns the bound port for a given connector. Layout matches
    /// the V reference: `base + 1` … `base + 16`.
    pub fn portFor(self: HexadecaSuite, c: Connector) u16 {
        return self.base_port + @as(u16, @intFromEnum(c)) + 1;
    }

    /// Start every connector. Each module's `start` is currently a
    /// log-only stub matching the V reference; real bind happens
    /// when each module is fleshed out individually.
    pub fn startAll(self: HexadecaSuite) void {
        grpc.start(self.portFor(.grpc));
        graphql.start(self.portFor(.graphql));
        rest.start(self.portFor(.rest));
        flatbuffers.start(self.portFor(.flatbuffers));
        bebop.start(self.portFor(.bebop));
        jsonrpc.start(self.portFor(.jsonrpc));
        websocket.start(self.portFor(.websocket));
        mqtt.start(self.portFor(.mqtt));
        trpc.start(self.portFor(.trpc));
        capnproto.start(self.portFor(.capnproto));
        soap.start(self.portFor(.soap));
        verisimdb_rest.start(self.portFor(.verisimdb_rest));
        bsp.start(self.portFor(.bsp));
        scip.start(self.portFor(.scip));
        ipfs.start(self.portFor(.ipfs));
        arrow_flight.start(self.portFor(.arrow_flight));
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
        @compileError("Hexadeca-Connector requires exactly 16 connectors");
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
    const suite = HexadecaSuite.init(8000);
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
