// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Hexadeca-Connector enum — mirror of the Zig
// `Connector` enum in `ffi/zig/src/hexadeca.zig` and the Idris2
// `Connector` data type in `src/abi/Types.idr`.
//
// **Wire ordering is load-bearing.** The integer value of each variant
// is the C ABI id used by `hypatia_connector_name(id)` and
// `hypatia_connector_port(id, base)`. Renumbering this enum without
// updating the Zig and Idris2 sides is a hard ABI break — the
// Idris2 `connectorCount : length allConnectors = 16` proof catches
// counts but not orderings.

use serde::{Deserialize, Serialize};

/// The sixteen protocol connectors exposed by the Hypatia
/// hexadeca-connector surface.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[repr(u8)]
pub enum Connector {
    // ---- Core 12 ----
    Grpc = 0,
    GraphQl = 1,
    Rest = 2,
    FlatBuffers = 3,
    Bebop = 4,
    JsonRpc = 5,
    WebSocket = 6,
    Mqtt = 7,
    Trpc = 8,
    CapnProto = 9,
    Soap = 10,
    VerisimdbRest = 11,
    // ---- Umoja-substrate 4 ----
    Bsp = 12,
    Scip = 13,
    Ipfs = 14,
    ArrowFlight = 15,
}

/// The total number of connectors. Pinned at 16 by the
/// `comptime` assertion in `hexadeca.zig` and the
/// `connectorCount : length allConnectors = 16` proof in
/// `src/abi/Types.idr`.
pub const CONNECTOR_COUNT: usize = 16;

impl Connector {
    /// Canonical wire name. Must agree with the Zig side and the
    /// Idris2 ABI; tests assert all sixteen.
    pub fn name(self) -> &'static str {
        match self {
            Connector::Grpc => "grpc",
            Connector::GraphQl => "graphql",
            Connector::Rest => "rest",
            Connector::FlatBuffers => "flatbuffers",
            Connector::Bebop => "bebop",
            Connector::JsonRpc => "jsonrpc",
            Connector::WebSocket => "websocket",
            Connector::Mqtt => "mqtt",
            Connector::Trpc => "trpc",
            Connector::CapnProto => "capnproto",
            Connector::Soap => "soap",
            Connector::VerisimdbRest => "verisimdb-rest",
            Connector::Bsp => "bsp",
            Connector::Scip => "scip",
            Connector::Ipfs => "ipfs",
            Connector::ArrowFlight => "arrow-flight",
        }
    }

    /// All sixteen connectors in wire order.
    pub fn all() -> [Connector; CONNECTOR_COUNT] {
        [
            Connector::Grpc,
            Connector::GraphQl,
            Connector::Rest,
            Connector::FlatBuffers,
            Connector::Bebop,
            Connector::JsonRpc,
            Connector::WebSocket,
            Connector::Mqtt,
            Connector::Trpc,
            Connector::CapnProto,
            Connector::Soap,
            Connector::VerisimdbRest,
            Connector::Bsp,
            Connector::Scip,
            Connector::Ipfs,
            Connector::ArrowFlight,
        ]
    }

    /// Try to construct a connector from its wire id. Returns
    /// `None` for ids outside `0..16`.
    pub fn from_id(id: u8) -> Option<Connector> {
        Self::all().get(id as usize).copied()
    }

    /// Bound port for this connector under a given base port.
    /// Layout matches the V-lang reference (`base + id + 1`).
    pub fn port(self, base_port: u16) -> u16 {
        base_port + (self as u8 as u16) + 1
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn count_is_sixteen() {
        assert_eq!(Connector::all().len(), CONNECTOR_COUNT);
        assert_eq!(CONNECTOR_COUNT, 16);
    }

    #[test]
    fn wire_ids_are_stable() {
        // Pinned ordering — renumbering is an ABI break.
        assert_eq!(Connector::Grpc as u8, 0);
        assert_eq!(Connector::VerisimdbRest as u8, 11);
        assert_eq!(Connector::Bsp as u8, 12);
        assert_eq!(Connector::ArrowFlight as u8, 15);
    }

    #[test]
    fn names_round_trip() {
        for c in Connector::all() {
            let name = c.name();
            assert!(!name.is_empty());
        }
        assert_eq!(Connector::Grpc.name(), "grpc");
        assert_eq!(Connector::VerisimdbRest.name(), "verisimdb-rest");
        assert_eq!(Connector::ArrowFlight.name(), "arrow-flight");
    }

    #[test]
    fn from_id_round_trip() {
        for (idx, c) in Connector::all().iter().enumerate() {
            assert_eq!(Connector::from_id(idx as u8), Some(*c));
        }
        assert_eq!(Connector::from_id(16), None);
        assert_eq!(Connector::from_id(255), None);
    }

    #[test]
    fn port_layout_matches_v_lang() {
        assert_eq!(Connector::Grpc.port(8000), 8001);
        assert_eq!(Connector::VerisimdbRest.port(8000), 8012);
        assert_eq!(Connector::ArrowFlight.port(8000), 8016);
    }
}
