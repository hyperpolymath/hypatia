// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// UnifiedApiAdapter enum — re-exported from the generated wire contract.
//
// The normative source is the Idris2 ABI `src/Hypatia/ABI/Types.idr`.
// `connector_generated.rs` is emitted from it by `just abi-gen`, as are the
// Zig enum in `ffi/zig/src/connector_generated.zig` and `ffi/connectors.json`.
// Do not edit the generated file; edit the ABI and regenerate.
//
// The tests below are hand-maintained on purpose. A generated test would
// re-assert its own source and prove nothing; these pin the wire contract
// independently of the generator.

pub use crate::connector_generated::{Connector, CONNECTOR_COUNT};

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
