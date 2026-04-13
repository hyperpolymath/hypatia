// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// hypatia-client — Rust client for the Hypatia hexadeca-connector
// surface. Replaces the deleted V-lang client at `api/v/hypatia.v`.
//
// Two transports, picked at runtime:
//
//   1. FFI primary    — `libhypatia_ffi.so` via `libloading`. Fastest;
//                       skips JSON marshalling and OS process spawn.
//   2. Subprocess     — shells out to the `hypatia` CLI binary.
//                       Used when the `.so` cannot be loaded (CI
//                       environments without the build artefact).
//
// Both transports speak the same `Severity`/`Finding`/`ScanResponse`
// types defined in `types.rs`. Consumers (e.g. 007 F7) only see the
// `Client` API; transport selection is invisible.
//
// The hexadeca surface itself (16 protocol adapters: gRPC, GraphQL,
// REST, FlatBuffers, Bebop, JSON-RPC, WebSocket, MQTT, tRPC,
// Cap'n Proto, SOAP, VeriSimDB-REST, BSP, SCIP, IPFS, Arrow Flight)
// lives on the Zig side at `hypatia/ffi/zig/src/hexadeca.zig` and is
// mirrored by the Idris2 ABI in `src/abi/Types.idr`. The Rust client
// knows about all sixteen via `Connector` so that future enumeration
// and dispatch endpoints (`hypatia_connector_count`,
// `hypatia_connector_name`, `hypatia_hexadeca_start_all`) are
// type-safe end-to-end.

pub mod client;
pub mod connector;
pub mod error;
pub mod types;

#[cfg(feature = "ffi")]
pub mod ffi;

#[cfg(feature = "subprocess")]
pub mod subprocess;

pub use client::{Client, ClientConfig, Transport};
pub use connector::{Connector, CONNECTOR_COUNT};
pub use error::HypatiaError;
pub use types::{Finding, ScanRequest, ScanResponse, ScanResult, Severity};
