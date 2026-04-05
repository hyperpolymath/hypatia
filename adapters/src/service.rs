// SPDX-License-Identifier: PMPL-1.0-or-later
//! Shared service-layer types.
//!
//! **Status:** `ForgeService` (ArangoDB-era coordinator) was removed — it
//! was never instantiated anywhere in the codebase, and depended on the
//! dead `VerisimClient` document stubs (see `data/src/verisim.rs` header).
//!
//! The two data types below remain because `SyncResult` is exercised by
//! `tests/seam_tests.rs` and `ServiceHealth` is a public re-export. They
//! may be re-used when a real data-layer coordinator is built.

/// Result of a sync operation across forges.
#[derive(Debug, Default)]
pub struct SyncResult {
    pub repos_synced: usize,
    pub alerts_synced: usize,
    pub workflows_synced: usize,
}

/// Health status for a data-backed service.
#[derive(Debug)]
pub struct ServiceHealth {
    pub verisim: String,
    pub dragonfly: String,
    pub adapters: String,
    pub overall: String,
}
