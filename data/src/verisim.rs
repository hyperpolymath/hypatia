// SPDX-License-Identifier: PMPL-1.0-or-later
//! VeriSimDB client for hypatia.
//!
//! **Status:** Minimal stub retained only for `DataLayer::health_check`.
//!
//! History: this module previously exposed an ArangoDB-shaped document API
//! (upsert_document, query_documents, apply_fix, create_hexad, etc.) whose
//! endpoints never existed in VeriSimDB. ArangoDB was removed (see
//! `data/src/lib.rs:9`) and its API never replaced. The document methods
//! were never called from production code — `ForgeService` was the only
//! consumer and was never instantiated. They have been deleted.
//!
//! When hypatia's Rust layer needs to call VeriSimDB, use the shared Zig
//! client at `verisim/connectors/clients/zig/` via FFI. See H1a.

use crate::error::{DataError, Result};
use crate::VerisimConfig;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::info;

/// Minimal VeriSimDB client.
///
/// Tracks connection state. Only `ping()` does anything beyond bookkeeping.
pub struct VerisimClient {
    connected: Arc<RwLock<bool>>,
    #[allow(dead_code)]
    url: String,
}

impl VerisimClient {
    /// Create a new VeriSimDB client and mark it connected.
    pub async fn new(config: VerisimConfig) -> Result<Self> {
        info!("Initialising VeriSimDB client for {}", config.url);

        let client = Self {
            connected: Arc::new(RwLock::new(false)),
            url: config.url.clone(),
        };

        client.connect().await?;
        Ok(client)
    }

    /// Mark the client as connected. No transport is established here.
    async fn connect(&self) -> Result<()> {
        let mut connected = self.connected.write().await;
        *connected = true;
        Ok(())
    }

    /// Health check. Returns Ok(()) if the client is in the connected state.
    ///
    /// This currently does NOT make a real HTTP request to VeriSimDB. It
    /// only checks the in-memory connected flag. A real ping will land when
    /// H1a wires this to the Zig VeriSimDB client via FFI.
    pub async fn ping(&self) -> Result<()> {
        let connected = self.connected.read().await;
        if *connected {
            Ok(())
        } else {
            Err(DataError::ConnectionError(
                "Not connected to VeriSimDB".into(),
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_client_creation() {
        let config = VerisimConfig::default();
        let client = VerisimClient::new(config).await.unwrap();
        assert!(client.ping().await.is_ok());
    }
}
