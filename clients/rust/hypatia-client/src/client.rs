// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// `Client` — top-level entry point. Picks an FFI or subprocess
// transport at construction time. Consumers (007 F7, panll panels,
// CI scripts) only ever talk to this struct.

use crate::error::HypatiaError;
use crate::types::{
    DispatchEntry, DispatchStrategy, HealthReport, OutcomeRecord, ScanRequest, ScanResponse,
};

#[cfg(feature = "ffi")]
use crate::ffi::FfiTransport;
#[cfg(feature = "subprocess")]
use crate::subprocess::SubprocessTransport;

/// Which transport the client is using. Determined at construction;
/// switching at runtime requires building a new `Client`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Transport {
    Ffi,
    Subprocess,
    /// Neither transport is available — the `Client` will return
    /// `HypatiaError::FfiUnavailable` (with details from both
    /// attempts) on every operation. F7 treats this as a
    /// "subsystem unavailable" Diagnostic and continues.
    None,
}

/// Configuration for `Client::with_config`. Defaults try FFI first,
/// then subprocess. Override `prefer` if you want to force one path.
#[derive(Debug, Clone, Default)]
pub struct ClientConfig {
    pub prefer: Option<Transport>,
}

/// Hypatia client. Carries an active transport (or `None` if both
/// failed at construction).
pub struct Client {
    transport: ActiveTransport,
}

enum ActiveTransport {
    #[cfg(feature = "ffi")]
    Ffi(FfiTransport),
    #[cfg(feature = "subprocess")]
    Subprocess(SubprocessTransport),
    None {
        ffi_error: Option<String>,
        subprocess_error: Option<String>,
    },
}

impl Client {
    /// Build a client with default configuration: try FFI, then
    /// subprocess. Never panics — degrades to `Transport::None` if
    /// both fail.
    pub fn new() -> Self {
        Self::with_config(ClientConfig::default())
    }

    pub fn with_config(cfg: ClientConfig) -> Self {
        match cfg.prefer {
            #[cfg(feature = "ffi")]
            Some(Transport::Ffi) => Self::try_ffi_only(),
            #[cfg(feature = "subprocess")]
            Some(Transport::Subprocess) => Self::try_subprocess_only(),
            _ => Self::try_both(),
        }
    }

    fn try_both() -> Self {
        #[allow(unused_assignments)]
        let mut ffi_err: Option<String> = None;
        #[allow(unused_assignments)]
        let mut sub_err: Option<String> = None;

        #[cfg(feature = "ffi")]
        {
            match FfiTransport::new().and_then(|t| t.ping().map(|_| t)) {
                Ok(t) => {
                    return Client {
                        transport: ActiveTransport::Ffi(t),
                    }
                }
                Err(e) => ffi_err = Some(e.to_string()),
            }
        }

        #[cfg(feature = "subprocess")]
        {
            match SubprocessTransport::new() {
                Ok(t) => {
                    return Client {
                        transport: ActiveTransport::Subprocess(t),
                    }
                }
                Err(e) => sub_err = Some(e.to_string()),
            }
        }

        Client {
            transport: ActiveTransport::None {
                ffi_error: ffi_err,
                subprocess_error: sub_err,
            },
        }
    }

    #[cfg(feature = "ffi")]
    fn try_ffi_only() -> Self {
        match FfiTransport::new().and_then(|t| t.ping().map(|_| t)) {
            Ok(t) => Client {
                transport: ActiveTransport::Ffi(t),
            },
            Err(e) => Client {
                transport: ActiveTransport::None {
                    ffi_error: Some(e.to_string()),
                    subprocess_error: None,
                },
            },
        }
    }

    #[cfg(feature = "subprocess")]
    fn try_subprocess_only() -> Self {
        match SubprocessTransport::new() {
            Ok(t) => Client {
                transport: ActiveTransport::Subprocess(t),
            },
            Err(e) => Client {
                transport: ActiveTransport::None {
                    ffi_error: None,
                    subprocess_error: Some(e.to_string()),
                },
            },
        }
    }

    /// Which transport this client picked. Useful for diagnostics
    /// and for the F7 panel's "transport in use" field.
    pub fn transport(&self) -> Transport {
        match &self.transport {
            #[cfg(feature = "ffi")]
            ActiveTransport::Ffi(_) => Transport::Ffi,
            #[cfg(feature = "subprocess")]
            ActiveTransport::Subprocess(_) => Transport::Subprocess,
            ActiveTransport::None { .. } => Transport::None,
        }
    }

    /// True if both transports failed to come up. Consumers can
    /// check this once after `Client::new` instead of waiting for
    /// the first scan to fail.
    pub fn is_unavailable(&self) -> bool {
        matches!(self.transport, ActiveTransport::None { .. })
    }

    /// If unavailable, return a combined diagnostic message
    /// describing both transport failures. Returns `None` if a
    /// transport is active.
    pub fn unavailability_reason(&self) -> Option<String> {
        match &self.transport {
            ActiveTransport::None {
                ffi_error,
                subprocess_error,
            } => {
                let mut parts: Vec<String> = Vec::new();
                if let Some(e) = ffi_error {
                    parts.push(format!("ffi: {}", e));
                }
                if let Some(e) = subprocess_error {
                    parts.push(format!("subprocess: {}", e));
                }
                if parts.is_empty() {
                    Some("no transport compiled in".into())
                } else {
                    Some(parts.join("; "))
                }
            }
            _ => None,
        }
    }

    /// Run a Hypatia scan via whichever transport is active.
    pub fn scan(&self, req: &ScanRequest) -> Result<ScanResponse, HypatiaError> {
        match &self.transport {
            #[cfg(feature = "ffi")]
            ActiveTransport::Ffi(t) => t.scan(req),
            #[cfg(feature = "subprocess")]
            ActiveTransport::Subprocess(t) => t.scan(req),
            ActiveTransport::None { .. } => Err(self.unavailable_error()),
        }
    }

    /// Run a health check. FFI-only — the subprocess transport
    /// would have to shell `hypatia health` and parse the output;
    /// for now we return `FfiUnavailable` if FFI is not active.
    /// Adding a subprocess fallback is one match arm and a
    /// `Command::new("hypatia")` call once that CLI subcommand is
    /// confirmed.
    pub fn health_check(&self) -> Result<HealthReport, HypatiaError> {
        match &self.transport {
            #[cfg(feature = "ffi")]
            ActiveTransport::Ffi(t) => t.health_check(),
            #[cfg(feature = "subprocess")]
            ActiveTransport::Subprocess(_) => Err(HypatiaError::FfiUnavailable(
                "health_check requires the FFI transport (subprocess fallback not yet wired)"
                    .into(),
            )),
            ActiveTransport::None { .. } => Err(self.unavailable_error()),
        }
    }

    /// Dispatch a fix entry to the fleet.
    pub fn dispatch(&self, entry: &DispatchEntry) -> Result<(), HypatiaError> {
        match &self.transport {
            #[cfg(feature = "ffi")]
            ActiveTransport::Ffi(t) => t.dispatch(entry),
            #[cfg(feature = "subprocess")]
            ActiveTransport::Subprocess(_) => Err(HypatiaError::FfiUnavailable(
                "dispatch requires the FFI transport (subprocess fallback not yet wired)".into(),
            )),
            ActiveTransport::None { .. } => Err(self.unavailable_error()),
        }
    }

    /// Record an outcome for the learning loop.
    pub fn record_outcome(&self, record: &OutcomeRecord) -> Result<(), HypatiaError> {
        match &self.transport {
            #[cfg(feature = "ffi")]
            ActiveTransport::Ffi(t) => t.record_outcome(record),
            #[cfg(feature = "subprocess")]
            ActiveTransport::Subprocess(_) => Err(HypatiaError::FfiUnavailable(
                "record_outcome requires the FFI transport (subprocess fallback not yet wired)"
                    .into(),
            )),
            ActiveTransport::None { .. } => Err(self.unavailable_error()),
        }
    }

    /// Force a learning cycle.
    pub fn force_learning_cycle(&self) -> Result<(), HypatiaError> {
        match &self.transport {
            #[cfg(feature = "ffi")]
            ActiveTransport::Ffi(t) => t.force_learning_cycle(),
            #[cfg(feature = "subprocess")]
            ActiveTransport::Subprocess(_) => Err(HypatiaError::FfiUnavailable(
                "force_learning_cycle requires the FFI transport (subprocess fallback not yet wired)"
                    .into(),
            )),
            ActiveTransport::None { .. } => Err(self.unavailable_error()),
        }
    }

    /// Look up the current confidence for a recipe id.
    pub fn get_confidence(&self, recipe_id: &str) -> Result<f64, HypatiaError> {
        match &self.transport {
            #[cfg(feature = "ffi")]
            ActiveTransport::Ffi(t) => t.get_confidence(recipe_id),
            #[cfg(feature = "subprocess")]
            ActiveTransport::Subprocess(_) => Err(HypatiaError::FfiUnavailable(
                "get_confidence requires the FFI transport (subprocess fallback not yet wired)"
                    .into(),
            )),
            ActiveTransport::None { .. } => Err(self.unavailable_error()),
        }
    }

    /// Map a confidence to a dispatch strategy. Falls back to the
    /// pure-Rust `DispatchStrategy::from_confidence` if no FFI is
    /// active — this method is total, never returns an error.
    pub fn dispatch_strategy(&self, confidence: f64) -> DispatchStrategy {
        match &self.transport {
            #[cfg(feature = "ffi")]
            ActiveTransport::Ffi(t) => t.dispatch_strategy_for(confidence),
            _ => DispatchStrategy::from_confidence(confidence),
        }
    }

    /// Build a `FfiUnavailable` describing why no transport is up.
    /// Centralised so the six new wrappers above stay tidy.
    fn unavailable_error(&self) -> HypatiaError {
        match &self.transport {
            ActiveTransport::None {
                ffi_error,
                subprocess_error,
            } => {
                let combined = format!(
                    "ffi: {} | subprocess: {}",
                    ffi_error.clone().unwrap_or_else(|| "n/a".into()),
                    subprocess_error.clone().unwrap_or_else(|| "n/a".into()),
                );
                HypatiaError::FfiUnavailable(combined)
            }
            _ => HypatiaError::FfiUnavailable("transport active — call routed incorrectly".into()),
        }
    }
}

impl Default for Client {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn client_degrades_gracefully_when_nothing_is_available() {
        // In CI we typically have neither libhypatia_ffi.so nor a
        // hypatia binary on PATH. The client should construct
        // anyway and report Transport::None.
        let client = Client::new();
        // We can't assert which transport was picked — depends on
        // environment. We CAN assert that construction does not
        // panic and that calling scan returns a structured error
        // when unavailable.
        if client.is_unavailable() {
            assert_eq!(client.transport(), Transport::None);
            assert!(client.unavailability_reason().is_some());
            let err = client
                .scan(&ScanRequest {
                    repo_path: "/tmp/x".into(),
                    rules: vec![],
                })
                .unwrap_err();
            assert!(matches!(err, HypatiaError::FfiUnavailable(_)));
        }
    }

    #[test]
    fn unavailable_client_returns_errors_from_all_six_new_methods() {
        let client = Client::new();
        if !client.is_unavailable() {
            // We have a live transport — skip; the structural test
            // is only meaningful when no transport is up.
            return;
        }

        use crate::types::{BotId, DispatchEntry, Outcome, OutcomeRecord, TriangleTier};

        assert!(matches!(
            client.health_check(),
            Err(HypatiaError::FfiUnavailable(_))
        ));

        let entry = DispatchEntry {
            bot: BotId::Rhodibot,
            repo: "test/repo".into(),
            file: "src/main.rs".into(),
            recipe_id: "PA001".into(),
            tier: TriangleTier::Eliminate,
            strategy: DispatchStrategy::Review,
        };
        assert!(matches!(
            client.dispatch(&entry),
            Err(HypatiaError::FfiUnavailable(_))
        ));

        let outcome = OutcomeRecord {
            recipe_id: "PA001".into(),
            repo: "test/repo".into(),
            file: "src/main.rs".into(),
            outcome: Outcome::Success,
            timestamp: "2026-04-13T00:00:00Z".into(),
            bot: "rhodibot".into(),
        };
        assert!(matches!(
            client.record_outcome(&outcome),
            Err(HypatiaError::FfiUnavailable(_))
        ));
        assert!(matches!(
            client.force_learning_cycle(),
            Err(HypatiaError::FfiUnavailable(_))
        ));
        assert!(matches!(
            client.get_confidence("PA001"),
            Err(HypatiaError::FfiUnavailable(_))
        ));
    }

    #[test]
    fn dispatch_strategy_falls_back_to_pure_rust_when_no_ffi() {
        // dispatch_strategy is the one method that never returns an
        // error — when FFI is unavailable it falls through to the
        // pure-Rust DispatchStrategy::from_confidence implementation.
        let client = Client::new();
        if client.is_unavailable() {
            assert_eq!(
                client.dispatch_strategy(0.99),
                DispatchStrategy::AutoExecute
            );
            assert_eq!(client.dispatch_strategy(0.90), DispatchStrategy::Review);
            assert_eq!(client.dispatch_strategy(0.50), DispatchStrategy::ReportOnly);
        }
    }
}
