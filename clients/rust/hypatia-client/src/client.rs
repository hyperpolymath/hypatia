// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// `Client` — top-level entry point. Picks an FFI or subprocess
// transport at construction time. Consumers (007 F7, panll panels,
// CI scripts) only ever talk to this struct.

use crate::error::HypatiaError;
use crate::types::{ScanRequest, ScanResponse};

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
#[derive(Debug, Clone)]
pub struct ClientConfig {
    pub prefer: Option<Transport>,
}

impl Default for ClientConfig {
    fn default() -> Self { ClientConfig { prefer: None } }
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
                Ok(t) => return Client { transport: ActiveTransport::Ffi(t) },
                Err(e) => ffi_err = Some(e.to_string()),
            }
        }

        #[cfg(feature = "subprocess")]
        {
            match SubprocessTransport::new() {
                Ok(t) => return Client { transport: ActiveTransport::Subprocess(t) },
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
            Ok(t) => Client { transport: ActiveTransport::Ffi(t) },
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
            Ok(t) => Client { transport: ActiveTransport::Subprocess(t) },
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
            ActiveTransport::None { ffi_error, subprocess_error } => {
                let mut parts: Vec<String> = Vec::new();
                if let Some(e) = ffi_error { parts.push(format!("ffi: {}", e)); }
                if let Some(e) = subprocess_error { parts.push(format!("subprocess: {}", e)); }
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
            ActiveTransport::None { ffi_error, subprocess_error } => {
                let combined = format!(
                    "ffi: {} | subprocess: {}",
                    ffi_error.clone().unwrap_or_else(|| "n/a".into()),
                    subprocess_error.clone().unwrap_or_else(|| "n/a".into()),
                );
                Err(HypatiaError::FfiUnavailable(combined))
            }
        }
    }
}

impl Default for Client {
    fn default() -> Self { Self::new() }
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
                .scan(&ScanRequest { repo_path: "/tmp/x".into(), rules: vec![] })
                .unwrap_err();
            assert!(matches!(err, HypatiaError::FfiUnavailable(_)));
        }
    }
}
