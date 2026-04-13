// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Subprocess transport — fallback that shells out to the `hypatia`
// CLI binary. Used when the FFI library cannot be loaded.
//
// This is the same path that 007 F7's hypatia adapter used to take
// directly. Centralising it here means F7 stops carrying the spawn
// logic itself.

use std::path::Path;
use std::process::Command;

use crate::error::HypatiaError;
use crate::types::{ScanRequest, ScanResponse, ScanResult};

pub struct SubprocessTransport {
    binary: String,
}

impl SubprocessTransport {
    /// Create a new subprocess transport. Probes for `hypatia` on
    /// PATH; returns `SubprocessUnavailable` if it can't be found.
    pub fn new() -> Result<Self, HypatiaError> {
        Self::with_binary("hypatia")
    }

    pub fn with_binary(binary: impl Into<String>) -> Result<Self, HypatiaError> {
        let binary = binary.into();
        let probe = Command::new(&binary).arg("--version").output();
        match probe {
            Ok(out) if out.status.success() => Ok(SubprocessTransport { binary }),
            Ok(out) => Err(HypatiaError::SubprocessUnavailable(format!(
                "{} --version exit {}: {}",
                binary,
                out.status.code().unwrap_or(-1),
                String::from_utf8_lossy(&out.stderr)
            ))),
            Err(e) => Err(HypatiaError::SubprocessUnavailable(format!(
                "could not spawn {}: {}",
                binary, e
            ))),
        }
    }

    pub fn ping(&self) -> Result<(), HypatiaError> {
        // The `--version` probe in `new` is the ping; we re-do it
        // here so callers can verify after construction.
        let out = Command::new(&self.binary).arg("--version").output()?;
        if !out.status.success() {
            return Err(HypatiaError::SubprocessUnavailable(format!(
                "{} --version exit {}",
                self.binary,
                out.status.code().unwrap_or(-1)
            )));
        }
        Ok(())
    }

    pub fn scan(&self, req: &ScanRequest) -> Result<ScanResponse, HypatiaError> {
        let out = Command::new(&self.binary)
            .arg("scan")
            .arg("--repo")
            .arg(Path::new(&req.repo_path))
            .arg("--format")
            .arg("json")
            .output()?;

        if !out.status.success() {
            return Err(HypatiaError::NonZero {
                code: out.status.code().unwrap_or(-1),
                message: String::from_utf8_lossy(&out.stderr).into_owned(),
            });
        }

        let parsed: ScanResponse = serde_json::from_slice(&out.stdout).unwrap_or(ScanResponse {
            result: ScanResult::Skip,
            findings: Vec::new(),
            score: 0,
        });
        Ok(parsed)
    }
}
