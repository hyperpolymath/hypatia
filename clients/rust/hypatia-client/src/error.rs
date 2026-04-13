// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

use thiserror::Error;

/// Top-level error returned by every `Client` method.
#[derive(Debug, Error)]
pub enum HypatiaError {
    #[error("FFI transport unavailable: {0}")]
    FfiUnavailable(String),

    #[error("Subprocess transport unavailable: {0}")]
    SubprocessUnavailable(String),

    #[error("Hypatia returned a non-zero status: {code}: {message}")]
    NonZero { code: i32, message: String },

    #[error("Failed to decode Hypatia response as JSON: {0}")]
    Decode(#[from] serde_json::Error),

    #[error("I/O error talking to Hypatia: {0}")]
    Io(#[from] std::io::Error),
}
