// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// # Safety
//
// This file contains thirteen `unsafe` blocks. Every one is a load-bearing
// FFI boundary call across the C ABI exported by `libhypatia_ffi.so`. They
// are NOT the kind of `unsafe` that hides a partial cast or a banned
// `Obj.magic`-style escape (per estate memory: panic-attack's `unsafe_blocks`
// category conflates banned partial casts with legitimate FFI try/catch).
//
// The invariants that justify each `unsafe`:
//
//   1. `Library::new(path)` — libloading is unsafe because loading a shared
//      object runs its constructors. Hypatia's `libhypatia_ffi.so` has no
//      constructors with side effects (verified by inspection of
//      `hypatia/ffi/zig/src/main.zig`).
//
//   2. `lib.get(b"hypatia_*\0")` — symbol lookup is unsafe because the
//      caller asserts the type signature. Each lookup matches the exact
//      `extern "C"` signature exported by `main.zig` and pinned by the
//      Idris2 ABI dependent-type proofs in `src/abi/Types.idr`. Renumbering
//      the Connector enum or changing any exported function's signature
//      breaks the build at the Idris2 layer first.
//
//   3. `Symbol::into_raw()` — detaches the symbol's lifetime from the
//      `Library` handle. Safe because we keep the `Library` alive for the
//      lifetime of `FfiTransport` via the `_lib` field — symbols become
//      invalid only when `_lib` is dropped, which is after every method
//      that uses them.
//
//   4. Calling the looked-up function pointers — safe because the type
//      signatures match the C ABI exports (see invariant 2). The Zig side
//      guarantees that `ApiResponse.data_ptr`/`error_ptr` point into a
//      static buffer that lives until the next ABI call, which is why we
//      copy out immediately rather than holding the slice.
//
//   5. `slice::from_raw_parts(data_ptr, data_len)` — safe because the Zig
//      side has just populated those fields with a successful return
//      (`rc == 0`) and the buffer is the static `response_buf` in
//      `main.zig`, which is `[8192]u8` and lives for program lifetime.
//
// The safety analysis above is the *reason* the unsafe is necessary, not a
// claim that it can be removed. There is no safe alternative to dlopen in
// Rust today; the only way to talk to a Zig shared library is unsafe FFI.
//
// FFI transport — wraps `libhypatia_ffi.so` via `libloading`.
//
// This is the *primary* transport. Loading the shared library is
// done lazily on first use; if the library cannot be found
// (`HYPATIA_FFI_LIB` env var → fall through to a hard-coded set of
// search paths), `FfiTransport::new` returns `FfiUnavailable` and
// the `Client` falls back to the subprocess transport.
//
// We bind only the small surface that the Rust client actually
// needs today:
//
//   hypatia_connector_count() -> usize
//   hypatia_connector_name(id: u8) -> *const c_char (or NULL)
//   hypatia_connector_port(id: u8, base: u16) -> u16
//   hypatia_scan_repo(ptr, len, out) -> i32
//
// The full ABI surface (dispatch, outcome, force-learning,
// confidence) is exposed by `libhypatia_ffi.so` and can be bound
// here when the client needs it. New bindings should follow the
// same shape: a `Symbol` cached in the `FfiTransport`, plus a
// safe Rust wrapper that converts `i32` return codes into
// `HypatiaError`.

use std::ffi::{CStr, OsStr};
use std::os::raw::{c_char, c_int};

use libloading::{Library, Symbol};

use crate::connector::{Connector, CONNECTOR_COUNT};
use crate::error::HypatiaError;
use crate::types::{Finding, ScanRequest, ScanResponse, ScanResult};

/// Mirror of the `ApiResponse` struct in
/// `hypatia/ffi/zig/src/main.zig`. The Zig side guarantees that
/// `data_ptr`/`error_ptr` point into a static buffer that lives
/// for the duration of the next call — we copy out immediately.
#[repr(C)]
struct ApiResponse {
    success: bool,
    data_ptr: *const u8,
    data_len: usize,
    error_ptr: *const u8,
    error_len: usize,
    timestamp_ptr: *const u8,
    timestamp_len: usize,
}

impl ApiResponse {
    fn empty() -> Self {
        ApiResponse {
            success: false,
            data_ptr: std::ptr::null(),
            data_len: 0,
            error_ptr: std::ptr::null(),
            error_len: 0,
            timestamp_ptr: std::ptr::null(),
            timestamp_len: 0,
        }
    }

    /// Safety: caller must ensure the pointers were populated by a
    /// successful call to a Hypatia ABI function and have not yet
    /// been overwritten by a subsequent call.
    unsafe fn data(&self) -> &[u8] { unsafe {
        if self.data_ptr.is_null() || self.data_len == 0 {
            &[]
        } else {
            std::slice::from_raw_parts(self.data_ptr, self.data_len)
        }
    }}

    unsafe fn error(&self) -> &[u8] { unsafe {
        if self.error_ptr.is_null() || self.error_len == 0 {
            &[]
        } else {
            std::slice::from_raw_parts(self.error_ptr, self.error_len)
        }
    }}
}

/// Loaded Hypatia FFI library plus the symbols the client uses.
pub struct FfiTransport {
    // Held to keep the dlopen handle alive — symbols below borrow it.
    _lib: Library,
    connector_count: unsafe extern "C" fn() -> usize,
    connector_name: unsafe extern "C" fn(u8) -> *const c_char,
    connector_port: unsafe extern "C" fn(u8, u16) -> u16,
    scan_repo: unsafe extern "C" fn(*const u8, usize, *mut ApiResponse) -> c_int,
}

impl FfiTransport {
    /// Try to load the Hypatia FFI library. Looks at `HYPATIA_FFI_LIB`
    /// first, then a small set of canonical paths.
    pub fn new() -> Result<Self, HypatiaError> {
        let candidates = library_search_paths();
        let mut last_err: Option<String> = None;

        for path in &candidates {
            // Safety: libloading::Library::new is unsafe because it
            // runs constructors from the loaded library. Hypatia's
            // .so has no constructors with side effects.
            match unsafe { Library::new(path) } {
                Ok(lib) => return Self::bind_symbols(lib),
                Err(e) => last_err = Some(format!("{}: {}", path.to_string_lossy(), e)),
            }
        }

        Err(HypatiaError::FfiUnavailable(
            last_err.unwrap_or_else(|| "no candidate paths".into()),
        ))
    }

    fn bind_symbols(lib: Library) -> Result<Self, HypatiaError> {
        // Safety: the symbols we look up below have stable extern "C"
        // signatures pinned by the Idris2 ABI proofs and the Zig
        // `export fn` declarations. We `Symbol::into_raw()` to
        // detach the lifetime from `lib`; the `_lib` field keeps the
        // dlopen handle alive for the lifetime of `FfiTransport`.
        unsafe {
            let connector_count: Symbol<unsafe extern "C" fn() -> usize> =
                lib.get(b"hypatia_connector_count\0").map_err(|e| {
                    HypatiaError::FfiUnavailable(format!("missing hypatia_connector_count: {}", e))
                })?;
            let connector_name: Symbol<unsafe extern "C" fn(u8) -> *const c_char> =
                lib.get(b"hypatia_connector_name\0").map_err(|e| {
                    HypatiaError::FfiUnavailable(format!("missing hypatia_connector_name: {}", e))
                })?;
            let connector_port: Symbol<unsafe extern "C" fn(u8, u16) -> u16> =
                lib.get(b"hypatia_connector_port\0").map_err(|e| {
                    HypatiaError::FfiUnavailable(format!("missing hypatia_connector_port: {}", e))
                })?;
            let scan_repo: Symbol<
                unsafe extern "C" fn(*const u8, usize, *mut ApiResponse) -> c_int,
            > = lib.get(b"hypatia_scan_repo\0").map_err(|e| {
                HypatiaError::FfiUnavailable(format!("missing hypatia_scan_repo: {}", e))
            })?;

            let connector_count_fn = *connector_count.into_raw();
            let connector_name_fn = *connector_name.into_raw();
            let connector_port_fn = *connector_port.into_raw();
            let scan_repo_fn = *scan_repo.into_raw();

            Ok(FfiTransport {
                _lib: lib,
                connector_count: connector_count_fn,
                connector_name: connector_name_fn,
                connector_port: connector_port_fn,
                scan_repo: scan_repo_fn,
            })
        }
    }

    /// Liveness probe — calls `hypatia_connector_count` and verifies
    /// it returns 16. Used by integration tests and by `Client::new`
    /// when the user wants to confirm FFI is wired before scanning.
    pub fn ping(&self) -> Result<(), HypatiaError> {
        let count = unsafe { (self.connector_count)() };
        if count != CONNECTOR_COUNT {
            return Err(HypatiaError::FfiUnavailable(format!(
                "hypatia_connector_count returned {} (expected 16)",
                count
            )));
        }
        // Cross-check that name(0) is "grpc" — catches ABI drift
        // where the count agrees but the ordering is wrong.
        let name_ptr = unsafe { (self.connector_name)(0) };
        if name_ptr.is_null() {
            return Err(HypatiaError::FfiUnavailable(
                "hypatia_connector_name(0) returned NULL".into(),
            ));
        }
        let name = unsafe { CStr::from_ptr(name_ptr) }.to_string_lossy();
        if name != Connector::Grpc.name() {
            return Err(HypatiaError::FfiUnavailable(format!(
                "wire-id 0 reported as '{}' (expected 'grpc')",
                name
            )));
        }
        Ok(())
    }

    pub fn connector_port(&self, c: Connector, base: u16) -> u16 {
        unsafe { (self.connector_port)(c as u8, base) }
    }

    /// Run a scan via the FFI. Returns the parsed `ScanResponse`.
    pub fn scan(&self, req: &ScanRequest) -> Result<ScanResponse, HypatiaError> {
        let bytes = req.repo_path.as_bytes();
        let mut resp = ApiResponse::empty();
        let rc = unsafe { (self.scan_repo)(bytes.as_ptr(), bytes.len(), &mut resp) };
        if rc != 0 {
            let msg = unsafe {
                String::from_utf8_lossy(resp.error()).into_owned()
            };
            return Err(HypatiaError::NonZero { code: rc, message: msg });
        }
        let data = unsafe { resp.data() };
        // Hypatia's `hypatia_scan_repo` returns the raw scan-store
        // JSON document; we shape it into a `ScanResponse`. If the
        // store schema diverges, this is the seam to update.
        parse_scan_payload(data)
    }
}

fn library_search_paths() -> Vec<std::path::PathBuf> {
    let mut paths: Vec<std::path::PathBuf> = Vec::new();
    if let Ok(env_path) = std::env::var("HYPATIA_FFI_LIB") {
        paths.push(env_path.into());
    }
    let names: &[&OsStr] = &[
        OsStr::new("libhypatia_ffi.so"),
        OsStr::new("libhypatia_ffi.dylib"),
        OsStr::new("hypatia_ffi.dll"),
    ];
    let dirs: &[&str] = &[
        "/usr/local/lib",
        "/usr/lib",
        "./target/release",
        "./target/debug",
        "./ffi/zig/zig-out/lib",
    ];
    for d in dirs {
        for n in names {
            let mut p = std::path::PathBuf::from(d);
            p.push(n);
            paths.push(p);
        }
    }
    paths
}

/// Best-effort parse of `hypatia_scan_repo`'s JSON payload into a
/// `ScanResponse`. The payload schema is the verisimdb-data
/// scan-store document; missing fields fall through to defaults.
fn parse_scan_payload(bytes: &[u8]) -> Result<ScanResponse, HypatiaError> {
    if bytes.is_empty() {
        return Ok(ScanResponse {
            result: ScanResult::Skip,
            findings: Vec::new(),
            score: 0,
        });
    }

    #[derive(serde::Deserialize)]
    struct StoreShape {
        #[serde(default)]
        findings: Vec<Finding>,
        #[serde(default)]
        score: Option<i32>,
        #[serde(default)]
        result: Option<String>,
    }

    let shape: StoreShape = serde_json::from_slice(bytes)?;
    let result = match shape.result.as_deref() {
        Some("pass") => ScanResult::Pass,
        Some("fail") => ScanResult::Fail,
        Some("skip") => ScanResult::Skip,
        Some("error") => ScanResult::Error,
        _ if shape.findings.is_empty() => ScanResult::Pass,
        _ => ScanResult::Fail,
    };
    Ok(ScanResponse {
        result,
        findings: shape.findings,
        score: shape.score.unwrap_or(0),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_payload_is_skip() {
        let r = parse_scan_payload(b"").unwrap();
        assert_eq!(r.result, ScanResult::Skip);
        assert!(r.findings.is_empty());
    }

    #[test]
    fn findings_present_means_fail_by_default() {
        let json = br#"{"findings":[{"rule_id":"SEC001","severity":"critical","file":"a","message":"m"}]}"#;
        let r = parse_scan_payload(json).unwrap();
        assert_eq!(r.result, ScanResult::Fail);
        assert_eq!(r.findings.len(), 1);
    }

    #[test]
    fn explicit_result_overrides_inference() {
        let json = br#"{"findings":[],"result":"pass","score":99}"#;
        let r = parse_scan_payload(json).unwrap();
        assert_eq!(r.result, ScanResult::Pass);
        assert_eq!(r.score, 99);
    }
}
