// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// Process-wide I/O capability for the C ABI boundary.

const std = @import("std");

/// Zig 0.16 made I/O capability an explicit `Io` parameter rather than ambient
/// global state. The `export fn` entry points in `main.zig` are a C ABI: they
/// cannot take one. In particular the six normative ABI functions enumerated in
/// `src/Hypatia/ABI/FFI.idr` -- HealthCheck, ScanRepo, Dispatch, RecordOutcome,
/// ForceLearningCycle, GetConfidence -- carry no handle to hang one off, so
/// there is nowhere to store a per-instance `Io` without changing the wire
/// contract that this FFI exists to pin.
///
/// The capability is therefore acquired here, once, at the boundary, and
/// threaded explicitly from there inwards: every internal function takes
/// `io: std.Io` as its first parameter and can be tested with an injected one.
/// This is a faithful translation rather than a new global: the 0.15 code was
/// already ambient (`std.fs.cwd()`, `std.time.timestamp()`, `std.posix.getenv`
/// are all process-global). 0.16 made the ambient explicit; the library's
/// contract did not change.
///
/// `init_single_threaded` is deliberate, not a simplification:
///
///   * `have_signal_handler = false`. `Io.Threaded.init` installs SIGIO and
///     SIGPIPE handlers (`std/Io/Threaded.zig:1660-1661`). This library is
///     loaded into a host process; silently replacing that process's signal
///     disposition on first call would be a serious bug.
///   * `async_limit = .nothing`. No worker threads are spawned, so loading the
///     shared object costs no threads.
///
/// It is a plain zero-initialised global -- no lazy initialisation, therefore
/// no initialisation race between concurrent C callers.
var threaded: std.Io.Threaded = .init_single_threaded;

/// The `Io` used by every `export fn` in `main.zig`.
pub fn io() std.Io {
    return threaded.io();
}

extern "c" fn getenv(name: [*:0]const u8) ?[*:0]u8;

/// Read a process environment variable.
///
/// Deliberately NOT routed through `Io`. Zig 0.16 removed `getenv` from `std`
/// entirely -- `std.posix.getenv` no longer exists -- and models the
/// environment as a `process.Environ` supplied to the `Io` implementation.
/// But `Io.Threaded.init_single_threaded` sets its environment block to
/// `.empty` on every non-Windows target (`std/Io/Threaded.zig:1675`:
/// `if (is_windows) .global else .empty`), so an `Io`-sourced lookup here would
/// return null for every key on Linux. Every `orelse default` would then
/// silently take the default branch and the call would be vacuous rather than
/// migrated.
///
/// `build.zig` sets `link_libc = true` on all six artifacts, so `getenv(3)` is
/// the honest equivalent of the 0.15 call this replaces.
pub fn getEnv(name: [*:0]const u8) ?[]const u8 {
    return std.mem.span(getenv(name) orelse return null);
}
