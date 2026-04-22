// SPDX-License-Identifier: PMPL-1.0-or-later
// Hypatia GUI JS loader — bridges the browser to the Gossamer/Ephapax
// WASM module. Replaces src/ui/tea/hypatia_tea.js (the AffineScript
// loader) per issue #177.
//
// The export surface (`hypatia_init`, `hypatia_update`, `hypatia_view`,
// `hypatia_subs`) is ABI-level, so this loader is language-agnostic as
// long as the wasm module preserves those symbol names. The TEA-level
// functions are defined in hypatia_gui.eph; the host integration (IPC
// + window handles) lives in bridge.eph.
//
// TODO (blockers before this file can actually load anything):
//
//   1. Resolve `wasi_snapshot_preview1` imports. Gossamer's wasm output
//      probably expects a Gossamer-flavoured host interface (not raw
//      WASI). Replace the fd_write stub below with the real import
//      object once gossamer's host-side convention is in view.
//
//   2. If Ephapax emits a string-passing convention other than
//      "pointer + leading i32 length prefix", update `readString`.
//      Coq-proven Ephapax strings are {ptr, len} pairs by convention
//      per the receipts, but the exact wasm ABI packing wasn't
//      specified in the material the drafter had access to.

/**
 * Wraps the Hypatia Ephapax/Gossamer Wasm module.
 */
class HypatiaTEA {
  constructor(exports) {
    this.exports = exports;
    this.memory = exports.memory;
  }

  init() {
    return this.exports.hypatia_init();
  }

  update(msg, model) {
    return this.exports.hypatia_update(msg, model);
  }

  view(model) {
    const ptr = this.exports.hypatia_view(model);
    return this.readString(ptr);
  }

  subs(model) {
    const ptr = this.exports.hypatia_subs(model);
    return this.readString(ptr);
  }

  // Reads a pointer + leading i32 length prefix. Matches the AffineScript
  // convention; verify Ephapax output before trusting.
  readString(ptr) {
    const view = new DataView(this.memory.buffer);
    const len = view.getInt32(ptr, true);
    const bytes = new Uint8Array(this.memory.buffer, ptr + 4, len);
    return new TextDecoder().decode(bytes);
  }
}

export async function load(url) {
  const response = await fetch(url);
  const bytes = await response.arrayBuffer();
  // TODO: replace stub wasi_snapshot_preview1 with the real Gossamer
  // host imports (see bridge.eph).
  const importObject = {
    wasi_snapshot_preview1: {
      fd_write: (fd, iovs, iovs_len, nwritten) => 0
    }
  };
  const result = await WebAssembly.instantiate(bytes, importObject);
  return new HypatiaTEA(result.instance.exports);
}

export const Msg = {
  Navigate: (dept) => ({ tag: 0, value: dept }),
  PopState: () => ({ tag: 1 })
};

// Internal Wasm tags for Msg — match the order of Ephapax constructors
// in hypatia_gui.eph (data Msg = Navigate(Department) | PopState).
export const MsgTag = {
  Navigate: 0,
  PopState: 1
};

// Match `data Department = Learning | Symbolic | Verification | Triangle`
// in hypatia_gui.eph.
export const Department = {
  Learning: 0,
  Symbolic: 1,
  Verification: 2,
  Triangle: 3
};
