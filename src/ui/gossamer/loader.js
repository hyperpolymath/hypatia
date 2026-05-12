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
// Host import surface
// -------------------
// bridge.eph declares an `extern "gossamer"` block. By convention, those
// externs land in the wasm module under the `gossamer` namespace, so the
// import object below mirrors the signatures one-for-one. When the real
// Gossamer JS host runtime is available, replace `makeGossamerHost()`
// with `import { host } from '@hyperpolymath/gossamer'` (or the runtime
// equivalent) and pass it as `gossamer: host`.
//
// String ABI
// ----------
// Ephapax strings are {ptr, len} pairs (Coq-proven invariant per the
// project receipts). The wasm-level packing isn't formally specified
// yet; the working assumption is "ptr returned from the export points
// to a header word containing the i32 byte length, followed by len
// bytes of UTF-8". `readString` enforces that with a bounds check so a
// mismatch surfaces as a clear error rather than a silent garble.
//
// The verification gate (issue #215) is hyperpolymath/ephapax#36, which
// tracks the v2 grammar in `ephapax-parser`: bridge.eph uses module /
// import / extern / data / match / let! constructs that the v1.0.0
// parser rejects, so no real `hypatia_gui.wasm` exists to test against
// yet. The `ephapax compile` subcommand already ships in v1.0.0; once
// the parser learns v2, this routine becomes the single point of
// change if the ABI differs from the assumed convention.

const WINDOW_HANDLE = 1;
// IPC channel handles are allocated per `ipc_open` starting at 2 so that
// 0 stays reserved for "no channel" / "no bytes available" returns and 1
// stays reserved for the singleton WINDOW_HANDLE.
const IPC_HANDLE_BASE = 2;

// DataView cache keyed by ArrayBuffer identity. WebAssembly memory.grow
// detaches the previous buffer and returns a fresh one, so a per-buffer
// WeakMap automatically picks up the new buffer on first use and lets the
// old DataView be garbage-collected. Eliminates the `new DataView(buffer)`
// allocation that used to happen on every readEphapaxString call.
const VIEW_CACHE = new WeakMap();
function getDataView(buffer) {
  let view = VIEW_CACHE.get(buffer);
  if (!view) {
    view = new DataView(buffer);
    VIEW_CACHE.set(buffer, view);
  }
  return view;
}

// Reads an Ephapax string: i32 length header at `ptr`, followed by
// `len` UTF-8 bytes from `buffer`. Throws on out-of-bounds rather than
// reading garbage past the heap, so an ABI mismatch is visible.
// Shared between HypatiaTEA (export-side decodes) and the gossamer
// host import namespace (host-side decodes) so both observe the same
// invariants.
export function readEphapaxString(buffer, ptr) {
  if (ptr < 0 || ptr + 4 > buffer.byteLength) {
    throw new RangeError(`readString: header ptr=${ptr} out of bounds`);
  }
  const view = getDataView(buffer);
  const len = view.getInt32(ptr, true);
  if (len < 0 || ptr + 4 + len > buffer.byteLength) {
    throw new RangeError(`readString: len=${len} at ptr=${ptr} out of bounds`);
  }
  const bytes = new Uint8Array(buffer, ptr + 4, len);
  return new TextDecoder().decode(bytes);
}

// Probes a wasm `instance.exports` table for a single-arg allocator
// function suitable for handing bytes back into wasm memory from the
// host. Returns the first match by name, or `null` if none of the
// candidate names export a function.
//
// The candidate list mirrors conventions used across the Ephapax /
// Gossamer ecosystem and the wider linear-types / TEA-on-wasm space:
//
//   __alloc, alloc                 — Ephapax / Gossamer convention
//   hypatia_alloc, gossamer_alloc  — namespaced variants
//   __wbindgen_malloc              — wasm-bindgen compatibility
//   malloc                         — emscripten / generic C-ABI
//
// Once hyperpolymath/ephapax#36 pins the canonical allocator export
// name, the list above can be tightened to just that one — for now we
// accept any of them so the loader is forward-compatible with whatever
// the upstream lands on.
export function probeAllocator(exports) {
  const candidates = [
    '__alloc',
    'alloc',
    'hypatia_alloc',
    'gossamer_alloc',
    '__wbindgen_malloc',
    'malloc',
  ];
  for (const name of candidates) {
    const fn = exports[name];
    if (typeof fn === 'function') return fn;
  }
  return null;
}

// Writes an Ephapax `Bytes` value (i32 length header + UTF-8/raw bytes)
// into wasm memory by calling `allocator(4 + bytes.length)`, populating
// the layout, and returning the pointer to the header. Used by
// `ipc_recv` to hand received frames back to the wasm-side bridge.
function writeEphapaxBytes(buffer, allocator, bytes) {
  const ptr = allocator(4 + bytes.length);
  if (typeof ptr !== 'number' || ptr <= 0) {
    throw new Error('allocator returned a non-pointer value');
  }
  if (ptr + 4 + bytes.length > buffer.byteLength) {
    throw new RangeError(
      `allocator returned ptr=${ptr} with no room for ${bytes.length} bytes (buffer=${buffer.byteLength})`,
    );
  }
  const view = getDataView(buffer);
  view.setInt32(ptr, bytes.length, true);
  new Uint8Array(buffer, ptr + 4, bytes.length).set(bytes);
  return ptr;
}

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

  readString(ptr) {
    return readEphapaxString(this.memory.buffer, ptr);
  }
}

// Build the `gossamer` import namespace. Signatures mirror the
// `extern "gossamer"` block in src/ui/gossamer/bridge.eph. The browser
// stubs here are minimal — enough to instantiate the module without a
// real Gossamer host so the TEA exports (init/update/view/subs) remain
// callable from index.html. The bridge's `main`/`run` loop (which calls
// window_open / ipc_open / ipc_recv) is exercised only when a real host
// invokes it; on the panel side, index.html drives the loop manually.
export function makeGossamerHost(memory, options = {}) {
  const readString = (ptr) => readEphapaxString(memory.buffer, ptr);
  // Body bytes ABI mirrors strings (i32 length header + `len` payload
  // bytes). Validated against the documented {ptr, len} convention in
  // bridge.eph; the host-side decoder enforces it via bounds checks so an
  // upstream ABI mismatch surfaces as a clear RangeError rather than a
  // silent garble. The smoke harness round-trips bytes through both
  // ipc_send and ipc_recv to lock the convention.
  const readBytes = (ptr) => {
    const buffer = memory.buffer;
    if (ptr < 0 || ptr + 4 > buffer.byteLength) {
      throw new RangeError(`readBytes: header ptr=${ptr} out of bounds`);
    }
    const view = getDataView(buffer);
    const len = view.getInt32(ptr, true);
    if (len < 0 || ptr + 4 + len > buffer.byteLength) {
      throw new RangeError(`readBytes: len=${len} at ptr=${ptr} out of bounds`);
    }
    return new Uint8Array(buffer.slice(ptr + 4, ptr + 4 + len));
  };

  // `WebSocketCtor` makes the host injectable for tests / Node harnesses
  // without a real browser WebSocket. Defaults to the global `WebSocket`
  // when present.
  const WebSocketCtor =
    options.WebSocket ||
    (typeof WebSocket !== 'undefined' ? WebSocket : null);

  // `allocator(len) -> ptr` is what `ipc_recv` uses to hand received
  // frames back into wasm memory. When unavailable (no matching export
  // on the wasm module), ipc_recv falls back to the "no bytes available"
  // stub. `load()` populates this by probing instance.exports after
  // instantiation; callers using makeGossamerHost directly may inject
  // their own allocator via options.allocator.
  const allocator = typeof options.allocator === 'function' ? options.allocator : null;

  // Per-handle channel state: { ws, recvQueue, pendingSends, endpoint }.
  // Multiple `ipc_open` calls allocate distinct handles so the prior socket
  // isn't clobbered (the old single-`ws` design leaked sockets and dropped
  // queued frames).
  const channels = new Map();
  let nextHandle = IPC_HANDLE_BASE;

  return {
    // Window handles -----------------------------------------------------
    window_open: (titlePtr, _bodyPtr) => {
      if (typeof document !== 'undefined') {
        document.title = readString(titlePtr);
      }
      return WINDOW_HANDLE;
    },
    window_set_body: (w, bodyPtr) => {
      if (typeof document !== 'undefined') {
        const app = document.getElementById('app');
        if (app) app.textContent = readString(bodyPtr);
      }
      return w;
    },
    window_close: (_w) => 0,

    // IPC channel --------------------------------------------------------
    ipc_open: (endpointPtr) => {
      const endpoint = readString(endpointPtr);
      const handle = nextHandle++;
      const state = { ws: null, recvQueue: [], pendingSends: [], endpoint };
      channels.set(handle, state);
      if (!WebSocketCtor) {
        // No WebSocket implementation (e.g. headless Node test harness
        // without a polyfill). Channel still exists so send/recv/close
        // semantics are observable; sends accumulate in pendingSends.
        return handle;
      }
      try {
        const wsUrl = endpoint.replace(/^http/, 'ws');
        const ws = new WebSocketCtor(wsUrl);
        ws.binaryType = 'arraybuffer';
        ws.onmessage = (evt) => {
          const bytes = evt.data instanceof ArrayBuffer
            ? new Uint8Array(evt.data)
            : new TextEncoder().encode(String(evt.data));
          state.recvQueue.push(bytes);
        };
        ws.onopen = () => {
          // Flush any sends queued before the socket opened. Without this
          // buffer, a `ipc_send` racing the connect would silently drop.
          while (state.pendingSends.length > 0) {
            ws.send(state.pendingSends.shift());
          }
        };
        state.ws = ws;
      } catch (err) {
        console.warn('ipc_open failed:', err);
      }
      return handle;
    },
    ipc_recv: (ch) => {
      // Returns a pointer to an Ephapax `Bytes` value (i32 length header
      // + payload) in wasm memory, or 0 when no frame is available.
      //
      // When `allocator` is wired up (load() probes instance.exports for
      // __alloc / alloc / hypatia_alloc / gossamer_alloc / etc.), the
      // dequeued frame is copied into wasm memory using the same
      // {len-header, bytes} layout as strings — that's the bytes ABI
      // bridge.eph's `ipc_recv` is documented to return.
      //
      // When no allocator is available we still pop the queue so it
      // doesn't grow unbounded, but return 0 ("no bytes available")
      // because we have no safe pointer to hand back. Returning the
      // channel handle here would have been dereferenced by wasm as a
      // bogus bytes pointer the moment bridge.eph's run loop ran
      // against this loader.
      const state = channels.get(ch);
      if (!state) return 0;
      const bytes = state.recvQueue.shift();
      if (!bytes || !allocator) return 0;
      try {
        return writeEphapaxBytes(memory.buffer, allocator, bytes);
      } catch (err) {
        console.warn('ipc_recv: allocator failed:', err);
        return 0;
      }
    },
    ipc_send: (ch, bodyPtr) => {
      const state = channels.get(ch);
      if (!state) return ch;
      let bytes;
      try {
        bytes = readBytes(bodyPtr);
      } catch (err) {
        console.warn('ipc_send: malformed body:', err);
        return ch;
      }
      const ws = state.ws;
      if (ws && ws.readyState === 1 /* OPEN */) {
        ws.send(bytes);
      } else {
        // Either the socket hasn't opened yet (race with `new WebSocket`)
        // or there's no WebSocket impl available. Buffer; `onopen` flushes.
        state.pendingSends.push(bytes);
      }
      return ch;
    },
    ipc_close: (ch) => {
      const state = channels.get(ch);
      if (!state) return 0;
      if (state.ws) {
        try { state.ws.close(); } catch (_) { /* noop */ }
      }
      channels.delete(ch);
      return 0;
    },

    // JSON helpers (will collapse into json::decode<Msg> once Ephapax
    // stdlib publishes a JSON module — see bridge.eph:63-73).
    bytes_to_string: (ptr) => ptr,
    json_parse_tag: (sPtr) => {
      try {
        const parsed = JSON.parse(readString(sPtr));
        return Number.isInteger(parsed.tag) ? parsed.tag : -1;
      } catch (_) {
        return -1;
      }
    },
    json_parse_int_field: (sPtr, fieldPtr) => {
      try {
        const parsed = JSON.parse(readString(sPtr));
        const field = readString(fieldPtr);
        return Number.isInteger(parsed[field]) ? parsed[field] : 0;
      } catch (_) {
        return 0;
      }
    },
  };
}

export async function load(url) {
  const response = await fetch(url);
  const bytes = await response.arrayBuffer();
  // Two-phase instantiate so the host imports can reference the wasm
  // memory the module itself exports. `compile` first to discover what
  // the module imports (memory vs. exports it), then synthesise the host
  // bound to the exported memory.
  const module = await WebAssembly.compile(bytes);

  // If the module imports memory, we'd allocate one here; current
  // Ephapax builds export memory, so we instantiate twice: the first
  // pass with a placeholder ArrayBuffer is unnecessary — instead, build
  // the host lazily by stashing exports on a holder the host closes
  // over.
  const holder = { memory: null, allocator: null };
  const lazyHost = makeGossamerHost(
    new Proxy({}, {
      get: (_t, prop) => {
        if (prop === 'buffer') {
          if (!holder.memory) throw new Error('gossamer host used before instantiate');
          return holder.memory.buffer;
        }
        return undefined;
      },
    }),
    {
      // Lazy allocator stub: defers to the real allocator once the
      // instance has been resolved. Returning null when no allocator
      // is wired up makes the host's ipc_recv fall back to the
      // "no bytes available" path, which is the safe default.
      allocator: (len) => {
        if (typeof holder.allocator !== 'function') return null;
        return holder.allocator(len);
      },
    },
  );

  const importObject = { gossamer: lazyHost };
  const instance = await WebAssembly.instantiate(module, importObject);
  holder.memory = instance.exports.memory;
  // Probe for a host-callable allocator. Optional: bridge.eph's
  // `ipc_recv` only needs one when the host has bytes to hand back; in
  // its absence ipc_recv returns 0 and the bridge's run loop treats
  // that as a polling miss.
  holder.allocator = probeAllocator(instance.exports);
  return new HypatiaTEA(instance.exports);
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
