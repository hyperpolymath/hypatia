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
// mismatch surfaces as a clear error rather than a silent garble. When
// the ABI is pinned upstream (tracked alongside hyperpolymath/ephapax#36)
// this routine becomes the single point of change.

const WINDOW_HANDLE = 1;
// IPC channel handles are allocated per `ipc_open` starting at 2 so that
// 0 stays reserved for "no channel" / "no bytes available" returns and 1
// stays reserved for the singleton WINDOW_HANDLE.
const IPC_HANDLE_BASE = 2;

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
  const view = new DataView(buffer);
  const len = view.getInt32(ptr, true);
  if (len < 0 || ptr + 4 + len > buffer.byteLength) {
    throw new RangeError(`readString: len=${len} at ptr=${ptr} out of bounds`);
  }
  const bytes = new Uint8Array(buffer, ptr + 4, len);
  return new TextDecoder().decode(bytes);
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
  // Body bytes ABI mirrors strings: i32 length header at `ptr`, then `len`
  // payload bytes. Confirmed against the documented {ptr, len} convention;
  // re-verify once hyperpolymath/ephapax#36 emits a real artifact.
  const readBytes = (ptr) => {
    const buffer = memory.buffer;
    if (ptr < 0 || ptr + 4 > buffer.byteLength) {
      throw new RangeError(`readBytes: header ptr=${ptr} out of bounds`);
    }
    const view = new DataView(buffer);
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
    ipc_recv: (_ch) => {
      // Polling stub: real host returns (ch, bytes-ptr). The full
      // implementation requires a wasm-side allocator (`__alloc(len)`)
      // to copy the queued bytes back into wasm memory; that's tracked
      // in #218 alongside the bytes ABI confirmation. Until then, we
      // pop the frame so the queue doesn't grow unbounded and return 0
      // ("no bytes available") — returning the channel handle would
      // have been dereferenced by wasm as a bogus bytes pointer the
      // moment bridge.eph's run loop ran against this loader.
      const state = channels.get(_ch);
      if (state) state.recvQueue.shift();
      return 0;
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
  const holder = { memory: null };
  const lazyHost = makeGossamerHost(new Proxy({}, {
    get: (_t, prop) => {
      if (prop === 'buffer') {
        if (!holder.memory) throw new Error('gossamer host used before instantiate');
        return holder.memory.buffer;
      }
      return undefined;
    },
  }));

  const importObject = { gossamer: lazyHost };
  const instance = await WebAssembly.instantiate(module, importObject);
  holder.memory = instance.exports.memory;
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
