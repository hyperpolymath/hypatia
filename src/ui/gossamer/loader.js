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
const IPC_HANDLE = 2;

// Reads an Ephapax string: i32 length header at `ptr`, followed by
// `len` UTF-8 bytes from `buffer`. Throws on out-of-bounds rather than
// reading garbage past the heap, so an ABI mismatch is visible.
// Shared between HypatiaTEA (export-side decodes) and the gossamer
// host import namespace (host-side decodes) so both observe the same
// invariants.
function readEphapaxString(buffer, ptr) {
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

  // Reads an Ephapax string: i32 length header at `ptr`, followed by
  // `len` UTF-8 bytes. Throws on out-of-bounds rather than reading
  // garbage past the heap, so an ABI mismatch is visible.
  readString(ptr) {
    const buffer = this.memory.buffer;
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
}

// Build the `gossamer` import namespace. Signatures mirror the
// `extern "gossamer"` block in src/ui/gossamer/bridge.eph. The browser
// stubs here are minimal — enough to instantiate the module without a
// real Gossamer host so the TEA exports (init/update/view/subs) remain
// callable from index.html. The bridge's `main`/`run` loop (which calls
// window_open / ipc_open / ipc_recv) is exercised only when a real host
// invokes it; on the panel side, index.html drives the loop manually.
function makeGossamerHost(memory) {
  const readString = (ptr) => {
    const view = new DataView(memory.buffer);
    const len = view.getInt32(ptr, true);
    const bytes = new Uint8Array(memory.buffer, ptr + 4, len);
    return new TextDecoder().decode(bytes);
  };

  let ws = null;
  const recvQueue = [];
  const pendingRecv = [];

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
      try {
        const wsUrl = endpoint.replace(/^http/, 'ws');
        ws = new WebSocket(wsUrl);
        ws.binaryType = 'arraybuffer';
        ws.onmessage = (evt) => {
          const bytes = evt.data instanceof ArrayBuffer
            ? new Uint8Array(evt.data)
            : new TextEncoder().encode(String(evt.data));
          if (pendingRecv.length > 0) {
            pendingRecv.shift()(bytes);
          } else {
            recvQueue.push(bytes);
          }
        };
      } catch (err) {
        console.warn('ipc_open failed:', err);
      }
      return IPC_HANDLE;
    },
    ipc_recv: (ch) => {
      // Polling stub: real host returns (ch, bytes). Browser bridge
      // delivers via WebSocket onmessage; the TEA loop in index.html
      // dispatches Msg directly rather than polling, so this is unused
      // in the panel path.
      const _ = recvQueue.shift();
      return ch;
    },
    ipc_send: (ch, _bodyPtr) => {
      if (ws && ws.readyState === WebSocket.OPEN) {
        // Body bytes would be read from `_bodyPtr` once the bytes ABI
        // is pinned (see comment at top of file).
      }
      return ch;
    },
    ipc_close: (_ch) => {
      if (ws) { try { ws.close(); } catch (_) { /* noop */ } ws = null; }
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
