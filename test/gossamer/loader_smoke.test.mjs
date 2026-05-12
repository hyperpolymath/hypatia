// SPDX-License-Identifier: PMPL-1.0-or-later
//
// Hypatia GUI loader smoke harness (issue #218).
//
// Purpose
// -------
// Locks the contract between `src/ui/gossamer/loader.js` and `bridge.eph`:
//
//   1. The Ephapax string ABI (i32 length header + UTF-8 bytes) round-trips
//      through `readEphapaxString` with bounds checking.
//   2. `makeGossamerHost` exposes exactly the import keys declared by the
//      `extern "gossamer"` block in `bridge.eph`. A future rename in either
//      file breaks CI here rather than at the manual browser smoke step.
//   3. Multi-channel IPC: distinct `ipc_open` calls allocate distinct
//      handles; `ipc_close` only tears down the requested channel.
//   4. `ipc_send` race / buffering: sends issued before `WebSocket.onopen`
//      are flushed at open time rather than silently dropped.
//   5. A hand-crafted minimal `.wasm` declaring the gossamer imports +
//      hypatia_init / _update / _view / _subs exports instantiates against
//      the loader's import object and the ABI helpers decode its memory.
//   6. `probeAllocator` finds the canonical names listed in loader.js
//      and returns null when none are present.
//   7. `ipc_recv` uses the allocator to round-trip received WebSocket
//      frames back into wasm memory as Ephapax `Bytes` values, and
//      falls back to returning 0 when no allocator is wired up.
//
// Run
// ---
//   node --test test/gossamer/
//
// No upstream Ephapax toolchain dependency: the fixture wasm is hand-built
// from raw bytes so this harness runs even while hyperpolymath/ephapax#36
// remains open.

import { test } from 'node:test';
import assert from 'node:assert/strict';

import {
  readEphapaxString,
  makeGossamerHost,
  probeAllocator,
  Msg,
  MsgTag,
  Department,
} from '../../src/ui/gossamer/loader.js';

// --- helpers ---------------------------------------------------------------

// Pack {i32 len header, utf-8 bytes} at offset 0 of a fresh ArrayBuffer.
function packString(s, capacity = 256) {
  const buf = new ArrayBuffer(capacity);
  const view = new DataView(buf);
  const bytes = new TextEncoder().encode(s);
  view.setInt32(0, bytes.length, true);
  new Uint8Array(buf, 4, bytes.length).set(bytes);
  return buf;
}

// Stub `memory` object compatible with what makeGossamerHost reads.
function fakeMemory(buffer) {
  return { buffer };
}

// Minimal in-memory WebSocket double. Mirrors the surface ipc_open touches:
// readyState, onmessage, onopen, send, close, binaryType.
class FakeWebSocket {
  constructor(url) {
    this.url = url;
    this.readyState = 0; // CONNECTING
    this.binaryType = '';
    this.sent = [];
    FakeWebSocket.last = this;
  }
  open() {
    this.readyState = 1; // OPEN
    if (this.onopen) this.onopen({});
  }
  send(bytes) { this.sent.push(bytes); }
  close() { this.readyState = 3; /* CLOSED */ }
}

// --- 1. readEphapaxString invariants --------------------------------------

test('readEphapaxString decodes well-formed strings', () => {
  const buf = packString('hello');
  assert.equal(readEphapaxString(buf, 0), 'hello');
});

test('readEphapaxString round-trips UTF-8', () => {
  const buf = packString('λ → π · 中文');
  assert.equal(readEphapaxString(buf, 0), 'λ → π · 中文');
});

test('readEphapaxString throws RangeError on negative ptr', () => {
  const buf = packString('x');
  assert.throws(() => readEphapaxString(buf, -1), RangeError);
});

test('readEphapaxString throws RangeError when header runs past buffer', () => {
  const buf = new ArrayBuffer(2);
  assert.throws(() => readEphapaxString(buf, 0), RangeError);
});

test('readEphapaxString throws RangeError when len exceeds buffer', () => {
  const buf = new ArrayBuffer(8);
  new DataView(buf).setInt32(0, 0x7fffffff, true);
  assert.throws(() => readEphapaxString(buf, 0), RangeError);
});

test('readEphapaxString throws RangeError on negative len', () => {
  const buf = new ArrayBuffer(8);
  new DataView(buf).setInt32(0, -1, true);
  assert.throws(() => readEphapaxString(buf, 0), RangeError);
});

// --- 2. gossamer import surface --------------------------------------------

test('makeGossamerHost exposes the bridge.eph extern keys (and only those)', () => {
  const host = makeGossamerHost(fakeMemory(new ArrayBuffer(64)));
  // Mirrors `extern "gossamer"` in src/ui/gossamer/bridge.eph. Adding a
  // new extern requires adding the key here; renaming one breaks the test.
  const expected = new Set([
    'window_open',
    'window_set_body',
    'window_close',
    'ipc_open',
    'ipc_recv',
    'ipc_send',
    'ipc_close',
    'bytes_to_string',
    'json_parse_tag',
    'json_parse_int_field',
  ]);
  const actual = new Set(Object.keys(host));
  assert.deepEqual(actual, expected);
  for (const key of expected) {
    assert.equal(typeof host[key], 'function', `${key} must be a function`);
  }
});

// --- 3. multi-channel IPC --------------------------------------------------

test('ipc_open allocates distinct handles per call', () => {
  const buf = packString('http://localhost:9090/ws');
  const host = makeGossamerHost(fakeMemory(buf), { WebSocket: FakeWebSocket });
  const a = host.ipc_open(0);
  const b = host.ipc_open(0);
  assert.notEqual(a, b);
  assert.ok(a >= 2 && b >= 2, 'channel handles must not collide with WINDOW_HANDLE (1) or 0');
});

test('ipc_close tears down only the requested channel', () => {
  const buf = packString('http://localhost:9090/ws');
  const host = makeGossamerHost(fakeMemory(buf), { WebSocket: FakeWebSocket });
  const a = host.ipc_open(0);
  const wsA = FakeWebSocket.last;
  const b = host.ipc_open(0);
  const wsB = FakeWebSocket.last;
  host.ipc_close(a);
  assert.equal(wsA.readyState, 3, 'closed channel WS should be CLOSED');
  assert.notEqual(wsB.readyState, 3, 'untouched channel WS should still be open-ish');
  // Subsequent send on the still-open channel must not throw.
  host.ipc_send(b, 0); // bodyPtr 0 → length header at offset 0 of `buf`
});

test('ipc_recv on an unknown handle returns 0 without throwing', () => {
  const host = makeGossamerHost(fakeMemory(new ArrayBuffer(64)), { WebSocket: FakeWebSocket });
  assert.equal(host.ipc_recv(9999), 0);
});

// --- 4. ipc_send race / buffered until onopen ------------------------------

test('ipc_send buffers payload before WS opens, flushes on onopen', () => {
  // Two strings packed into one buffer: endpoint at offset 0, body at 64.
  const buf = new ArrayBuffer(256);
  const view = new DataView(buf);
  const endpointBytes = new TextEncoder().encode('http://localhost:9090/ws');
  view.setInt32(0, endpointBytes.length, true);
  new Uint8Array(buf, 4, endpointBytes.length).set(endpointBytes);
  const bodyOffset = 64;
  const bodyBytes = new TextEncoder().encode('{"tag":1}');
  view.setInt32(bodyOffset, bodyBytes.length, true);
  new Uint8Array(buf, bodyOffset + 4, bodyBytes.length).set(bodyBytes);

  const host = makeGossamerHost(fakeMemory(buf), { WebSocket: FakeWebSocket });
  const ch = host.ipc_open(0);
  const ws = FakeWebSocket.last;
  // WS still CONNECTING — ipc_send must NOT silently drop.
  host.ipc_send(ch, bodyOffset);
  assert.equal(ws.sent.length, 0, 'sends before onopen should be buffered, not delivered');
  ws.open();
  assert.equal(ws.sent.length, 1, 'buffered send should flush on onopen');
  const decoded = new TextDecoder().decode(ws.sent[0]);
  assert.equal(decoded, '{"tag":1}');
});

test('ipc_send delivers immediately once WS is open', () => {
  const buf = new ArrayBuffer(256);
  const view = new DataView(buf);
  const ep = new TextEncoder().encode('http://localhost:9090/ws');
  view.setInt32(0, ep.length, true);
  new Uint8Array(buf, 4, ep.length).set(ep);
  const bodyOffset = 64;
  const body = new TextEncoder().encode('post-open');
  view.setInt32(bodyOffset, body.length, true);
  new Uint8Array(buf, bodyOffset + 4, body.length).set(body);

  const host = makeGossamerHost(fakeMemory(buf), { WebSocket: FakeWebSocket });
  const ch = host.ipc_open(0);
  FakeWebSocket.last.open();
  host.ipc_send(ch, bodyOffset);
  assert.equal(FakeWebSocket.last.sent.length, 1);
  assert.equal(new TextDecoder().decode(FakeWebSocket.last.sent[0]), 'post-open');
});

// --- 5. JSON helpers ------------------------------------------------------

test('json_parse_tag extracts the Msg tag', () => {
  const buf = packString(JSON.stringify({ tag: 0, value: 2 }));
  const host = makeGossamerHost(fakeMemory(buf), { WebSocket: FakeWebSocket });
  assert.equal(host.json_parse_tag(0), 0);
});

test('json_parse_tag returns -1 on malformed JSON', () => {
  const buf = packString('not json');
  const host = makeGossamerHost(fakeMemory(buf), { WebSocket: FakeWebSocket });
  assert.equal(host.json_parse_tag(0), -1);
});

test('json_parse_int_field reads named field', () => {
  // Pack {ptr=0: source} and {ptr=128: field name "value"}.
  const buf = new ArrayBuffer(256);
  const src = JSON.stringify({ tag: 0, value: 42 });
  const view = new DataView(buf);
  const srcBytes = new TextEncoder().encode(src);
  view.setInt32(0, srcBytes.length, true);
  new Uint8Array(buf, 4, srcBytes.length).set(srcBytes);
  const fieldBytes = new TextEncoder().encode('value');
  view.setInt32(128, fieldBytes.length, true);
  new Uint8Array(buf, 132, fieldBytes.length).set(fieldBytes);
  const host = makeGossamerHost(fakeMemory(buf), { WebSocket: FakeWebSocket });
  assert.equal(host.json_parse_int_field(0, 128), 42);
});

// --- 6. Allocator probe ----------------------------------------------------

test('probeAllocator finds __alloc / alloc / hypatia_alloc / gossamer_alloc', () => {
  for (const name of ['__alloc', 'alloc', 'hypatia_alloc', 'gossamer_alloc', '__wbindgen_malloc', 'malloc']) {
    const fn = () => 0;
    const exports = { memory: {}, [name]: fn };
    assert.strictEqual(probeAllocator(exports), fn, `should find ${name}`);
  }
});

test('probeAllocator returns null when no candidate is present', () => {
  assert.strictEqual(probeAllocator({ memory: {} }), null);
  assert.strictEqual(probeAllocator({ memory: {}, some_other_export: () => 0 }), null);
});

test('probeAllocator ignores non-function values', () => {
  assert.strictEqual(probeAllocator({ __alloc: 42 }), null);
  assert.strictEqual(probeAllocator({ alloc: 'not a function' }), null);
});

test('probeAllocator prefers __alloc over alternatives when multiple match', () => {
  const dunder = () => 1;
  const plain = () => 2;
  const exports = { __alloc: dunder, alloc: plain, malloc: () => 3 };
  assert.strictEqual(probeAllocator(exports), dunder);
});

// --- 7. ipc_recv with and without an allocator ----------------------------

test('ipc_recv returns 0 when no allocator is configured', () => {
  const buf = packString('http://localhost:9090/ws');
  const host = makeGossamerHost(fakeMemory(buf), { WebSocket: FakeWebSocket });
  const ch = host.ipc_open(0);
  // Inject a frame via the WS double's onmessage path.
  const ws = FakeWebSocket.last;
  ws.onmessage({ data: new TextEncoder().encode('hi').buffer });
  // No allocator → recv pops the frame but cannot hand it back; returns 0.
  assert.equal(host.ipc_recv(ch), 0);
  // Subsequent recv with empty queue also returns 0.
  assert.equal(host.ipc_recv(ch), 0);
});

test('ipc_recv with allocator writes Bytes (i32 len + payload) into wasm memory', () => {
  const buf = new ArrayBuffer(1024);
  const view = new DataView(buf);
  const ep = new TextEncoder().encode('http://localhost:9090/ws');
  view.setInt32(0, ep.length, true);
  new Uint8Array(buf, 4, ep.length).set(ep);

  // Bump allocator hands out monotonically-increasing pointers starting
  // at offset 256 (well past the endpoint string).
  let nextPtr = 256;
  const allocator = (len) => {
    const ptr = nextPtr;
    nextPtr += len;
    return ptr;
  };

  const host = makeGossamerHost(fakeMemory(buf), {
    WebSocket: FakeWebSocket,
    allocator,
  });
  const ch = host.ipc_open(0);
  const ws = FakeWebSocket.last;

  const payload = new TextEncoder().encode('hello-from-server');
  ws.onmessage({ data: payload.buffer });

  const ptr = host.ipc_recv(ch);
  assert.ok(ptr > 0, 'returned ptr should be non-zero');
  assert.equal(ptr, 256, 'first allocation lands at the bump base');
  // Decode using the same convention as readEphapaxString.
  const len = view.getInt32(ptr, true);
  assert.equal(len, payload.length);
  const got = new Uint8Array(buf, ptr + 4, len);
  assert.equal(new TextDecoder().decode(got), 'hello-from-server');

  // Empty queue → 0 even when allocator is present.
  assert.equal(host.ipc_recv(ch), 0);
});

test('ipc_recv survives a broken allocator without throwing', () => {
  const buf = packString('http://localhost:9090/ws');
  const allocator = () => { throw new Error('boom'); };
  const host = makeGossamerHost(fakeMemory(buf), { WebSocket: FakeWebSocket, allocator });
  const ch = host.ipc_open(0);
  FakeWebSocket.last.onmessage({ data: new TextEncoder().encode('x').buffer });
  // Doesn't throw, returns 0 — the bridge's run loop interprets that
  // as "no bytes available this iteration" rather than crashing.
  assert.equal(host.ipc_recv(ch), 0);
});

test('ipc_recv with allocator returning bad ptr falls back to 0', () => {
  const buf = packString('http://localhost:9090/ws');
  // Allocator returning a negative number signals "out of memory" or
  // similar — the host should treat it as a failed allocation.
  const host = makeGossamerHost(fakeMemory(buf), {
    WebSocket: FakeWebSocket,
    allocator: () => -1,
  });
  const ch = host.ipc_open(0);
  FakeWebSocket.last.onmessage({ data: new TextEncoder().encode('x').buffer });
  assert.equal(host.ipc_recv(ch), 0);
});

// --- 8. Msg / Department constructor parity --------------------------------

test('Msg.Navigate carries Department value with tag 0', () => {
  const m = Msg.Navigate(Department.Symbolic);
  assert.equal(m.tag, MsgTag.Navigate);
  assert.equal(m.value, Department.Symbolic);
});

test('Msg.PopState has tag 1 and no value', () => {
  const m = Msg.PopState();
  assert.equal(m.tag, MsgTag.PopState);
});

// --- 7. Real wasm instantiate against the loader's import surface ---------
//
// Hand-built minimal wasm module. Declares:
//   * import "gossamer".window_open : (i32, i32) -> i32
//   * memory (export "memory") of 1 page
//   * function hypatia_init : () -> i32  (returns const 7)
//   * function hypatia_view : (i32) -> i32  (returns 0)
//   * function hypatia_update : (i32, i32) -> i32  (returns local 1)
//   * function hypatia_subs : (i32) -> i32  (returns 0)
// Built byte-by-byte to avoid any Ephapax-toolchain dependency.

function buildMinimalWasm() {
  // Wasm binary format encoding helpers.
  const u8 = (...xs) => new Uint8Array(xs);
  const concat = (...arrs) => {
    const total = arrs.reduce((n, a) => n + a.length, 0);
    const out = new Uint8Array(total);
    let off = 0;
    for (const a of arrs) { out.set(a, off); off += a.length; }
    return out;
  };
  const leb128 = (n) => {
    const bytes = [];
    let v = n;
    do {
      let byte = v & 0x7f;
      v >>>= 7;
      if (v !== 0) byte |= 0x80;
      bytes.push(byte);
    } while (v !== 0);
    return u8(...bytes);
  };
  const str = (s) => {
    const b = new TextEncoder().encode(s);
    return concat(leb128(b.length), b);
  };
  const section = (id, payload) => concat(u8(id), leb128(payload.length), payload);
  const vec = (items) => concat(leb128(items.length), ...items);

  // Type section: 4 distinct function signatures.
  // type 0: () -> i32                   (hypatia_init)
  // type 1: (i32) -> i32                (hypatia_view, hypatia_subs)
  // type 2: (i32, i32) -> i32           (hypatia_update, window_open import)
  const types = vec([
    u8(0x60, 0x00, 0x01, 0x7f),
    u8(0x60, 0x01, 0x7f, 0x01, 0x7f),
    u8(0x60, 0x02, 0x7f, 0x7f, 0x01, 0x7f),
  ]);

  // Import section: gossamer.window_open : type 2
  const imports = vec([
    concat(str('gossamer'), str('window_open'), u8(0x00), leb128(2)),
  ]);

  // Function section: 4 funcs (init, view, update, subs).
  // Local wasm function indices: 0=init, 1=view, 2=update, 3=subs.
  // Imported functions take indices 0; our funcs start at 1.
  // Wait — imports occupy the function index space first; with 1 import,
  // local funcs start at index 1.
  const funcs = vec([leb128(0), leb128(1), leb128(2), leb128(1)]);

  // Memory section: 1 memory entry, limits = {flags=0, min=1 page}.
  const memory = vec([concat(u8(0x00), leb128(1))]);

  // Export section: memory + 4 funcs.
  // Function indices in exports: 1=hypatia_init, 2=hypatia_view, 3=hypatia_update, 4=hypatia_subs.
  const exportEntries = vec([
    concat(str('memory'), u8(0x02), leb128(0)),
    concat(str('hypatia_init'), u8(0x00), leb128(1)),
    concat(str('hypatia_view'), u8(0x00), leb128(2)),
    concat(str('hypatia_update'), u8(0x00), leb128(3)),
    concat(str('hypatia_subs'), u8(0x00), leb128(4)),
  ]);

  // Code section: each body = u32 length, u32 local-decl count (=0),
  // then opcodes ending in 0x0b (end).
  const body = (...opcodes) => {
    const inner = concat(u8(0x00), u8(...opcodes), u8(0x0b)); // 0 local decls
    return concat(leb128(inner.length), inner);
  };
  // i32.const 7 ; end          → returns 7
  const initBody  = body(0x41, 0x07);
  // i32.const 0 ; end          → returns 0
  const viewBody  = body(0x41, 0x00);
  // local.get 1 ; end          → returns model arg
  const updateBody = body(0x20, 0x01);
  const subsBody  = body(0x41, 0x00);
  const code = vec([initBody, viewBody, updateBody, subsBody]);

  const magic = u8(0x00, 0x61, 0x73, 0x6d);
  const version = u8(0x01, 0x00, 0x00, 0x00);
  return concat(
    magic, version,
    section(0x01, types),
    section(0x02, imports),
    section(0x03, funcs),
    section(0x05, memory),
    section(0x07, exportEntries),
    section(0x0a, code),
  );
}

test('hand-built minimal wasm instantiates against gossamer host imports', async () => {
  const bytes = buildMinimalWasm();
  // Sanity: WebAssembly.validate accepts the hand-built module.
  assert.equal(WebAssembly.validate(bytes), true, 'fixture wasm must validate');

  const module = await WebAssembly.compile(bytes);
  // Mirror loader.js's lazy-host pattern: build the host with a Proxy
  // that defers .buffer reads until we have the instance memory.
  const holder = { memory: null };
  const memProxy = new Proxy({}, {
    get: (_t, prop) => {
      if (prop === 'buffer') {
        if (!holder.memory) throw new Error('host used before instantiate');
        return holder.memory.buffer;
      }
      return undefined;
    },
  });
  const host = makeGossamerHost(memProxy, { WebSocket: FakeWebSocket });
  const instance = await WebAssembly.instantiate(module, { gossamer: host });
  holder.memory = instance.exports.memory;

  assert.equal(typeof instance.exports.hypatia_init, 'function');
  assert.equal(instance.exports.hypatia_init(), 7);
  assert.equal(instance.exports.hypatia_update(0, 42), 42);
  assert.equal(instance.exports.hypatia_view(0), 0);
  assert.equal(instance.exports.hypatia_subs(0), 0);

  // ABI roundtrip through real wasm memory: write a string into the
  // exported memory, then decode via readEphapaxString.
  const view = new DataView(holder.memory.buffer);
  const payload = new TextEncoder().encode('hypatia');
  view.setInt32(16, payload.length, true);
  new Uint8Array(holder.memory.buffer, 20, payload.length).set(payload);
  assert.equal(readEphapaxString(holder.memory.buffer, 16), 'hypatia');
});
