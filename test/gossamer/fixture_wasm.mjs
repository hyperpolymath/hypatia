// SPDX-License-Identifier: PMPL-1.0-or-later
// Hand-built fixture .wasm module that satisfies the Hypatia GUI loader ABI.
//
// Purpose: lets `test/gossamer/browser_smoke.test.mjs` close issue #216
// (manual browser smoke test) without waiting on the upstream Ephapax
// CLI (hyperpolymath/ephapax#36). We don't ship this fixture as a file
// (Scorecard `Binary-Artifacts` flags committed .wasm) — we build it
// byte-by-byte from this module at test time.
//
// The fixture exports the four TEA symbols the loader expects
// (hypatia_init, _update, _view, _subs) plus `memory`, and implements
// a small in-memory state machine that mirrors what
// `src/ui/gossamer/hypatia_gui.eph` is intended to do once Ephapax
// emits real wasm:
//
//   - Model is a single i32. Low 4 bits = current department
//     (0..3 mapped to Learning/Symbolic/Verification/Triangle, or 0xF
//     for "no selection"). Next four 4-bit slots are the history
//     stack (oldest in the high slot, 0xF marker for empty).
//   - hypatia_init returns 0xFFFFF (no current, history all empty).
//   - hypatia_update receives a packed Msg (see loader.js encodeMsg)
//     and either pushes (Navigate) or pops (PopState) the stack.
//   - hypatia_view returns a pointer to a pre-baked `{i32 len, bytes}`
//     string in the data segment chosen by the current department.
//   - hypatia_subs returns the same "no selection" pointer; the panel
//     path in index.html does not consume subs but the export must
//     exist.
//
// String layout in linear memory (page 0):
//
//   0x100  Hypatia: System Ready          (used when current = 0xF)
//   0x120  Department: Learning           (dept 0)
//   0x140  Department: Symbolic           (dept 1)
//   0x160  Department: Verification       (dept 2)
//   0x180  Department: Safety Triangle    (dept 3)
//
// The 0x20-byte stride matches the loader's {i32 length header,
// UTF-8 bytes} layout and gives each slot room for its payload.

const STRINGS = [
  { offset: 0x100, text: 'Hypatia: System Ready' },
  { offset: 0x120, text: 'Department: Learning' },
  { offset: 0x140, text: 'Department: Symbolic' },
  { offset: 0x160, text: 'Department: Verification' },
  { offset: 0x180, text: 'Department: Safety Triangle' },
];

export const FIXTURE_STRINGS = {
  none: STRINGS[0].text,
  learning: STRINGS[1].text,
  symbolic: STRINGS[2].text,
  verification: STRINGS[3].text,
  triangle: STRINGS[4].text,
};

// --- WASM binary helpers --------------------------------------------------

function uleb128(n) {
  const out = [];
  let v = n >>> 0;
  while (true) {
    const b = v & 0x7f;
    v >>>= 7;
    if (v === 0) { out.push(b); return out; }
    out.push(b | 0x80);
  }
}

function sleb128(n) {
  const out = [];
  let value = n | 0;
  while (true) {
    const byte = value & 0x7f;
    value >>= 7;
    const signBitSet = (byte & 0x40) !== 0;
    if ((value === 0 && !signBitSet) || (value === -1 && signBitSet)) {
      out.push(byte);
      return out;
    }
    out.push(byte | 0x80);
  }
}

function section(id, bytes) {
  return [id, ...uleb128(bytes.length), ...bytes];
}

function vec(items) {
  const out = [...uleb128(items.length)];
  for (const item of items) {
    for (const b of item) out.push(b);
  }
  return out;
}

function name(s) {
  const enc = new TextEncoder().encode(s);
  return [...uleb128(enc.length), ...enc];
}

// --- Module construction --------------------------------------------------

// Types:
//   0: () -> i32           init
//   1: (i32, i32) -> i32   update
//   2: (i32) -> i32        view / subs
const TYPE_INIT = 0;
const TYPE_UPDATE = 1;
const TYPE_VIEW = 2;

const typeSection = section(0x01, vec([
  [0x60, 0x00, 0x01, 0x7f],
  [0x60, 0x02, 0x7f, 0x7f, 0x01, 0x7f],
  [0x60, 0x01, 0x7f, 0x01, 0x7f],
]));

// Function section: 4 funcs, types [INIT, UPDATE, VIEW, VIEW]
const funcSection = section(0x03, [
  ...uleb128(4),
  TYPE_INIT, TYPE_UPDATE, TYPE_VIEW, TYPE_VIEW,
]);

// Memory section: 1 memory, min 1 page (64 KiB), no max.
const memSection = section(0x05, [
  ...uleb128(1),
  0x00, ...uleb128(1),
]);

// Export section: memory + 4 functions.
const exportEntries = [
  [...name('memory'),         0x02, ...uleb128(0)],
  [...name('hypatia_init'),   0x00, ...uleb128(0)],
  [...name('hypatia_update'), 0x00, ...uleb128(1)],
  [...name('hypatia_view'),   0x00, ...uleb128(2)],
  [...name('hypatia_subs'),   0x00, ...uleb128(3)],
];
const exportSection = section(0x07, vec(exportEntries));

// --- Function bodies ------------------------------------------------------

// Each function body is `size :: locals :: expr`. Locals are encoded as
// a vec of (count, type) pairs. We bundle locals declarations as
// pre-encoded byte arrays for readability.

function body(locals, expr) {
  const inner = [...locals, ...expr, 0x0b];
  return [...uleb128(inner.length), ...inner];
}

const NO_LOCALS = [0x00];                  // vec(): count 0
const ONE_I32_LOCAL = [0x01, 0x01, 0x7f];  // 1 group of 1 × i32

// hypatia_init: return 0xFFFFF.
const bodyInit = body(NO_LOCALS, [
  0x41, ...sleb128(0xFFFFF),
]);

// hypatia_update(msg_packed, model):
//   if (msg_packed & 0xFF) == 1 {            // PopState
//     return (model >>> 4) | 0xF0000;
//   } else {                                  // Navigate(dept)
//     dept = (msg_packed >>> 8) & 0xF;
//     return ((model << 4) | dept) & 0xFFFFF;
//   }
const bodyUpdate = body(NO_LOCALS, [
  0x20, 0x00,                       // local.get msg
  0x41, ...sleb128(0xFF),
  0x71,                             // i32.and
  0x41, ...sleb128(1),
  0x46,                             // i32.eq
  0x04, 0x7f,                       // if (result i32)
    0x20, 0x01,                     //   local.get model
    0x41, ...sleb128(4),
    0x76,                           //   i32.shr_u
    0x41, ...sleb128(0xF0000),
    0x72,                           //   i32.or
  0x05,                             // else
    0x20, 0x01,                     //   local.get model
    0x41, ...sleb128(4),
    0x74,                           //   i32.shl
    0x20, 0x00,                     //   local.get msg
    0x41, ...sleb128(8),
    0x76,                           //   i32.shr_u
    0x41, ...sleb128(0xF),
    0x71,                           //   i32.and
    0x72,                           //   i32.or
    0x41, ...sleb128(0xFFFFF),
    0x71,                           //   i32.and
  0x0b,                             // end if
]);

// hypatia_view(model):
//   dept = model & 0xF
//   return dept == 0xF ? 0x100 : 0x120 + (dept << 5)
const bodyView = body(ONE_I32_LOCAL, [
  0x20, 0x00,
  0x41, ...sleb128(0xF),
  0x71,
  0x22, 0x01,                       // local.tee 1
  0x41, ...sleb128(0xF),
  0x46,
  0x04, 0x7f,
    0x41, ...sleb128(0x100),
  0x05,
    0x20, 0x01,
    0x41, ...sleb128(5),
    0x74,
    0x41, ...sleb128(0x120),
    0x6a,                           // i32.add
  0x0b,
]);

// hypatia_subs(model): return 0x100 (we never read it).
const bodySubs = body(NO_LOCALS, [
  0x41, ...sleb128(0x100),
]);

const codeSection = section(0x0a, [
  ...uleb128(4),
  ...bodyInit, ...bodyUpdate, ...bodyView, ...bodySubs,
]);

// --- Data section ---------------------------------------------------------

function dataSegment(offset, payloadBytes) {
  return [
    0x00,                                   // active, memory index 0
    0x41, ...sleb128(offset), 0x0b,         // offset expression: i32.const <offset>
    ...uleb128(payloadBytes.length),
    ...payloadBytes,
  ];
}

function ephapaxStringBytes(text) {
  const enc = new TextEncoder().encode(text);
  const len = enc.length;
  return [
    len & 0xff, (len >>> 8) & 0xff, (len >>> 16) & 0xff, (len >>> 24) & 0xff,
    ...enc,
  ];
}

const dataSegments = STRINGS.map(({ offset, text }) =>
  dataSegment(offset, ephapaxStringBytes(text)));

const dataSection = section(0x0b, [
  ...uleb128(dataSegments.length),
  ...dataSegments.flat(),
]);

// --- Final assembly -------------------------------------------------------

const WASM_MAGIC = [0x00, 0x61, 0x73, 0x6d];
const WASM_VERSION = [0x01, 0x00, 0x00, 0x00];

export function buildFixtureWasm() {
  const bytes = [
    ...WASM_MAGIC, ...WASM_VERSION,
    ...typeSection,
    ...funcSection,
    ...memSection,
    ...exportSection,
    ...codeSection,
    ...dataSection,
  ];
  return new Uint8Array(bytes);
}
