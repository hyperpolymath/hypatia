// SPDX-License-Identifier: PMPL-1.0-or-later
// Automated browser-equivalent smoke for the Hypatia GUI loader.
//
// Closes the manual procedure documented in issue #216 by exercising
// the same surface the manual step did:
//
//   1. The loader fetches `./assets/wasm/hypatia_gui.wasm` and
//      instantiates it against the `gossamer` host.
//   2. The initial `view(init)` text renders into `#app`.
//   3. Each of the four department buttons updates `#app`.
//   4. "Step Back (History)" pops the navigation stack.
//
// We don't depend on the real Ephapax CLI (hyperpolymath/ephapax#36)
// or on a real browser. The fixture .wasm is built in-process by
// `fixture_wasm.mjs`; the DOM surface that `index.html` and
// `makeGossamerHost` poke at is mocked with a minimal stand-in. The
// same loader code that the panel runs is invoked under `node --test`,
// so any drift in the wasm ABI, msg packing, or host import surface
// fails CI rather than the manual smoke step.
//
// Run with:  node --test test/gossamer/browser_smoke.test.mjs

import { describe, it, before, beforeEach } from 'node:test';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { dirname, resolve } from 'node:path';

import { buildFixtureWasm, FIXTURE_STRINGS } from './fixture_wasm.mjs';

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, '..', '..');
const LOADER = resolve(REPO, 'src/ui/gossamer/loader.js');
const INDEX_HTML = resolve(REPO, 'src/ui/public/index.html');

// Minimal DOM that satisfies the loader host import surface plus the
// wiring index.html does on load. Buttons collect their last click
// handler so the test can invoke them in the same order a user would.
function makeFakeDom() {
  const handlers = new Map();
  const app = { textContent: '' };
  const button = (id) => ({
    id,
    addEventListener: (kind, fn) => {
      assert.equal(kind, 'click', `${id} listens for clicks`);
      handlers.set(id, fn);
    },
  });
  const buttons = new Map([
    ['app', app],
    ['btn-learning', button('btn-learning')],
    ['btn-symbolic', button('btn-symbolic')],
    ['btn-verification', button('btn-verification')],
    ['btn-triangle', button('btn-triangle')],
    ['btn-back', button('btn-back')],
  ]);
  return {
    document: {
      title: '',
      getElementById: (id) => buttons.get(id) ?? null,
    },
    app,
    click: (id) => {
      const fn = handlers.get(id);
      if (!fn) throw new Error(`no handler bound for ${id}`);
      fn();
    },
    handlerIds: () => [...handlers.keys()],
  };
}

// Replicates the bootstrap in src/ui/public/index.html so the same
// loader code path the browser runs is also what the smoke runs.
async function runPanelBootstrap({ load, Msg, Department }, dom) {
  const tea = await load('./assets/wasm/hypatia_gui.wasm');
  let model = tea.init();
  const render = () => { dom.app.textContent = tea.view(model); };
  const dispatch = (m) => { model = tea.update(m, model); render(); };

  dom.document.getElementById('btn-learning')
    .addEventListener('click', () => dispatch(Msg.Navigate(Department.Learning)));
  dom.document.getElementById('btn-symbolic')
    .addEventListener('click', () => dispatch(Msg.Navigate(Department.Symbolic)));
  dom.document.getElementById('btn-verification')
    .addEventListener('click', () => dispatch(Msg.Navigate(Department.Verification)));
  dom.document.getElementById('btn-triangle')
    .addEventListener('click', () => dispatch(Msg.Navigate(Department.Triangle)));
  dom.document.getElementById('btn-back')
    .addEventListener('click', () => dispatch(Msg.PopState()));

  render();
}

// `fetch('./assets/wasm/hypatia_gui.wasm')` returns the fixture bytes.
// Anything else 404s loudly so a misrouted URL is obvious.
function installFetch(wasmBytes) {
  const before = globalThis.fetch;
  globalThis.fetch = async (url) => {
    if (typeof url === 'string' && url.endsWith('hypatia_gui.wasm')) {
      return { arrayBuffer: async () => wasmBytes.buffer.slice(
        wasmBytes.byteOffset, wasmBytes.byteOffset + wasmBytes.byteLength) };
    }
    throw new Error(`unexpected fetch(${url})`);
  };
  return () => { globalThis.fetch = before; };
}

describe('Hypatia GUI browser smoke (closes #216)', () => {
  let loader;
  let wasmBytes;

  before(async () => {
    loader = await import(pathToFileURL(LOADER).href);
    wasmBytes = buildFixtureWasm();
  });

  let dom;
  let restoreFetch;
  let restoreDocument;
  beforeEach(() => {
    dom = makeFakeDom();
    restoreFetch = installFetch(wasmBytes);
    const prev = globalThis.document;
    globalThis.document = dom.document;
    restoreDocument = () => { globalThis.document = prev; };
  });

  it('index.html references the loader + Msg/Department exports the bootstrap relies on', async () => {
    const html = await readFile(INDEX_HTML, 'utf8');
    assert.match(html, /from\s+['"]\.\.\/gossamer\/loader\.js['"]/,
      'index.html imports the gossamer loader');
    for (const sym of ['load', 'Msg', 'Department']) {
      assert.match(html, new RegExp(`\\b${sym}\\b`),
        `index.html uses ${sym} from loader.js`);
    }
    for (const id of ['app', 'btn-learning', 'btn-symbolic', 'btn-verification', 'btn-triangle', 'btn-back']) {
      assert.match(html, new RegExp(`id=["']${id}["']`), `index.html exposes #${id}`);
    }
  });

  it('loader exports the in-process Msg ABI (encodeMsg) used by the panel', () => {
    const { encodeMsg, Msg, Department, MSG_TAG_NAVIGATE, MSG_TAG_POP_STATE } = loader;
    assert.equal(MSG_TAG_NAVIGATE, 0);
    assert.equal(MSG_TAG_POP_STATE, 1);
    assert.equal(encodeMsg(Msg.Navigate(Department.Learning)), 0x000);
    assert.equal(encodeMsg(Msg.Navigate(Department.Symbolic)), 0x100);
    assert.equal(encodeMsg(Msg.Navigate(Department.Verification)), 0x200);
    assert.equal(encodeMsg(Msg.Navigate(Department.Triangle)), 0x300);
    assert.equal(encodeMsg(Msg.PopState()), 0x001);
    assert.throws(() => encodeMsg({ tag: 99 }), /unknown msg.tag=99/);
  });

  it('initial hypatia_view(init) renders into #app without errors', async () => {
    await runPanelBootstrap(loader, dom);
    assert.equal(dom.app.textContent, FIXTURE_STRINGS.none);
  });

  it('each of the four department buttons updates #app', async () => {
    await runPanelBootstrap(loader, dom);
    dom.click('btn-learning');
    assert.equal(dom.app.textContent, FIXTURE_STRINGS.learning);
    dom.click('btn-symbolic');
    assert.equal(dom.app.textContent, FIXTURE_STRINGS.symbolic);
    dom.click('btn-verification');
    assert.equal(dom.app.textContent, FIXTURE_STRINGS.verification);
    dom.click('btn-triangle');
    assert.equal(dom.app.textContent, FIXTURE_STRINGS.triangle);
  });

  it('Step Back pops state in reverse navigation order', async () => {
    await runPanelBootstrap(loader, dom);
    for (const id of ['btn-learning', 'btn-symbolic', 'btn-verification', 'btn-triangle']) {
      dom.click(id);
    }
    assert.equal(dom.app.textContent, FIXTURE_STRINGS.triangle);
    dom.click('btn-back');
    assert.equal(dom.app.textContent, FIXTURE_STRINGS.verification);
    dom.click('btn-back');
    assert.equal(dom.app.textContent, FIXTURE_STRINGS.symbolic);
    dom.click('btn-back');
    assert.equal(dom.app.textContent, FIXTURE_STRINGS.learning);
    dom.click('btn-back');
    assert.equal(dom.app.textContent, FIXTURE_STRINGS.none);
  });

  it('Step Back from the initial state is a no-op (history empty marker preserved)', async () => {
    await runPanelBootstrap(loader, dom);
    assert.equal(dom.app.textContent, FIXTURE_STRINGS.none);
    dom.click('btn-back');
    // History slots are all 0xF (empty); popping yields the same "none" view.
    assert.equal(dom.app.textContent, FIXTURE_STRINGS.none);
  });

  it('all five panel click handlers are bound (no orphan buttons)', async () => {
    await runPanelBootstrap(loader, dom);
    const expected = ['btn-learning', 'btn-symbolic', 'btn-verification', 'btn-triangle', 'btn-back'];
    assert.deepEqual(dom.handlerIds().sort(), expected.sort());
  });

  it('console.error / console.warn stay silent during the full panel flow', async () => {
    const calls = [];
    const realError = console.error;
    const realWarn = console.warn;
    console.error = (...args) => calls.push(['error', ...args]);
    console.warn = (...args) => calls.push(['warn', ...args]);
    try {
      await runPanelBootstrap(loader, dom);
      for (const id of ['btn-learning', 'btn-symbolic', 'btn-verification', 'btn-triangle', 'btn-back']) {
        dom.click(id);
      }
    } finally {
      console.error = realError;
      console.warn = realWarn;
    }
    assert.deepEqual(calls, [], `unexpected console output: ${JSON.stringify(calls)}`);
    restoreDocument();
    restoreFetch();
  });
});
