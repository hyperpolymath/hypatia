'use strict';
const test = require('node:test');
const assert = require('node:assert');
const { classify, clamp } = require('../src/classify.js');

// Property-style: fixed-seed LCG, 500 cases.
function lcg(seed) {
  let s = seed;
  return () => (s = (s * 1664525 + 1013904223) % 4294967296) / 4294967296;
}

test('property: classify says small on 1..100', () => {
  const rnd = lcg(42);
  for (let i = 0; i < 500; i++) {
    const n = 1 + Math.floor(rnd() * 100); // 1..100
    assert.ok(classify(n) === 'small');
  }
});

test('property: clamp stays within bounds', () => {
  const rnd = lcg(7);
  for (let i = 0; i < 500; i++) {
    const v = Math.floor(rnd() * 200) - 100;
    const r = clamp(v, -20, 20);
    assert.ok(r >= -20 && r <= 20);
  }
});
