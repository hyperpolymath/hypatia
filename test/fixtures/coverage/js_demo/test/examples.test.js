'use strict';
const test = require('node:test');
const assert = require('node:assert');
const { classify, clamp } = require('../src/classify.js');

test('example: classify zero', () => {
  assert.strictEqual(classify(0), 'zero');
});

test('example: clamp inside', () => {
  assert.strictEqual(clamp(5, 0, 10), 5);
});

test('example: classify negative and large', () => {
  assert.strictEqual(classify(-3), 'negative');
  assert.strictEqual(classify(1000), 'large');
});

test('example: clamp rejects lo > hi and clamps below', () => {
  assert.throws(() => clamp(1, 5, 0), RangeError);
  assert.strictEqual(clamp(-9, 0, 10), 0);
});
