'use strict';

function classify(n) {
  if (n < 0) {
    return 'negative';
  }
  if (n === 0) {
    return 'zero';
  }
  return n > 100 ? 'large' : 'small';
}

function clamp(value, lo, hi) {
  if (lo > hi) {
    throw new RangeError('lo > hi');
  }
  if (value < lo) return lo;
  if (value > hi) return hi;
  return value;
}

function neverCalled(x) {
  if (x) {
    return 1;
  }
  return 2;
}

module.exports = { classify, clamp, neverCalled };
