/*---
description: WeakSet constructor creates weak sets from iterables
features: [WeakSet, Symbol]
---*/

test("empty WeakSet constructor", () => {
  const set = new WeakSet();
  expect(set instanceof WeakSet).toBe(true);
});

test("WeakSet constructor accepts null and undefined", () => {
  expect(new WeakSet(null) instanceof WeakSet).toBe(true);
  expect(new WeakSet(undefined) instanceof WeakSet).toBe(true);
});

test("WeakSet constructor rejects non-iterables", () => {
  expect(() => new WeakSet(1)).toThrow(TypeError);
  expect(() => new WeakSet({})).toThrow(TypeError);
});

test("WeakSet constructor with object values", () => {
  const a = {};
  const b = {};
  const set = new WeakSet([a, b]);
  expect(set.has(a)).toBe(true);
  expect(set.has(b)).toBe(true);
});

test("WeakSet constructor accepts non-registered symbols", () => {
  const value = Symbol("local");
  const set = new WeakSet([value]);
  expect(set.has(value)).toBe(true);
});

test("WeakSet constructor rejects primitives and registered symbols", () => {
  expect(() => new WeakSet([1])).toThrow(TypeError);
  expect(() => new WeakSet([null])).toThrow(TypeError);
  expect(() => new WeakSet([undefined])).toThrow(TypeError);
  expect(() => new WeakSet([Symbol.for("registered")])).toThrow(TypeError);
});

test("WeakSet constructor accepts arbitrary iterables", () => {
  const value = {};
  const iterable = {
    [Symbol.iterator]() {
      let done = false;
      return {
        next() {
          if (done) return { done: true };
          done = true;
          return { value, done: false };
        },
      };
    },
  };
  const set = new WeakSet(iterable);
  expect(set.has(value)).toBe(true);
});

test("WeakSet constructor uses the current add method", () => {
  const value = {};
  let calls = 0;
  let receiver;
  let receivedValue;
  const original = WeakSet.prototype.add;
  WeakSet.prototype.add = {
    add(v) {
      calls = calls + 1;
      receiver = this;
      receivedValue = v;
      return this;
    },
  }.add;
  try {
    const set = new WeakSet([value]);
    expect(calls).toBe(1);
    expect(receiver).toBe(set);
    expect(receivedValue).toBe(value);
  } finally {
    WeakSet.prototype.add = original;
  }
});

test("WeakSet constructor closes iterator on abrupt completion", () => {
  let closed = false;
  const iterable = {
    [Symbol.iterator]() {
      return {
        next() {
          return { value: 1, done: false };
        },
        return() {
          closed = true;
          return {};
        },
      };
    },
  };
  expect(() => new WeakSet(iterable)).toThrow(TypeError);
  expect(closed).toBe(true);
});

test("WeakSet.prototype.constructor is WeakSet", () => {
  expect(WeakSet.prototype.constructor).toBe(WeakSet);
  expect(new WeakSet().constructor).toBe(WeakSet);
});

test("WeakSet has no collection enumeration APIs", () => {
  expect(WeakSet.prototype.size).toBe(undefined);
  expect(WeakSet.prototype.clear).toBe(undefined);
  expect(WeakSet.prototype.forEach).toBe(undefined);
  expect(WeakSet.prototype.keys).toBe(undefined);
  expect(WeakSet.prototype.values).toBe(undefined);
  expect(WeakSet.prototype.entries).toBe(undefined);
  expect(WeakSet.prototype[Symbol.iterator]).toBe(undefined);
});
