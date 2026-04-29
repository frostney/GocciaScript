/*---
description: WeakSet.prototype.add stores weakly held values
features: [WeakSet, Symbol]
---*/

test("add stores object values and returns the WeakSet", () => {
  const value = {};
  const set = new WeakSet();
  expect(set.add(value)).toBe(set);
  expect(set.has(value)).toBe(true);
});

test("add is idempotent for the same value", () => {
  const value = {};
  const set = new WeakSet();
  set.add(value);
  set.add(value);
  expect(set.has(value)).toBe(true);
});

test("add accepts non-registered symbols", () => {
  const value = Symbol("local");
  const set = new WeakSet();
  set.add(value);
  expect(set.has(value)).toBe(true);
});

test("add rejects primitive values and registered symbols", () => {
  const set = new WeakSet();
  expect(() => set.add(1)).toThrow(TypeError);
  expect(() => set.add("x")).toThrow(TypeError);
  expect(() => set.add(null)).toThrow(TypeError);
  expect(() => set.add(undefined)).toThrow(TypeError);
  expect(() => set.add(Symbol.for("registered"))).toThrow(TypeError);
});

test("add throws TypeError when receiver is not a WeakSet", () => {
  const add = WeakSet.prototype.add;
  expect(() => add.call({}, {})).toThrow(TypeError);
  expect(() => add.call(WeakSet.prototype, {})).toThrow(TypeError);
});
