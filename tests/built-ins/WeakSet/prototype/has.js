/*---
description: WeakSet.prototype.has checks weakly held values
features: [WeakSet, Symbol]
---*/

test("has returns true for present object value", () => {
  const value = {};
  const set = new WeakSet([value]);
  expect(set.has(value)).toBe(true);
});

test("has returns false for missing or invalid values", () => {
  const set = new WeakSet();
  expect(set.has({})).toBe(false);
  expect(set.has(1)).toBe(false);
  expect(set.has(null)).toBe(false);
  expect(set.has(undefined)).toBe(false);
  expect(set.has(Symbol.for("registered"))).toBe(false);
});

test("has works with non-registered symbols", () => {
  const value = Symbol("local");
  const set = new WeakSet([value]);
  expect(set.has(value)).toBe(true);
});

test("has throws TypeError when receiver is not a WeakSet", () => {
  const has = WeakSet.prototype.has;
  expect(() => has.call({}, {})).toThrow(TypeError);
  expect(() => has.call(WeakSet.prototype, {})).toThrow(TypeError);
});
