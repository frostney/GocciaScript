/*---
description: WeakSet.prototype.delete removes weakly held values
features: [WeakSet, Symbol]
---*/

test("delete removes an object value and returns true", () => {
  const value = {};
  const set = new WeakSet([value]);
  expect(set.delete(value)).toBe(true);
  expect(set.has(value)).toBe(false);
});

test("delete returns false for missing or invalid values", () => {
  const set = new WeakSet();
  expect(set.delete({})).toBe(false);
  expect(set.delete(1)).toBe(false);
  expect(set.delete(null)).toBe(false);
  expect(set.delete(undefined)).toBe(false);
  expect(set.delete(Symbol.for("registered"))).toBe(false);
});

test("delete works with non-registered symbols", () => {
  const value = Symbol("local");
  const set = new WeakSet([value]);
  expect(set.delete(value)).toBe(true);
  expect(set.has(value)).toBe(false);
});

test("delete throws TypeError when receiver is not a WeakSet", () => {
  const del = WeakSet.prototype.delete;
  expect(() => del.call({}, {})).toThrow(TypeError);
  expect(() => del.call(WeakSet.prototype, {})).toThrow(TypeError);
});
