/*---
description: WeakMap.prototype.delete removes weak keys
features: [WeakMap, Symbol]
---*/

test("delete removes an object key and returns true", () => {
  const key = {};
  const map = new WeakMap([[key, "value"]]);
  expect(map.delete(key)).toBe(true);
  expect(map.has(key)).toBe(false);
  expect(map.get(key)).toBe(undefined);
});

test("delete returns false for missing or invalid keys", () => {
  const map = new WeakMap();
  expect(map.delete({})).toBe(false);
  expect(map.delete(1)).toBe(false);
  expect(map.delete(null)).toBe(false);
  expect(map.delete(undefined)).toBe(false);
  expect(map.delete(Symbol.for("registered"))).toBe(false);
});

test("delete works with non-registered symbols", () => {
  const key = Symbol("local");
  const map = new WeakMap([[key, "value"]]);
  expect(map.delete(key)).toBe(true);
  expect(map.has(key)).toBe(false);
});

test("delete throws TypeError when receiver is not a WeakMap", () => {
  const del = WeakMap.prototype.delete;
  expect(() => del.call({}, {})).toThrow(TypeError);
  expect(() => del.call(WeakMap.prototype, {})).toThrow(TypeError);
});
