/*---
description: WeakMap.prototype.has checks weak key presence
features: [WeakMap, Symbol]
---*/

test("has returns true for present object key", () => {
  const key = {};
  const map = new WeakMap([[key, 1]]);
  expect(map.has(key)).toBe(true);
});

test("has returns false for missing or invalid keys", () => {
  const map = new WeakMap();
  expect(map.has({})).toBe(false);
  expect(map.has(1)).toBe(false);
  expect(map.has(null)).toBe(false);
  expect(map.has(undefined)).toBe(false);
  expect(map.has(Symbol.for("registered"))).toBe(false);
});

test("has works with non-registered symbols", () => {
  const key = Symbol("local");
  const map = new WeakMap([[key, "value"]]);
  expect(map.has(key)).toBe(true);
});

test("has throws TypeError when receiver is not a WeakMap", () => {
  const has = WeakMap.prototype.has;
  expect(() => has.call({}, {})).toThrow(TypeError);
  expect(() => has.call(WeakMap.prototype, {})).toThrow(TypeError);
});
