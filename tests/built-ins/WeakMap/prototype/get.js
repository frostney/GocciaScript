/*---
description: WeakMap.prototype.get returns values for weak keys
features: [WeakMap, Symbol]
---*/

test("get returns value for object key", () => {
  const key = {};
  const map = new WeakMap([[key, "value"]]);
  expect(map.get(key)).toBe("value");
});

test("get returns undefined for missing object key", () => {
  const map = new WeakMap();
  expect(map.get({})).toBe(undefined);
});

test("get returns undefined for primitive and registered symbol keys", () => {
  const map = new WeakMap();
  expect(map.get(1)).toBe(undefined);
  expect(map.get("x")).toBe(undefined);
  expect(map.get(null)).toBe(undefined);
  expect(map.get(undefined)).toBe(undefined);
  expect(map.get(Symbol.for("registered"))).toBe(undefined);
});

test("get works with non-registered symbols", () => {
  const key = Symbol("local");
  const map = new WeakMap([[key, 123]]);
  expect(map.get(key)).toBe(123);
});

test("get throws TypeError when receiver is not a WeakMap", () => {
  const get = WeakMap.prototype.get;
  expect(() => get.call({}, {})).toThrow(TypeError);
  expect(() => get.call(WeakMap.prototype, {})).toThrow(TypeError);
});
