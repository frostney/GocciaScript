/*---
description: WeakMap.prototype.set stores object and non-registered symbol keys
features: [WeakMap, Symbol]
---*/

test("set stores object keys and returns the WeakMap", () => {
  const key = {};
  const map = new WeakMap();
  expect(map.set(key, "value")).toBe(map);
  expect(map.get(key)).toBe("value");
});

test("set overwrites an existing key", () => {
  const key = {};
  const map = new WeakMap();
  map.set(key, 1);
  map.set(key, 2);
  expect(map.get(key)).toBe(2);
});

test("set accepts non-registered symbols", () => {
  const key = Symbol("local");
  const map = new WeakMap();
  map.set(key, "symbol-value");
  expect(map.get(key)).toBe("symbol-value");
});

test("set rejects primitive keys and registered symbols", () => {
  const map = new WeakMap();
  expect(() => map.set(1, "bad")).toThrow(TypeError);
  expect(() => map.set("x", "bad")).toThrow(TypeError);
  expect(() => map.set(null, "bad")).toThrow(TypeError);
  expect(() => map.set(undefined, "bad")).toThrow(TypeError);
  expect(() => map.set(Symbol.for("registered"), "bad")).toThrow(TypeError);
});

test("set throws TypeError when receiver is not a WeakMap", () => {
  const set = WeakMap.prototype.set;
  expect(() => set.call({}, {}, 1)).toThrow(TypeError);
  expect(() => set.call(WeakMap.prototype, {}, 1)).toThrow(TypeError);
});
