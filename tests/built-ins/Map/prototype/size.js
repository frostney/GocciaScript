/*---
description: Map.prototype.size returns the number of entries
features: [Map.prototype.size]
---*/

test("size reflects the number of entries", () => {
  const map = new Map([["a", 1], ["b", 2], ["c", 3]]);
  expect(map.size).toBe(3);
});

test("size of empty Map is 0", () => {
  const map = new Map();
  expect(map.size).toBe(0);
});

test("size updates after set", () => {
  const map = new Map();
  expect(map.size).toBe(0);
  map.set("a", 1);
  expect(map.size).toBe(1);
  map.set("b", 2);
  expect(map.size).toBe(2);
  map.set("a", 99);
  expect(map.size).toBe(2);
});

test("size updates after delete", () => {
  const map = new Map([["a", 1], ["b", 2], ["c", 3]]);
  expect(map.size).toBe(3);
  map.delete("b");
  expect(map.size).toBe(2);
});

test("size is 0 after clear", () => {
  const map = new Map([["a", 1], ["b", 2]]);
  map.clear();
  expect(map.size).toBe(0);
});

test("size is a configurable non-enumerable prototype getter", () => {
  const descriptor = Object.getOwnPropertyDescriptor(Map.prototype, "size");
  expect(typeof descriptor.get).toBe("function");
  expect(descriptor.get.name).toBe("get size");
  expect(descriptor.get.length).toBe(0);
  expect(descriptor.set).toBe(undefined);
  expect(descriptor.enumerable).toBe(false);
  expect(descriptor.configurable).toBe(true);
});

test("size getter rejects values without Map data", () => {
  const getter = Object.getOwnPropertyDescriptor(Map.prototype, "size").get;
  expect(() => getter.call(Map.prototype)).toThrow(TypeError);
  expect(() => getter.call(new Set())).toThrow(TypeError);
  expect(() => getter.call(new WeakMap())).toThrow(TypeError);
});
