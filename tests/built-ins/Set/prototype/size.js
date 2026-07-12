/*---
description: Set.prototype.size returns the number of values
features: [Set.prototype.size]
---*/

test("size reflects the number of unique values", () => {
  const set = new Set([1, 2, 3]);
  expect(set.size).toBe(3);
});

test("size of empty Set is 0", () => {
  const set = new Set();
  expect(set.size).toBe(0);
});

test("size updates after add", () => {
  const set = new Set();
  expect(set.size).toBe(0);
  set.add("a");
  expect(set.size).toBe(1);
  set.add("b");
  expect(set.size).toBe(2);
  set.add("a");
  expect(set.size).toBe(2);
});

test("size updates after delete", () => {
  const set = new Set([1, 2, 3]);
  expect(set.size).toBe(3);
  set.delete(2);
  expect(set.size).toBe(2);
});

test("size is 0 after clear", () => {
  const set = new Set([1, 2, 3]);
  set.clear();
  expect(set.size).toBe(0);
});

test("size is a configurable non-enumerable prototype getter", () => {
  const descriptor = Object.getOwnPropertyDescriptor(Set.prototype, "size");
  expect(typeof descriptor.get).toBe("function");
  expect(descriptor.get.name).toBe("get size");
  expect(descriptor.get.length).toBe(0);
  expect(descriptor.set).toBe(undefined);
  expect(descriptor.enumerable).toBe(false);
  expect(descriptor.configurable).toBe(true);
});

test("size getter rejects values without Set data", () => {
  const getter = Object.getOwnPropertyDescriptor(Set.prototype, "size").get;
  expect(() => getter.call(Set.prototype)).toThrow(TypeError);
  expect(() => getter.call(new Map())).toThrow(TypeError);
});
