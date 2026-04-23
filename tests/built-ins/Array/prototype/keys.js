/*---
description: Array.prototype.keys returns an iterator over array indices
features: [Array.prototype.keys, Iterator]
---*/

test("returns an iterator over array indices", () => {
  const arr = ["a", "b", "c"];
  const iter = arr.keys();
  expect(iter.next().value).toBe(0);
  expect(iter.next().value).toBe(1);
  expect(iter.next().value).toBe(2);
  expect(iter.next().done).toBe(true);
});

test("works with spread operator", () => {
  expect([...["a", "b", "c"].keys()]).toEqual([0, 1, 2]);
});

test("empty array keys", () => {
  expect([...[].keys()]).toEqual([]);
});

test("generic receiver iterates array-like indices", () => {
  const obj = { 0: 'a', 1: 'b', length: 2 };
  const iter = Array.prototype.keys.call(obj);
  expect(iter.next().value).toBe(0);
  expect(iter.next().value).toBe(1);
  expect(iter.next().done).toBe(true);
});
