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
