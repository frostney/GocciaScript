/*---
description: Map.prototype.keys returns an iterator over keys
features: [Map.prototype.keys, Iterator]
---*/

test("keys returns all keys in insertion order", () => {
  const map = new Map([["c", 3], ["a", 1], ["b", 2]]);
  expect([...map.keys()]).toEqual(["c", "a", "b"]);
});

test("keys on empty Map", () => {
  const map = new Map();
  expect([...map.keys()]).toEqual([]);
});

test("keys with non-string keys", () => {
  const map = new Map([[1, "one"], [true, "yes"], [null, "nothing"]]);
  const keys = [...map.keys()];
  expect(keys).toEqual([1, true, null]);
});

test("keys returns an iterator with next()", () => {
  const map = new Map([["a", 1], ["b", 2], ["c", 3]]);
  const iter = map.keys();
  expect(iter.next().value).toBe("a");
  expect(iter.next().value).toBe("b");
  expect(iter.next().value).toBe("c");
  expect(iter.next().done).toBe(true);
});
