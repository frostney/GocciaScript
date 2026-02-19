/*---
description: Set.prototype.values returns an iterator over values
features: [Set.prototype.values, Iterator]
---*/

test("values returns all values in insertion order", () => {
  const set = new Set([3, 1, 4, 1, 5]);
  const vals = [...set.values()];
  expect(vals).toEqual([3, 1, 4, 5]);
});

test("values on empty Set", () => {
  const set = new Set();
  expect([...set.values()]).toEqual([]);
});

test("values with mixed types", () => {
  const set = new Set([1, "hello", true, null]);
  const vals = [...set.values()];
  expect(vals).toEqual([1, "hello", true, null]);
});

test("values returns an iterator with next()", () => {
  const set = new Set([1, 2, 3]);
  const iter = set.values();
  expect(iter.next().value).toBe(1);
  expect(iter.next().value).toBe(2);
  expect(iter.next().value).toBe(3);
  expect(iter.next().done).toBe(true);
});

test("keys is same as values for Set", () => {
  const set = new Set([1, 2, 3]);
  expect([...set.keys()]).toEqual([1, 2, 3]);
});

test("Set default iterator is values", () => {
  const set = new Set([1, 2, 3]);
  const iter = set[Symbol.iterator]();
  expect(iter.next().value).toBe(1);
  expect(iter.next().value).toBe(2);
  expect(iter.next().value).toBe(3);
  expect(iter.next().done).toBe(true);
});

test("spread on set", () => {
  const set = new Set([1, 2, 3]);
  expect([...set]).toEqual([1, 2, 3]);
});

test("Set.entries() returns [value, value] pairs", () => {
  const set = new Set([10, 20, 30]);
  const entries = [...set.entries()];
  expect(entries[0]).toEqual([10, 10]);
  expect(entries[1]).toEqual([20, 20]);
  expect(entries[2]).toEqual([30, 30]);
});

test("Set.entries() iterator with next()", () => {
  const set = new Set(["a", "b"]);
  const iter = set.entries();
  const first = iter.next().value;
  expect(first[0]).toBe("a");
  expect(first[1]).toBe("a");
  const second = iter.next().value;
  expect(second[0]).toBe("b");
  expect(second[1]).toBe("b");
  expect(iter.next().done).toBe(true);
});
