/*---
description: Map.prototype.entries returns an iterator over [key, value] pairs
features: [Map.prototype.entries, Iterator]
---*/

test("entries returns key-value pairs in insertion order", () => {
  const map = new Map([["a", 1], ["b", 2], ["c", 3]]);
  expect([...map.entries()]).toEqual([["a", 1], ["b", 2], ["c", 3]]);
});

test("entries on empty Map", () => {
  const map = new Map();
  expect([...map.entries()]).toEqual([]);
});

test("entries with non-string keys", () => {
  const map = new Map([[1, "one"], [true, "yes"]]);
  expect([...map.entries()]).toEqual([[1, "one"], [true, "yes"]]);
});

test("entries returns an iterator with next()", () => {
  const map = new Map([["a", 1], ["b", 2]]);
  const iter = map.entries();
  const first = iter.next().value;
  expect(first[0]).toBe("a");
  expect(first[1]).toBe(1);
  const second = iter.next().value;
  expect(second[0]).toBe("b");
  expect(second[1]).toBe(2);
  expect(iter.next().done).toBe(true);
});

test("Map default iterator is entries", () => {
  const map = new Map([["a", 1], ["b", 2]]);
  const iter = map[Symbol.iterator]();
  const first = iter.next().value;
  expect(first[0]).toBe("a");
  expect(first[1]).toBe(1);
});

test("spread on map produces entries", () => {
  const map = new Map([["a", 1], ["b", 2]]);
  expect([...map]).toEqual([["a", 1], ["b", 2]]);
});
