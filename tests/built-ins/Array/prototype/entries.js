/*---
description: Array.prototype.entries returns an iterator over [index, value] pairs
features: [Array.prototype.entries, Iterator]
---*/

test("returns an iterator over [index, value] pairs", () => {
  const arr = ["a", "b"];
  const iter = arr.entries();
  const first = iter.next().value;
  expect(first[0]).toBe(0);
  expect(first[1]).toBe("a");
  const second = iter.next().value;
  expect(second[0]).toBe(1);
  expect(second[1]).toBe("b");
  expect(iter.next().done).toBe(true);
});

test("works with spread and destructuring", () => {
  const entries = [...[10, 20, 30].entries()];
  expect(entries[0]).toEqual([0, 10]);
  expect(entries[1]).toEqual([1, 20]);
  expect(entries[2]).toEqual([2, 30]);
});

test("empty array entries", () => {
  expect([...[].entries()]).toEqual([]);
});
