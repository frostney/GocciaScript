/*---
description: Array.prototype[Symbol.iterator] returns a values iterator
features: [Array, Symbol.iterator, Iterator]
---*/

test("returns a values iterator", () => {
  const arr = [1, 2, 3];
  const iter = arr[Symbol.iterator]();
  expect(iter.next().value).toBe(1);
  expect(iter.next().value).toBe(2);
  expect(iter.next().value).toBe(3);
  expect(iter.next().done).toBe(true);
});

test("spread uses Symbol.iterator", () => {
  const arr = [1, 2, 3];
  expect([...arr]).toEqual([1, 2, 3]);
});

test("destructuring uses Symbol.iterator", () => {
  const [a, b, c] = [10, 20, 30];
  expect(a).toBe(10);
  expect(b).toBe(20);
  expect(c).toBe(30);
});
