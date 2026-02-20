/*---
description: Array.prototype.values returns an iterator over array values
features: [Array.prototype.values, Iterator]
---*/

test("returns an iterator over array values", () => {
  const arr = [10, 20, 30];
  const iter = arr.values();
  expect(iter.next().value).toBe(10);
  expect(iter.next().value).toBe(20);
  expect(iter.next().value).toBe(30);
  expect(iter.next().done).toBe(true);
});

test("done is false until exhausted", () => {
  const iter = [1].values();
  const first = iter.next();
  expect(first.value).toBe(1);
  expect(first.done).toBe(false);
  expect(iter.next().done).toBe(true);
});

test("returns undefined value when done", () => {
  const iter = [].values();
  const result = iter.next();
  expect(result.value).toBe(undefined);
  expect(result.done).toBe(true);
});

test("works with spread operator", () => {
  const arr = [1, 2, 3];
  expect([...arr.values()]).toEqual([1, 2, 3]);
});
