/*---
description: Array.prototype.flat
features: [Array.prototype.flat]
---*/

test("Array.prototype.flat", () => {
  const arr = [1, 2, [3, 4]];
  expect(arr.flat()).toEqual([1, 2, 3, 4]);
});

test("Array.prototype.flat with depth", () => {
  const arr = [1, 2, [3, [4, 5]]];
  expect(arr.flat(2)).toEqual([1, 2, 3, 4, 5]);
});

test("Array.prototype.flat with depth 0", () => {
  const arr = [1, 2, [3, [4, 5]]];
  expect(arr.flat(0)).toEqual([1, 2, [3, [4, 5]]]);
});

test("Array.prototype.flat on sparse arrays", () => {
  const arr = [1, 2, , 4, 5];
  expect(arr.flat()).toEqual([1, 2, 4, 5]);

  const arr2 = [1, , 3, ["a", , "c"]];
  expect(arr2.flat()).toEqual([1, 3, "a", "c"]);
});

test("flat with default depth of 1", () => {
  const arr = [1, [2, [3]]];
  expect(arr.flat()).toEqual([1, 2, [3]]);
});

test("flat with Infinity depth", () => {
  const arr = [1, [2, [3, [4, [5]]]]];
  expect(arr.flat(Infinity)).toEqual([1, 2, 3, 4, 5]);
});

test('flat with negative Infinity depth returns a cloned array', () => {
  const arr = [1, [2, [3, [4, [5]]]]];
  const result = arr.flat(-Infinity);
  expect(result).not.toBe(arr);
  expect(result).toEqual([1, [2, [3, [4, [5]]]]]);
});

test("flat returns a new array", () => {
  const arr = [1, 2, 3];
  const result = arr.flat();
  expect(result).not.toBe(arr);
  expect(result).toEqual([1, 2, 3]);
});

test("flat on empty array", () => {
  expect([].flat()).toEqual([]);
});

test("flat on already flat array", () => {
  expect([1, 2, 3].flat()).toEqual([1, 2, 3]);
});
