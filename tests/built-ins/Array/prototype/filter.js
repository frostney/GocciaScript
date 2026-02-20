/*---
description: Array.prototype.filter
features: [Array.prototype.filter]
---*/

test("Array.prototype.filter", () => {
  const arr = [1, 2, 3, 4, 5];
  expect(arr.filter((x) => x % 2 === 0)).toEqual([2, 4]);
});

test("Array.prototype.filter with index and array parameters", () => {
  const arr = [1, 2, 3, 4, 5];
  expect(
    arr.filter((x, index, array) => {
      expect(array).toBe(arr);

      if (index === 0) {
        expect(x).toBe(1);
        expect(index).toBe(0);
      }

      if (index === 1) {
        expect(x).toBe(2);
        expect(index).toBe(1);
      }

      if (index === 2) {
        expect(x).toBe(3);
        expect(index).toBe(2);
      }

      if (index === 3) {
        expect(x).toBe(4);
        expect(index).toBe(3);
      }

      if (index === 4) {
        expect(x).toBe(5);
        expect(index).toBe(4);
      }

      return x % 2 === 0;
    })
  ).toEqual([2, 4]);
});

test("Array.prototype.filter with sparse array", () => {
  expect([1, , undefined].filter((x) => x === undefined)).toEqual([undefined]);
  expect([1, , undefined].filter((x) => x !== 2)).toEqual([1, undefined]);
});

test("Array.prototype.filter with empty array", () => {
  const arr = [];
  expect(arr.filter((x) => x % 2 === 0)).toEqual([]);
});

test("filter returns a new array, not the original", () => {
  const arr = [1, 2, 3];
  const filtered = arr.filter(() => true);
  expect(filtered).not.toBe(arr);
  expect(filtered).toEqual(arr);
});

test("filter preserves element order", () => {
  const arr = [5, 3, 1, 4, 2];
  const result = arr.filter((x) => x > 2);
  expect(result).toEqual([5, 3, 4]);
});

test("filter with all elements passing", () => {
  const arr = [1, 2, 3];
  expect(arr.filter(() => true)).toEqual([1, 2, 3]);
});

test("filter with no elements passing", () => {
  const arr = [1, 2, 3];
  expect(arr.filter(() => false)).toEqual([]);
});

test("filter with null and undefined values", () => {
  const arr = [0, null, undefined, "", false, 1];
  const truthy = arr.filter((x) => x);
  expect(truthy).toEqual([1]);
});

test("filter does not mutate the original array", () => {
  const arr = [1, 2, 3, 4, 5];
  arr.filter((x) => x > 3);
  expect(arr).toEqual([1, 2, 3, 4, 5]);
});
