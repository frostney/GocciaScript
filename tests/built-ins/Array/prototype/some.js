/*---
description: Array.prototype.some checks if at least one element in the array satisfies a condition
features: [Array.prototype.some]
---*/

test("Array.prototype.some checks if at least one element in the array satisfies a condition", () => {
  const arr = [1, 2, 3];
  const some = arr.some((x) => x > 2);
  expect(some).toBe(true);
});

test("Array.prototype.some with index and array parameters", () => {
  const arr = [1, 2, 3];
  const some = arr.some((x, index, array) => {
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

    return x > 2;
  });
  expect(some).toBe(true);
});

test("Array.prototype.some with empty array", () => {
  const arr = [];
  const some = arr.some((x) => x > 2);
  expect(some).toBe(false);
});

test("Array.prototype.some with sparse array", () => {
  expect([1, , 3].some((x) => x === undefined)).toBe(false);
  expect([1, , 1].some((x) => x !== 1)).toBe(false);
  expect([1, undefined, 1].some((x) => x !== 1)).toBe(true);
});
