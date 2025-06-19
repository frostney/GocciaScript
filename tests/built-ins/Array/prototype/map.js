/*---
description: Array.prototype.map returns a new array with the results of calling a function on each element
features: [Array.prototype.map]
---*/

test("Array.prototype.map with arrow function expression", () => {
  const arr = [1, 2, 3];
  const mapped = arr.map((x) => x * 2);
  expect(arr).toEqual([1, 2, 3]);
  expect(mapped).toEqual([2, 4, 6]);
});

test("Array.prototype.map with arrow function block", () => {
  const arr = [1, 2, 3];
  const mapped = arr.map((x) => {
    return x * 2;
  });
  expect(arr).toEqual([1, 2, 3]);
  expect(mapped).toEqual([2, 4, 6]);
});

test("Array.prototype.map with index and array parameters", () => {
  const arr = [1, 2, 3];
  const mapped = arr.map((x, index, array) => {
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

    return x * 2;
  });
  expect(arr).toEqual([1, 2, 3]);
  expect(mapped).toEqual([2, 4, 6]);
});

test("Array.prototype.map with empty array", () => {
  const arr = [];
  const mapped = arr.map((x) => x * 2);
  expect(mapped).toEqual([]);
});
