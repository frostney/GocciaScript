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

test("map returns a new array, not the original", () => {
  const arr = [1, 2, 3];
  const mapped = arr.map((x) => x);
  expect(mapped).not.toBe(arr);
  expect(mapped).toEqual(arr);
});

test("map preserves length including undefined elements", () => {
  const arr = [1, undefined, 3];
  const mapped = arr.map((x) => x);
  expect(mapped.length).toBe(3);
  expect(mapped[1]).toBe(undefined);
});

test("map callback receives correct arguments for each element", () => {
  const indices = [];
  const values = [];
  [10, 20, 30].map((val, idx) => {
    values.push(val);
    indices.push(idx);
    return val;
  });
  expect(values).toEqual([10, 20, 30]);
  expect(indices).toEqual([0, 1, 2]);
});

test("map with null and undefined elements", () => {
  const arr = [null, undefined, 0, "", false];
  const mapped = arr.map((x) => x);
  expect(mapped).toEqual([null, undefined, 0, "", false]);
});

test("map does not mutate the original array", () => {
  const arr = [1, 2, 3];
  arr.map((x) => x * 10);
  expect(arr).toEqual([1, 2, 3]);
});
