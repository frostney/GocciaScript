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

test("map property descriptor on Array.prototype", () => {
  const desc = Object.getOwnPropertyDescriptor(Array.prototype, "map");
  expect(desc.writable).toBe(true);
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(true);
});

test("map has correct name and length", () => {
  expect(Array.prototype.map.name).toBe("map");
  expect(Array.prototype.map.length).toBe(1);
  const nameDesc = Object.getOwnPropertyDescriptor(Array.prototype.map, "name");
  expect(nameDesc.configurable).toBe(true);
  expect(nameDesc.enumerable).toBe(false);
  const lengthDesc = Object.getOwnPropertyDescriptor(Array.prototype.map, "length");
  expect(lengthDesc.configurable).toBe(true);
  expect(lengthDesc.enumerable).toBe(false);
  expect(lengthDesc.writable).toBe(false);
});

test("generic receiver transforms array-like", () => {
  const arrayLike = { 0: 'a', 1: 'b', 2: 'c', length: 3 };
  const result = Array.prototype.map.call(arrayLike, x => x.toUpperCase());
  expect(result).toEqual(['A', 'B', 'C']);
});

test("non-string primitive (boolean) this returns empty array", () => {
  expect(Array.prototype.map.call(true, x => x)).toEqual([]);
});

test("null this throws TypeError even without callback", () => {
  expect(() => Array.prototype.map.call(null)).toThrow(TypeError);
});

test("map preserves trailing holes in sparse array", () => {
  const arr = [1, 2, ,];
  const result = arr.map(x => x);
  expect(result.length).toBe(3);
  expect(0 in result).toBe(true);
  expect(2 in result).toBe(false);
});
