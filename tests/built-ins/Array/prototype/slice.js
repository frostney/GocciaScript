/*---
description: Array.prototype.slice
features: [Array.prototype.slice]
---*/

test("Array.protoype.slice copies the array from the start index", () => {
  const arr = [1, 2, 3, 4, 5];
  const sliced = arr.slice(2);
  expect(sliced).toEqual([3, 4, 5]);
  expect(sliced).not.toBe(arr);
});

test("Array.prototype.slice copies the array with negative start index", () => {
  const fruits = ["Apple", "Banana", "Orange", "Mango", "Pineapple"];

  const lastTwo = fruits.slice(-2);
  expect(lastTwo).toEqual(["Mango", "Pineapple"]);
});

test("Array.protoype.slice copies the array from the start index to the end index", () => {
  const arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  const sliced = arr.slice(2, 8);
  expect(sliced).toEqual([3, 4, 5, 6, 7, 8]);
  expect(sliced).not.toBe(arr);
});

test("Array.prototype.slice copies the array with positive start index and negative end index", () => {
  const fruits = ["Apple", "Banana", "Orange", "Mango", "Pineapple"];

  const sliceExample = fruits.slice(1, -1);
  expect(sliceExample).toEqual(["Banana", "Orange", "Mango"]);
});

test("Array.prototype.slice shallowly copies the array", () => {
  const arr = [1, 2, 3, 4, 5];
  const sliced = arr.slice();
  expect(sliced).toEqual([1, 2, 3, 4, 5]);
  expect(sliced).not.toBe(arr);
});

test("slice with both negative indices", () => {
  const arr = [1, 2, 3, 4, 5];
  expect(arr.slice(-3, -1)).toEqual([3, 4]);
});

test("slice with start >= end returns empty array", () => {
  expect([1, 2, 3].slice(2, 1)).toEqual([]);
  expect([1, 2, 3].slice(5)).toEqual([]);
});

test("slice with start beyond length returns empty array", () => {
  expect([1, 2, 3].slice(100)).toEqual([]);
});

test("slice with negative start beyond length starts from 0", () => {
  expect([1, 2, 3].slice(-100)).toEqual([1, 2, 3]);
});

test("slice with end beyond length clamps to length", () => {
  expect([1, 2, 3].slice(0, 100)).toEqual([1, 2, 3]);
});

test("slice preserves undefined and null elements", () => {
  expect([1, undefined, null, 4].slice(1, 3)).toEqual([undefined, null]);
});

test("slice on empty array", () => {
  expect([].slice()).toEqual([]);
  expect([].slice(0, 5)).toEqual([]);
});

test("generic receiver extracts from array-like", () => {
  const arrayLike = { 0: 'a', 1: 'b', 2: 'c', length: 3 };
  const result = Array.prototype.slice.call(arrayLike, 1, 3);
  expect(result).toEqual(['b', 'c']);
  expect(Array.isArray(result)).toBe(true);
});

test("primitive this returns empty array", () => {
  expect(Array.prototype.slice.call(42)).toEqual([]);
});

test("slice preserves holes in sparse array", () => {
  const result = [1, , 3].slice();
  expect(result.length).toBe(3);
  expect(0 in result).toBe(true);
  expect(1 in result).toBe(false);
  expect(2 in result).toBe(true);
});

test("slice throws RangeError when proxied length exceeds 2**32 - 1", () => {
  const target = [];
  const proxy = new Proxy(target, {
    get(t, key) {
      if (key === 'length') return 2 ** 32;
      return Reflect.get(t, key);
    },
  });
  expect(() => Array.prototype.slice.call(proxy, 0)).toThrow(RangeError);
});

test("slice throws RangeError on array-like with length > 2**32 - 1", () => {
  const obj = { length: 2 ** 32 };
  expect(() => Array.prototype.slice.call(obj, 0)).toThrow(RangeError);
});

test("slice(NaN) treats start as 0", () => {
  const arr = [1, 2, 3, 4, 5];
  expect(arr.slice(NaN)).toEqual([1, 2, 3, 4, 5]);
});

test("slice(0, NaN) treats end as 0", () => {
  const arr = [1, 2, 3, 4, 5];
  expect(arr.slice(0, NaN)).toEqual([]);
});

test("slice(NaN, NaN) returns empty", () => {
  const arr = [1, 2, 3, 4, 5];
  expect(arr.slice(NaN, NaN)).toEqual([]);
});

test("slice(+Infinity) starts past the end and yields empty", () => {
  const arr = [1, 2, 3, 4, 5];
  expect(arr.slice(Infinity)).toEqual([]);
});

test("slice(-Infinity) clamps to 0", () => {
  const arr = [1, 2, 3, 4, 5];
  expect(arr.slice(-Infinity)).toEqual([1, 2, 3, 4, 5]);
});

test("slice(0, +Infinity) clamps end to len", () => {
  const arr = [1, 2, 3, 4, 5];
  expect(arr.slice(0, Infinity)).toEqual([1, 2, 3, 4, 5]);
});

test("slice(0, -Infinity) clamps end to 0", () => {
  const arr = [1, 2, 3, 4, 5];
  expect(arr.slice(0, -Infinity)).toEqual([]);
});

test("slice(-Infinity, +Infinity) returns full copy", () => {
  const arr = [1, 2, 3, 4, 5];
  const copy = arr.slice(-Infinity, Infinity);
  expect(copy).toEqual([1, 2, 3, 4, 5]);
  expect(copy).not.toBe(arr);
});

test("slice(NaN) on array-like with positive length is a no-op start", () => {
  const arrayLike = { length: 3, 0: "a", 1: "b", 2: "c" };
  const sliced = Array.prototype.slice.call(arrayLike, NaN);
  expect(sliced).toEqual(["a", "b", "c"]);
});

test("slice on generic array-like with length > 2**31 and high start index", () => {
  const high = 2 ** 32;
  const obj = { length: 2 ** 33 };
  obj[high] = "present";
  const result = Array.prototype.slice.call(obj, high, high + 3);
  expect(result.length).toBe(3);
  expect(result[0]).toBe("present");
  expect(result[1]).toBe(undefined);
  expect(result[2]).toBe(undefined);
  expect(0 in result).toBe(true);
  expect(1 in result).toBe(false);
  expect(2 in result).toBe(false);
});

test("slice on generic array-like with high start returns empty when start >= end", () => {
  const high = 2 ** 32;
  const obj = { length: 2 ** 33 };
  const result = Array.prototype.slice.call(obj, high + 5, high);
  expect(result).toEqual([]);
});

test("slice on generic array-like with high negative end", () => {
  const high = 2 ** 32;
  const obj = { length: 2 ** 33 };
  obj[high] = "x";
  obj[high + 1] = "y";
  const result = Array.prototype.slice.call(obj, high, high + 2);
  expect(result).toEqual(["x", "y"]);
});
