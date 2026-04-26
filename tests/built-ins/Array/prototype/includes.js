/*---
description: Array.prototype.includes checks if an array contains a value
features: [Array.prototype.includes]
---*/

test("Array.prototype.includes checks if an array contains a number value", () => {
  const arr = [1, 2, 3];
  const includes = arr.includes(2);
  const includesFour = arr.includes(4);
  expect(includes).toBe(true);
  expect(includesFour).toBe(false);
});

test("Array.prototype.includes checks if an array contains a string value", () => {
  const pets = ["cat", "dog", "bat"];
  const includes = pets.includes("cat");
  const includesAt = pets.includes("at");
  expect(includes).toBe(true);
  expect(includesAt).toBe(false);
});

test("Array.prototype.includes with fromValue", () => {
  const arr = ["cat", "dog", "bat"];
  const includes = arr.includes("cat", 1);
  const includesBat = arr.includes("bat", 1);
  const includesAt = arr.includes("at", 1);
  expect(includes).toBe(false);
  expect(includesBat).toBe(true);
  expect(includesAt).toBe(false);
});

test("Array.protoype.includes with fromIndex is greater than or equal to the array length", () => {
  const arr = ["a", "b", "c"];

  expect(arr.includes("c", 3)).toBe(false);
  expect(arr.includes("c", 100)).toBe(false);
});

test("Array.protoype.includes with fromIndex is negative", () => {
  const arr = ["a", "b", "c"];

  expect(arr.includes("a", -100)).toBe(true);
  expect(arr.includes("b", -100)).toBe(true);
  expect(arr.includes("c", -100)).toBe(true);
  expect(arr.includes("a", -2)).toBe(false);
});

test("Array.prototype.includes with empty array", () => {
  const arr = [];
  const includes = arr.includes(2);
  expect(includes).toBe(false);
});

test("Array.prototype.includes with null", () => {
  expect([null].includes(null)).toBe(true);
});

test("Array.prototype.includes with undefined", () => {
  expect([undefined].includes(undefined)).toBe(true);
});

test("Array.prototype.includes finds undefined in array", () => {
  expect([1, undefined, 3].includes(undefined)).toBe(true);
});

test("includes uses SameValueZero so NaN is found", () => {
  expect([1, NaN, 3].includes(NaN)).toBe(true);
});

test("includes treats +0 and -0 as equal", () => {
  expect([0].includes(-0)).toBe(true);
  expect([-0].includes(0)).toBe(true);
});

test("includes with boolean values", () => {
  expect([true, false].includes(true)).toBe(true);
  expect([true, false].includes(false)).toBe(true);
  expect([0].includes(false)).toBe(false);
  expect([1].includes(true)).toBe(false);
});

test("generic receiver searches array-like", () => {
  const arrayLike = { 0: 'a', 1: 'b', 2: 'c', length: 3 };
  expect(Array.prototype.includes.call(arrayLike, 'a')).toBe(true);
  expect(Array.prototype.includes.call(arrayLike, 'z')).toBe(false);
});

test("primitive this returns false", () => {
  expect(Array.prototype.includes.call(false, false)).toBe(false);
});

test("sparse-path includes(undefined) with fromIndex narrowing window to filled index returns false", () => {
  // Use string keys for the indices: the test pinpoints the sparse-path
  // window-narrowing case independently of any number-to-property-key
  // formatting in the engine.
  const huge = { length: 9007199254740991 };
  huge["9007199254740990"] = 1;
  const result = Array.prototype.includes.call(
    huge, undefined, 9007199254740990
  );
  expect(result).toBe(false);
});

test("sparse-path includes(undefined) returns true when window contains a hole", () => {
  const huge = { length: 9007199254740991 };
  huge["9007199254740990"] = 1;
  const result = Array.prototype.includes.call(
    huge, undefined, 9007199254740989
  );
  expect(result).toBe(true);
});

test("sparse-path includes(undefined) returns true when an own value is undefined", () => {
  const huge = { length: 9007199254740991 };
  huge["9007199254740990"] = undefined;
  const result = Array.prototype.includes.call(
    huge, undefined, 9007199254740990
  );
  expect(result).toBe(true);
});
