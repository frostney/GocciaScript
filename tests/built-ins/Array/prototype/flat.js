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
