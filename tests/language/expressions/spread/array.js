/*---
description: Spread syntax for arrays
features: [array-spread]
---*/

test("spread syntax for arrays at the start of an array", () => {
  const arr = [1, 2, 3];
  const newArr = [...arr, 4, 5, 6];
  expect(newArr).toEqual([1, 2, 3, 4, 5, 6]);
});

test("spread syntax for arrays at the end of an array", () => {
  const arr = [1, 2, 3];
  const newArr = [0, ...arr];
  expect(newArr).toEqual([0, 1, 2, 3]);
});

test("spread syntax for sparse arrays", () => {
  const arr = [1, , 3];
  const newArr = [...arr, 4, 5, 6];
  expect(newArr).toEqual([1, undefined, 3, 4, 5, 6]);
});

test("spread syntax for concatatening arrays", () => {
  const fruits = ["apple", "banana"];
  const vegetables = ["carrot", "broccoli"];
  const food = [...fruits, "orange", ...vegetables, "spinach"];
  expect(food).toEqual([
    "apple",
    "banana",
    "orange",
    "carrot",
    "broccoli",
    "spinach",
  ]);
});

test("spread syntax for shallow copying arrays", () => {
  const original = [1, 2, 3];
  const copy = [...original];
  expect(copy).toEqual([1, 2, 3]);
  expect(copy).not.toBe(original);

  const deepArray = [
    [1, 2, 3],
    [4, 5, 6],
  ];
  const shallowCopy = [...deepArray];
  expect(shallowCopy).toEqual([
    [1, 2, 3],
    [4, 5, 6],
  ]);
  expect(shallowCopy).not.toBe(deepArray);
  expect(shallowCopy[0]).toBe(deepArray[0]);
  expect(shallowCopy[1]).toBe(deepArray[1]);
});

test("spread to created nested arrays", () => {
  let arr1 = [1, 2];
  const arr2 = [3, 4];
  const nested = [...arr1, [...arr2, 5]];
  expect(nested).toEqual([1, 2, [3, 4, 5]]);
});

test("spread to flattens nested arrays", () => {
  const nested = [
    [1, 2],
    [3, 4],
  ];
  const flattened = [...nested[0], ...nested[1]];
  expect(flattened).toEqual([1, 2, 3, 4]);
});

test("spread with empty arrays are being flattened", () => {
  const arr = [...[]];
  expect(arr).toEqual([]);

  const arr2 = [...[1, 2, 3], ...[]];
  expect(arr2).toEqual([1, 2, 3]);
});
