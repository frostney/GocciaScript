/*---
description: structuredClone deep-clones arrays
features: [structuredClone]
---*/

test("clones a simple array", () => {
  const original = [1, 2, 3];
  const clone = structuredClone(original);
  expect(clone.length).toBe(3);
  expect(clone[0]).toBe(1);
  expect(clone[1]).toBe(2);
  expect(clone[2]).toBe(3);
});

test("clone is a distinct array", () => {
  const original = [1, 2, 3];
  const clone = structuredClone(original);
  clone[0] = 99;
  expect(original[0]).toBe(1);
  expect(clone[0]).toBe(99);
});

test("clones nested arrays", () => {
  const original = [[1, 2], [3, [4, 5]]];
  const clone = structuredClone(original);
  expect(clone[0][0]).toBe(1);
  expect(clone[1][1][0]).toBe(4);
  clone[0][0] = 99;
  expect(original[0][0]).toBe(1);
});

test("clones array with objects", () => {
  const original = [{ a: 1 }, { b: 2 }];
  const clone = structuredClone(original);
  expect(clone[0].a).toBe(1);
  expect(clone[1].b).toBe(2);
  clone[0].a = 99;
  expect(original[0].a).toBe(1);
});

test("clones empty array", () => {
  const clone = structuredClone([]);
  expect(clone.length).toBe(0);
});
