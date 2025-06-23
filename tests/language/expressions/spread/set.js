/*---
description: Spread syntax for Set
features: [set-spread]
---*/

test.skip("spread syntax for Set", () => {
  const set = new Set([1, 2, 3]);
  const newSet = [...set, 4, 5, 6];
  expect(newSet).toEqual([1, 2, 3, 4, 5, 6]);
});

test.skip("removes duplicates when spread", () => {
  const set = new Set([1, 2, 3, 2, 1]);
  const fromSet = [...set];
  expect(fromSet).toEqual([1, 2, 3]);
});

test.skip("spread to create new Set", () => {
  const set1 = new Set([1, 2, 3, 4]);
  const set2 = new Set([3, 4, 5, 6]);
  const combined = new Set([...set1, ...set2]);
  expect(combined).toEqual(new Set([1, 2, 3, 4, 5, 6]));
});
