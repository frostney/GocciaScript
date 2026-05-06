/*---
description: Traditional for-loop counts up/down with let
features: [compat-traditional-for-loop]
---*/

test("counts up", () => {
  const result = [];
  for (let i = 0; i < 5; i++) result.push(i);
  expect(result).toEqual([0, 1, 2, 3, 4]);
});

test("counts down", () => {
  const result = [];
  for (let i = 5; i > 0; i--) result.push(i);
  expect(result).toEqual([5, 4, 3, 2, 1]);
});

test("step by 2", () => {
  const result = [];
  for (let i = 0; i < 10; i += 2) result.push(i);
  expect(result).toEqual([0, 2, 4, 6, 8]);
});

test("comma-separated init bindings", () => {
  const result = [];
  for (let i = 0, j = 10; i < 3; i++, j--) result.push([i, j]);
  expect(result).toEqual([[0, 10], [1, 9], [2, 8]]);
});

test("zero-iteration loop", () => {
  const result = [];
  for (let i = 0; i < 0; i++) result.push(i);
  expect(result).toEqual([]);
});
