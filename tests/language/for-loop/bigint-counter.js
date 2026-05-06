/*---
description: Traditional for-loop with BigInt counter variable
features: [compat-traditional-for-loop, bigint]
---*/

test("BigInt counter counts up", () => {
  const result = [];
  for (let i = 0n; i < 5n; i++) result.push(i);
  expect(result).toEqual([0n, 1n, 2n, 3n, 4n]);
});

test("BigInt counter counts down", () => {
  const result = [];
  for (let i = 5n; i > 0n; i--) result.push(i);
  expect(result).toEqual([5n, 4n, 3n, 2n, 1n]);
});

test("BigInt counter with prefix increment", () => {
  const result = [];
  for (let i = 0n; i < 5n; ++i) result.push(i);
  expect(result).toEqual([0n, 1n, 2n, 3n, 4n]);
});

test("BigInt counter with prefix decrement", () => {
  const result = [];
  for (let i = 5n; i > 0n; --i) result.push(i);
  expect(result).toEqual([5n, 4n, 3n, 2n, 1n]);
});

test("BigInt counter large range", () => {
  let sum = 0n;
  for (let i = 10n; i < 20n; i++) sum += i;
  expect(sum).toBe(145n);
});
