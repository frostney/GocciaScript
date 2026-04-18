/*---
description: BigInt comparison operations
features: [bigint]
---*/

test("strict equality between BigInts", () => {
  expect(1n === 1n).toBe(true);
  expect(1n === 2n).toBe(false);
  expect(0n === 0n).toBe(true);
});

test("BigInt is never strictly equal to Number", () => {
  expect(1n === 1).toBe(false);
  expect(0n === 0).toBe(false);
});

test("strict inequality between BigInts", () => {
  expect(1n !== 2n).toBe(true);
  expect(1n !== 1n).toBe(false);
});

test("relational comparisons between BigInts", () => {
  expect(1n < 2n).toBe(true);
  expect(2n > 1n).toBe(true);
  expect(1n <= 1n).toBe(true);
  expect(1n >= 1n).toBe(true);
  expect(2n < 1n).toBe(false);
});

test("cross-type comparisons BigInt vs Number", () => {
  expect(1n < 2).toBe(true);
  expect(2n > 1).toBe(true);
  expect(1 < 2n).toBe(true);
  expect(2 > 1n).toBe(true);
});
