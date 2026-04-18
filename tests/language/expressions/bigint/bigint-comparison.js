/*---
description: BigInt comparison operations
features: [bigint]
---*/

test("strict equality between BigInts", () => {
  const one = 1n;
  const alsoOne = BigInt("1");
  expect(one === alsoOne).toBe(true);
  expect(1n === 2n).toBe(false);
  const zero = 0n;
  const alsoZero = BigInt(0);
  expect(zero === alsoZero).toBe(true);
});

test("BigInt is never strictly equal to Number", () => {
  expect(1n === 1).toBe(false);
  expect(0n === 0).toBe(false);
});

test("strict inequality between BigInts", () => {
  expect(1n !== 2n).toBe(true);
  const a = 1n;
  const b = BigInt("1");
  expect(a !== b).toBe(false);
});

test("relational comparisons between BigInts", () => {
  expect(1n < 2n).toBe(true);
  expect(2n > 1n).toBe(true);
  const x = 1n;
  const y = BigInt("1");
  expect(x <= y).toBe(true);
  expect(x >= y).toBe(true);
  expect(2n < 1n).toBe(false);
});

test("cross-type comparisons BigInt vs Number", () => {
  expect(1n < 2).toBe(true);
  expect(2n > 1).toBe(true);
  expect(1 < 2n).toBe(true);
  expect(2 > 1n).toBe(true);
});
