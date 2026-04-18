/*---
description: BigInt arithmetic operations
features: [bigint]
---*/

test("addition", () => {
  expect(1n + 2n).toBe(3n);
  expect(0n + 0n).toBe(0n);
});

test("subtraction", () => {
  expect(5n - 3n).toBe(2n);
  expect(3n - 5n).toBe(-2n);
});

test("multiplication", () => {
  expect(3n * 4n).toBe(12n);
  expect(0n * 100n).toBe(0n);
});

test("division truncates toward zero", () => {
  expect(7n / 2n).toBe(3n);
  expect(-7n / 2n).toBe(-3n);
});

test("modulo sign follows dividend", () => {
  expect(7n % 3n).toBe(1n);
  expect(-7n % 3n).toBe(-1n);
});

test("exponentiation", () => {
  expect(2n ** 10n).toBe(1024n);
  expect(2n ** 0n).toBe(1n);
});

test("unary negation", () => {
  expect(-42n).toBe(-42n);
  expect(-(-42n)).toBe(42n);
});
