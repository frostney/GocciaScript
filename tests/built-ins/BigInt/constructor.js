/*---
description: BigInt() conversion function
features: [bigint]
---*/

test("BigInt from Number", () => {
  expect(BigInt(42)).toBe(42n);
  expect(BigInt(0)).toBe(0n);
  expect(BigInt(-1)).toBe(-1n);
});

test("BigInt from Boolean", () => {
  expect(BigInt(true)).toBe(1n);
  expect(BigInt(false)).toBe(0n);
});

test("BigInt from String", () => {
  expect(BigInt("42")).toBe(42n);
  expect(BigInt("0")).toBe(0n);
});

test("BigInt from BigInt (identity)", () => {
  expect(BigInt(42n)).toBe(42n);
});

test("BigInt from non-integer Number throws", () => {
  expect(() => { BigInt(1.5); }).toThrow();
});

test("BigInt from NaN throws", () => {
  expect(() => { BigInt(NaN); }).toThrow();
});

test("BigInt from Infinity throws", () => {
  expect(() => { BigInt(Infinity); }).toThrow();
});

test("BigInt from non-numeric string throws", () => {
  expect(() => { BigInt("abc"); }).toThrow();
});
