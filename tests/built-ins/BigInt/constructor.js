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

test("BigInt from hex string", () => {
  expect(BigInt("0xFF")).toBe(255n);
  expect(BigInt("0x10")).toBe(16n);
});

test("BigInt from binary string", () => {
  expect(BigInt("0b1010")).toBe(10n);
});

test("BigInt from octal string", () => {
  expect(BigInt("0o77")).toBe(63n);
});

test("BigInt from empty string", () => {
  expect(BigInt("")).toBe(0n);
});

test("BigInt.asIntN", () => {
  expect(BigInt.asIntN(8, 255n)).toBe(-1n);
  expect(BigInt.asIntN(8, 128n)).toBe(-128n);
  expect(BigInt.asIntN(8, 127n)).toBe(127n);
  expect(BigInt.asIntN(8, 0n)).toBe(0n);
  expect(BigInt.asIntN(1, 1n)).toBe(-1n);
});

test("BigInt.asUintN", () => {
  expect(BigInt.asUintN(8, 255n)).toBe(255n);
  expect(BigInt.asUintN(8, 256n)).toBe(0n);
  expect(BigInt.asUintN(8, -1n)).toBe(255n);
  expect(BigInt.asUintN(8, 0n)).toBe(0n);
});

test("BigInt.asIntN/asUintN with invalid bits throws RangeError", () => {
  expect(() => { BigInt.asIntN(-1, 0n); }).toThrow();
  expect(() => { BigInt.asIntN(Infinity, 0n); }).toThrow();
  expect(() => { BigInt.asUintN(-1, 0n); }).toThrow();
});
