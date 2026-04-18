/*---
description: TypeError for mixed BigInt/Number arithmetic and invalid BigInt operations
features: [bigint]
---*/

test("cannot mix BigInt and Number in addition", () => {
  expect(() => { 1n + 1; }).toThrow();
});

test("cannot mix BigInt and Number in subtraction", () => {
  expect(() => { 1n - 1; }).toThrow();
});

test("cannot mix BigInt and Number in multiplication", () => {
  expect(() => { 1n * 1; }).toThrow();
});

test("unary + on BigInt throws TypeError", () => {
  expect(() => { +1n; }).toThrow();
});

test("unsigned right shift with BigInt throws TypeError", () => {
  expect(() => { 1n >>> 0n; }).toThrow();
});

test("negative exponent throws", () => {
  expect(() => { 2n ** -1n; }).toThrow();
});

test("division by zero throws", () => {
  expect(() => { 1n / 0n; }).toThrow();
});
