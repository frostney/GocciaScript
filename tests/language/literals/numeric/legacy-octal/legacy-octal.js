/*---
description: Legacy octal integer literals in non-strict and strict code
features: [compat-non-strict-mode, numeric-literals, unsafe-function-constructor]
---*/

test("legacy octal integer literals use octal mathematical values in non-strict code", () => {
  expect(00).toBe(0);
  expect(01).toBe(1);
  expect(07).toBe(7);
  expect(010).toBe(8);
  expect(077).toBe(63);
});

test("legacy octal integer literals are rejected in strict dynamic code", () => {
  expect(() => Function("\"use strict\"; 01;")).toThrow(SyntaxError);
});
