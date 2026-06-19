/*---
description: Legacy octal integer literals in non-strict code
features: [compat-non-strict-mode, numeric-literals]
---*/

test("legacy octal integer literals use octal mathematical values in non-strict code", () => {
  expect(00).toBe(0);
  expect(01).toBe(1);
  expect(07).toBe(7);
  expect(010).toBe(8);
  expect(077).toBe(63);
});
