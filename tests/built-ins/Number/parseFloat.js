/*---
description: Number.parseFloat functions work correctly
features: [Number.parseFloat]
---*/

test("Number.parseFloat", () => {
  expect(Number.parseFloat("123.45")).toBe(123.45);
  expect(Number.parseFloat("123")).toBe(123);
  expect(Number.parseFloat("123.45abc")).toBe(123.45);
});
