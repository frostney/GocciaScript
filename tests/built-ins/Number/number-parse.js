/*---
description: Number.parseInt and Number.parseFloat functions work correctly
features: [Number.parseInt, Number.parseFloat]
---*/

test("Number.parseInt and Number.parseFloat", () => {
  expect(Number.parseInt("123")).toBe(123);
  expect(Number.parseInt("123.45")).toBe(123);
  expect(Number.parseInt("123abc")).toBe(123);

  expect(Number.parseFloat("123.45")).toBe(123.45);
  expect(Number.parseFloat("123")).toBe(123);
  expect(Number.parseFloat("123.45abc")).toBe(123.45);
});
