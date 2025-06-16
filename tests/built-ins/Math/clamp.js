/*---
description: Math.clamp
features: [Math.clamp]
---*/

test("Math.clamp", () => {
  expect(Math.clamp(5, 0, 10)).toBe(5);
  expect(Math.clamp(-5, 0, 10)).toBe(0);
  expect(Math.clamp(15, 0, 10)).toBe(10);
  expect(Math.clamp(5, 0, Infinity)).toBe(5);
  expect(Math.clamp(-5, -Infinity, 10)).toBe(-5);
  expect(() => Math.clamp(10, 5, 0)).toThrow(RangeError);
});
