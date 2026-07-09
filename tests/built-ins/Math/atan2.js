/*---
description: Math.atan2
features: [Math.atan2]
---*/

test("Math.atan2", () => {
  expect(Math.atan2(0, 1)).toBe(0);
  expect(Math.atan2(1, 0)).toBe(Math.PI / 2);
  expect(Number.isNaN(Math.atan2(NaN, 1))).toBe(true);
});

test("Math.atan2 preserves signed zero for y", () => {
  expect(Object.is(Math.atan2(-0, +0), -0)).toBe(true);
  expect(Object.is(Math.atan2(-0, 1), -0)).toBe(true);
  expect(Math.atan2(+0, -0)).toBe(Math.PI);
  expect(Math.atan2(-0, -0)).toBe(-Math.PI);
});

test("Math.atan2 returns NaN for missing arguments", () => {
  expect(Number.isNaN(Math.atan2(1))).toBe(true);
});

test("Math.atan2 handles infinity quadrants", () => {
  expect(Math.atan2(Infinity, Infinity)).toBe(Math.PI / 4);
  expect(Math.atan2(Infinity, -Infinity)).toBe((3 * Math.PI) / 4);
  expect(Math.atan2(-Infinity, Infinity)).toBe(-Math.PI / 4);
  expect(Math.atan2(-Infinity, -Infinity)).toBe((-3 * Math.PI) / 4);
  expect(Object.is(Math.atan2(-1, Infinity), -0)).toBe(true);
  expect(Math.atan2(1, -Infinity)).toBe(Math.PI);
});
