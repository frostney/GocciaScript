/*---
description: Math.clamp
features: [Math.clamp]
---*/

const hasMathClamp = typeof Math.clamp !== "undefined";

describe.runIf(hasMathClamp)("Math.clamp", () => {
  expect(Math.clamp(5, 0, 10)).toBe(5);
  expect(Math.clamp(-5, 0, 10)).toBe(0);
  expect(Math.clamp(15, 0, 10)).toBe(10);
});

describe.runIf(hasMathClamp)("Math.clamp to support Infinities", () => {
  expect(Math.clamp(5, 0, Infinity)).toBe(5);
  expect(Math.clamp(-5, -Infinity, 10)).toBe(-5);

  expect(Math.clamp(5, 0, Infinity)).toBe(Math.max(5, 0));
  expect(Math.clamp(-5, -Infinity, 10)).toBe(Math.min(-5, 10));
});

describe.runIf(hasMathClamp)("Math.clamp to support NaN", () => {
  expect(Math.clamp(NaN, 0, 10)).toBeNaN();
  expect(Math.clamp(5, NaN, 10)).toBeNaN();
  expect(Math.clamp(5, 0, NaN)).toBeNaN();
});

describe.runIf(hasMathClamp)("Math.clamp to support negative zero", () => {
  expect(Math.clamp(-2, -0, 10)).toBeCloseTo(0);
  expect(Math.clamp(-0, -0, 10)).toBeCloseTo(0);
  expect(Math.clamp(0, -0, 10)).toBeCloseTo(0);
});

describe.runIf(hasMathClamp)("Math.clamp to throw error if minimum bound is larger than the maximum bound", () => {
  expect(() => Math.clamp(10, 5, 0)).toThrow(RangeError);
});
