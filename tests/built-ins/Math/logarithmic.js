/*---
description: Math logarithmic functions work correctly
features: [Math.log, Math.log10, Math.exp]
---*/

test("Math logarithmic functions", () => {
  expect(Math.log(Math.E)).toBeCloseTo(1, 10);
  expect(Math.log(1)).toBeCloseTo(0, 10);
  expect(Math.log10(100)).toBeCloseTo(2, 10);
  expect(Math.log10(1000)).toBeCloseTo(3, 10);
  expect(Math.exp(0)).toBeCloseTo(1, 10);
  expect(Math.exp(1)).toBeCloseTo(Math.E, 10);

  expect(Math.exp(NaN)).toBeNaN();
  expect(Math.exp(-Infinity)).toBe(0);
  expect(Math.exp(Infinity)).toBe(Infinity);
  expect(Math.log(NaN)).toBeNaN();
  expect(Math.log(-1)).toBeNaN();
  expect(Math.log(0)).toBe(-Infinity);
  expect(Math.log(Infinity)).toBe(Infinity);
  expect(Math.log(-Infinity)).toBeNaN();
  expect(Math.log10(NaN)).toBeNaN();
  expect(Math.log10(-1)).toBeNaN();
  expect(Math.log10(0)).toBe(-Infinity);
  expect(Math.log10(Infinity)).toBe(Infinity);
  expect(Math.log10(-Infinity)).toBeNaN();
});
