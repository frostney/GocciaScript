/*---
description: Date.prototype.getTimezoneOffset
features: [Date]
---*/

test("returns a finite number for a valid Date", () => {
  expect(Number.isFinite(new Date(0).getTimezoneOffset())).toBe(true);
});

test("returns NaN for an invalid Date", () => {
  expect(new Date(NaN).getTimezoneOffset()).toBeNaN();
});
