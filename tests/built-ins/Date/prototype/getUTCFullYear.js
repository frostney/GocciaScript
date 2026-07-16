/*---
description: Date.prototype.getUTCFullYear
features: [Date]
---*/

test("returns the UTC year", () => {
  expect(new Date(1718451045123).getUTCFullYear()).toBe(2024);
  expect(Number.isNaN(new Date(NaN).getUTCFullYear())).toBe(true);
});
