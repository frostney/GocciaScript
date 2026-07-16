/*---
description: Date.prototype.valueOf
features: [Date]
---*/

test("returns the stored epoch milliseconds", () => {
  expect(new Date(1718451045123).valueOf()).toBe(1718451045123);
  expect(Number.isNaN(new Date(NaN).valueOf())).toBe(true);
});
