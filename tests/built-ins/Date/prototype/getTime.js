/*---
description: Date.prototype.getTime
features: [Date]
---*/

test("returns the stored epoch milliseconds", () => {
  expect(new Date(1718451045123).getTime()).toBe(1718451045123);
  expect(Number.isNaN(new Date(NaN).getTime())).toBe(true);
});
