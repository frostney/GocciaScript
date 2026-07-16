/*---
description: Date.prototype.getUTCMonth
features: [Date]
---*/

test("returns the zero-based UTC month", () => {
  expect(new Date(1718451045123).getUTCMonth()).toBe(5);
});
