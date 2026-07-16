/*---
description: Date.prototype.getUTCDate
features: [Date]
---*/

test("returns the UTC day of the month", () => {
  expect(new Date(1718451045123).getUTCDate()).toBe(15);
});
