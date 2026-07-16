/*---
description: Date.prototype.getUTCDay
features: [Date]
---*/

test("returns the UTC weekday", () => {
  expect(new Date(1718451045123).getUTCDay()).toBe(6);
});
