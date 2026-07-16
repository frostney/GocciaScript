/*---
description: Date.prototype.getUTCHours
features: [Date]
---*/

test("returns the UTC hour", () => {
  expect(new Date(1718451045123).getUTCHours()).toBe(11);
});
