/*---
description: Date.prototype.getUTCMinutes
features: [Date]
---*/

test("returns the UTC minute", () => {
  expect(new Date(1718451045123).getUTCMinutes()).toBe(30);
});
