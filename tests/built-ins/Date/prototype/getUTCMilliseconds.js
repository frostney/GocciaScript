/*---
description: Date.prototype.getUTCMilliseconds
features: [Date]
---*/

test("returns the UTC millisecond", () => {
  expect(new Date(1718451045123).getUTCMilliseconds()).toBe(123);
});
