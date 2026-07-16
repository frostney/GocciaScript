/*---
description: Date.prototype.setUTCMilliseconds
features: [Date]
---*/

test("updates UTC milliseconds and returns the new epoch milliseconds", () => {
  const value = new Date(1718451045123);
  expect(value.setUTCMilliseconds(456)).toBe(value.getTime());
  expect(value.getUTCMilliseconds()).toBe(456);
});
