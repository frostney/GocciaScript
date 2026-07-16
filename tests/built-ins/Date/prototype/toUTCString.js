/*---
description: Date.prototype.toUTCString
features: [Date]
---*/

test("formats the epoch in UTC", () => {
  expect(new Date(0).toUTCString()).toBe("Thu, 01 Jan 1970 00:00:00 GMT");
});

test("returns Invalid Date for an invalid time value", () => {
  expect(new Date(NaN).toUTCString()).toBe("Invalid Date");
});
