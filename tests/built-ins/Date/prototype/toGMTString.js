/*---
description: Date.prototype.toGMTString
features: [Date]
---*/

test("is the legacy alias of toUTCString", () => {
  const value = new Date(0);
  expect(value.toGMTString()).toBe(value.toUTCString());
});

test("returns Invalid Date for an invalid time value", () => {
  expect(new Date(NaN).toGMTString()).toBe("Invalid Date");
});
