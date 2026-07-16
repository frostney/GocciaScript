/*---
description: Date.prototype.setUTCSeconds
features: [Date]
---*/

test("preserves UTC milliseconds when omitted", () => {
  const value = new Date(Date.UTC(2024, 0, 2, 3, 4, 5, 6));
  expect(value.setUTCSeconds(10)).toBe(value.getTime());
  expect(value.getUTCSeconds()).toBe(10);
  expect(value.getUTCMilliseconds()).toBe(6);
});

test("explicit undefined invalidates the Date", () => {
  const value = new Date(Date.UTC(2024, 0, 2, 3, 4, 5, 6));
  expect(Number.isNaN(value.setUTCSeconds(10, undefined))).toBe(true);
});
