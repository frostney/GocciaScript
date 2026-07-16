/*---
description: Date.prototype.setUTCMinutes
features: [Date]
---*/

test("preserves trailing UTC fields when omitted", () => {
  const value = new Date(Date.UTC(2024, 0, 2, 3, 4, 5, 6));
  expect(value.setUTCMinutes(20)).toBe(value.getTime());
  expect(value.getUTCMinutes()).toBe(20);
  expect(value.getUTCSeconds()).toBe(5);
  expect(value.getUTCMilliseconds()).toBe(6);
});

test("explicit undefined invalidates the Date", () => {
  const value = new Date(Date.UTC(2024, 0, 2, 3, 4, 5, 6));
  expect(Number.isNaN(value.setUTCMinutes(20, undefined))).toBe(true);
});
