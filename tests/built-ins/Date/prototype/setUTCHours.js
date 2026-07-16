/*---
description: Date.prototype.setUTCHours
features: [Date]
---*/

test("preserves trailing UTC fields when omitted", () => {
  const value = new Date(Date.UTC(2024, 0, 2, 3, 4, 5, 6));
  expect(value.setUTCHours(9)).toBe(value.getTime());
  expect(value.getUTCHours()).toBe(9);
  expect(value.getUTCMinutes()).toBe(4);
  expect(value.getUTCSeconds()).toBe(5);
  expect(value.getUTCMilliseconds()).toBe(6);
});

test("explicit undefined invalidates the Date", () => {
  const value = new Date(Date.UTC(2024, 0, 2, 3, 4, 5, 6));
  expect(Number.isNaN(value.setUTCHours(9, undefined))).toBe(true);
});
