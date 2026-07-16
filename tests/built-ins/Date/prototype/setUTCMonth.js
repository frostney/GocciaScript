/*---
description: Date.prototype.setUTCMonth
features: [Date]
---*/

test("preserves the UTC date when omitted", () => {
  const value = new Date(Date.UTC(2024, 0, 2, 3, 4, 5, 6));
  expect(value.setUTCMonth(6)).toBe(value.getTime());
  expect(value.getUTCMonth()).toBe(6);
  expect(value.getUTCDate()).toBe(2);
});

test("explicit undefined invalidates the Date", () => {
  const value = new Date(Date.UTC(2024, 0, 2, 3, 4, 5, 6));
  expect(Number.isNaN(value.setUTCMonth(6, undefined))).toBe(true);
});
