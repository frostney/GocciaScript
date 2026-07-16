/*---
description: Date.prototype.setSeconds
features: [Date]
---*/

test("preserves milliseconds when the argument is omitted", () => {
  const value = new Date(Date.UTC(2024, 0, 2, 3, 4, 5, 6));
  const milliseconds = value.getMilliseconds();
  expect(value.setSeconds(10)).toBe(value.getTime());
  expect(value.getSeconds()).toBe(10);
  expect(value.getMilliseconds()).toBe(milliseconds);
});

test("explicit undefined invalidates the Date", () => {
  const value = new Date(Date.UTC(2024, 0, 2, 3, 4, 5, 6));
  expect(Number.isNaN(value.setSeconds(10, undefined))).toBe(true);
});
