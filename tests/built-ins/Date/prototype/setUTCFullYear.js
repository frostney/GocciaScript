/*---
description: Date.prototype.setUTCFullYear
features: [Date]
---*/

test("updates supplied UTC fields and returns epoch milliseconds", () => {
  const value = new Date(Date.UTC(2024, 5, 15));
  expect(value.setUTCFullYear(2025, 0, 2)).toBe(value.getTime());
  expect(value.getUTCFullYear()).toBe(2025);
  expect(value.getUTCMonth()).toBe(0);
  expect(value.getUTCDate()).toBe(2);
});

test("distinguishes omitted month from explicit undefined", () => {
  const base = Date.UTC(2024, 0, 2, 3, 4, 5, 6);
  const omitted = new Date(base);
  expect(omitted.setUTCFullYear(2025)).toBe(omitted.getTime());
  expect(omitted.getUTCMonth()).toBe(0);

  const explicit = new Date(base);
  expect(Number.isNaN(explicit.setUTCFullYear(2025, undefined))).toBe(true);
});
