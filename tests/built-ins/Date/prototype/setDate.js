/*---
description: Date.prototype.setDate
features: [Date]
---*/

test("updates the local day and returns the new epoch milliseconds", () => {
  const value = new Date(2024, 0, 1);
  const result = value.setDate(value.getDate() + 1);
  expect(result).toBe(value.getTime());
  expect(value.getDate()).toBe(2);
});
