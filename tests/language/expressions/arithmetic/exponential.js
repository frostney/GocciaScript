/*---
description: Exponential operator works correctly
features: [exponential-operator]
---*/

test("exponential operator", () => {
  expect(2 ** 3).toBe(8);
  expect(2 ** 0).toBe(1);
  expect(2 ** 1).toBe(2);
});
