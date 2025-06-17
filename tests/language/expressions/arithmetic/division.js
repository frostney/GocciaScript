/*---
description: Division operator works correctly
features: [division-operator]
---*/

test("division operator", () => {
  expect(15 / 3).toBe(5);
  expect(7 / 2).toBe(3.5);
  expect(-10 / 2).toBe(-5);
  expect(0 / 5).toBe(0);
});
