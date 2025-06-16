/*---
description: Subtraction operator works correctly
features: [subtraction-operator]
---*/

test("subtraction operator", () => {
  expect(10 - 3).toBe(7);
  expect(5.5 - 2.5).toBe(3);
  expect(0 - 5).toBe(-5);
  expect(10 - 10).toBe(0);
});

runTests();
