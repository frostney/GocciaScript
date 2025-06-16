/*---
description: Multiplication operator works correctly
features: [multiplication-operator]
---*/

test("multiplication operator", () => {
  expect(4 * 5).toBe(20);
  expect(2.5 * 4).toBe(10);
  expect(-3 * 7).toBe(-21);
  expect(0 * 100).toBe(0);
});

runTests();
