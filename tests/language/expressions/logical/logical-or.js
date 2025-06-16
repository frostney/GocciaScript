/*---
description: Logical OR (||) operator works correctly
features: [logical-or-operator]
---*/

test("logical OR (||)", () => {
  expect(true || true).toBeTruthy();
  expect(true || false).toBeTruthy();
  expect(false || true).toBeTruthy();
  expect(false || false).toBeFalsy();
});

runTests();
