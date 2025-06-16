/*---
description: Logical AND (&&) operator works correctly
features: [logical-and-operator]
---*/

test("logical AND (&&)", () => {
  expect(true && true).toBeTruthy();
  expect(true && false).toBeFalsy();
  expect(false && true).toBeFalsy();
  expect(false && false).toBeFalsy();
});

runTests();
