/*---
description: Strict equality operator (===) works correctly
features: [strict-equality-operator]
---*/

test("strict equality operator (===)", () => {
  expect(5 === 5).toBeTruthy();
  expect(5 === "5").toBeFalsy();
  expect(true === 1).toBeFalsy();
  expect(null === undefined).toBeFalsy();
});
