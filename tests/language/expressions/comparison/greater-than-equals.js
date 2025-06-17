/*---
description: Greater than or equal to operator (>=)
features: [greater-than-equals]
---*/

test("greater than or equal to operator (>=)", () => {
  expect(5 >= 5).toBeTruthy();
  expect(5 >= 4).toBeTruthy();
  expect(5 >= 6).toBeFalsy();
});
