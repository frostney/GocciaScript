/*---
description: Addition operation with negative numbers
features: [addition-operator, negative-numbers]
---*/

test("addition with negative numbers", () => {
  expect(-5 + 3).toBe(-2);
  expect(5 + -3).toBe(2);
  expect(-5 + -3).toBe(-8);
});

runTests();
