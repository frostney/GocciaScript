/*---
description: Addition operation with zero operand
features: [addition-operator]
---*/

test("addition with zero", () => {
  expect(5 + 0).toBe(5);
  expect(0 + 7).toBe(7);
  expect(0 + 0).toBe(0);
});
