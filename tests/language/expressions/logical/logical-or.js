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

test("logical OR (||) with short-circuit evaluation", () => {
  let a = 0;
  expect(false || (a = 1)).toBeTruthy();
  expect(a).toBe(1);
});
