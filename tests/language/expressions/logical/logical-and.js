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

test("logical AND (&&) with short-circuit evaluation", () => {
  let a = 0;
  expect(true && (a = 1)).toBeTruthy();
  expect(a).toBe(1);
});
