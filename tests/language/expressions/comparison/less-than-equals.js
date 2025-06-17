/*---
description: Less than or equal to operator (<=)
features: [less-than-equals]
---*/

test("less than or equal to operator (<=)", () => {
  expect(5 <= 5).toBeTruthy();
  expect(5 <= 4).toBeFalsy();
  expect(5 <= 6).toBeTruthy();
});
