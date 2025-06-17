/*---
description: Greater than or equal to operator (>=)
features: [greater-than-equals]
---*/

test("greater than or equal to operator (>=)", () => {
  expect(5 >= 5).toBeTruthy();
  expect(5 >= 4).toBeTruthy();
  expect(5 >= 6).toBeFalsy();

  expect(NaN >= NaN).toBeFalsy();
  expect(NaN >= 1).toBeFalsy();
  expect(1 >= NaN).toBeFalsy();

  expect(Infinity >= 1).toBeTruthy();
  expect(1 >= Infinity).toBeFalsy();
  expect(-Infinity >= 1).toBeFalsy();
  expect(1 >= -Infinity).toBeTruthy();

  expect(null >= null).toBeTruthy();
  expect(null >= undefined).toBeFalsy();
  expect(undefined >= null).toBeFalsy();

  expect(undefined >= undefined).toBeFalsy();
  expect(undefined >= null).toBeFalsy();
  expect(null >= undefined).toBeFalsy();
});
