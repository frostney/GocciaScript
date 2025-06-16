/*---
description: Relational operators work correctly
features: [relational-operators]
---*/

test("relational operators", () => {
  expect(5 > 3).toBeTruthy();
  expect(5 < 3).toBeFalsy();
  expect(5 >= 5).toBeTruthy();
  expect(5 <= 5).toBeTruthy();
  expect(3 >= 5).toBeFalsy();
  expect(7 <= 5).toBeFalsy();
});

runTests();
