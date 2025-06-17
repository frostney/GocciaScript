/*---
description: Less than operator (<)
features: [less-than]
---*/

test("less than operator (<)", () => {
  expect(5 < 5).toBeFalsy();
  expect(5 < 4).toBeFalsy();
  expect(5 < 6).toBeTruthy();
});
