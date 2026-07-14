/*---
description: Bitwise OR operator works correctly
features: [bitwise-or]
---*/

test("bitwise OR operator", () => {
  let maximumInt32 = 2147483647;
  let minimumInt32 = -2147483648;

  expect(5 | 3).toBe(7);
  expect(5 | 2).toBe(7);
  expect(5 | 0).toBe(5);
  expect(0 | 0).toBe(0);
  expect(maximumInt32 | minimumInt32).toBe(-1);
});
