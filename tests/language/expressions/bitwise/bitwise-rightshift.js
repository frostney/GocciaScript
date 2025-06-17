/*---
description: Bitwise right shift operator works correctly
features: [bitwise-rightshift]
---*/

test("bitwise right shift operator", () => {
  expect(5 >> 3).toBe(0);
  expect(5 >> 2).toBe(1);
});
