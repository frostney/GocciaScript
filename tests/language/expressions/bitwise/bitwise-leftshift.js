/*---
description: Bitwise left shift operator works correctly
features: [bitwise-leftshift]
---*/

test("bitwise left shift operator", () => {
  expect(5 << 3).toBe(40);
  expect(5 << 2).toBe(20);
});
