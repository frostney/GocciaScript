/*---
description: Bitwise XOR operator works correctly
features: [bitwise-xor]
---*/

test("bitwise XOR operator", () => {
  expect(5 ^ 3).toBe(6);
  expect(5 ^ 2).toBe(7);
});
