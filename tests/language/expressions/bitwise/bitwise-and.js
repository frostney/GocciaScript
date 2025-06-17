/*---
description: Bitwise AND operator works correctly
features: [bitwise-and]
---*/

test("bitwise AND operator", () => {
  expect(5 & 3).toBe(1);
  expect(5 & 2).toBe(0);
});
