/*---
description: Bitwise NOT operator works correctly
features: [bitwise-not]
---*/

test("bitwise NOT operator", () => {
  expect(~0).toBe(-1);
  expect(~-1).toBe(0);
});
