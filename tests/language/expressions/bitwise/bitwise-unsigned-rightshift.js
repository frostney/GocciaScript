/*---
description: Bitwise unsigned right shift operator works correctly
features: [bitwise-unsigned-rightshift]
---*/

test("bitwise unsigned right shift operator", () => {
  expect(-1 >>> 0).toBe(4294967295);
});
