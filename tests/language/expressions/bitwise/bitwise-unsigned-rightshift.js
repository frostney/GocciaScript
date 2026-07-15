/*---
description: Bitwise unsigned right shift operator works correctly
features: [bitwise-unsigned-rightshift]
---*/

test("bitwise unsigned right shift operator", () => {
  let negativeOne = -1;
  let minimumInt32 = -2147483648;
  let zero = 0;
  let one = 1;
  let thirtyOne = 31;

  expect(negativeOne >>> zero).toBe(4294967295);
  expect(negativeOne >>> one).toBe(2147483647);
  expect(minimumInt32 >>> thirtyOne).toBe(1);
});
