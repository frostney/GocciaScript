/*---
description: Bitwise left shift operator works correctly
features: [bitwise-leftshift]
---*/

test("bitwise left shift operator", () => {
  let one = 1;
  let thirtyOne = 31;
  let minimumInt32 = -2147483648;
  let negativeOne = -1;

  expect(5 << 3).toBe(40);
  expect(5 << 2).toBe(20);
  expect(1 << 3).toBe(8);
  expect(one << thirtyOne).toBe(-2147483648);
  expect(minimumInt32 << one).toBe(0);
  expect(one << negativeOne).toBe(-2147483648);
});
