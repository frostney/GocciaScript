/*---
description: Compound bitwise operators work correctly
features: [compound-bitwise-operators]
---*/

test("compound bitwise AND", () => {
  let a = 5;
  a &= 3;
  expect(a).toBe(1);
});

test("compound bitwise OR", () => {
  let a = 5;
  a |= 3;
  expect(a).toBe(7);
});

test("compound bitwise XOR", () => {
  let a = 5;
  a ^= 3;
});

test("compound bitwise left shift", () => {
  let a = 5;
  a <<= 3;
});

test("compound bitwise right shift", () => {
  let a = 5;
  a >>= 3;
});
