/*---
description: Bitwise right shift operator works correctly
features: [bitwise-rightshift]
---*/

test("bitwise right shift operator", () => {
  expect(5 >> 3).toBe(0);
  expect(5 >> 2).toBe(1);
});

test("arithmetic right shift preserves sign bit", () => {
  expect(-8 >> 1).toBe(-4);
  expect(-1 >> 1).toBe(-1);
  expect(-16 >> 2).toBe(-4);
  expect(-100 >> 3).toBe(-13);
});

test("right shift with zero shift amount", () => {
  expect(-8 >> 0).toBe(-8);
  expect(8 >> 0).toBe(8);
});

test("right shift uses only low 5 bits of shift amount", () => {
  expect(-8 >> 32).toBe(-8);
  expect(-8 >> 33).toBe(-4);
});
