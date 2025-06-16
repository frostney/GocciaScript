/*---
description: Math rounding methods (round, floor, ceil) work correctly
features: [Math.round, Math.floor, Math.ceil]
---*/

test("Math.round, Math.floor, Math.ceil", () => {
  expect(Math.round(4.3)).toBe(4);
  expect(Math.round(4.7)).toBe(5);
  expect(Math.round(-4.3)).toBe(-4);
  expect(Math.round(-4.7)).toBe(-5);

  expect(Math.floor(4.3)).toBe(4);
  expect(Math.floor(4.7)).toBe(4);
  expect(Math.floor(-4.3)).toBe(-5);
  expect(Math.floor(-4.7)).toBe(-5);

  expect(Math.ceil(4.3)).toBe(5);
  expect(Math.ceil(4.7)).toBe(5);
  expect(Math.ceil(-4.3)).toBe(-4);
  expect(Math.ceil(-4.7)).toBe(-4);
});

runTests();
