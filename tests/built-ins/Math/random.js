/*---
description: Math.random function works correctly
features: [Math.random]
---*/

test("Math.random", () => {
  const random1 = Math.random();
  const random2 = Math.random();

  expect(random1).toBeGreaterThanOrEqual(0);
  expect(random1).toBeLessThan(1);
  expect(random2).toBeGreaterThanOrEqual(0);
  expect(random2).toBeLessThan(1);
  expect(random1).not.toBe(random2); // Very unlikely to be equal
});

runTests();
