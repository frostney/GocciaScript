/*---
description: IIFE
features: [iife]
---*/

test("IIFE", () => {
  const result = (() => {
    return 42;
  })();

  expect(result).toBe(42);
});
