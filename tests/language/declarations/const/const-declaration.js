/*---
description: Const declaration with initializer
features: [const-declaration]
---*/

test("const declaration with initializer", () => {
  const b = 100;
  expect(b).toBe(100);
});

test("const declarations cannot be re-assigned", () => {
  const a = 1;
  expect(() => {
    a = 2;
  }).toThrow(TypeError);
});
