/*---
description: Basic let declaration without initializer
features: [let-declaration]
---*/

test("let declaration without initializer", () => {
  let a;
  expect(a).toBeUndefined();
});

test("let declarations can be re-assigned", () => {
  let a = 1;
  expect(a).toBe(1);
  a = 2;
  expect(a).toBe(2);
});
