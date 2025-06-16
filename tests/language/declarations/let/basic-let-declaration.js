/*---
description: Basic let declaration without initializer
features: [let-declaration]
---*/

test("let declaration without initializer", () => {
  let a;
  expect(a).toBeUndefined();
});

runTests();
