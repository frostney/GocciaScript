/*---
description: Let declaration without initializer results in undefined
features: [let-declaration]
---*/

test("let declaration without initializer", () => {
  let a;
  expect(a).toBeUndefined();
});

runTests();
