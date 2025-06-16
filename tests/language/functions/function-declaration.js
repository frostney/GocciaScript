/*---
description: Basic function declaration and invocation works correctly
features: [function-declaration]
---*/

test("basic function declaration", () => {
  function add(a, b) {
    return a + b;
  }
  expect(add(2, 3)).toBe(5);
  expect(typeof add).toBe("function");
});

runTests();
