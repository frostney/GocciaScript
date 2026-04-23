/*---
description: Function declarations with rest parameters
features: [compat-function]
---*/

test("rest parameter collects remaining arguments", () => {
  function sum(...numbers) {
    return numbers.reduce((a, b) => a + b, 0);
  }
  expect(sum(1, 2, 3)).toBe(6);
});

test("rest parameter after named parameters", () => {
  function tag(first, ...rest) {
    return first + ": " + rest.join(", ");
  }
  expect(tag("items", "a", "b", "c")).toBe("items: a, b, c");
});
