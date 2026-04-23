/*---
description: Function declarations with default parameters
features: [compat-function]
---*/

test("default parameter value", () => {
  function greet(name = "world") {
    return "Hello, " + name;
  }
  expect(greet()).toBe("Hello, world");
  expect(greet("Alice")).toBe("Hello, Alice");
});

test("multiple default parameters", () => {
  function create(x = 0, y = 0, z = 0) {
    return { x, y, z };
  }
  expect(create()).toEqual({ x: 0, y: 0, z: 0 });
  expect(create(1, 2)).toEqual({ x: 1, y: 2, z: 0 });
});
