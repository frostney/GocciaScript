/*---
description: Function parameters work correctly with various scenarios
features: [function-parameters, default-parameters]
---*/

test("function with no parameters", () => {
  const getConstant = () => {
    return 100;
  };
  expect(getConstant()).toBe(100);
});

test("function with single parameter", () => {
  const double = (x) => {
    return x * 2;
  };
  expect(double(7)).toBe(14);
});

test("function with multiple parameters", () => {
  const multiply = (a, b, c) => {
    return a * b * c;
  };
  expect(multiply(2, 3, 4)).toBe(24);
});

test("excess parameters are ignored", () => {
  const add = (a, b) => {
    return a + b;
  };
  expect(add(1, 2, 3, 4, 5)).toBe(3);
});

test("missing parameters are undefined", () => {
  const checkParams = (a, b, c) => {
    return [a, b, c];
  };
  const result = checkParams(1, 2);
  expect(result[0]).toBe(1);
  expect(result[1]).toBe(2);
  expect(result[2]).toBeUndefined();
});

test("default parameter values", () => {
  const greet = (name = "World") => {
    return "Hello, " + name + "!";
  };
  expect(greet()).toBe("Hello, World!");
  expect(greet("Alice")).toBe("Hello, Alice!");
});
