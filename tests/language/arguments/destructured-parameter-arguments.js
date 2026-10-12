/*---
description: The arguments object of a function with destructured parameters is available to their defaults, and a default after a pattern reads the destructured value
features: [compat-function, compat-arguments-object, destructuring, default-parameters, temporal-dead-zone]
---*/

describe("arguments object with destructured parameters", () => {
  test("a pattern default reads arguments", () => {
    function length({ a = arguments.length }, b) {
      return a;
    }
    function second({ a = arguments[1] }, b) {
      return a;
    }
    expect(length({}, 2, 3)).toBe(3);
    expect(second({}, 2)).toBe(2);
  });

  test("a default after a pattern reads the destructured value", () => {
    function f({ a }, b = a) {
      return [b, arguments.length, arguments[0].a];
    }
    expect(f({ a: 1 })).toEqual([1, 1, 1]);
  });

  test("a pattern default reading a later parameter throws ReferenceError", () => {
    function f({ a = b }, b) {
      return arguments.length;
    }
    expect(() => f({}, 1)).toThrow(ReferenceError);
  });

  test("the arguments object is unmapped", () => {
    function f({ a }, b) {
      arguments[1] = 5;
      return b;
    }
    expect(f({ a: 1 }, 2)).toBe(2);
  });
});
