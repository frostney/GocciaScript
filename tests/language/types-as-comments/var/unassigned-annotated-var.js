/*---
description: An annotated var holds undefined until it is assigned, and operators on it give the generic result
features: [types-as-comments, strict-type-enforcement, compat-var]
---*/

// ES2026 §14.3.2.1: a var without an initializer keeps the undefined its
// instantiation bound (§10.2.11), and undefined + 1 is NaN (§13.15.3).

describe("annotated var declared without an initializer", () => {
  test("adding a small integer gives NaN", () => {
    const run = () => {
      var value: number;
      return value + 1;
    };

    expect(run()).toBeNaN();
  });

  test("subtracting a small integer gives NaN", () => {
    const run = () => {
      var value: number;
      return value - 1;
    };

    expect(run()).toBeNaN();
  });

  test("an initialized var read before its declaration runs gives NaN", () => {
    const run = (start) => {
      switch (start) {
        case 0:
          var value: number = 1;
        case 1:
          return value + 1;
      }
    };

    expect(run(1)).toBeNaN();
  });

  test("assigning a value of another type still throws", () => {
    const run = () => {
      var value: number;
      value = "text";
    };

    expect(run).toThrow(TypeError);
  });
});
