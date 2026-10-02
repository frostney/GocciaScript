/*---
description: A finally block run by return or break still sees a var declared in the try block, but not the try block's lexical bindings
features: [compat-var, try-finally, class-static-block]
---*/

describe("var declared in a try block left by an abrupt exit", () => {
  test("is visible in the finally block of a function", () => {
    const label = "outer";
    const run = () => {
      const seen = [];
      try {
        var declaredInTry = "var";
        const label = "inner";
        seen.push(label);
        return seen;
      } finally {
        seen.push(declaredInTry, label);
      }
    };

    expect(run()).toEqual(["inner", "var", "outer"]);
  });

  test("is visible in the finally block of a class static block", () => {
    const label = "outer";
    const seen = [];
    class Holder {
      static {
        for (const item of [1]) {
          try {
            var declaredInTry = "var";
            const label = "inner";
            seen.push(label);
            break;
          } finally {
            seen.push(declaredInTry, label);
          }
        }
      }
    }

    expect(seen).toEqual(["inner", "var", "outer"]);
  });
});
