/*---
description: A var in a function declaration or expression starts as undefined, whatever the call passes
features: [compat-function, compat-var]
---*/

// ES2026 §10.2.11 FunctionDeclarationInstantiation step 29.c.i.3: every var
// the body declares starts as undefined; a body function declaration's name
// holds its function object (step 37).

describe("var initial value in function declarations and expressions", () => {
  test("function declaration with surplus arguments", () => {
    function fn(a) {
      var x, y;
      return [x, y];
    }
    expect(fn(1, 2, 3)).toEqual([undefined, undefined]);
  });

  test("function expression with surplus arguments", () => {
    const fn = function (p) {
      var t;
      return t;
    };
    expect(fn(2, 7)).toBeUndefined();
  });

  test("generator function declaration", () => {
    function* gen(a) {
      var x;
      yield x;
    }
    expect(gen(1, 2).next().value).toBeUndefined();
  });

  test("async function declaration", () => {
    async function run(a) {
      var x;
      return x;
    }
    return run(1, 2).then((value) => {
      expect(value).toBeUndefined();
    });
  });

  test("destructured parameters", () => {
    function fn({ a }, [b]) {
      var x;
      return [a, b, x];
    }
    expect(fn({ a: 1 }, [2])).toEqual([1, 2, undefined]);
  });

  test("a body function declaration holds its function", () => {
    function outer(a) {
      var before = typeof inner;
      var inner;
      function inner() {
        return "inner";
      }
      return [before, inner()];
    }
    expect(outer(1, 2, 3)).toEqual(["function", "inner"]);
  });

  test("a var assigned over a body function declaration", () => {
    function outer(a) {
      var h = 1;
      function h() {}
      return typeof h;
    }
    expect(outer(1, 2, 3)).toBe("number");
  });
});
