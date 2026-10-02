/*---
description: A finally block run by return or break still sees a var declared in the try block, including the head of a for...of loop, but not the try block's lexical bindings
features: [compat-var, try-finally, class-static-block, for-of]
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

describe("var head of a for...of loop in a try block left by an abrupt exit", () => {
  test("is visible in the finally block after a return out of the loop", () => {
    const run = () => {
      const seen = [];
      try {
        for (var key of ["a", "b"]) {
          if (key === "b") return seen;
        }
      } finally {
        seen.push(typeof key, key);
      }
    };

    expect(run()).toEqual(["string", "b"]);
  });

  test("is visible when the head is a destructuring pattern", () => {
    const run = () => {
      const seen = [];
      try {
        for (var [first, second] of [[1, 2]]) {
          return seen;
        }
      } finally {
        seen.push(first, second);
      }
    };

    expect(run()).toEqual([1, 2]);
  });

  test("is visible after a break out of an outer loop", () => {
    const seen = [];
    for (var outer of [1, 2]) {
      try {
        for (var inner of [10, 20]) {
          if (inner === 10) break;
        }
        break;
      } finally {
        seen.push(outer, inner);
      }
    }

    expect(seen).toEqual([1, 10]);
  });

  test("is the function's own var, not an outer var or const of the same name", () => {
    var key = "outer var";
    const label = "outer const";
    const seen = [];
    const runVar = () => {
      try {
        for (var key of ["loop"]) {
          return 1;
        }
      } finally {
        seen.push(key);
      }
    };
    const runConst = () => {
      try {
        for (var label of ["loop"]) {
          return 1;
        }
      } finally {
        seen.push(label);
      }
    };
    runVar();
    runConst();

    expect(seen).toEqual(["loop", "loop"]);
    expect(key).toBe("outer var");
    expect(label).toBe("outer const");
  });

  test("shares the binding of a var declared earlier in the function", () => {
    const run = () => {
      const seen = [];
      var key = "declared";
      try {
        for (var key of ["a", "b"]) {
          if (key === "b") return seen;
        }
      } finally {
        seen.push(key);
      }
    };

    expect(run()).toEqual(["b"]);
  });
});
