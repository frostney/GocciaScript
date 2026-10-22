/*---
description: A var in a class static block is scoped to the block's own var environment (#1329)
features: [class-static-block, compat-var]
---*/

const outerLexical = "outer";
var outerVar = "outer";

describe("var in a class static block", () => {
  test("is hoisted to the top of the static block", () => {
    let before;
    let after;
    class C {
      static {
        before = hoisted;
        var hoisted = 1;
        after = hoisted;
      }
    }
    expect(before).toBeUndefined();
    expect(after).toBe(1);
  });

  test("shadows an outer var from the start of the static block", () => {
    let seen;
    class C {
      static {
        seen = outerVar;
        var outerVar = "inner";
      }
    }
    expect(seen).toBeUndefined();
    expect(outerVar).toBe("outer");
  });

  test("is not visible after the class", () => {
    class C {
      static {
        var staticOnly = 1;
      }
    }
    expect(typeof staticOnly).toBe("undefined");
  });

  test("is not shared between two static blocks", () => {
    let second;
    class C {
      static {
        var perBlock = 1;
      }
      static {
        second = typeof perBlock;
      }
    }
    expect(second).toBe("undefined");
  });

  test("a closure created before the declaration sees its later value", () => {
    let read;
    class C {
      static {
        const early = () => late;
        var late = 5;
        read = early();
      }
    }
    expect(read).toBe(5);
  });

  test("a nested block's const ends with that block when a var follows it", () => {
    let types;
    class C {
      static {
        {
          const inner = 1;
          var late = 2;
        }
        types = [typeof inner, typeof late];
      }
    }
    expect(types).toEqual(["undefined", "number"]);
  });

  test("a nested block's const does not hide an outer binding after the block", () => {
    let seen;
    class C {
      static {
        {
          const outerLexical = "inner";
          var afterLexical = 1;
        }
        seen = [outerLexical, afterLexical];
      }
    }
    expect(seen).toEqual(["outer", 1]);
  });

  test("a case block's const ends with the block when a var follows it", () => {
    let seen;
    class C {
      static {
        switch (1) {
          case 1: {
            const caseLocal = 1;
            var fromCase = caseLocal;
          }
        }
        seen = [typeof caseLocal, fromCase];
      }
    }
    expect(seen).toEqual(["undefined", 1]);
  });

  test("a for-of var head stays bound after the loop", () => {
    let seen;
    class C {
      static {
        for (var item of [1, 2]) {}
        const laterName = 3;
        seen = [item, typeof laterName];
      }
    }
    expect(seen).toEqual([2, "number"]);
  });

  test("a for-of var head is undefined before the loop", () => {
    let before;
    class C {
      static {
        before = item;
        for (var item of [1]) {}
      }
    }
    expect(before).toBeUndefined();
  });

  test("a var declared in a try block keeps its value after the try", () => {
    const seen = [];
    class C {
      static {
        try {
          var completed = 101;
        } catch (error) {}
        seen.push(completed);
        try {
          var thrown = 102;
          throw new Error("stop");
        } catch (error) {}
        seen.push(thrown);
      }
    }
    expect(seen).toEqual([101, 102]);
  });
});
