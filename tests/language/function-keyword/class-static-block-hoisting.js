/*---
description: Function declarations and vars in a class static block are instantiated like a function body's (#1329)
features: [class-static-block, compat-function, compat-var]
---*/

describe("declarations in a class static block", () => {
  test("a top-level function declaration is callable before it", () => {
    let result;
    class C {
      static {
        result = early();
        function early() {
          return "hoisted";
        }
      }
    }
    expect(result).toBe("hoisted");
  });

  test("a var with a function's name keeps the function", () => {
    let type;
    class C {
      static {
        function shared() {}
        var shared;
        type = typeof shared;
      }
    }
    expect(type).toBe("function");
  });

  test("a function declaration does not leak out of the static block", () => {
    class C {
      static {
        function staticHelper() {}
      }
    }
    expect(typeof staticHelper).toBe("undefined");
  });
});
