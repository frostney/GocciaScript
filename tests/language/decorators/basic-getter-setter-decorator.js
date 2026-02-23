/*---
description: Getter and setter decorators can wrap accessor functions
features: [decorators]
---*/

describe("getter and setter decorators", () => {
  test("getter decorator receives function and context", () => {
    let receivedContext;

    const log = (fn, context) => {
      receivedContext = context;
      return fn;
    };

    class C {
      #value = 42;

      @log
      get value() {
        return this.#value;
      }
    }

    expect(receivedContext.kind).toBe("getter");
    expect(receivedContext.name).toBe("value");
  });

  test("setter decorator receives function and context", () => {
    let receivedContext;

    const log = (fn, context) => {
      receivedContext = context;
      return fn;
    };

    class C {
      #value = 0;

      @log
      set value(v) {
        this.#value = v;
      }
    }

    expect(receivedContext.kind).toBe("setter");
    expect(receivedContext.name).toBe("value");
  });
});
