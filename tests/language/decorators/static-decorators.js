/*---
description: Decorators work on static class elements
features: [decorators]
---*/

describe("static element decorators", () => {
  test("static method decorator", () => {
    let receivedContext;

    const log = (method, context) => {
      receivedContext = context;
      return method;
    };

    class C {
      @log
      static greet() {
        return "hello";
      }
    }

    expect(receivedContext.kind).toBe("method");
    expect(receivedContext.name).toBe("greet");
    expect(receivedContext.static).toBe(true);
    expect(C.greet()).toBe("hello");
  });

  test("static field decorator", () => {
    let receivedContext;

    const log = (value, context) => {
      receivedContext = context;
    };

    class C {
      @log
      static x = 42;
    }

    expect(receivedContext.kind).toBe("field");
    expect(receivedContext.name).toBe("x");
    expect(receivedContext.static).toBe(true);
  });
});
