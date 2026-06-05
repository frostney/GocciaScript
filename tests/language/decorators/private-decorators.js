/*---
description: Decorators work on private class elements
features: [decorators]
---*/

describe("private element decorators", () => {
  test("private method decorator reports private name", () => {
    let receivedContext;

    const log = (method, context) => {
      receivedContext = context;
      return method;
    };

    class C {
      @log
      #greet() {
        return "hello";
      }

      callGreet() {
        return this.#greet();
      }
    }

    const c = new C();
    expect(c.callGreet()).toBe("hello");
    expect(receivedContext.kind).toBe("method");
    expect(receivedContext.name).toBe("#greet");
    expect(receivedContext.private).toBe(true);
  });

  test("static private method decorator receives and replaces method", () => {
    let receivedContext;
    let receivedValueType = "unset";

    const wrap = (method, context) => {
      receivedContext = context;
      receivedValueType = typeof method;
      return () => "wrapped:" + method();
    };

    class C {
      @wrap
      static #greet() {
        return "hello";
      }

      static callGreet() {
        return this.#greet();
      }
    }

    expect(C.callGreet()).toBe("wrapped:hello");
    expect(receivedValueType).toBe("function");
    expect(receivedContext.kind).toBe("method");
    expect(receivedContext.name).toBe("#greet");
    expect(receivedContext.static).toBe(true);
    expect(receivedContext.private).toBe(true);
  });
});
