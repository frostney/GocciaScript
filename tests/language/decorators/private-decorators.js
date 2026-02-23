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
});
