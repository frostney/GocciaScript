/*---
description: Method decorators can wrap and replace class methods
features: [decorators]
---*/

describe("method decorators", () => {
  test("decorator receives method and context", () => {
    let receivedContext;

    const log = (method, context) => {
      receivedContext = context;
      return method;
    };

    class C {
      @log
      greet() {
        return "hello";
      }
    }

    const c = new C();
    expect(c.greet()).toBe("hello");
    expect(receivedContext.kind).toBe("method");
    expect(receivedContext.name).toBe("greet");
    expect(receivedContext.static).toBe(false);
    expect(receivedContext.private).toBe(false);
  });

  test("decorator can replace method", () => {
    const double = (method, context) => {
      return (/* ...args */) => {
        const result = method.call(this);
        return result * 2;
      };
    };

    class Calculator {
      @double
      value() {
        return 21;
      }
    }

    const calc = new Calculator();
    expect(calc.value()).toBe(42);
  });

  test("decorator returning undefined keeps original", () => {
    const noop = (method, context) => {
      return undefined;
    };

    class C {
      @noop
      greet() {
        return "hello";
      }
    }

    const c = new C();
    expect(c.greet()).toBe("hello");
  });

  test("multiple decorators applied bottom-up", () => {
    const calls = [];

    const first = (method, context) => {
      calls.push("first");
      return method;
    };

    const second = (method, context) => {
      calls.push("second");
      return method;
    };

    class C {
      @first
      @second
      greet() {
        return "hello";
      }
    }

    expect(calls[0]).toBe("second");
    expect(calls[1]).toBe("first");
  });
});
