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

  test("decorator can replace method with arrow wrapper", () => {
    const double = (method, context) => {
      return (...args) => {
        return method(...args) * 2;
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

  test("wrapper preserves call-site this", () => {
    const wrap = (method, context) => ({
      [context.name](...args) {
        return method.apply(this, args);
      },
    })[context.name];

    class Counter {
      count = 10;

      @wrap
      getCount() {
        return this.count;
      }
    }

    const c = new Counter();
    expect(c.getCount()).toBe(10);
    c.count = 42;
    expect(c.getCount()).toBe(42);
  });

  test("wrapper supports recursive self-call", () => {
    const calls = [];
    const trace = (method, context) => ({
      [context.name](...args) {
        calls.push(args[0]);
        return method.apply(this, args);
      },
    })[context.name];

    class Math2 {
      @trace
      fib(n) {
        if (n < 2) return n;
        return this.fib(n - 1) + this.fib(n - 2);
      }
    }

    const result = new Math2().fib(6);
    expect(result).toBe(8);
    expect(calls[0]).toBe(6);
    expect(calls.length).toBeGreaterThan(1);
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
