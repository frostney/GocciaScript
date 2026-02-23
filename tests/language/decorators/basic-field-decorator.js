/*---
description: Field decorators receive undefined as value and can return initializer functions
features: [decorators]
---*/

describe("field decorators", () => {
  test("decorator receives undefined and context", () => {
    let receivedValue;
    let receivedContext;

    const log = (value, context) => {
      receivedValue = value;
      receivedContext = context;
    };

    class C {
      @log
      x = 42;
    }

    expect(receivedValue).toBe(undefined);
    expect(receivedContext.kind).toBe("field");
    expect(receivedContext.name).toBe("x");
    expect(receivedContext.static).toBe(false);
    expect(receivedContext.private).toBe(false);
  });

  test("decorator can return initializer function", () => {
    const doubleInit = (value, context) => {
      return (initialValue) => {
        return initialValue * 2;
      };
    };

    class C {
      @doubleInit
      x = 21;
    }

    const c = new C();
    expect(c.x).toBe(42);
  });

  test("decorator returning undefined keeps original initializer", () => {
    const noop = (value, context) => {
      return undefined;
    };

    class C {
      @noop
      x = 42;
    }

    const c = new C();
    expect(c.x).toBe(42);
  });

  test("decorator context for static field", () => {
    let ctx;
    const capture = (value, context) => {
      ctx = context;
    };

    class C {
      @capture
      static x = 1;
    }

    expect(ctx.kind).toBe("field");
    expect(ctx.name).toBe("x");
    expect(ctx.static).toBe(true);
    expect(ctx.private).toBe(false);
  });

  test("decorator context for private field", () => {
    let ctx;
    const capture = (value, context) => {
      ctx = context;
    };

    class C {
      @capture
      #secret = 1;
    }

    expect(ctx.kind).toBe("field");
    expect(ctx.name).toBe("#secret");
    expect(ctx.static).toBe(false);
    expect(ctx.private).toBe(true);
  });

  test("decorator that throws propagates error", () => {
    const bomb = (value, context) => {
      throw new Error("decorator failed");
    };

    expect(() => {
      class C {
        @bomb
        x = 1;
      }
    }).toThrow(Error);
  });
});
