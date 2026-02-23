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
});
