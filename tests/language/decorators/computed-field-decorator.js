/*---
description: Decorated computed fields use the resolved property key in context and initializer wiring
features: [decorators, class-fields, computed-property-names]
---*/

describe("computed field decorators", () => {
  test("string computed field context and initializer use resolved key", () => {
    let receivedName;
    const key = "x";
    const decorate = (value, context) => {
      receivedName = context.name;
      return (initialValue) => initialValue + 1;
    };

    class C {
      @decorate
      [key] = 41;
    }

    const instance = new C();
    expect(receivedName).toBe("x");
    expect(instance.x).toBe(42);
  });

  test("symbol computed field context and initializer preserve symbol key", () => {
    let receivedName;
    const key = Symbol("field");
    const decorate = (value, context) => {
      receivedName = context.name;
      return (initialValue) => initialValue + 1;
    };

    class C {
      @decorate
      [key] = 41;
    }

    const instance = new C();
    expect(receivedName === key).toBe(true);
    expect(instance[key]).toBe(42);
  });

  test("symbol computed field access helper preserves symbol key", () => {
    const key = Symbol("field-access");
    const decorate = (value, context) => {
      context.addInitializer(({ init() { context.access.set(this, 42); } }).init);
    };

    class C {
      @decorate
      [key] = 1;
    }

    expect(new C()[key]).toBe(42);
  });

  test("static symbol computed field access helper preserves class symbol key", () => {
    const key = Symbol("static-field-access");
    const decorate = (value, context) => {
      context.addInitializer(({ init() { context.access.set(this, 42); } }).init);
    };

    class C {
      @decorate
      static [key] = 1;
    }

    expect(C[key]).toBe(42);
  });

  test("symbol computed method decorator uses resolved key", () => {
    let receivedName;
    let accessGet;
    const key = Symbol("method");
    const decorate = (value, context) => {
      receivedName = context.name;
      accessGet = context.access.get;
      return () => 42;
    };

    class C {
      @decorate
      [key]() {
        return 1;
      }
    }

    expect(receivedName === key).toBe(true);
    const instance = new C();
    expect(instance[key]()).toBe(42);
    expect(accessGet(instance)()).toBe(42);
  });

  test("decorated computed method context uses property-key coercion", () => {
    let receivedName;
    let toStringCalls = 0;
    const key = {
      toString() {
        toStringCalls++;
        return "coerced";
      }
    };
    const decorate = (value, context) => {
      receivedName = context.name;
      return value;
    };

    class C {
      @decorate
      [key]() {
        return 42;
      }
    }

    expect(receivedName).toBe("coerced");
    expect(toStringCalls).toBe(1);
    expect(new C().coerced()).toBe(42);
  });

  test("static symbol computed method access helper preserves class symbol key", () => {
    let accessGet;
    const key = Symbol("static-method-access");
    const decorate = (value, context) => {
      accessGet = context.access.get;
      return () => 42;
    };

    class C {
      @decorate
      static [key]() {
        return 1;
      }
    }

    expect(C[key]()).toBe(42);
    expect(accessGet(C)()).toBe(42);
  });

  test("symbol computed getter decorator uses resolved key", () => {
    let receivedName;
    let accessGet;
    const key = Symbol("getter");
    const decorate = (value, context) => {
      receivedName = context.name;
      accessGet = context.access.get;
      return () => 42;
    };

    class C {
      @decorate
      get [key]() {
        return 1;
      }
    }

    expect(receivedName === key).toBe(true);
    const instance = new C();
    expect(instance[key]).toBe(42);
    expect(accessGet(instance)).toBe(42);
  });

  test("symbol computed setter decorator uses resolved key", () => {
    let receivedName;
    let accessSet;
    let stored = 0;
    const key = Symbol("setter");
    const decorate = (value, context) => {
      receivedName = context.name;
      accessSet = context.access.set;
      return (next) => {
        stored = next * 2;
      };
    };

    class C {
      @decorate
      set [key](next) {
        stored = next;
      }
    }

    const instance = new C();
    instance[key] = 21;
    expect(receivedName === key).toBe(true);
    expect(stored).toBe(42);
    accessSet(instance, 7);
    expect(stored).toBe(14);
  });

  test("static computed field context uses resolved key", () => {
    let receivedName;
    const key = "staticName";
    const decorate = (value, context) => {
      receivedName = context.name;
    };

    class C {
      @decorate
      static [key] = 1;
    }

    expect(receivedName).toBe("staticName");
    expect(C.staticName).toBe(1);
  });
});
