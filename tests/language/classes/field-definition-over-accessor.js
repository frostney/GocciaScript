/*---
description: A public field is defined on the instance, not assigned, so it never calls a setter
features: [classes, class-fields-public, Object.defineProperty, accessor-properties]
---*/

// ES2026 §7.3.32 DefineField step 6.b creates a public field with
// CreateDataPropertyOrThrow. An accessor of the same name, own or inherited,
// is not called; an own one is replaced when it is configurable.

describe("public field definition", () => {
  test("replaces a configurable own accessor installed by the base constructor", () => {
    let setterCalls = 0;
    class Base {
      constructor() {
        Object.defineProperty(this, "x", {
          set(value) {
            setterCalls += 1;
          },
          configurable: true,
        });
      }
    }
    class Derived extends Base {
      x = 1;
    }

    const instance = new Derived();

    expect(setterCalls).toBe(0);
    expect(Object.getOwnPropertyDescriptor(instance, "x")).toEqual({
      value: 1,
      writable: true,
      enumerable: true,
      configurable: true,
    });
  });

  test("throws TypeError over a non-configurable own accessor without calling its setter", () => {
    let setterCalls = 0;
    class Base {
      constructor() {
        Object.defineProperty(this, "x", {
          set(value) {
            setterCalls += 1;
          },
        });
      }
    }
    class Derived extends Base {
      x = 1;
    }

    expect(() => new Derived()).toThrow(TypeError);
    expect(setterCalls).toBe(0);
  });

  test("does not call an inherited setter", () => {
    let setterCalls = 0;
    class Base {
      get x() {
        return "from prototype";
      }

      set x(value) {
        setterCalls += 1;
      }
    }
    class Derived extends Base {
      x = 1;
    }

    const instance = new Derived();

    expect(setterCalls).toBe(0);
    expect(Object.hasOwn(instance, "x")).toBe(true);
    expect(instance.x).toBe(1);
  });

  test("without an initializer, defines undefined instead of calling an inherited setter", () => {
    const received = [];
    class Base {
      set x(value) {
        received.push(value);
      }
    }
    class Derived extends Base {
      x;
    }

    const instance = new Derived();

    expect(received).toEqual([]);
    expect(Object.getOwnPropertyDescriptor(instance, "x")).toEqual({
      value: undefined,
      writable: true,
      enumerable: true,
      configurable: true,
    });
  });

  test("shadows an inherited getter-only accessor", () => {
    class Base {
      get x() {
        return "from prototype";
      }
    }
    class Derived extends Base {
      x = 2;
    }

    const instance = new Derived();

    expect(Object.hasOwn(instance, "x")).toBe(true);
    expect(instance.x).toBe(2);
  });

  test("replaces a configurable non-writable own data property", () => {
    class Base {
      constructor() {
        Object.defineProperty(this, "x", { value: 1, writable: false, configurable: true });
      }
    }
    class Derived extends Base {
      x = 2;
    }

    expect(Object.getOwnPropertyDescriptor(new Derived(), "x")).toEqual({
      value: 2,
      writable: true,
      enumerable: true,
      configurable: true,
    });
  });

  test("makes a non-enumerable own data property enumerable", () => {
    class Base {
      constructor() {
        Object.defineProperty(this, "x", {
          value: 1,
          writable: true,
          enumerable: false,
          configurable: true,
        });
      }
    }
    class Derived extends Base {
      x = 2;
    }

    expect(Object.getOwnPropertyDescriptor(new Derived(), "x")).toEqual({
      value: 2,
      writable: true,
      enumerable: true,
      configurable: true,
    });
  });

  test("throws TypeError on a non-extensible instance", () => {
    class Base {
      constructor() {
        Object.preventExtensions(this);
      }
    }
    class Derived extends Base {
      x = 1;
    }

    expect(() => new Derived()).toThrow(TypeError);
  });

  test("does not call an inherited setter in a class without a superclass constructor call", () => {
    let setterCalls = 0;
    const proto = {
      set x(value) {
        setterCalls += 1;
      },
    };
    class Standalone {
      x = 1;
    }
    Object.setPrototypeOf(Standalone.prototype, proto);

    const instance = new Standalone();

    expect(setterCalls).toBe(0);
    expect(Object.hasOwn(instance, "x")).toBe(true);
    expect(instance.x).toBe(1);
  });

  test("does not call an inherited setter on an instance of a class extending Error", () => {
    let setterCalls = 0;
    class Base extends Error {
      get x() {
        return "from prototype";
      }

      set x(value) {
        setterCalls += 1;
      }
    }
    class Derived extends Base {
      x = 1;
    }

    const instance = new Derived("message");

    expect(setterCalls).toBe(0);
    expect(Object.hasOwn(instance, "x")).toBe(true);
    expect(instance.x).toBe(1);
  });

  test("does not call an inherited setter on an instance of a class extending Promise", () => {
    let setterCalls = 0;
    class Base extends Promise {
      set x(value) {
        setterCalls += 1;
      }
    }
    class Derived extends Base {
      x = 1;
    }

    const instance = new Derived(() => {});

    expect(setterCalls).toBe(0);
    expect(Object.hasOwn(instance, "x")).toBe(true);
    expect(instance.x).toBe(1);
  });
});
