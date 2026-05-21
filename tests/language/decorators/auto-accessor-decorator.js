/*---
description: Auto-accessor decorators receive {get, set} and can return {get?, set?, init?}
features: [decorators, auto-accessor]
---*/

describe("auto-accessor decorators", () => {
  test("accessor decorator receives get/set object and context", () => {
    let receivedValue;
    let receivedContext;

    const log = (value, context) => {
      receivedValue = value;
      receivedContext = context;
    };

    class C {
      @log
      accessor x = 42;
    }

    expect(receivedContext.kind).toBe("accessor");
    expect(receivedContext.name).toBe("x");
    expect(typeof receivedValue).toBe("object");
  });

  test("computed accessor decorator context and access use resolved symbol key", () => {
    let receivedName;
    let accessGet;
    const key = Symbol("accessor");
    const decorate = (value, context) => {
      receivedName = context.name;
      accessGet = context.access.get;
      return {
        get() {
          return value.get.call(this) + 1;
        }
      };
    };

    class C {
      @decorate
      accessor [key] = 41;
    }

    const instance = new C();
    expect(receivedName === key).toBe(true);
    expect(instance[key]).toBe(42);
    expect(accessGet(instance)).toBe(42);
  });

  test("static accessor decorator initializer runs before class replacement", () => {
    let observedOriginalValue;
    const accessor = (value, context) => {
      return {
        init() {
          return 42;
        }
      };
    };
    const replace = (cls, context) => {
      observedOriginalValue = cls.value;
      return class Replacement {
        static value = 100;
      };
    };

    @replace
    class C {
      @accessor
      static accessor value = 41;
    }

    expect(observedOriginalValue).toBe(42);
    expect(C.value).toBe(100);
  });
});
