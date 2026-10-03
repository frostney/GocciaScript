/*---
description: A static auto-accessor receives its initial value once, on the class, in order with other static elements
features: [decorators, auto-accessor, class-static-fields-public, class-static-block]
---*/

// proposal-decorators ClassDefinitionEvaluation initializes each static
// element of kind ~field~ or ~accessor~ on the constructor F with
// InitializeFieldOrAccessor, in source order with static blocks.

describe("static public auto-accessor", () => {
  test("has its initial value", () => {
    class S {
      static accessor s = 5;
    }

    expect(S.s).toBe(5);
    S.s = 6;
    expect(S.s).toBe(6);
  });

  test("without an initializer holds undefined", () => {
    class S {
      static accessor s;
    }

    expect(S.s).toBe(undefined);
  });

  test("does not initialize anything on instances", () => {
    let calls = 0;
    class S {
      static accessor s = ++calls;
    }

    const instance = new S();

    expect(calls).toBe(1);
    expect(Object.keys(instance)).toEqual([]);
    expect(Reflect.ownKeys(instance)).toEqual([]);
  });

  test("keeps its storage private", () => {
    class S {
      static accessor s = 5;
    }

    expect(Object.hasOwn(S, "__accessor_s")).toBe(false);
    expect(Object.keys(S)).toEqual(["s"]);
    expect(Object.getOwnPropertyDescriptor(S, "s").enumerable).toBe(true);
  });

  test("throws TypeError when read or written through a subclass", () => {
    class S {
      static accessor s = 5;
    }
    class Sub extends S {}

    expect(() => Sub.s).toThrow(TypeError);
    expect(() => {
      Sub.s = 1;
    }).toThrow(TypeError);
    expect(S.s).toBe(5);
  });

  test("is initialized in source order with static fields and blocks", () => {
    const order = [];
    class S {
      static a = order.push("a");
      static accessor b = order.push("b");
      static {
        order.push(`block sees b=${this.b}`);
      }
      static accessor [`c`] = order.push("c");
    }

    expect(order).toEqual(["a", "b", "block sees b=2", "c"]);
    expect(S.c).toBe(4);
  });

  test("computed and symbol keys hold their initial values", () => {
    const name = "computed";
    const symbol = Symbol("symbol");
    class S {
      static accessor [name] = 1;
      static accessor [symbol] = 2;
    }

    expect(S.computed).toBe(1);
    expect(S[symbol]).toBe(2);
  });

  test("a decorator's init receives the initial value", () => {
    const tenfold = () => ({
      init(value) {
        return value * 10;
      },
    });
    class S {
      @tenfold static accessor s = 4;
    }

    expect(S.s).toBe(40);
  });
});
