/*---
description: >
  A ClassBody is strict-mode code (ES2026 §15.7.1) even when the surrounding
  script runs with the non-strict compatibility profile, so an assignment to an
  undeclared name inside one raises ReferenceError instead of creating a global.
features: [compat-function, compat-non-strict-mode, compat-var]
---*/

// The regression was in how class-body code inherited strictness: the
// construction routes handed a field initializer the *caller's* evaluation
// context, which under this profile still carries the script's sloppy flags,
// so an initializer's assignment to an undeclared identifier created a global
// instead of throwing. Node throws ReferenceError for every case below in both
// a sloppy script and a module.
describe("class bodies stay strict under the non-strict compatibility profile", () => {
  test("an instance field initializer throws for an undeclared assignment", () => {
    class Leaky {
      f = (undeclaredFieldTarget_q9 = 41);
    }

    expect(() => new Leaky()).toThrow(ReferenceError);
    expect("undeclaredFieldTarget_q9" in globalThis).toBe(false);
  });

  test("Reflect.construct throws the same ReferenceError", () => {
    class Leaky {
      f = (undeclaredReflectTarget_q9 = 41);
    }

    expect(() => Reflect.construct(Leaky, [])).toThrow(ReferenceError);
    expect("undeclaredReflectTarget_q9" in globalThis).toBe(false);
  });

  test("a proxy without a construct trap throws it too", () => {
    class Leaky {
      f = (undeclaredProxyTarget_q9 = 41);
    }

    const P = new Proxy(Leaky, {});

    expect(() => new P()).toThrow(ReferenceError);
    expect("undeclaredProxyTarget_q9" in globalThis).toBe(false);
  });

  test("a subclass running the base class fields throws", () => {
    class Base {
      f = (undeclaredSubclassTarget_q9 = 41);
    }
    class Derived extends Base {}

    expect(() => new Derived()).toThrow(ReferenceError);
    expect("undeclaredSubclassTarget_q9" in globalThis).toBe(false);
  });

  test("a derived class with its own field initializer throws", () => {
    class Derived extends Object {
      f = (undeclaredDerivedTarget_q9 = 41);
    }

    expect(() => new Derived()).toThrow(ReferenceError);
    expect("undeclaredDerivedTarget_q9" in globalThis).toBe(false);
  });

  test("a private field initializer throws", () => {
    class Leaky {
      #p = (undeclaredPrivateTarget_q9 = 41);

      read() {
        return this.#p;
      }
    }

    expect(() => new Leaky()).toThrow(ReferenceError);
    expect("undeclaredPrivateTarget_q9" in globalThis).toBe(false);
  });

  test("a computed-key field's initializer throws", () => {
    const key = "computed";

    class Leaky {
      [key] = (undeclaredComputedInitTarget_q9 = 41);
    }

    expect(() => new Leaky()).toThrow(ReferenceError);
    expect("undeclaredComputedInitTarget_q9" in globalThis).toBe(false);
  });

  test("a computed key expression throws at class-definition time", () => {
    expect(() => {
      class Leaky {
        [(undeclaredComputedKeyTarget_q9 = 41)] = 1;
      }

      return Leaky;
    }).toThrow(ReferenceError);
    expect("undeclaredComputedKeyTarget_q9" in globalThis).toBe(false);
  });

  test("a static field initializer throws at class-definition time", () => {
    expect(() => {
      class Leaky {
        static s = (undeclaredStaticFieldTarget_q9 = 41);
      }

      return Leaky;
    }).toThrow(ReferenceError);
    expect("undeclaredStaticFieldTarget_q9" in globalThis).toBe(false);
  });

  test("a static initialization block throws at class-definition time", () => {
    expect(() => {
      class Leaky {
        static {
          undeclaredStaticBlockTarget_q9 = 41;
        }
      }

      return Leaky;
    }).toThrow(ReferenceError);
    expect("undeclaredStaticBlockTarget_q9" in globalThis).toBe(false);
  });

  test("method, constructor, getter and setter bodies throw", () => {
    class Methods {
      constructor(run) {
        if (run) {
          undeclaredCtorTarget_q9 = 41;
        }
      }

      m() {
        undeclaredMethodTarget_q9 = 41;
      }

      get g() {
        undeclaredGetterTarget_q9 = 41;
        return 1;
      }

      set s(value) {
        undeclaredSetterTarget_q9 = value;
      }
    }

    const instance = new Methods(false);

    expect(() => new Methods(true)).toThrow(ReferenceError);
    expect(() => instance.m()).toThrow(ReferenceError);
    expect(() => instance.g).toThrow(ReferenceError);
    expect(() => {
      instance.s = 1;
    }).toThrow(ReferenceError);
  });

  test("nested function bodies inside a field initializer are strict too", () => {
    class Nested {
      fromFunction = (function () {
        return this;
      })();

      fromArrow = (() => this)();
    }

    const instance = new Nested();

    // Strict code leaves a nullish `this` alone instead of coercing it to the
    // global object, and an arrow still sees the instance under construction.
    expect(instance.fromFunction).toBeUndefined();
    expect(instance.fromArrow).toBe(instance);
  });

  test("an unqualified method call still gets a strict undefined this", () => {
    class Detached {
      m() {
        return this;
      }
    }

    const detached = new Detached().m;

    expect(detached()).toBeUndefined();
  });
});

// Guards the other direction: the fix must confine itself to class bodies and
// leave the profile's sloppy semantics outside one exactly as they were.
describe("non-class code keeps the compatibility profile's sloppy semantics", () => {
  test("an undeclared assignment outside a class body still creates a global", () => {
    delete globalThis.sloppyOutsideClassTarget_q9;

    try {
      const result = (sloppyOutsideClassTarget_q9 = 3);

      expect(result).toBe(3);
      expect(globalThis.sloppyOutsideClassTarget_q9).toBe(3);
    } finally {
      delete globalThis.sloppyOutsideClassTarget_q9;
    }
  });

  test("an undeclared assignment inside a plain function still creates a global", () => {
    delete globalThis.sloppyFunctionTarget_q9;

    const create = function () {
      sloppyFunctionTarget_q9 = 5;
    };

    try {
      create();

      expect(globalThis.sloppyFunctionTarget_q9).toBe(5);
    } finally {
      delete globalThis.sloppyFunctionTarget_q9;
    }
  });

  test("a plain function still binds a nullish this to globalThis", () => {
    const readThis = function () {
      return this;
    };

    expect(readThis()).toBe(globalThis);
  });

  test("with statements are still available outside a class body", () => {
    const scope = { value: 7 };
    let seen = 0;

    with (scope) {
      seen = value;
    }

    expect(seen).toBe(7);
  });
});
