/*---
description: Non-strict compatibility binds regular-function nullish this to globalThis
features: [compat-function, compat-non-strict-mode]
---*/

describe("non-strict function this binding", () => {
  test("unattached function calls bind this to globalThis", () => {
    function f() {
      return this;
    }

    expect(f()).toBe(globalThis);
  });

  test("call and apply coerce nullish this values to globalThis", () => {
    function f() {
      return this;
    }

    expect(f.call(undefined)).toBe(globalThis);
    expect(f.call(null)).toBe(globalThis);
    expect(f.apply(undefined, [])).toBe(globalThis);
    expect(f.apply(null, [])).toBe(globalThis);
  });

  test("call boxes a primitive this to its wrapper object", () => {
    function f() {
      return this;
    }

    const bound = f.call(5);
    expect(typeof bound).toBe("object");
    expect(bound.valueOf()).toBe(5);
  });

  test("method calls box a primitive receiver for non-strict functions", () => {
    function readThis() {
      return this;
    }

    Boolean.prototype.readThis = readThis;
    Number.prototype.readThis = readThis;
    String.prototype.readThis = readThis;
    try {
      expect(true.readThis() instanceof Boolean).toBe(true);
      expect((42).readThis() instanceof Number).toBe(true);
      expect("value".readThis() instanceof String).toBe(true);
    } finally {
      delete Boolean.prototype.readThis;
      delete Number.prototype.readThis;
      delete String.prototype.readThis;
    }
  });

  test("apply boxes a primitive this to its wrapper object", () => {
    function f() {
      return this;
    }

    const bound = f.apply("abc", []);
    expect(typeof bound).toBe("object");
    expect(bound.valueOf()).toBe("abc");
  });

  test("prototype getter on a primitive receiver sees the boxed this", () => {
    Object.defineProperty(Object.prototype, "boxedSelf", {
      get() {
        return this;
      },
      configurable: true,
    });
    try {
      expect((5).boxedSelf === 5).toBe(false);
      expect(typeof (5).boxedSelf).toBe("object");
    } finally {
      delete Object.prototype.boxedSelf;
    }
  });

  test("use strict directive keeps a primitive this unboxed", () => {
    function f() {
      "use strict";
      return this;
    }

    expect(f.call(5)).toBe(5);
  });

  test("arrow functions keep lexical this", () => {
    function outer() {
      const arrow = () => this;
      return arrow();
    }

    const receiver = { marker: 1 };
    expect(outer.call(receiver)).toBe(receiver);
  });

  test("use strict directive keeps standalone function this undefined", () => {
    function f() {
      "use strict";
      return this;
    }

    expect(f()).toBeUndefined();
    expect(f.call(undefined)).toBeUndefined();
    expect(f.call(null)).toBeNull();
  });

  test("use strict directive is found after earlier string directives", () => {
    function f() {
      "not strict";
      "use strict";
      return this;
    }

    expect(f()).toBeUndefined();
  });

  test("use strict outside the directive prologue does not change this", () => {
    function f() {
      0;
      "use strict";
      return this;
    }

    expect(f()).toBe(globalThis);
  });

  test("escaped use strict string is not a strict directive", () => {
    function f() {
      "use\u0020strict";
      return this;
    }

    expect(f()).toBe(globalThis);
  });

  test("use strict directive applies to object methods and accessors", () => {
    const obj = {
      method() {
        "use strict";
        return this;
      },
      get value() {
        "use strict";
        return this;
      }
    };
    const method = obj.method;
    const getter = Object.getOwnPropertyDescriptor(obj, "value").get;

    expect(method()).toBeUndefined();
    expect(getter()).toBeUndefined();
  });

  test("use strict directive applies to generator functions", () => {
    function* gen() {
      "use strict";
      yield this;
    }

    const method = ({
      *gen() {
        "use strict";
        yield this;
      }
    }).gen;

    expect(gen().next().value).toBeUndefined();
    expect(method().next().value).toBeUndefined();
  });

  test("functions created inside strict code inherit strict this", () => {
    function outer() {
      "use strict";
      return function inner() {
        return this;
      };
    }

    expect(outer()()).toBeUndefined();
  });
});
