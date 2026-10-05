/*---
description: A var read before its first assignment is undefined however many arguments the call passes
features: [compat-var]
---*/

// ES2026 §10.2.11 FunctionDeclarationInstantiation step 29.c.i.3: every var
// the body declares starts as undefined. Arguments past the last parameter
// must not show through it.

const twoVars = (a) => {
  var x, y;
  return [x, y];
};

describe("var initial value with surplus arguments", () => {
  test("arrow function", () => {
    const fn = (p) => {
      var t;
      return t;
    };
    expect(fn(2, 7)).toBeUndefined();
  });

  test("several vars and no parameters", () => {
    const fn = () => {
      var x, y;
      return [x, y];
    };
    expect(fn(5, 6)).toEqual([undefined, undefined]);
  });

  test("typeof", () => {
    const fn = (a) => {
      var x;
      return typeof x;
    };
    expect(fn(1, "surplus")).toBe("undefined");
  });

  test("strict equality with undefined", () => {
    const fn = (a) => {
      var x;
      return x === undefined;
    };
    expect(fn(1, null)).toBe(true);
  });

  test("increment", () => {
    const fn = (a) => {
      var n;
      n++;
      return n;
    };
    expect(fn(1, 41)).toBeNaN();
  });

  test("compound assignment", () => {
    const fn = (a) => {
      var s;
      s += "!";
      return s;
    };
    expect(fn(1, "surplus")).toBe("undefined!");
  });

  test("var declared in a block that does not run", () => {
    const fn = (a) => {
      if (false) {
        var x = 1;
      }
      return x;
    };
    expect(fn(1, 2)).toBeUndefined();
  });

  test("var declared in a catch block", () => {
    const fn = (a) => {
      try {
        throw 1;
      } catch {
        var c;
      }
      return c;
    };
    expect(fn(1, 2)).toBeUndefined();
  });

  test("var in a for-of head over an empty iterable", () => {
    const fn = (a) => {
      for (var k of []) {}
      return k;
    };
    expect(fn(1, 2)).toBeUndefined();
  });

  test("var destructuring declaration that does not run", () => {
    const fn = (a) => {
      if (false) {
        var { p, q } = {};
      }
      return [p, q];
    };
    expect(fn(1, 2, 3)).toEqual([undefined, undefined]);
  });

  test("var captured by a closure", () => {
    const fn = (a) => {
      var v;
      return () => v;
    };
    expect(fn(1, 2, 3)()).toBeUndefined();
  });

  test("object method", () => {
    const obj = {
      m(a) {
        var x;
        return x;
      },
    };
    expect(obj.m(1, 2)).toBeUndefined();
  });

  test("class constructor", () => {
    class C {
      constructor(a) {
        var x;
        this.x = x;
      }
    }
    expect(new C(1, 2).x).toBeUndefined();
  });

  test("class method", () => {
    class C {
      m(a) {
        var y;
        return y;
      }
    }
    expect(new C().m(1, 2)).toBeUndefined();
  });

  test("static method", () => {
    class C {
      static s(a) {
        var z;
        return z;
      }
    }
    expect(C.s(1, 2)).toBeUndefined();
  });

  test("derived constructor after super()", () => {
    class Base {
      constructor(a) {
        this.a = a;
      }
    }
    class Derived extends Base {
      constructor(a) {
        super(a);
        var q;
        this.q = q;
      }
    }
    const derived = new Derived(1, 2, 3);
    expect([derived.a, derived.q]).toEqual([1, undefined]);
  });

  test("getter called with arguments", () => {
    const obj = {
      get v() {
        var x;
        return x;
      },
    };
    const getter = Object.getOwnPropertyDescriptor(obj, "v").get;
    expect(getter.call(obj, 5, 6)).toBeUndefined();
  });

  test("setter called with surplus arguments", () => {
    const obj = {
      set v(value) {
        var x;
        this.seen = x;
      },
    };
    const setter = Object.getOwnPropertyDescriptor(obj, "v").set;
    setter.call(obj, 1, 2);
    expect(obj.seen).toBeUndefined();
  });

  test("generator method", () => {
    const obj = {
      *gen(a) {
        var x;
        yield x;
      },
    };
    expect(obj.gen(1, 2).next().value).toBeUndefined();
  });

  test("async arrow function", () => {
    const fn = async (a) => {
      var x;
      return x;
    };
    return fn(1, 2).then((value) => {
      expect(value).toBeUndefined();
    });
  });

  test("rest parameter keeps the surplus arguments", () => {
    const fn = (a, ...rest) => {
      var x;
      return [x, rest];
    };
    expect(fn(1, 2, 3)).toEqual([undefined, [2, 3]]);
  });

  test("spread call", () => {
    expect(twoVars(...[1, 2, 3])).toEqual([undefined, undefined]);
  });

  test("Function.prototype.call", () => {
    expect(twoVars.call(null, 1, 2, 3)).toEqual([undefined, undefined]);
  });

  test("Function.prototype.apply", () => {
    expect(twoVars.apply(null, [1, 2, 3])).toEqual([undefined, undefined]);
  });

  test("bound function", () => {
    expect(twoVars.bind(null, 1, 2)(3)).toEqual([undefined, undefined]);
  });

  test("Reflect.apply", () => {
    expect(Reflect.apply(twoVars, null, [1, 2, 3])).toEqual([
      undefined,
      undefined,
    ]);
  });

  test("more than 255 arguments", () => {
    const args = Array.from({ length: 300 }, (_, index) => index);
    const fn = (a, ...rest) => {
      var x, y;
      return [x, y, rest.length, rest[298]];
    };
    expect(fn(...args)).toEqual([undefined, undefined, 299, 299]);
  });

  test("var named like a parameter keeps the argument", () => {
    const fn = (a) => {
      var a;
      return a;
    };
    expect(fn(4, 5)).toBe(4);
  });
});
