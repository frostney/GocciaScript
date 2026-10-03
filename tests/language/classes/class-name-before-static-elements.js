/*---
description: An anonymous class has its contextual name before its static fields and static blocks run
features: [classes, class-static-fields-public, class-static-block, Function.name]
---*/

describe("an anonymous class named by its context", () => {
  const key = "K";
  const symbol = Symbol("sym");

  test("static fields and blocks see the binding name", () => {
    let seen;
    const C = class {
      static n = this.name;
      static {
        seen = this.name;
      }
    };
    expect(C.n).toBe("C");
    expect(seen).toBe("C");

    let L = class {
      static n = this.name;
    };
    expect(L.n).toBe("L");
  });

  test("static fields see the name of an assignment target", () => {
    let x;
    x = class {
      static n = this.name;
    };
    expect(x.n).toBe("x");

    let y;
    y ??= class {
      static n = this.name;
    };
    expect(y.n).toBe("y");

    let z = 1;
    z &&= class {
      static n = this.name;
    };
    expect(z.n).toBe("z");
  });

  test("static fields see a property key, computed or not", () => {
    expect({ K: class { static n = this.name; } }.K.n).toBe("K");
    expect({ [key]: class { static n = this.name; } }.K.n).toBe("K");
    expect({ [symbol]: class { static n = this.name; } }[symbol].n).toBe("[sym]");
    expect({ 1: class { static n = this.name; } }[1].n).toBe("1");
  });

  test("static fields see the name of a destructuring default", () => {
    const { a = class { static n = this.name; } } = {};
    expect(a.n).toBe("a");

    const [b = class { static n = this.name; }] = [];
    expect(b.n).toBe("b");

    let c;
    ({ c = class { static n = this.name; } } = {});
    expect(c.n).toBe("c");
  });

  test("a class in a computed class field sees its final name from its static elements", () => {
    const Static = class {
      static [key] = class {
        static n = this.name;
      };
    };
    expect(Static.K.n).toBe(Static.K.name);

    const instance = new (class {
      [key] = class {
        static n = this.name;
      };
    })();
    expect(instance.K.n).toBe(instance.K.name);

    const Frozen = class {
      static [key] = class {
        static {
          Object.freeze(this);
        }
      };
    };
    expect(Object.isFrozen(Frozen.K)).toBe(true);
  });

  test("a class that freezes or seals itself in a static block keeps its name", () => {
    const Frozen = class {
      static {
        Object.freeze(this);
      }
    };
    expect(Frozen.name).toBe("Frozen");
    expect(Object.isFrozen(Frozen)).toBe(true);

    const frozen = { [key]: class { static { Object.freeze(this); } } }.K;
    expect(frozen.name).toBe("K");
    expect(Object.isFrozen(frozen)).toBe(true);

    const sealed = { [key]: class { static { Object.seal(this); } } }.K;
    expect(sealed.name).toBe("K");
    expect(Object.isSealed(sealed)).toBe(true);
  });

  test("a static name field or method replaces the contextual name", () => {
    const Field = class {
      static name = "field";
    };
    expect(Field.name).toBe("field");
    expect({ [key]: class { static name = "field"; } }.K.name).toBe("field");

    const Method = class {
      static name() {}
    };
    expect(typeof Method.name).toBe("function");
    expect(typeof { [key]: class { static name() {} } }.K.name).toBe("function");
  });

  test("the contextual name is a synthesized name property", () => {
    const C = { [key]: class { static x = 1; } }.K;
    expect(Object.getOwnPropertyDescriptor(C, "name")).toEqual({
      value: "K",
      writable: false,
      enumerable: false,
      configurable: true,
    });
    expect(Reflect.ownKeys(C)).toEqual(["length", "name", "prototype", "x"]);
    expect(() => C()).toThrow(TypeError);
  });

  test("a named class and a non-contextual position are not renamed", () => {
    const Q = class R {
      static n = this.name;
    };
    expect([Q.n, Q.name]).toEqual(["R", "R"]);

    const Comma = (0, class { static n = this.name; });
    expect([Comma.n, Comma.name]).toEqual(["", ""]);

    const target = {};
    target.p = class {
      static n = this.name;
    };
    expect([target.p.n, target.p.name]).toEqual(["", ""]);
  });

  test("a generator that resumes after the class builds it once", () => {
    // Each generator below yields once; the second next() resumes it.
    const drive = (generator) => {
      const iterator = generator();
      iterator.next();
      return iterator.next(5).value;
    };

    const literal = drive(
      {
        *build() {
          const result = {
            K: class {
              static self = this;
              static n = this.name;
            },
            after: yield 1,
          };
          return result;
        },
      }.build,
    );
    expect(literal.K.self).toBe(literal.K);
    expect(literal.K.n).toBe("K");
    expect(literal.after).toBe(5);

    const computed = drive(
      {
        *build() {
          return {
            [key]: class {
              static self = this;
            },
            after: yield 1,
          };
        },
      }.build,
    );
    expect(computed.K.self).toBe(computed.K);
    expect(computed.K.name).toBe("K");

    const destructured = drive(
      {
        *build() {
          let a;
          let b;
          ({ a = class { static self = this; }, b = yield 1 } = {});
          return { a, b };
        },
      }.build,
    );
    expect(destructured.a.self).toBe(destructured.a);
    expect(destructured.a.name).toBe("a");
    expect(destructured.b).toBe(5);
  });

  // The message text is implementation-defined (V8 leaves the name out for a
  // class named from a computed key), so this pins GocciaScript's.
  test.runIf(typeof Goccia !== "undefined")("an error from calling the class without new names it", () => {
    const messageOf = (callback) => {
      try {
        callback();
      } catch (error) {
        return error.message;
      }
      return "";
    };
    const C = { [key]: class {} }.K;
    expect(messageOf(() => C())).toBe("Class constructor K cannot be invoked without 'new'");
    const D = class {};
    expect(messageOf(() => D())).toBe("Class constructor D cannot be invoked without 'new'");
  });

});
