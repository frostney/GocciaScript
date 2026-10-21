/*---
description: Decorators work on private class elements
features: [decorators]
---*/

describe("private element decorators", () => {
  test("private method decorator reports private name", () => {
    let receivedContext;

    const log = (method, context) => {
      receivedContext = context;
      return method;
    };

    class C {
      @log
      #greet() {
        return "hello";
      }

      callGreet() {
        return this.#greet();
      }
    }

    const c = new C();
    expect(c.callGreet()).toBe("hello");
    expect(receivedContext.kind).toBe("method");
    expect(receivedContext.name).toBe("#greet");
    expect(receivedContext.private).toBe(true);
  });

  test("static private method decorator receives and replaces method", () => {
    let receivedContext;
    let receivedValueType = "unset";

    const wrap = (method, context) => {
      receivedContext = context;
      receivedValueType = typeof method;
      return () => "wrapped:" + method();
    };

    class C {
      @wrap
      static #greet() {
        return "hello";
      }

      static callGreet() {
        return this.#greet();
      }
    }

    expect(C.callGreet()).toBe("wrapped:hello");
    expect(receivedValueType).toBe("function");
    expect(receivedContext.kind).toBe("method");
    expect(receivedContext.name).toBe("#greet");
    expect(receivedContext.static).toBe(true);
    expect(receivedContext.private).toBe(true);
  });
});

describe("private instance method decorators", () => {
  // A replacement that keeps the receiver: a method, not an arrow function.
  const addHundred = (value) => ({
    wrapped(...args) {
      return value.call(this, ...args) + 100;
    },
  }).wrapped;

  test("receives the method and its replacement runs", () => {
    const log = [];
    const dec = (value, context) => {
      log.push(`${context.kind} ${context.name} ${typeof value}`);
      return ({
        wrapped(...args) {
          log.push("wrapped");
          return value.call(this, ...args) + 100;
        },
      }).wrapped;
    };

    class A {
      @dec #m() { return 1; }
      callM() { return this.#m(); }
    }

    expect(new A().callM()).toBe(101);
    expect(log).toEqual(["method #m function", "wrapped"]);
  });

  test("the original method is the class's own and keeps this", () => {
    let original;
    const capture = (value) => {
      original = value;
    };

    class A {
      x = 5;
      @capture #m() { return this.x; }
      static read(o) { return o.#m; }
    }

    const a = new A();
    expect(typeof original).toBe("function");
    expect(A.read(a)).toBe(original);
    expect(original.call(a)).toBe(5);
  });

  test("the replacement is the method for every instance", () => {
    let replacement;
    const dec = (value) => {
      replacement = addHundred(value);
      return replacement;
    };

    class A {
      @dec #m() { return 1; }
      static read(o) { return o.#m; }
    }

    const a = new A();
    const b = new A();
    expect(A.read(a)).toBe(replacement);
    expect(A.read(b)).toBe(replacement);
  });

  test("field initializers see the replacement", () => {
    class A {
      @addHundred #m() { return 1; }
      field = this.#m();
    }

    expect(new A().field).toBe(101);
  });

  test("the replacement can read the instance's private fields", () => {
    class A {
      #x = 3;
      @addHundred #m() { return this.#x; }
      callM() { return this.#m(); }
    }

    expect(new A().callM()).toBe(103);
  });

  test("returning undefined keeps the method", () => {
    let original;
    class A {
      @((value) => { original = value; }) #m() { return 7; }
      callM() { return this.#m(); }
      static read(o) { return o.#m; }
    }

    const a = new A();
    expect(a.callM()).toBe(7);
    expect(A.read(a)).toBe(original);
  });

  test("several decorators apply innermost first", () => {
    const tag = (label) => (value) => ({
      wrapped() {
        return `${label}(${value.call(this)})`;
      },
    }).wrapped;

    class A {
      @tag("outer") @tag("inner") #m() { return "m"; }
      callM() { return this.#m(); }
    }

    expect(new A().callM()).toBe("outer(inner(m))");
  });

  test("each evaluation of a class body decorates its own method", () => {
    const make = (label) => class {
      @((value) => ({
        wrapped() {
          return `${label}:${value.call(this)}`;
        },
      }).wrapped) #m() { return "m"; }
      callM() { return this.#m(); }
      static callOn(o) { return o.#m(); }
    };

    const A = make("a");
    const B = make("b");
    expect(new A().callM()).toBe("a:m");
    expect(new B().callM()).toBe("b:m");
    expect(() => A.callOn(new B())).toThrow(TypeError);
  });

  test("a subclass instance calls the decorated method", () => {
    class A {
      @addHundred #m() { return 1; }
      callM() { return this.#m(); }
    }
    class B extends A {}

    expect(new B().callM()).toBe(101);
  });

  test("a decorated generator method", () => {
    const prepend = (value) => ({
      *wrapped() {
        yield 0;
        yield* value.call(this);
      },
    }).wrapped;

    class A {
      @prepend *#g() { yield 1; yield 2; }
      all() { return [...this.#g()]; }
    }

    expect(new A().all()).toEqual([0, 1, 2]);
  });

  test("the brand check still passes and the method stays read-only", () => {
    class A {
      @addHundred #m() { return 1; }
      static has(o) { return #m in o; }
      assign() { this.#m = 1; }
    }

    expect(A.has(new A())).toBe(true);
    expect(A.has({})).toBe(false);
    expect(() => new A().assign()).toThrow(TypeError);
  });
});

describe("private accessor decorators", () => {
  test("instance private getter and setter receive and replace", () => {
    let getterType;
    let setterType;
    class A {
      #x = 1;
      @((getter) => {
        getterType = typeof getter;
        return ({ wrapped() { return getter.call(this) * 10; } }).wrapped;
      })
      get #v() { return this.#x; }
      @((setter) => {
        setterType = typeof setter;
        return ({ wrapped(v) { setter.call(this, v + 1); } }).wrapped;
      })
      set #v(v) { this.#x = v; }
      read() { return this.#v; }
      write(v) { this.#v = v; }
    }

    const a = new A();
    a.write(4);
    expect(a.read()).toBe(50);
    expect(getterType).toBe("function");
    expect(setterType).toBe("function");
  });

  test("static private getter and setter receive and replace", () => {
    let getterType;
    let setterType;
    class A {
      static #x = 1;
      @((getter) => {
        getterType = typeof getter;
        return ({ wrapped() { return getter.call(this) * 10; } }).wrapped;
      })
      static get #v() { return A.#x; }
      @((setter) => {
        setterType = typeof setter;
        return ({ wrapped(v) { setter.call(this, v + 1); } }).wrapped;
      })
      static set #v(v) { A.#x = v; }
      static read() { return A.#v; }
      static write(v) { A.#v = v; }
    }

    A.write(4);
    expect(A.read()).toBe(50);
    expect(getterType).toBe("function");
    expect(setterType).toBe("function");
  });

  test("a callable Proxy replaces a static or instance private getter and setter", () => {
    const proxied = (fn) => new Proxy(fn, {});
    const wrapGetter = (getter) =>
      proxied(({ wrapped() { return getter.call(this) * 10; } }).wrapped);
    const wrapSetter = (setter) =>
      proxied(({ wrapped(v) { setter.call(this, v + 1); } }).wrapped);
    class S {
      static #x = 1;
      @wrapGetter static get #v() { return S.#x; }
      @wrapSetter static set #v(v) { S.#x = v; }
      static read() { return S.#v; }
      static write(v) { S.#v = v; }
    }
    class I {
      #x = 1;
      @wrapGetter get #v() { return this.#x; }
      @wrapSetter set #v(v) { this.#x = v; }
      read() { return this.#v; }
      write(v) { this.#v = v; }
    }

    S.write(4);
    expect(S.read()).toBe(50);
    const instance = new I();
    instance.write(4);
    expect(instance.read()).toBe(50);
  });

  test("a private getter decorator that returns undefined keeps the getter", () => {
    class A {
      @(() => undefined) get #v() { return 1; }
      read() { return this.#v; }
    }

    expect(new A().read()).toBe(1);
  });
});
