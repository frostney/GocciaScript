/*---
description: Decorators, the values they return, and addInitializer initializers may be callable Proxies
features: [decorators, Proxy]
---*/

// The decorators proposal calls a decorator and its initializers through
// Call(), and only checks IsCallable on what it gets; a Proxy whose target is
// a function qualifies.
const proxied = (fn) => new Proxy(fn, {});

describe("callable Proxies as decorators", () => {
  test("a class decorator", () => {
    const seen = [];
    const dec = proxied((value, context) => { seen.push(context.kind); });
    @dec class A {}
    expect(typeof A).toBe("function");
    expect(seen).toEqual(["class"]);
  });

  test("a method decorator replaces the method", () => {
    const dec = proxied((method, context) => ({ m() { return "decorated " + method.call(this); } }).m);
    class B { @dec m() { return "m"; } }
    expect(new B().m()).toBe("decorated m");
  });

  test("a field decorator returns the initializer", () => {
    const dec = proxied((value, context) => (x) => x + 1);
    class F { @dec x = 4; }
    expect(new F().x).toBe(5);
  });

  test("getter and accessor decorators that return undefined leave the member", () => {
    const dec = proxied(() => undefined);
    class G { @dec get x() { return 1; } @dec accessor y = 2; }
    expect(new G().x).toBe(1);
    expect(new G().y).toBe(2);
  });
});

describe("callable Proxies returned by decorators", () => {
  test("as a field initializer", () => {
    const dec = (value, context) => proxied((x) => x + 1);
    class F { @dec x = 4; }
    expect(new F().x).toBe(5);
  });

  test("as a static field initializer", () => {
    const dec = (value, context) => proxied((x) => x + 2);
    class S { @dec static x = 4; }
    expect(S.x).toBe(6);
  });

  test("as a public getter and a method", () => {
    const getter = (g, context) => proxied(() => "pg");
    const method = (m, context) => proxied(() => "pm");
    class P { @getter get x() { return 1; } @method m() { return 1; } }
    expect(new P().x).toBe("pg");
    expect(new P().m()).toBe("pm");
  });

  test("as an auto-accessor's get, set and init", () => {
    const log = [];
    const dec = (value, context) => ({
      get: proxied(() => "ag"),
      set: proxied((x) => { log.push(x); }),
      init: proxied((x) => x * 10),
    });
    class A { @dec accessor x = 1; }
    const a = new A();
    expect(a.x).toBe("ag");
    a.x = 2;
    expect(log[log.length - 1]).toBe(2);
  });

  test("as a private getter and setter, reading and writing them does not crash", () => {
    const getter = (g, context) => proxied(() => "privg");
    const setter = (s, context) => proxied(() => {});
    class P {
      @getter get #x() { return 1; }
      @setter set #y(v) {}
      read() { return this.#x; }
      write() { this.#y = 8; }
    }
    expect(() => new P().read()).not.toThrow();
    expect(() => new P().write()).not.toThrow();
  });
});

describe("callable Proxies passed to addInitializer", () => {
  test("a class decorator's initializer runs with the class as this", () => {
    let receiver;
    const dec = (value, context) => {
      context.addInitializer(proxied(({ m() { receiver = this; } }).m));
    };
    @dec class C {}
    expect(receiver).toBe(C);
  });

  test("method and field initializers run for each instance", () => {
    const log = [];
    const method = (m, context) => { context.addInitializer(proxied(() => log.push("method"))); };
    const field = (v, context) => { context.addInitializer(proxied(() => log.push("field"))); };
    class I { @method m() {} @field x = 1; }
    new I();
    expect(log.sort()).toEqual(["field", "method"]);
  });

  test("static method and static field initializers run once", () => {
    const log = [];
    const method = (m, context) => { context.addInitializer(proxied(() => log.push("static method"))); };
    const field = (v, context) => { context.addInitializer(proxied(() => log.push("static field"))); };
    class S { @method static m() {} @field static x = 1; }
    expect(log.sort()).toEqual(["static field", "static method"]);
  });
});
