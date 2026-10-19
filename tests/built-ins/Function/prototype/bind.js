/*---
description: Function.prototype.bind
features: [Function]
---*/

describe("Function.prototype.bind", () => {
  test("returns a bound function", () => {
    const fn = (a, b) => a + b;
    const bound = fn.bind(null, 1);
    expect(typeof bound).toBe("function");
    expect(bound(2)).toBe(3);
  });

  test("binds this value", () => {
    class Obj {
      constructor(x) { this.x = x; }
      getX() { return this.x; }
    }
    const obj = new Obj(42);
    const getX = obj.getX.bind(obj);
    expect(getX()).toBe(42);
  });

  test("accessible via Function.prototype.bind", () => {
    expect(typeof Function.prototype.bind).toBe("function");
  });

  test("has correct name and length", () => {
    expect(Function.prototype.bind.name).toBe("bind");
    expect(Function.prototype.bind.length).toBe(1);
  });

  test("works on class constructors", () => {
    const bnc = Number.bind(null);
    expect(bnc(42)).toBe(42);
    expect(bnc("3.14")).toBe(3.14);

    const bsc = String.bind(null);
    expect(bsc(42)).toBe("42");

    const bbc = Boolean.bind(null);
    expect(bbc(1)).toBe(true);
    expect(bbc(0)).toBe(false);
  });

  test("bound length ignores non-number and inherited target length", () => {
    const foo = () => {};

    Object.defineProperty(foo, "length", { value: undefined });
    expect(foo.bind(null, 1).length).toBe(0);

    Object.defineProperty(foo, "length", { value: "1" });
    expect(foo.bind(null, 1).length).toBe(0);

    const bar = () => {};
    Object.setPrototypeOf(bar, { length: 42 });
    expect(delete bar.length).toBe(true);
    expect(Function.prototype.bind.call(bar, null, 1).length).toBe(0);
  });

  test("bound length is snapshotted at bind time", () => {
    const fn = (a, b, c) => {};
    const bound = fn.bind(null);

    Object.defineProperty(fn, "length", { value: 0, configurable: true });

    expect(bound.length).toBe(3);
  });

  test("bound length snapshots non-number and infinite target lengths", () => {
    const nonNumber = () => {};
    Object.defineProperty(nonNumber, "length", {
      value: undefined,
      configurable: true,
    });
    const nonNumberBound = nonNumber.bind(null);
    Object.defineProperty(nonNumber, "length", { value: 3, configurable: true });
    expect(nonNumberBound.length).toBe(0);

    const infinite = () => {};
    Object.defineProperty(infinite, "length", {
      value: Infinity,
      configurable: true,
    });
    expect(infinite.bind(null, 1).length).toBe(Infinity);
  });

  test("bound length normalizes negative zero", () => {
    const negativeZeroLength = () => {};
    Object.defineProperty(negativeZeroLength, "length", {
      value: -0,
      configurable: true,
    });

    const bound = negativeZeroLength.bind(null);

    expect(bound.length).toBe(0);
    expect(Object.is(bound.length, -0)).toBe(false);
  });

  test("bound name is snapshotted at bind time", () => {
    const fn = () => {};
    Object.defineProperty(fn, "name", { value: "before", configurable: true });
    const bound = fn.bind(null);

    Object.defineProperty(fn, "name", { value: "after", configurable: true });

    expect(bound.name).toBe("bound before");
  });

  test("an own bind property is invoked instead of the intrinsic", () => {
    const host = () => "host";
    host.bind = (...args) => `own bind(${args.join(",")})`;

    expect(host.bind("t")).toBe("own bind(t)");
    expect(host.bind("t", 1, 2)).toBe("own bind(t,1,2)");
    expect(Function.prototype.bind.call(host, null)()).toBe("host");
  });

  test("a class static named bind is the class's own method", () => {
    class Registry {
      static bind(name) {
        return `Registry.bind(${name})`;
      }
    }

    expect(Registry.bind("a")).toBe("Registry.bind(a)");
    expect(Registry.bind).not.toBe(Function.prototype.bind);
  });

  describe("BoundFunctionCreate on a Proxy target", () => {
    test("the bound function's prototype is the target's [[GetPrototypeOf]]", () => {
      const bound = new Proxy(class {}, {}).bind(null);
      expect(Object.getPrototypeOf(bound)).toBe(Function.prototype);
      expect(typeof bound.bind).toBe("function");
    });

    test("the getPrototypeOf trap runs exactly once, with the target, and its result is the prototype", () => {
      const proto = { marker: true };
      const calls = [];
      const target = () => {};
      const proxy = new Proxy(target, {
        getPrototypeOf(t) {
          calls.push(t);
          return proto;
        },
      });
      const bound = Function.prototype.bind.call(proxy, null);
      expect(calls.length).toBe(1);
      expect(calls[0]).toBe(target);
      expect(Object.getPrototypeOf(bound)).toBe(proto);
      expect(Object.getPrototypeOf(bound)).toBe(Object.getPrototypeOf(proxy));
    });

    test("a null trap result makes the bound function's prototype null", () => {
      const proxy = new Proxy(() => {}, {
        getPrototypeOf() {
          return null;
        },
      });
      const bound = Function.prototype.bind.call(proxy, null);
      expect(Object.getPrototypeOf(bound)).toBe(null);
    });

    test("the trap runs before the length and name are read", () => {
      const log = [];
      const proxy = new Proxy(() => {}, {
        getPrototypeOf(t) {
          log.push("getPrototypeOf");
          return Reflect.getPrototypeOf(t);
        },
        getOwnPropertyDescriptor(t, key) {
          log.push(`getOwnPropertyDescriptor:${String(key)}`);
          return Reflect.getOwnPropertyDescriptor(t, key);
        },
        get(t, key, receiver) {
          log.push(`get:${String(key)}`);
          return Reflect.get(t, key, receiver);
        },
      });
      Function.prototype.bind.call(proxy, null);
      expect(log[0]).toBe("getPrototypeOf");
      expect(log.filter((entry) => entry === "getPrototypeOf").length).toBe(1);
    });

    test("an abrupt completion from the trap propagates and the length is never read", () => {
      let reads = 0;
      const proxy = new Proxy(() => {}, {
        getPrototypeOf() {
          throw new RangeError("trap");
        },
        get(t, key, receiver) {
          reads += 1;
          return Reflect.get(t, key, receiver);
        },
      });
      expect(() => Function.prototype.bind.call(proxy, null)).toThrow(RangeError);
      expect(reads).toBe(0);
    });

    test("a trap result that is neither an object nor null is a TypeError", () => {
      const proxy = new Proxy(() => {}, {
        getPrototypeOf() {
          return 1;
        },
      });
      expect(() => Function.prototype.bind.call(proxy, null)).toThrow(TypeError);
    });

    test("a revoked proxy throws a TypeError", () => {
      const { proxy, revoke } = Proxy.revocable(() => {}, {});
      revoke();
      expect(() => Function.prototype.bind.call(proxy, null)).toThrow(TypeError);
    });

    test("a target that is not a proxy keeps its own prototype", () => {
      const fn = () => {};
      const proto = Object.create(Function.prototype);
      Object.setPrototypeOf(fn, proto);
      expect(Object.getPrototypeOf(fn.bind(null))).toBe(proto);
      Object.setPrototypeOf(fn, null);
      expect(Object.getPrototypeOf(Function.prototype.bind.call(fn, null))).toBe(null);
    });

    test("a bound function of a bound proxy inherits through the first bound function", () => {
      const proto = Object.create(Function.prototype);
      const proxy = new Proxy(() => {}, {
        getPrototypeOf() {
          return proto;
        },
      });
      const first = Function.prototype.bind.call(proxy, null);
      const second = Function.prototype.bind.call(first, null);
      expect(Object.getPrototypeOf(first)).toBe(proto);
      expect(Object.getPrototypeOf(second)).toBe(proto);
    });
  });
});
