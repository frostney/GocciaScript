describe("Proxy getOwnPropertyDescriptor trap", () => {
  test("intercepts Object.getOwnPropertyDescriptor", () => {
    const target = { x: 1 };
    const proxy = new Proxy(target, {
      getOwnPropertyDescriptor: (t, prop) => {
        return {
          value: 42,
          writable: true,
          enumerable: true,
          configurable: true,
        };
      },
    });
    const desc = Object.getOwnPropertyDescriptor(proxy, "x");
    expect(desc.value).toBe(42);
    expect(desc.writable).toBe(true);
    expect(desc.enumerable).toBe(true);
    expect(desc.configurable).toBe(true);
  });

  test("returns undefined for non-existent property", () => {
    const proxy = new Proxy(
      {},
      {
        getOwnPropertyDescriptor: (t, prop) => undefined,
      }
    );
    const desc = Object.getOwnPropertyDescriptor(proxy, "x");
    expect(desc).toBe(undefined);
  });

  test("falls back to target when no trap", () => {
    const target = { x: 1 };
    const proxy = new Proxy(target, {});
    const desc = Object.getOwnPropertyDescriptor(proxy, "x");
    expect(desc.value).toBe(1);
  });

  test("receives correct target and property", () => {
    const target = { foo: "bar" };
    let receivedTarget, receivedProp;
    const proxy = new Proxy(target, {
      getOwnPropertyDescriptor: (t, prop) => {
        receivedTarget = t;
        receivedProp = prop;
        return Object.getOwnPropertyDescriptor(t, prop);
      },
    });
    Object.getOwnPropertyDescriptor(proxy, "foo");
    expect(receivedTarget).toBe(target);
    expect(receivedProp).toBe("foo");
  });

  test("rejects a descriptor for a missing property on a non-extensible target", () => {
    const target = {};
    Object.preventExtensions(target);

    const proxy = new Proxy(target, {
      getOwnPropertyDescriptor: () => ({
        value: 1,
        writable: true,
        enumerable: true,
        configurable: true,
      }),
    });

    expect(() => Object.getOwnPropertyDescriptor(proxy, "x")).toThrow(TypeError);
  });

  test("applies non-extensible target invariants through nested proxies", () => {
    const target = {};
    Object.preventExtensions(target);

    const inner = new Proxy(target, {
      isExtensible() {
        return false;
      },
    });
    const outer = new Proxy(inner, {
      getOwnPropertyDescriptor() {
        return {
          value: 1,
          writable: true,
          enumerable: true,
          configurable: true,
        };
      },
    });

    expect(() => Object.getOwnPropertyDescriptor(outer, "x")).toThrow(TypeError);
  });

  test("rejects incompatible descriptors for non-configurable target properties", () => {
    const target = {};
    Object.defineProperty(target, "x", {
      value: 1,
      writable: false,
      enumerable: false,
      configurable: false,
    });

    const proxy = new Proxy(target, {
      getOwnPropertyDescriptor: () => ({
        value: 2,
        writable: false,
        enumerable: false,
        configurable: false,
      }),
    });

    expect(() => Object.getOwnPropertyDescriptor(proxy, "x")).toThrow(TypeError);
  });

  test("rejects non-configurable results for configurable target properties", () => {
    const target = {};
    Object.defineProperty(target, "x", {
      value: 1,
      writable: true,
      enumerable: false,
      configurable: true,
    });

    const proxy = new Proxy(target, {
      getOwnPropertyDescriptor: () => ({
        value: 1,
        writable: true,
        enumerable: false,
        configurable: false,
      }),
    });

    expect(() => Object.getOwnPropertyDescriptor(proxy, "x")).toThrow(TypeError);
  });

  test("rejects non-configurable non-writable results for writable target properties", () => {
    const target = {};
    Object.defineProperty(target, "x", {
      value: 1,
      writable: true,
      enumerable: false,
      configurable: false,
    });

    const proxy = new Proxy(target, {
      getOwnPropertyDescriptor: () => ({
        value: 1,
        writable: false,
        enumerable: false,
        configurable: false,
      }),
    });

    expect(() => Object.getOwnPropertyDescriptor(proxy, "x")).toThrow(TypeError);
  });

  test("applies descriptor invariants to symbol properties", () => {
    const key = Symbol("x");
    const target = {};
    Object.preventExtensions(target);

    const proxy = new Proxy(target, {
      getOwnPropertyDescriptor: () => ({
        value: 1,
        writable: true,
        enumerable: true,
        configurable: true,
      }),
    });

    expect(() => Object.getOwnPropertyDescriptor(proxy, key)).toThrow(TypeError);
  });
});

// ES2026 §10.5.5 steps 7-11: the trap result's type is checked before the
// target is read, IsExtensible(target) runs before ToPropertyDescriptor, and
// targetDesc is a copy of the target's property as step 8 read it. Guest code
// in either later step that deletes or redefines that property does not change
// the checks. Expected values are from Node.js 24.
describe("Proxy getOwnPropertyDescriptor reads the target's descriptor once", () => {
  const expected = { value: 2, writable: true, enumerable: true, configurable: true };

  test("a trap result accessor that deletes the target property", () => {
    const target = { x: 1 };
    const proxy = new Proxy(target, {
      getOwnPropertyDescriptor: () => ({
        get enumerable() {
          delete target.x;
          return true;
        },
        value: 2,
        writable: true,
        configurable: true,
      }),
    });

    expect(Object.getOwnPropertyDescriptor(proxy, "x")).toEqual(expected);
    expect(Object.hasOwn(target, "x")).toBe(false);
  });

  test("a trap result accessor that deletes the target's symbol property", () => {
    const key = Symbol("x");
    const target = { [key]: 1 };
    const proxy = new Proxy(target, {
      getOwnPropertyDescriptor: () => ({
        get enumerable() {
          delete target[key];
          return true;
        },
        value: 2,
        writable: true,
        configurable: true,
      }),
    });

    expect(Object.getOwnPropertyDescriptor(proxy, key)).toEqual(expected);
  });

  test("a trap result accessor that makes the target property non-configurable", () => {
    const target = { x: 1 };
    const proxy = new Proxy(target, {
      getOwnPropertyDescriptor: () => ({
        get enumerable() {
          Object.defineProperty(target, "x", { value: 3, writable: false, configurable: false });
          return true;
        },
        value: 2,
        writable: true,
        configurable: true,
      }),
    });

    expect(Object.getOwnPropertyDescriptor(proxy, "x")).toEqual(expected);
    expect(Object.getOwnPropertyDescriptor(target, "x").configurable).toBe(false);
  });

  test("a target proxy whose isExtensible trap deletes the property", () => {
    const inner = { x: 1 };
    const target = new Proxy(inner, {
      isExtensible: (t) => {
        delete inner.x;
        return Reflect.isExtensible(t);
      },
    });
    const proxy = new Proxy(target, {
      getOwnPropertyDescriptor: () => ({ value: 2, writable: true, enumerable: true, configurable: true }),
    });

    expect(Object.getOwnPropertyDescriptor(proxy, "x")).toEqual(expected);
  });

  test("a target proxy whose isExtensible trap deletes the symbol property", () => {
    const key = Symbol("x");
    const inner = { [key]: 1 };
    const target = new Proxy(inner, {
      isExtensible: (t) => {
        delete inner[key];
        return Reflect.isExtensible(t);
      },
    });
    const proxy = new Proxy(target, {
      getOwnPropertyDescriptor: () => ({ value: 2, writable: true, enumerable: true, configurable: true }),
    });

    expect(Object.getOwnPropertyDescriptor(proxy, key)).toEqual(expected);
  });

  test("asks IsExtensible before reading the trap result", () => {
    const log = [];
    const target = new Proxy({ x: 1 }, {
      isExtensible: (t) => {
        log.push("isExtensible");
        return Reflect.isExtensible(t);
      },
    });
    const proxy = new Proxy(target, {
      getOwnPropertyDescriptor: () => ({
        get enumerable() {
          log.push("enumerable");
          return true;
        },
        value: 2,
        writable: true,
        configurable: true,
      }),
    });

    Object.getOwnPropertyDescriptor(proxy, "x");
    expect(log).toEqual(["isExtensible", "enumerable"]);
  });

  test("rejects a non-object trap result before reading the target", () => {
    const log = [];
    const target = new Proxy({ x: 1 }, {
      getOwnPropertyDescriptor: (t, key) => {
        log.push("target");
        return Reflect.getOwnPropertyDescriptor(t, key);
      },
    });
    const proxy = new Proxy(target, { getOwnPropertyDescriptor: () => 1 });

    expect(() => Object.getOwnPropertyDescriptor(proxy, "x")).toThrow(TypeError);
    expect(log).toEqual([]);
  });
});
