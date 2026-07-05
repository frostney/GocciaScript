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
