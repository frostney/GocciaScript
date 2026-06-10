describe("Proxy ownKeys trap", () => {
  test("intercepts Object.keys", () => {
    const target = { a: 1, b: 2, secret: 3 };
    const proxy = new Proxy(target, {
      ownKeys: (t) => {
        return ["a", "b"];
      },
    });
    expect(Object.keys(proxy)).toEqual(["a", "b"]);
  });

  test("intercepts Object.getOwnPropertyNames", () => {
    const target = { x: 1, y: 2 };
    const proxy = new Proxy(target, {
      ownKeys: () => ["x", "y", "z"],
    });
    const names = Object.getOwnPropertyNames(proxy);
    expect(names).toEqual(["x", "y", "z"]);
  });

  test("falls back to target when no ownKeys trap", () => {
    const target = { a: 1, b: 2 };
    const proxy = new Proxy(target, {});
    expect(Object.keys(proxy)).toEqual(["a", "b"]);
  });

  test("receives correct target", () => {
    const target = { x: 1 };
    let receivedTarget;
    const proxy = new Proxy(target, {
      ownKeys: (t) => {
        receivedTarget = t;
        return Object.keys(t);
      },
    });
    Object.keys(proxy);
    expect(receivedTarget).toBe(target);
  });

  test("can add virtual keys", () => {
    const proxy = new Proxy(
      {},
      {
        ownKeys: () => ["virtual1", "virtual2"],
      }
    );
    const keys = Object.getOwnPropertyNames(proxy);
    expect(keys).toEqual(["virtual1", "virtual2"]);
  });

  test("rejects duplicate symbol entries even when caller filters strings", () => {
    const symbol = Symbol("duplicate");
    const proxy = new Proxy({}, {
      ownKeys: () => [symbol, symbol],
    });

    expect(() => Object.keys(proxy)).toThrow(TypeError);
  });

  test("Reflect.ownKeys preserves symbol entries and trap order", () => {
    const symbol = Symbol("s");
    const proxy = new Proxy({}, {
      ownKeys: () => [symbol, "a"],
      getOwnPropertyDescriptor() {
        return {
          configurable: true,
          enumerable: true,
          value: 1,
          writable: true,
        };
      },
    });

    const keys = Reflect.ownKeys(proxy);
    expect(keys.length).toBe(2);
    expect(keys[0]).toBe(symbol);
    expect(keys[1]).toBe("a");
  });

  test("Object.getOwnPropertySymbols observes proxy ownKeys symbols", () => {
    const symbol = Symbol("visible");
    const proxy = new Proxy({}, {
      ownKeys: () => ["a", symbol],
      getOwnPropertyDescriptor() {
        return {
          configurable: true,
          enumerable: true,
          value: 1,
          writable: true,
        };
      },
    });

    expect(Object.getOwnPropertySymbols(proxy)).toEqual([symbol]);
  });
});
