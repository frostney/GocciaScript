describe("Proxy defineProperty trap", () => {
  test("intercepts Object.defineProperty", () => {
    const log = [];
    const target = {};
    const proxy = new Proxy(target, {
      defineProperty: (t, prop, descriptor) => {
        log.push(prop);
        Object.defineProperty(t, prop, descriptor);
        return true;
      },
    });
    Object.defineProperty(proxy, "x", { value: 42 });
    expect(log).toEqual(["x"]);
    expect(target.x).toBe(42);
  });

  test("can prevent property definition", () => {
    const proxy = new Proxy(
      {},
      {
        defineProperty: () => false,
      }
    );
    expect(() => Object.defineProperty(proxy, "x", { value: 1 })).toThrow(
      TypeError
    );
  });

  test("falls back to target when no trap", () => {
    const target = {};
    const proxy = new Proxy(target, {});
    Object.defineProperty(proxy, "x", {
      value: 42,
      writable: true,
      enumerable: true,
      configurable: true,
    });
    expect(target.x).toBe(42);
  });

  test("receives correct descriptor", () => {
    let receivedDescriptor;
    const proxy = new Proxy(
      {},
      {
        defineProperty: (t, prop, desc) => {
          receivedDescriptor = desc;
          return true;
        },
      }
    );
    Object.defineProperty(proxy, "x", {
      value: 123,
      writable: false,
      enumerable: true,
      configurable: false,
    });
    expect(receivedDescriptor.value).toBe(123);
    expect(receivedDescriptor.writable).toBe(false);
    expect(receivedDescriptor.enumerable).toBe(true);
    expect(receivedDescriptor.configurable).toBe(false);
  });

  test("preserves omitted descriptor fields", () => {
    let receivedDescriptor;
    const proxy = new Proxy(
      {},
      {
        defineProperty: (t, prop, desc) => {
          receivedDescriptor = desc;
          return true;
        },
      }
    );

    Reflect.defineProperty(proxy, "x", { value: 1 });

    expect(Object.keys(receivedDescriptor)).toEqual(["value"]);
    expect(receivedDescriptor.value).toBe(1);
    expect(receivedDescriptor.writable).toBeUndefined();
    expect(receivedDescriptor.enumerable).toBeUndefined();
    expect(receivedDescriptor.configurable).toBeUndefined();
  });

  test("Object.defineProperty invokes trap for symbol keys", () => {
    const key = Symbol("x");
    const log = [];
    const target = {};
    const proxy = new Proxy(target, {
      defineProperty: (object, prop, desc) => {
        log.push(prop);
        Object.defineProperty(object, prop, desc);
        return true;
      },
    });

    Object.defineProperty(proxy, key, {
      value: 42,
      configurable: true,
    });

    expect(log.length).toBe(1);
    expect(log[0]).toBe(key);
    expect(target[key]).toBe(42);
    expect(proxy[key]).toBe(42);
  });

  test("Object.defineProperty throws when symbol-key trap returns false", () => {
    const key = Symbol("blocked");
    const proxy = new Proxy(
      {},
      {
        defineProperty: () => false,
      }
    );

    expect(() => {
      Object.defineProperty(proxy, key, { value: 1 });
    }).toThrow(TypeError);
  });
});
