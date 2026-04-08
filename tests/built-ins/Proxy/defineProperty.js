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
});
