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
});
