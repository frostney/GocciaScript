describe("Proxy preventExtensions trap", () => {
  test("intercepts Object.preventExtensions", () => {
    let intercepted = false;
    const proxy = new Proxy(
      {},
      {
        preventExtensions: (t) => {
          intercepted = true;
          Object.preventExtensions(t);
          return true;
        },
      }
    );
    Object.preventExtensions(proxy);
    expect(intercepted).toBe(true);
  });

  test("throws when trap returns false", () => {
    const proxy = new Proxy(
      {},
      {
        preventExtensions: () => false,
      }
    );
    expect(() => Object.preventExtensions(proxy)).toThrow(TypeError);
  });

  test("falls back to target when no trap", () => {
    const target = {};
    const proxy = new Proxy(target, {});
    Object.preventExtensions(proxy);
    expect(Object.isExtensible(target)).toBe(false);
  });

  test("receives correct target", () => {
    const target = {};
    let receivedTarget;
    const proxy = new Proxy(target, {
      preventExtensions: (t) => {
        receivedTarget = t;
        Object.preventExtensions(t);
        return true;
      },
    });
    Object.preventExtensions(proxy);
    expect(receivedTarget).toBe(target);
  });
});
