describe("Proxy isExtensible trap", () => {
  test("intercepts Object.isExtensible", () => {
    const proxy = new Proxy(
      {},
      {
        isExtensible: () => false,
      }
    );
    expect(Object.isExtensible(proxy)).toBe(false);
  });

  test("can return true", () => {
    const proxy = new Proxy(
      {},
      {
        isExtensible: () => true,
      }
    );
    expect(Object.isExtensible(proxy)).toBe(true);
  });

  test("falls back to target when no trap", () => {
    const target = {};
    const proxy = new Proxy(target, {});
    expect(Object.isExtensible(proxy)).toBe(true);
    Object.preventExtensions(target);
    expect(Object.isExtensible(proxy)).toBe(false);
  });

  test("receives correct target", () => {
    const target = {};
    let receivedTarget;
    const proxy = new Proxy(target, {
      isExtensible: (t) => {
        receivedTarget = t;
        return Object.isExtensible(t);
      },
    });
    Object.isExtensible(proxy);
    expect(receivedTarget).toBe(target);
  });
});
