describe("Proxy getPrototypeOf trap", () => {
  test("intercepts Object.getPrototypeOf", () => {
    const customProto = { custom: true };
    const proxy = new Proxy(
      {},
      {
        getPrototypeOf: () => customProto,
      }
    );
    expect(Object.getPrototypeOf(proxy)).toBe(customProto);
  });

  test("can return null", () => {
    const proxy = new Proxy(
      {},
      {
        getPrototypeOf: () => null,
      }
    );
    expect(Object.getPrototypeOf(proxy)).toBe(null);
  });

  test("falls back to target when no trap", () => {
    const proto = { base: true };
    const target = Object.create(proto);
    const proxy = new Proxy(target, {});
    expect(Object.getPrototypeOf(proxy)).toBe(proto);
  });

  test("receives correct target", () => {
    const target = {};
    let receivedTarget;
    const proxy = new Proxy(target, {
      getPrototypeOf: (t) => {
        receivedTarget = t;
        return Object.getPrototypeOf(t);
      },
    });
    Object.getPrototypeOf(proxy);
    expect(receivedTarget).toBe(target);
  });
});
