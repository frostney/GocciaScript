describe("Proxy setPrototypeOf trap", () => {
  test("intercepts Object.setPrototypeOf", () => {
    let intercepted = false;
    const proxy = new Proxy(
      {},
      {
        setPrototypeOf: (t, proto) => {
          intercepted = true;
          return true;
        },
      }
    );
    Object.setPrototypeOf(proxy, {});
    expect(intercepted).toBe(true);
  });

  test("receives correct target and proto", () => {
    const target = {};
    const newProto = { test: true };
    let receivedTarget, receivedProto;
    const proxy = new Proxy(target, {
      setPrototypeOf: (t, proto) => {
        receivedTarget = t;
        receivedProto = proto;
        return true;
      },
    });
    Object.setPrototypeOf(proxy, newProto);
    expect(receivedTarget).toBe(target);
    expect(receivedProto).toBe(newProto);
  });

  test("falls back to target when no trap", () => {
    const target = {};
    const newProto = { added: true };
    const proxy = new Proxy(target, {});
    Object.setPrototypeOf(proxy, newProto);
    expect(Object.getPrototypeOf(target)).toBe(newProto);
  });
});
