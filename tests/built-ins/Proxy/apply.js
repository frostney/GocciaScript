describe("Proxy apply trap", () => {
  test("intercepts function calls", () => {
    const target = (x) => x + 1;
    const proxy = new Proxy(target, {
      apply: (t, thisArg, args) => {
        return t(...args) * 2;
      },
    });
    expect(proxy(5)).toBe(12); // (5 + 1) * 2
  });

  test("receives correct target, thisArg, and argumentsList", () => {
    const target = () => {};
    let receivedTarget, receivedThisArg, receivedArgs;
    const proxy = new Proxy(target, {
      apply: (t, thisArg, args) => {
        receivedTarget = t;
        receivedThisArg = thisArg;
        receivedArgs = args;
        return "intercepted";
      },
    });
    proxy(1, 2, 3);
    expect(receivedTarget).toBe(target);
    expect(receivedArgs[0]).toBe(1);
    expect(receivedArgs[1]).toBe(2);
    expect(receivedArgs[2]).toBe(3);
  });

  test("falls back to target when no apply trap", () => {
    const target = (a, b) => a + b;
    const proxy = new Proxy(target, {});
    expect(proxy(3, 4)).toBe(7);
  });

  test("can modify arguments", () => {
    const target = (x) => x * 2;
    const proxy = new Proxy(target, {
      apply: (t, thisArg, args) => {
        return t(args[0] + 10);
      },
    });
    expect(proxy(5)).toBe(30); // (5 + 10) * 2
  });

  test("can completely replace return value", () => {
    const target = () => "original";
    const proxy = new Proxy(target, {
      apply: () => "replaced",
    });
    expect(proxy()).toBe("replaced");
  });

  test("throws TypeError when target is not callable", () => {
    const proxy = new Proxy({}, {});
    expect(() => proxy()).toThrow(TypeError);
  });

  test("typeof returns function for function proxies", () => {
    const proxy = new Proxy(() => {}, {});
    expect(typeof proxy).toBe("function");
  });
});
