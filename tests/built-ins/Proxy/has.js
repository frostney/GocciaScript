describe("Proxy has trap", () => {
  test("intercepts in operator", () => {
    const proxy = new Proxy(
      {},
      {
        has: (t, prop) => {
          return prop === "magic";
        },
      }
    );
    expect("magic" in proxy).toBe(true);
    expect("other" in proxy).toBe(false);
  });

  test("receives correct target and property", () => {
    const target = { x: 1 };
    let receivedTarget, receivedProp;
    const proxy = new Proxy(target, {
      has: (t, prop) => {
        receivedTarget = t;
        receivedProp = prop;
        return prop in t;
      },
    });
    "x" in proxy;
    expect(receivedTarget).toBe(target);
    expect(receivedProp).toBe("x");
  });

  test("falls back to target when no has trap", () => {
    const target = { a: 1, b: 2 };
    const proxy = new Proxy(target, {});
    expect("a" in proxy).toBe(true);
    expect("c" in proxy).toBe(false);
  });

  test("can hide properties", () => {
    const target = { secret: "hidden", public: "visible" };
    const proxy = new Proxy(target, {
      has: (t, prop) => {
        if (prop === "secret") return false;
        return prop in t;
      },
    });
    expect("secret" in proxy).toBe(false);
    expect("public" in proxy).toBe(true);
  });

  test("can pretend properties exist", () => {
    const proxy = new Proxy(
      {},
      {
        has: () => true,
      }
    );
    expect("anything" in proxy).toBe(true);
    expect("doesNotExist" in proxy).toBe(true);
  });
});
