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
});
