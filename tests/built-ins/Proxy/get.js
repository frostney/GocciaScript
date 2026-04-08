describe("Proxy get trap", () => {
  test("intercepts property reads", () => {
    const target = { x: 1 };
    const proxy = new Proxy(target, {
      get: (t, prop, receiver) => {
        return t[prop] * 10;
      },
    });
    expect(proxy.x).toBe(10);
  });

  test("receives correct target, property, and receiver", () => {
    const target = { x: 42 };
    let receivedTarget, receivedProp, receivedReceiver;
    const proxy = new Proxy(target, {
      get: (t, prop, receiver) => {
        receivedTarget = t;
        receivedProp = prop;
        receivedReceiver = receiver;
        return t[prop];
      },
    });
    proxy.x;
    expect(receivedTarget).toBe(target);
    expect(receivedProp).toBe("x");
    expect(receivedReceiver).toBe(proxy);
  });

  test("can return any value regardless of target", () => {
    const proxy = new Proxy(
      {},
      {
        get: () => "intercepted",
      }
    );
    expect(proxy.anything).toBe("intercepted");
    expect(proxy.doesNotExist).toBe("intercepted");
  });

  test("falls back to target when no get trap", () => {
    const target = { a: 1, b: "hello" };
    const proxy = new Proxy(target, {});
    expect(proxy.a).toBe(1);
    expect(proxy.b).toBe("hello");
    expect(proxy.nonexistent).toBe(undefined);
  });

  test("intercepts computed property access", () => {
    const target = { foo: "bar" };
    const proxy = new Proxy(target, {
      get: (t, prop) => {
        return prop + "!";
      },
    });
    const key = "foo";
    expect(proxy[key]).toBe("foo!");
  });

  test("intercepts method access", () => {
    const target = {
      greet: (name) => "Hello, " + name,
    };
    const proxy = new Proxy(target, {
      get: (t, prop) => {
        if (prop === "greet") {
          return (name) => "Hi, " + name;
        }
        return t[prop];
      },
    });
    expect(proxy.greet("World")).toBe("Hi, World");
  });

  test("can implement default values", () => {
    const proxy = new Proxy(
      {},
      {
        get: (t, prop) => {
          return prop in t ? t[prop] : "default";
        },
      }
    );
    expect(proxy.missing).toBe("default");
  });

  test("can implement negative array indices", () => {
    const arr = [10, 20, 30, 40, 50];
    const proxy = new Proxy(arr, {
      get: (t, prop) => {
        const index = Number(prop);
        if (Number.isInteger(index) && index < 0) {
          return t[t.length + index];
        }
        return t[prop];
      },
    });
    expect(proxy[0]).toBe(10);
    expect(proxy[-1]).toBe(50);
    expect(proxy[-2]).toBe(40);
  });
});
