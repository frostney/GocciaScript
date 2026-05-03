describe("Proxy set trap", () => {
  test("intercepts property assignments", () => {
    const log = [];
    const target = {};
    const proxy = new Proxy(target, {
      set: (t, prop, value) => {
        log.push(prop + "=" + value);
        t[prop] = value;
        return true;
      },
    });
    proxy.x = 1;
    proxy.y = 2;
    expect(log).toEqual(["x=1", "y=2"]);
    expect(target.x).toBe(1);
    expect(target.y).toBe(2);
  });

  test("can validate property values", () => {
    const proxy = new Proxy(
      {},
      {
        set: (t, prop, value) => {
          if (typeof value !== "number") {
            throw new TypeError("Only numbers allowed");
          }
          t[prop] = value;
          return true;
        },
      }
    );
    proxy.x = 42;
    expect(proxy.x).toBe(42);
    expect(() => {
      proxy.y = "string";
    }).toThrow(TypeError);
  });

  test("throws TypeError when trap returns false", () => {
    const proxy = new Proxy(
      {},
      {
        set: () => false,
      }
    );
    expect(() => {
      proxy.x = 1;
    }).toThrow(TypeError);
  });

  test("falls back to target when no set trap", () => {
    const target = {};
    const proxy = new Proxy(target, {});
    proxy.x = 42;
    expect(target.x).toBe(42);
    expect(proxy.x).toBe(42);
  });

  test("receives correct arguments", () => {
    const target = {};
    let receivedArgs;
    const proxy = new Proxy(target, {
      set: (t, prop, value, receiver) => {
        receivedArgs = { target: t, prop, value, receiver };
        t[prop] = value;
        return true;
      },
    });
    proxy.foo = "bar";
    expect(receivedArgs.target).toBe(target);
    expect(receivedArgs.prop).toBe("foo");
    expect(receivedArgs.value).toBe("bar");
    expect(receivedArgs.receiver).toBe(proxy);
  });

  test("can implement logging", () => {
    const changes = [];
    const target = { x: 1 };
    const proxy = new Proxy(target, {
      set: (t, prop, value) => {
        changes.push({ prop, from: t[prop], to: value });
        t[prop] = value;
        return true;
      },
    });
    proxy.x = 2;
    proxy.y = 3;
    expect(changes.length).toBe(2);
    expect(changes[0].prop).toBe("x");
    expect(changes[0].from).toBe(1);
    expect(changes[0].to).toBe(2);
  });

  test("primitive property assignment uses boxed prototype [[Set]]", () => {
    const originalPrototype = Object.getPrototypeOf(Number.prototype);
    let count = 0;
    const proxy = new Proxy(
      {},
      {
        set: () => {
          count++;
          return true;
        },
      }
    );

    try {
      Object.setPrototypeOf(Number.prototype, proxy);
      0..test262 = null;
    } finally {
      Object.setPrototypeOf(Number.prototype, originalPrototype);
    }

    expect(count).toBe(1);
  });

  test("primitive compound property assignment uses boxed prototype [[Set]]", () => {
    const originalPrototype = Object.getPrototypeOf(Number.prototype);
    let count = 0;
    const proxy = new Proxy(
      {},
      {
        set: () => {
          count++;
          return true;
        },
      }
    );

    try {
      Object.setPrototypeOf(Number.prototype, proxy);
      0..test262 += 1;
    } finally {
      Object.setPrototypeOf(Number.prototype, originalPrototype);
    }

    expect(count).toBe(1);
  });
});
