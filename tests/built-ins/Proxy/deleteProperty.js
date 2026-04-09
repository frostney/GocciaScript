describe("Proxy deleteProperty trap", () => {
  test("intercepts delete operator", () => {
    const log = [];
    const target = { x: 1, y: 2 };
    const proxy = new Proxy(target, {
      deleteProperty: (t, prop) => {
        log.push(prop);
        delete t[prop];
        return true;
      },
    });
    delete proxy.x;
    expect(log).toEqual(["x"]);
    expect(target.x).toBe(undefined);
    expect("x" in target).toBe(false);
  });

  test("can prevent deletion", () => {
    const target = { x: 1 };
    const proxy = new Proxy(target, {
      deleteProperty: () => false,
    });
    expect(() => delete proxy.x).toThrow(TypeError);
    expect(target.x).toBe(1);
  });

  test("falls back to target when no deleteProperty trap", () => {
    const target = { a: 1, b: 2 };
    const proxy = new Proxy(target, {});
    delete proxy.a;
    expect("a" in target).toBe(false);
    expect(target.b).toBe(2);
  });

  test("receives correct target and property", () => {
    const target = { foo: "bar" };
    let receivedTarget, receivedProp;
    const proxy = new Proxy(target, {
      deleteProperty: (t, prop) => {
        receivedTarget = t;
        receivedProp = prop;
        delete t[prop];
        return true;
      },
    });
    delete proxy.foo;
    expect(receivedTarget).toBe(target);
    expect(receivedProp).toBe("foo");
  });
});
