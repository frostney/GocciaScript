describe("Proxy constructor", () => {
  test("creates a proxy with target and handler", () => {
    const target = { x: 1, y: 2 };
    const handler = {};
    const proxy = new Proxy(target, handler);
    expect(proxy.x).toBe(1);
    expect(proxy.y).toBe(2);
  });

  test("throws TypeError when called without new", () => {
    expect(() => Proxy({}, {})).toThrow(TypeError);
  });

  test("throws TypeError when target is not an object", () => {
    expect(() => new Proxy(42, {})).toThrow(TypeError);
    expect(() => new Proxy("string", {})).toThrow(TypeError);
    expect(() => new Proxy(null, {})).toThrow(TypeError);
    expect(() => new Proxy(undefined, {})).toThrow(TypeError);
    expect(() => new Proxy(true, {})).toThrow(TypeError);
  });

  test("throws TypeError when handler is not an object", () => {
    expect(() => new Proxy({}, 42)).toThrow(TypeError);
    expect(() => new Proxy({}, "string")).toThrow(TypeError);
    expect(() => new Proxy({}, null)).toThrow(TypeError);
    expect(() => new Proxy({}, undefined)).toThrow(TypeError);
  });

  test("throws TypeError with fewer than 2 arguments", () => {
    expect(() => new Proxy({})).toThrow(TypeError);
    expect(() => new Proxy()).toThrow(TypeError);
  });

  test("revocable helper is not constructable", () => {
    expect(() => new Proxy.revocable({}, {})).toThrow(TypeError);
  });

  test("proxy with empty handler passes through all operations", () => {
    const target = { a: 1, b: 2, c: 3 };
    const proxy = new Proxy(target, {});
    expect(proxy.a).toBe(1);
    proxy.d = 4;
    expect(target.d).toBe(4);
    expect(delete proxy.c).toBe(true);
    expect(target.c).toBe(undefined);
  });

  test("proxy of an array", () => {
    const target = [1, 2, 3];
    const proxy = new Proxy(target, {});
    expect(proxy.length).toBe(3);
    expect(proxy[0]).toBe(1);
  });

  test("proxy of a function", () => {
    const target = (x) => x * 2;
    const proxy = new Proxy(target, {});
    expect(proxy(5)).toBe(10);
    expect(typeof proxy).toBe("function");
  });

  test("nested proxies", () => {
    const target = { value: 42 };
    const inner = new Proxy(target, {
      get: (t, p) => {
        if (p === "inner") return true;
        return t[p];
      },
    });
    const outer = new Proxy(inner, {
      get: (t, p) => {
        if (p === "outer") return true;
        return t[p];
      },
    });
    expect(outer.value).toBe(42);
    expect(outer.outer).toBe(true);
    expect(outer.inner).toBe(true);
  });
});
