describe("Proxy.revocable", () => {
  test("returns an object with proxy and revoke", () => {
    const { proxy, revoke } = Proxy.revocable({}, {});
    expect(typeof revoke).toBe("function");
    expect(proxy).toBeDefined();
  });

  test("proxy works before revocation", () => {
    const target = { x: 1, y: 2 };
    const { proxy, revoke } = Proxy.revocable(target, {});
    expect(proxy.x).toBe(1);
    expect(proxy.y).toBe(2);
  });

  test("all operations throw after revocation", () => {
    const target = { x: 1 };
    const { proxy, revoke } = Proxy.revocable(target, {});
    revoke();
    expect(() => proxy.x).toThrow(TypeError);
    expect(() => {
      proxy.x = 2;
    }).toThrow(TypeError);
    expect(() => "x" in proxy).toThrow(TypeError);
    expect(() => delete proxy.x).toThrow(TypeError);
  });

  test("revoke can be called multiple times", () => {
    const { proxy, revoke } = Proxy.revocable({}, {});
    revoke();
    revoke(); // second call is a no-op
    expect(() => proxy.x).toThrow(TypeError);
  });

  test("revocable proxy with handler traps", () => {
    const log = [];
    const { proxy, revoke } = Proxy.revocable(
      {},
      {
        get: (t, p) => {
          log.push("get:" + p);
          return 42;
        },
      }
    );
    expect(proxy.x).toBe(42);
    expect(log).toEqual(["get:x"]);
    revoke();
    expect(() => proxy.x).toThrow(TypeError);
  });

  test("throws TypeError for non-object arguments", () => {
    expect(() => Proxy.revocable(42, {})).toThrow(TypeError);
    expect(() => Proxy.revocable({}, 42)).toThrow(TypeError);
    expect(() => Proxy.revocable(null, {})).toThrow(TypeError);
  });
});

describe.runIf(typeof Goccia !== "undefined")("Proxy.revocable across a collection", () => {
  // Fresh revocable proxies reuse the memory a wrongly collected revoker
  // leaves behind, so a stale revoke would revoke one of them instead.
  const churn = () => Array.from({ length: 2000 }, () => Proxy.revocable({}, {}));

  test("revoke still revokes after the result object is collected", () => {
    const holder = {};
    const makeRevoke = () => {
      const result = Proxy.revocable({ value: 1 }, {});
      holder.proxy = result.proxy;
      return result.revoke;
    };
    const revoke = makeRevoke();
    Goccia.gc();
    const others = churn();
    revoke();
    expect(() => holder.proxy.value).toThrow(TypeError);
    expect(others.every(({ proxy }) => typeof proxy === "object")).toBe(true);
  });
});
